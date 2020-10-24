;;; raven.el --- Efficient selection and navigation -*- lexical-binding: t -*-

;;; Commentary:

;; Utilities for selecting from lists of candidates in the minibuffer.
;; Stupidly simple.
;; Multiple sources, minus the bloat.

;;; Code:
(require 'dash)
(require 'f)
(require 'subr-x)
(require 'recentf)
(when (featurep 'evil)
  (require 'evil))

(defgroup raven nil
  "Efficient selection and navigation."
  :group 'convenience)

(defgroup raven-faces nil
  "Faces for `raven'."
  :group 'raven
  :group 'faces)

(defface raven-source-name
  '((default :underline t :inherit bold)
    (((class color) (background dark)) :foreground "white"))
  "Face used to highlight source names.")

(defface raven-highlight
  '((t :inherit highlight))
  "Face used to highlight the current selection.")

(defvar raven-minibuffer-lines 20
  "Number of lines to display in the minibuffer.")

(defvar raven-exit-hook nil
  "Hook run when exiting minibuffer selection.")

(defvar raven--sources '())
(defvar raven--last nil)
(defvar raven--matching '())
(defvar raven--source 0)
(defvar raven--index 0)
(defvar raven--action nil)
(defvar raven--result nil)

(defvar raven-minibuffer-map (make-sparse-keymap))
(define-key raven-minibuffer-map (kbd "\\") (lambda () (interactive) nil))
(define-key raven-minibuffer-map (kbd "C-g") 'raven-quit)
(define-key raven-minibuffer-map (kbd "C-c") 'raven-quit)
(define-key raven-minibuffer-map (kbd "<return>") 'raven-do)
(define-key raven-minibuffer-map (kbd "<backtab>") 'raven-previous)
(define-key raven-minibuffer-map (kbd "<tab>") 'raven-next)
(define-key raven-minibuffer-map (kbd "<up>") 'raven-previous)
(define-key raven-minibuffer-map (kbd "<down>") 'raven-next)
(define-key raven-minibuffer-map (kbd "<prior>") 'raven-previous-source)
(define-key raven-minibuffer-map (kbd "<next>") 'raven-next-source)

(when (fboundp 'evil-define-key)
  (evil-define-key 'normal raven-minibuffer-map
    (kbd "k") 'raven-previous
    (kbd "j") 'raven-next
    (kbd "K") 'raven-previous-source
    (kbd "J") 'raven-next-source))

(defun raven-minibuffer-line (str)
  "Write STR to the minibuffer."
  (goto-char (point-max))
  (insert (concat "\n" str)))

(defun raven-minibuffer-line-face (str face)
  "Write STR to the minibuffer in FACE."
  (let ((before (point)))
    (raven-minibuffer-line str)
    (goto-char before)
    (forward-line)
    (put-text-property (line-beginning-position) (point-max) 'face face)))

(defun raven-minibuffer-clear ()
  "Clear minibuffer."
  (save-excursion
    (goto-char (minibuffer-prompt-end))
    (delete-region (line-end-position) (point-max))))

(defun raven-minibuffer-input ()
  "Get current minibuffer input."
  (buffer-substring-no-properties
   (minibuffer-prompt-end)
   (line-end-position)))

(cl-defstruct (raven-candidate (:constructor raven-candidate--create)
                               (:copier nil)
                               (:conc-name raven-candidate--))
  type (display nil :type string) face value action)

(cl-defun raven-candidate-create
    (display &key
             value
             (type 'normal)
             (face 'default)
             (action '()))
  "Create a candidate.
DISPLAY is the string to display (using FACE) / match against.
VALUE is the value to pass to actions when the candidate is selected.
TYPE is either normal or dummy - dummy candidates always appear in the
results list regardless of the input pattern.
ACTION is an alist mapping keybindings to candidate-specific actions."
  (raven-candidate--create
   :type type
   :display display
   :face face
   :value (if value value display)
   :action action))

(defun raven-candidate-display (candidate)
  "Return the display string for CANDIDATE."
  (cond ((raven-candidate-p candidate) (raven-candidate--display candidate))
        (t candidate)))

(defun raven-candidate-value (candidate)
  "Return the value of CANDIDATE."
  (cond ((raven-candidate-p candidate) (raven-candidate--value candidate))
        (t candidate)))

(defun raven-candidate-type (candidate)
  "Return the candidate type of CANDIDATE."
  (cond ((raven-candidate-p candidate) (raven-candidate--type candidate))
        (t 'normal)))

(defun raven-candidate-face (candidate)
  "Return the display face for CANDIDATE."
  (cond ((raven-candidate-p candidate) (raven-candidate--face candidate))
        (t 'default)))

(defun raven-candidate-action (candidate)
  "Return the actions for CANDIDATE."
  (cond ((raven-candidate-p candidate) (raven-candidate--action candidate))
        (t '())))

(defun raven-candidate-display-string (candidate)
  "Return the display of CANDIDATE as a string."
  (let ((display (raven-candidate-display candidate)))
    (cond ((stringp display) display)
          (t (error "Invalid candidate display %s for candidate %s (of type %s)"
                    display candidate (type-of display))))))

(defun raven-highlight-candidate (candidate)
  "Return a copy of CANDIDATE with the face set to raven-highlight."
  (raven-candidate--create
   :type (raven-candidate-type candidate)
   :display (raven-candidate-display candidate)
   :face 'raven-highlight
   :value (raven-candidate-value candidate)
   :action (raven-candidate-action candidate)))

(defun raven-match (candidate regex)
  "Determine whether CANDIDATE is a match for REGEX."
  (let ((type (raven-candidate-type candidate)))
    (cond ((eq 'dummy type) t)
          (t (string-match-p regex (raven-candidate-display-string candidate))))))

(cl-defstruct (raven-source (:constructor raven-source--create)
                            (:copier nil))
  name candidates actions keymap)

(defun raven-action-function (action)
  "Return the function associated with ACTION."
  (if (functionp action)
      action
    (cdr action)))

(defun raven-actions-keymap (actions)
  "Return a keymap for ACTIONS."
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap raven-minibuffer-map)
    (mapc
     (lambda (a)
       (unless (functionp a)
         (define-key keymap (car a)
           (lambda () (interactive)
             (raven-do (cdr a))))))
     actions)
    keymap))

(cl-defun raven-source-create (name &key candidates (actions '()))
  "Create a new source named NAME with the given CANDIDATES and ACTIONS.

CANDIDATES is either:
- A list of candidates
- A function returning a list of candidates given a regex
ACTIONS is a list of actions, which can be:
- functions taking candidate values as arguments
- pairs of key strings and such functions"
  (raven-source--create
   :name name
   :candidates
   (if (functionp candidates) candidates
     (--map (if (raven-candidate-p it) it (raven-candidate-create it))
            candidates))
   :actions actions
   :keymap (raven-actions-keymap actions)))

(defun raven-matching-candidates (candidates pattern)
  "Return the candidates in CANDIDATES matching PATTERN."
  (cond ((functionp candidates) (funcall candidates pattern))
        (t (let ((regex (raven-pattern-regex pattern)))
             (--filter (raven-match it regex) candidates)))))

(defun raven-filter-source (source pattern)
  "Return a copy of SOURCE including only the candidates matching PATTERN."
  (raven-source--create
   :name (raven-source-name source)
   :candidates (raven-matching-candidates (raven-source-candidates source) pattern)
   :actions (raven-source-actions source)
   :keymap (raven-source-keymap source)))

(defun raven-pattern-regex (pattern)
  "Convert PATTERN into a regular expression."
  (apply #'string-join
         (--map (concat "\\(" it "\\)") (split-string pattern))
         '(".*")))

(defun raven-matching-sources (sources pattern)
  "Return the sources in SOURCES matching PATTERN."
  (let* ((matches (--map (raven-filter-source it pattern) sources)))
    (-filter #'raven-source-candidates matches)))

(defun raven-display-source (source)
  "Display SOURCE."
  (when source
    (raven-minibuffer-line-face (raven-source-name source) 'raven-source-name)
    (--map (raven-minibuffer-line-face
            (raven-candidate-display-string it)
            (raven-candidate-face it))
           (raven-source-candidates source))))

(defun raven-nearby (sources)
  "Filter SOURCES to only include candidates close to the selected candidate."
  (let* ((adjacent
          (--map
           (cond ((and (< (cdr it) (+ raven--source raven-minibuffer-lines))
                       (> (cdr it) raven--source))
                  (cons (car it) 'g))
                 ((= (cdr it) raven--source)
                  (cons (car it) 'e))
                 (t nil))
           (-zip-pair sources (number-sequence 0 (length sources))))))
    (--map
     (when it
       (let* ((candidates (raven-source-candidates (car it))))
         (raven-source--create
          :name (raven-source-name (car it))
          :candidates
          (cond ((eq (cdr it) 'g)
                 (-take raven-minibuffer-lines candidates))
                (t
                 (cl-loop for i from (max (- raven--index
                                             (- (/ raven-minibuffer-lines 2) 1))
                                          0)
                          for j in (-take
                                    raven-minibuffer-lines
                                    (-drop
                                     (- raven--index
                                        (- (/ raven-minibuffer-lines 2) 1))
                                     candidates))
                          collect (if (= i raven--index)
                                      (raven-highlight-candidate j)
                                    j))))
          :actions (raven-source-actions (car it))
          :keymap (raven-source-keymap (car it)))))
     adjacent)))

(defun raven-update-transient-map ()
  "Update the transient keymap to match the current source."
  (let ((source (car (nthcdr raven--source raven--matching))))
    (when source
      (set-transient-map (raven-source-keymap source)))))

(defun raven-minibuffer-render ()
  "Draw matching candidates to minibuffer."
  (save-excursion
    (let ((pattern (raven-minibuffer-input)))
      ;; Check if the pattern is valid.
      (condition-case err
          (string-match-p pattern "")
        (invalid-regexp
         ;; Nope, it's not.  Probably the user typed \ in order to fix a file
         ;; extension like entering raven\.el.  Print the error and simply use
         ;; the old pattern until it becomes valid again.
         (minibuffer-error-function err "" 'raven-minibuffer-render)
         (setq pattern raven--last)))
      (unless (string= pattern raven--last)
        (setq raven--last pattern
              raven--index 0
              raven--source 0
              raven--matching (raven-matching-sources
                               raven--sources
                               pattern))))
    (-map #'raven-display-source (raven-nearby raven--matching))
    (goto-char (minibuffer-prompt-end))
    (put-text-property (line-end-position) (point-max) 'readonly t))
  (raven-update-transient-map))

(defun raven-minibuffer-setup (initial)
  "Ready minibuffer for completion with INITIAL as initial input."
  (add-hook 'pre-command-hook 'raven-minibuffer-clear nil t)
  (add-hook 'post-command-hook 'raven-minibuffer-render nil t)
  (setq-local max-mini-window-height raven-minibuffer-lines)
  (when initial
    (save-excursion
      (minibuffer-prompt-end)
      (insert initial)))
  (end-of-line)
  (raven-update-transient-map)
  (when (fboundp 'evil-insert-state)
    (evil-insert-state)))

(defun raven-previous-source ()
  "Move to the previous source."
  (interactive)
  (setq raven--index 0)
  (setq raven--source (if (= raven--source 0)
                          (- (length raven--matching) 1)
                        (- raven--source 1))))

(defun raven-next-source ()
  "Move to the next source."
  (interactive)
  (setq raven--index 0)
  (setq raven--source (% (+ raven--source 1) (length raven--matching))))

(defun raven-previous ()
  "Move to the previous candidate."
  (interactive)
  (let* ((new-source-index (if (= raven--source 0)
                               (- (length raven--matching) 1)
                             (- raven--source 1)))
         (source (car (nthcdr new-source-index raven--matching))))
    (setq raven--index (- raven--index 1))
    (when (< raven--index 0)
      (setq raven--index (- (length (raven-source-candidates source)) 1)
            raven--source new-source-index))))

(defun raven-next ()
  "Move to the next candidate."
  (interactive)
  (let* ((source (car (nthcdr raven--source raven--matching))))
    (setq raven--index (+ raven--index 1))
    (when (= raven--index (length (raven-source-candidates source)))
      (setq raven--index 0
            raven--source (% (+ raven--source 1) (length raven--matching))))))

(defun raven-quit ()
  "Quit the selection interface without running an action."
  (interactive)
  (run-hooks 'raven-exit-hook)
  (keyboard-escape-quit))

(defun raven-do (&optional action-function)
  "Act upon selected candidate.
If ACTION-FUNCTION is given use it, otherwise use the first action for the candidate."
  (interactive)
  (if (null raven--matching)
      (progn
        (setq raven--action (lambda (x) x)
              raven--result nil))
    (progn
      (let* ((source (car (nthcdr raven--source raven--matching)))
             (candidate (car (nthcdr raven--index (raven-source-candidates source)))))
        (setq raven--action (cond (action-function
                                   action-function)
                                  ((raven-candidate-action candidate)
                                   (raven-candidate-action candidate))
                                  (t
                                   (let ((actions (raven-source-actions source)))
                                     (if actions
                                         (raven-action-function (car actions))
                                       (lambda (x) x)))))
              raven--result (raven-candidate-value candidate)))))
  (run-hooks 'raven-exit-hook)
  (exit-minibuffer))

;;;###autoload
(cl-defun raven (sources &key prompt initial)
  "Select a candidate and run an action using SOURCES.
Display PROMPT as the prompt, or \"pattern: \" if not given.
Use INITIAL as the initial input."
  (setq raven--sources sources
        raven--last nil
        raven--matching (raven-matching-sources sources "")
        raven--source 0
        raven--index 0
        raven--action nil
        raven--result nil)
  (let ((inhibit-message t))
    (minibuffer-with-setup-hook
        (apply-partially 'raven-minibuffer-setup initial)
      (read-from-minibuffer (or prompt "pattern: ") nil raven-minibuffer-map)))
  (funcall raven--action raven--result))

;;;###autoload
(defun raven-completing-read (prompt collection &optional predicate require-match
                                     initial-input hist def inherit-input-method)
  "Replacement for `completing-read'.
PROMPT, COLLECTION, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD have the same meaning as in `completing-read'."
  (ignore predicate)
  (or
   (cond ((functionp collection)
          (read-string prompt initial-input hist def inherit-input-method))
         ((hash-table-p collection)
          (raven (list (raven-source-create "Completions" :candidates (hash-table-keys collection)))
                 :prompt prompt
                 :initial initial-input))
         ((obarrayp collection)
          (let ((candidates (list)))
            (mapatoms (lambda (x) (push (raven-candidate-create (symbol-name x)) candidates)) collection)
            (raven (list (raven-source-create "Completions" :candidates candidates))
                   :prompt prompt
                   :initial initial-input)))
         (t (raven (cons (raven-source-create
                          "Completions"
                          :candidates
                          (--map (if (consp it)
                                     (raven-candidate-create (car it))
                                   it)
                                 collection))
                         (if require-match
                             '()
                           (list
                            (raven-source-create
                             "Other"
                             :candidates
                             (list (raven-candidate-create
                                    "Specify"
                                    :type 'dummy
                                    :action (lambda (_) (raven-input))))))))
                   :prompt prompt
                   :initial initial-input)))
   (raven-input)))

(defun raven-input () "Return last minibuffer input." raven--last)

(defvar raven-extended-command-actions
  (list (lambda (c)
          (add-to-list 'extended-command-history c)
          (command-execute (intern-soft c)))
        (cons (kbd "C-h") (lambda (c) (describe-function (intern-soft c))))))

(defmacro raven--make-obarray-candidates-fn (pred)
  (let ((pattern (gensym "pattern"))
        (prefix  (gensym "prefix"))
        (regexp  (gensym "regexp"))
        (item    (gensym "item")))
    `(lambda (,pattern)
       (let ((,prefix (if (string-match "\\([^ \f\t\n\r\v]+\\)" ,pattern)
                          (match-string 1 ,pattern)
                        ""))
             (,regexp (raven-pattern-regex ,pattern)))
         (all-completions
          ,prefix obarray
          (lambda (,item)
            (and (funcall ,pred ,item)
                 (string-match-p ,regexp (symbol-name ,item)))))))))

;;;###autoload
(defun raven-extended-commands-source ()
  "Source for extended commands (`M-x')."
  (raven-source-create
   "Commands"
   :candidates
   (raven--make-obarray-candidates-fn #'commandp)
   :actions
   raven-extended-command-actions))

;;;###autoload
(defun raven-extended-command-history-source ()
  "Source for extended command history."
  (raven-source-create
   "Command History"
   :candidates
   (-map
    #'raven-candidate-create
    extended-command-history)
   :actions
   raven-extended-command-actions))

;;;###autoload
(defun raven-apropos-command-source ()
  "Source for command lookup."
  (raven-source-create
   "Commands"
   :candidates
   (raven--make-obarray-candidates-fn #'commandp)
   :actions
   (list (lambda (c) (describe-function (intern-soft c))))))

;;;###autoload
(defun raven-apropos-function-source ()
  "Source for function lookup."
  (raven-source-create
   "Functions, Macros & Special Forms"
   :candidates
   (raven--make-obarray-candidates-fn #'fboundp)
   :actions
   (list (lambda (c) (describe-function (intern-soft c))))))

;;;###autoload
(defun raven-apropos-variable-source ()
  "Source for variable lookup."
  (raven-source-create
   "Variables"
   :candidates
   (raven--make-obarray-candidates-fn
    (lambda (sym)
      (and (boundp sym)
           (not (keywordp sym)))))
   :actions
   (list (lambda (c) (describe-variable (intern-soft c))))))

(defvar raven-buffer-actions
  (list 'switch-to-buffer
        (cons (kbd "M-D") 'kill-buffer)))

;;;###autoload
(defun raven-buffers-source (&optional sort-pred)
  "Source for open buffers.
An optional SORT-PRED may be provided to sort the buffers (see `sort')."
  (raven-source-create
   "Buffers"
   :candidates
   (--map (raven-candidate-create (buffer-name it))
          (if sort-pred (sort (buffer-list) sort-pred) (buffer-list)))
   :actions
   raven-buffer-actions))

;;;###autoload
(defun raven-create-buffer-source ()
  "Dummy source to create a buffer."
  (raven-source-create
   "Other"
   :candidates
   (list (raven-candidate-create
          "Create buffer"
          :type 'dummy
          :action (lambda (_) (switch-to-buffer (raven-input)))))))

(defvar raven-file-actions
  (list 'find-file
        (cons (kbd "M-D") (lambda (f)
                            (when (y-or-n-p (concat "Delete file " f "? "))
                              (delete-file f))))))

;;;###autoload
(defun raven-files-source ()
  "Source for files in current directory."
  (raven-source-create
   "Files"
   :candidates
   (-map #'raven-candidate-create (directory-files default-directory))
   :actions
   raven-file-actions))

;;;###autoload
(defun raven-create-file-source ()
  "Dummy source to create a file."
  (raven-source-create
   "Other"
   :candidates
   (list (raven-candidate-create
          "Create file"
          :type 'dummy
          :action (lambda (_) (find-file (raven-input)))))))

;;;###autoload
(defun raven-recentf-source ()
  "Source for recentf."
  (raven-source-create
   "Recent Files"
   :candidates
   (-map #'raven-candidate-create recentf-list)
   :actions
   raven-file-actions))

;;;###autoload
(defun raven-M-x ()
  "Preconfigured `raven' interface to replace `execute-external-command'."
  (interactive)
  (raven (list (raven-extended-command-history-source)
               (raven-extended-commands-source))))

;;;###autoload
(defun raven-apropos (&optional initial)
  "Preconfigured `raven' interface to replace `apropos'.
INITIAL is the initial text to match."
  (interactive)
  (raven (list (raven-apropos-command-source)
               (raven-apropos-function-source)
               (raven-apropos-variable-source))
         :initial (if initial initial (thing-at-point 'symbol t))))

;;;###autoload
(defun raven-for-buffers ()
  "Preconfigured `raven' interface for open buffers and recentf."
  (interactive)
  (raven (list (raven-buffers-source)
               (raven-recentf-source)
               (raven-create-buffer-source))))

;;;###autoload
(defun raven-for-files ()
  "Preconfigured `raven' interface for files in the current directory."
  (interactive)
  (raven (list (raven-files-source)
               (raven-create-file-source))))

;;;###autoload
(defun raven-read-file-name (prompt &optional dir default-filename mustmatch initial predicate)
  "Replacement for `read-file-name'.
PROMPT, DIR, DEFAULT-FILENAME, MUSTMATCH, INITIAL and PREDICATE have the same
meaning as in `read-file-name'."
  (ignore default-filename mustmatch predicate)
  (let ((d (if dir dir default-directory)))
    (concat d (raven (list (raven-source-create
                            "Files"
                            :candidates (-map #'raven-candidate-create (directory-files d)))
                           (raven-source-create
                            "Other"
                            :candidates (list (raven-candidate-create
                                               "New file"
                                               :type 'dummy
                                               :action (lambda (_) (raven-input))))))
                     :prompt prompt
                     :initial initial))))

(defun raven-file-contents-actions (file)
  "Actions for candidate values corresponding to lines in FILE."
  (list
   (lambda (index)
     (find-file file)
     (goto-char (point-min))
     (forward-line index)
     (pulse-momentary-highlight-one-line (point)))))

(defun raven-file-contents-source (file)
  "Source for lines in FILE."
  (raven-source-create
   file
   :candidates
   (-map-indexed
    (lambda (index l)
      (raven-candidate-create l :value index))
    (split-string (f-read-text file) "\n"))
   :actions
   (raven-file-contents-actions file)))

(provide 'raven)
;;; raven.el ends here
