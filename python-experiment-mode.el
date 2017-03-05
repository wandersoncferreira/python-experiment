;;; package --- Python Experimental Mode
;;; Commentary:
;;; This Python Project is intended to help you easily test several functionalities of the Python Progrmaming Language.
;;; Code:

(defvar py-buffer-name "Python Experiment"
  "The name of the buffer that will be opened.")

(defun create-new-pyexperiment-frame ()
  "Create a new frame in Python Experiment mode."
  (interactive)
  (switch-to-buffer-other-frame py-buffer-name)
  (python-experiment-mode))

(defun python-open-inferior-shell ()
  "Function to open an inferior Python Process inside the new frame."
  (run-python (python-shell-parse-command) nil nil)
  (python-shell-send-buffer)
  (python-shell-switch-to-shell))

(defun python-set-default-environment ()
  "Function to set the default python environment."
  (message "ENV"))


(defun python-insert-mutable-data ()
  "Function to insert mutable data types in the buffer."
  (message "MUT"))

(defun python-insert-immutable-data ()
  "Function to insert immutable data types in the buffer."
  (message "IMMUT"))



;; name of the python packages should be placed as alist objects
;; desired format (numpy . np) for aliased imports
;; desired format (numpy . nil) for non-aliased imports

(defvar list-of-modules '((os . nil) (sys . nil) (numpy . np) (pandas . pd))
  "Alist of modules to be imported during the call of the program.")


(defun python-insert-imports ()
  "Function to insert the desired python imports."
  (dolist (module-pair list-of-modules)
    (let ((module-name (car module-pair))
          (module-nickname (cdr module-pair)))
      (if (check-import (car module-pair))
          (if module-nickname
              (progn
                (insert (format "import %s as %s" module-name module-nickname))
                (newline))
            (insert (format "import %s" module-name))
            (newline))))))


(defun check-import (module-name)
  "Function to check if a MODULE-NAME exists in the users's system."
  (let
      ((output-command (shell-command-to-string (format "python -c 'import %s'" module-name))))
    (if (string-match "^Traceback*" output-command)
        nil
      t)))

(defun check-experiment-python-running ()
  "Function to check if there is already an instance of the Python Experiment opened."
  (if (get-buffer py-buffer-name)
      (progn
        t)
    nil))

;; Python Experiment Mode have direct inheritance of a Python Major mode.
(define-derived-mode python-experiment-mode python-mode "Python Experiment"
  (define-key python-experiment-mode-map (kbd "C-c C-t") 'create-new-pyexperiment-frame))


(defun python-experiment ()
  "Main function to start the Python Experiment Mode."
  (interactive)
  (if (check-experiment-python-running)
      (progn
        (switch-to-buffer-other-frame py-buffer-name)
        (python-open-inferior-shell))

    (create-new-pyexperiment-frame)
    (python-insert-imports)
    (python-insert-mutable-data)
    (python-insert-immutable-data)
    (python-open-inferior-shell)))



(provide 'python-experiment-mode)
;;; python-experiment-mode.el ends here
