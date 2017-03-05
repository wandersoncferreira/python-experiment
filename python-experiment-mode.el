;;; package --- Python Experimental Mode
;;; Commentary:
;;; This Python Project is intended to help you easily test several functionalities of the Python Progrmaming Language.
;;; Code:

(require 'python)


(defvar py-buffer-name "Python Experiment"
  "The name of the buffer that will be opened.")

(defvar py-setup-file ".python-experiment"
  "The name of the custom config file.")

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


(defun insert-datatypes ()
  "Function to insert the desired datatypes in folder /data/."
  (let
      ((datatype-files (file-expand-wildcards (concat user-emacs-directory "site-packages/python-experiment-mode/data/*.dat"))))
    (dolist (file-name-full datatype-files)
      (let*
          ((file-name (nth 1 (split-string file-name-full "/")))
           (module-nickname-p (car (split-string file-name ".dat")))
           (list-module-nickname (split-string module-nickname-p "_"))
           (module-name (car list-module-nickname))
           (module-nickname (car (cdr list-module-nickname))))
        (if (or (check-import module-name) (string-equal module-name "builtins"))
            (progn
              (open-line 2)
              (insert-file-contents file-name-full)
              (newline)
              (if (not (string-equal module-name "builtins"))
                  (progn
                    (save-excursion
                      (goto-char (point-min))
                      (if module-nickname
                          (progn
                            (insert (format "import %s as %s" module-name module-nickname))
                            (newline))
                        (insert (format "import %s" module-name))
                        (newline)))))))))))

;; name of the python packages should be placed as alist objects
;; desired format (numpy . np) for aliased imports
;; desired format (numpy . nil) for non-aliased imports
(defvar list-of-modules '((os . nil) (sys . nil))
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


;; better to bind it later to something better to you.
(defun python-experiment-lived-too-long ()
  "Meeseeks!"
  (interactive)
  (when (get-buffer-process "*Python*")
    (set-process-query-on-exit-flag (get-buffer-process "*Python*") nil)
    (kill-process (get-buffer-process "*Python*"))
    (kill-buffer "*Python*"))
  (when (get-buffer py-buffer-name)
    (switch-to-buffer-other-frame py-buffer-name)
    (kill-buffer-and-window)
    (delete-frame)))

(defun python-experiment-reload ()
  "If you want to type new things at Python Experiment buffer."
  (interactive)
  (switch-to-buffer-other-frame py-buffer-name)
  (python-shell-send-buffer))


(defun python-experiment-buffer-to-file ()
  "If you desired to save your Python Experiment buffer to a file to be loaded the next time."
  (interactive)
  (switch-to-buffer-other-frame py-buffer-name)
  (write-file py-setup-file)
  (message (format "Your Python Experiment buffer was written to %s" (concat user-emacs-directory py-setup-file))))


;; Python Experiment Mode have direct inheritance of a Python Major mode.
(define-derived-mode python-experiment-mode python-mode "Python Experiment")


(defun python-experiment ()
  "Main function to start the Python Experiment Mode."
  (interactive)
  (if (check-experiment-python-running)
      (progn
        (switch-to-buffer-other-frame py-buffer-name)
        (python-open-inferior-shell))

    (create-new-pyexperiment-frame)

    (if (file-exists-p py-setup-file)
        (insert-file-contents py-setup-file)
      (python-insert-imports)
      (insert-datatypes))
    (python-open-inferior-shell)))


;; global suggested bindings
(global-set-key (kbd "<f9>") 'python-experiment)
(global-set-key (kbd "<f10>") 'python-experiment-lived-too-long)
(global-set-key (kbd "<f11>") 'python-experiment-reload)
(global-set-key (kbd "<f12>") 'python-experiment-buffer-to-file)

(provide 'python-experiment-mode)
;;; python-experiment-mode.el ends here
