;; python-experiment.el --- Python Experimental Mode
;; Copyright (C)
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Package: python-experiment
;; Version: 0.2

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; This Python Project is intended to help you easily test several functionalities of the Python Progrmaming Language.
;;; Code:

(require 'python)


(defvar python-experiment-name "Python Experiment"
  "The name of the buffer that will be opened.")

(defvar python-experiment-file ".python-experiment"
  "The name of the custom config file.")

(defun python-experiment-new-frame ()
  "Create a new frame in Python Experiment."
  (interactive)
  (switch-to-buffer-other-frame python-experiment-name)
  (python-mode))

(defun python-experiment-inferior-shell ()
  "Function to open an inferior Python Process inside the new frame."
  (run-python (python-shell-parse-command) nil nil)
  (python-shell-send-buffer)
  (python-shell-switch-to-shell))


(defun python-experiment-insert-datatypes ()
  "Function to insert the desired datatypes in folder /data/."
  (let
      ((datatype-files (file-expand-wildcards (concat user-emacs-directory "site-packages/python-experiment-mode/data/*.dat"))))
    (dolist (file-name-full datatype-files)
      (let*
	  ((file-name (car (last (split-string file-name-full "/"))))
	   (module-nickname-p (car (split-string file-name ".dat")))
	   (list-module-nickname (split-string module-nickname-p "_"))
	   (module-name (car list-module-nickname))
	   (module-nickname (car (cdr list-module-nickname))))
	(if (or (python-experiment-check-import module-name) (string-equal module-name "builtins"))
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
(defvar python-experiment-builtins '((os . nil) (sys . nil) (datetime . dt) (abc . nil))
  "Alist of modules to be imported during the call of the program.")

(defun python-experiment-insert-imports ()
  "Function to insert the desired python imports."
  (dolist (module-pair python-experiment-builtins)
    (let ((module-name (car module-pair))
          (module-nickname (cdr module-pair)))
      (if (python-experiment-check-import (car module-pair))
          (if module-nickname
              (progn
                (insert (format "import %s as %s" module-name module-nickname))
                (newline))
            (insert (format "import %s" module-name))
            (newline))))))


(defun python-experiment-check-import (module-name)
  "Function to check if a MODULE-NAME exists in the users's system."
  (let
      ((output-command (shell-command-to-string (format "python -c 'import %s'" module-name))))
    (if (string-match "^Traceback*" output-command)
        nil
      t)))


(defun python-experiment-is-running ()
  "Function to check if there is already an instance of the Python Experiment opened."
  (if (get-buffer python-experiment-name)
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
  (when (get-buffer python-experiment-name)
    (if (string-equal (buffer-name) python-experiment-name)
	(kill-buffer-and-window)
      (switch-to-buffer-other-frame python-experiment-name)
      (kill-buffer-and-window))
    (delete-frame)))

(defun python-experiment-reload ()
  "If you want to type new things at Python Experiment buffer."
  (interactive)
  (switch-to-buffer-other-frame python-experiment-name)
  (python-shell-send-buffer))


(defun python-experiment-buffer-to-file ()
  "If you desired to save your Python Experiment buffer to a file to be loaded the next time."
  (interactive)
  (switch-to-buffer-other-frame python-experiment-name)
  (write-file (concat user-emacs-directory python-experiment-file))
  (message (format "Your Python Experiment buffer was written to %s" python-experiment-file)))


(defun python-experiment ()
  "Main function to start the Python Experiment Mode."
  (interactive)
  (if (python-experiment-is-running)
      (progn
        (switch-to-buffer-other-frame python-experiment-name)
        (python-experiment-inferior-shell))

    (python-experiment-new-frame)

    (if (file-exists-p python-experiment-file)
        (insert-file-contents python-experiment-file)
      (python-experiment-insert-imports)
      (python-experiment-insert-datatypes))
    (python-experiment-inferior-shell)))


;; global suggested bindings
(global-set-key (kbd "<f9>") 'python-experiment)
(global-set-key (kbd "<f10>") 'python-experiment-lived-too-long)
(global-set-key (kbd "<f11>") 'python-experiment-reload)
(global-set-key (kbd "<f12>") 'python-experiment-buffer-to-file)

(provide 'python-experiment)
;;; python-experiment.el ends here
