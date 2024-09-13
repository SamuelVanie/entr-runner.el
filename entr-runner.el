;;; entr-runner.el --- Run entr on selected files or regex with user-friendly options -*- lexical-binding: t; -*-

;; Author: Samuel Michael Vanié
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, files
;; URL: https://github.com/SamuelVanie/entr-runner

;;; Commentary:

;; This package provides a convenient way to run entr on files selected
;; in Dired or specified by a regex, with user-friendly options.

;;; Code:

(require 'dired)

(defvar entr-runner-last-command nil
  "The last command run with entr.")

(defvar entr-runner-options
  '((?r "Restart persistent child" "-r")
    (?c "Clear screen before each execution" "-c")
    (?s "Track files by modification time" "-s")
    (?d "Track files by modification time or directory change" "-d")
    (?p "Postpone first execution until file is modified" "-p"))
  "List of options for entr.")

(defun entr-runner-build-command (options command)
  "Build the entr command with OPTIONS and COMMAND."
  (concat "entr -n " (mapconcat 'identity options " ") " " command))

(defun entr-runner-select-options ()
  "Prompt user to select options for entr. Press Enter when done."
  (let ((choices (mapcar (lambda (opt) (cons (format "%c: %s" (car opt) (cadr opt)) (car opt))) entr-runner-options))
        selected-options
        (done nil))
    (while (not done)
      (let* ((prompt (concat "Select options (or press Enter when done):\n"
                             (mapconcat (lambda (c) (car c)) choices "\n")
                             "\n\nSelected: "
                             (mapconcat (lambda (o) (cadr (assoc o entr-runner-options)))
                                        selected-options
                                        " ")
                             "\n"))
             (choice (read-char prompt)))
        (cond
         ((= choice 13) ; Enter key
          (setq done t))
         ((assoc choice entr-runner-options)
          (let ((option (assoc choice entr-runner-options)))
            (unless (member choice selected-options)
              (setq selected-options (cons choice selected-options))
              (message "Added option: %s" (cadr option))
              (sit-for 1))))
         (t
          (message "Invalid option. Please try again.")
          (sit-for 1)))))  ; Wait for 1 second before redrawing
    (mapcar (lambda (c) (caddr (assoc c entr-runner-options)))
            (nreverse selected-options))))

(defun entr-runner-run-on-files (files)
  "Run entr on FILES with user-selected options and command."
  (let* ((options (entr-runner-select-options))
         (command (read-string "Enter command to run: " entr-runner-last-command))
         (full-command (entr-runner-build-command options command))
         (files-string (mapconcat 'shell-quote-argument files "\\n")))
    (setq entr-runner-last-command command)
    (async-shell-command
     (format "echo -e %s | %s" files-string full-command))))

(defun entr-runner-dired ()
  "Run entr on marked files in Dired."
  (interactive)
  (let ((marked-files (dired-get-marked-files)))
    (if marked-files
        (entr-runner-run-on-files marked-files)
      (message "No files selected in Dired."))))

(defun entr-runner-regex ()
  "Run entr on files matching a regex."
  (interactive)
  (let* ((regex (read-string "Enter regex for files: "))
         (files (directory-files-recursively default-directory regex)))
    (if files
        (entr-runner-run-on-files files)
      (message "No files found matching the regex."))))

(provide 'entr-runner)

;;; entr-runner.el ends here
