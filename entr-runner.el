;;; entr-runner.el --- The most complete hotreload package -*- lexical-binding: t; -*-

;; Author: Samuel Michael Vanié
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, files
;; URL: https://github.com/SamuelVanie/entr-runner

;;; Commentary:

;; This package provides a convenient way to do hotreload using entr on the files
;; or directories marked
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

(defun entr-runner-get-files-from-dired ()
  "Get files from Dired, including all files in selected directories."
  (let ((marked-items (dired-get-marked-files t)))
    (cl-loop for item in marked-items
             if (file-directory-p item)
             collect (shell-command-to-string (format "find %s -type f" (shell-quote-argument item)))
             else
             collect (format "%s" (shell-quote-argument item)))))


(defvar entr-runner-process nil
  "The current entr-runner process.")

(defun entr-runner-kill-process ()
  "Kill the current entr-runner process."
  (interactive)
  (when entr-runner-process
    (delete-process entr-runner-process)
    (setq entr-runner-process nil)
    (message "entr-runner process killed")))

(defun entr-runner-execute (files-or-command)
  "Execute entr with FILES-OR-COMMAND and user-selected options."
  (let* ((options (entr-runner-select-options))
         (command (read-string "Enter command to run: " entr-runner-last-command))
         (full-command (entr-runner-build-command options command))
         (buffer-name "*entr-runner command*"))
    (setq entr-runner-last-command command)
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (let ((buffer (get-buffer-create buffer-name)))
      (with-current-buffer buffer
        (erase-buffer)
        (special-mode)  ; Use special-mode for read-only buffer
        (local-set-key (kbd "C-c k e") 'entr-runner-kill-process))
      (setq entr-runner-process
            (start-process-shell-command
             "entr-runner" buffer
             (format "echo -e \"%s\" | %s" files-or-command full-command)))
      (set-process-sentinel
       entr-runner-process
       (lambda (proc event)
         (when (string= event "finished\n")
           (message "entr-runner process finished"))))
      (message "entr-runner started in background. Check buffer %s for output. Use C-c C-k to kill the process." buffer-name))))



(defun entr-runner-dired ()
  "Run entr on marked files in Dired, including all files in marked directories."
  (interactive)
  (let ((files (entr-runner-get-files-from-dired)))
    (if files
        (entr-runner-execute (mapconcat 'identity files "\n"))
      (message "No files or directories selected in Dired."))))

(defun entr-runner-regex ()
  "Run entr on files matching a regex."
  (interactive)
  (let* ((regex (read-string "Enter regex for files: "))
         (files (directory-files-recursively default-directory regex)))
    (if files
        (entr-runner-execute (mapconcat 'shell-quote-argument files ""))
      (message "No files found matching the regex."))))


(provide 'entr-runner)

;;; entr-runner.el ends here
