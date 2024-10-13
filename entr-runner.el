;;; entr-runner.el --- The most complete hotreload package -*-
;; lexical-binding: t; -*-
;; Author: Samuel Michael Vanié
;; Version: 1.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, files
;; URL: https://github.com/SamuelVanie/entr-runner

;;; Commentary:

;; This package provides a convenient way to do hotreload using entr
;; on the files or directories marked
;; in Dired or specified by a regex, with user-friendly options.

;;; Code:

(require 'dired)

(defgroup entr-runner nil
  "Customization group for entr-runner."
  :group 'convenience)

(defcustom entr-runner-kill-process-key (kbd "C-c k e")
  "Key binding to kill the entr-runner process in the output buffer."
  :type 'key-sequence
  :group 'entr-runner)

(defcustom entr-git-link "https://github.com/eradman/entr"
  "The link to the entr package"
  :type 'string
  :group 'entr-runner)

(defvar entr-location (format "%sentr" (expand-file-name user-emacs-directory))
  "The location of the entr binary")

(defvar entr-runner-last-command nil
  "The last command run with entr.")

(defvar entr-runner-process nil
  "The current entr-runner process.")

(defvar entr-runner-options
  '((?r "Restart persistent child" "-r")
    (?c "Clear screen before each execution" "-c")
    (?s "Track files by modification time" "-s")
    (?d "Track files by modification time or directory change" "-d")
    (?p "Postpone first execution until file is modified" "-p"))
  "List of options for entr.")

(defun entr-runner-build-command (options command)
  "Build the entr command with OPTIONS and COMMAND."
  (concat (format "%s/entr" entr-location) " -n " (mapconcat 'identity options " ") " " command))

(defun entr-runner-select-options ()
  "Prompt user to select options for entr. Press Enter when done."
  (let ((choices (mapcar (lambda (opt) (cons (format "%c: %s" (car
                                                               opt)
                                                     (cadr opt)) (car
                                                                  opt)))
                         entr-runner-options))
        selected-options
        (done nil))
    (while (not done)
      (let* ((prompt (concat "Select options (or press Enter when done):\n"
                             (mapconcat (lambda (c) (car c)) choices "\n")
                             "\n\nSelected: "
                             (mapconcat (lambda (o) (cadr (assoc o
                                                                 entr-runner-options)))
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
             collect (shell-command-to-string (format "find %s -type f"
                                                      (shell-quote-argument item)))
             else
             collect (format "%s" (shell-quote-argument item)))))


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
        (local-set-key entr-runner-kill-process-key 'entr-runner-kill-process))
      (setq entr-runner-process
            (start-process-shell-command
             "entr-runner" buffer
             (format "echo -e \"%s\" | %s" files-or-command full-command)))
      (set-process-sentinel
       entr-runner-process
       (lambda (proc event)
         (when (string= event "finished\n")
           (message "entr-runner process finished"))))
      (message "entr-runner started in background. Check buffer %s for
output. Use %s to kill the process."
               buffer-name
               (key-description entr-runner-kill-process-key)))))



(defun entr-runner-dired ()
  "Run entr on marked files in Dired, including all files in marked
directories."
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
        (entr-runner-execute
         (mapconcat (lambda (file)
                      (replace-regexp-in-string
                       (regexp-quote default-directory) ""
                       (expand-file-name file)))
                    files
                    "\n"))
      (message "No files found matching the regex."))))

(defun entr-runner-install ()
  "Automatically download and install entr based on the system type."
  (interactive)
  (if (eq system-type 'windows-nt)
      (message "entr is not available for Windows systems.")
    (let* ((temp-dir (make-temp-file "entr-install" t))
           (install-script (expand-file-name "install-entr.sh" temp-dir)))
      (with-temp-file install-script
        (insert "#!/bin/bash\n")
        (insert "if command -v entr > /dev/null 2>&1;then\n")
        (insert (format " mkdir -p %s\n" entr-location))
        (insert (format " ln -s '$(which entr)' '%s/entr'\n" entr-location))
        (insert " if [ $? -eq 0 ]; then\n")
        (insert "  echo 'symlink successfully created'\n")
        (insert " else\n")
        (insert "  echo 'failed to create symlink'\n")
        (insert " fi\n")
        (insert "else\n")
        (insert " git clone ")
        (insert entr-git-link)
        (insert (format " %s%s\n" (expand-file-name user-emacs-directory) "entr"))
        (insert (format " cd %sentr\n" (expand-file-name user-emacs-directory)))
        (insert " ./configure\n")
        (insert " make test\n")
        (insert "fi"))
      (set-file-modes install-script #o755)
      (if (zerop (call-process-shell-command
                  (format "bash %s" 
                          (shell-quote-argument install-script))
                  nil (get-buffer-create "*entr-install*") t))
          (message "entr has been successfully installed.")
        (message "Failed to install entr. Check the *entr-install* buffer for details.")))))

(provide 'entr-runner)

;;; entr-runner.el ends here
