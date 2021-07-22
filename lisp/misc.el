;;; -*- lexical-binding: t -*-

(defun my/text-reflow ()
  (shell-command-on-region (point-min) (point-max) "fmt -w 72" t t))

(defun my/shell-buffer-replace (command)
  (shell-command-on-region (point-min) (point-max) command t t))

(defun my/shell-buffer-replace-interactive (command)
  (interactive "sShell command on buffer: ")
  (my/shell-buffer-replace command))

(global-set-key (kbd "C-M-|") 'my/shell-buffer-replace-interactive)

(defun my/shell-region-replace (command)
  (shell-command-on-region (region-beginning) (region-end) command t t))

(defun my/shell-region-replace-interactive (command)
  (interactive "sShell command on region: ")
  (my/shell-region-replace command))

(global-set-key (kbd "M-|") 'my/shell-region-replace-interactive)

(defun my/kill-to-ws ()
  (interactive)
  (kill-region (point) (save-excursion (skip-syntax-forward "^ ") (point))))

(global-set-key (kbd "M-D") 'my/kill-to-ws)

(defun my/switch-to-new-temp-buffer ()
  (interactive)
  (let ((now (format-time-string "%F %T" (current-time))))
    (switch-to-buffer (generate-new-buffer-name (concat "*temp " now "*")))))

(global-set-key (kbd "C-c b b") 'my/switch-to-new-temp-buffer)

(defun my/jwt-show-info ()
  (interactive)
  (let ((buffer (generate-new-buffer-name "*temp-jwt*")))
    (switch-to-buffer buffer)
    (yank)
    (my/shell-buffer-replace "jwt-show-info")
    (json-mode)))

(defun my/json-pretty-print ()
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*temp-json*"))
  (yank)
  (my/shell-buffer-replace "jq '.'"))

(global-set-key (kbd "C-c b j") 'my/json-pretty-print)

(defun my/xml-pretty-print ()
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*temp-xml*"))
  (yank)
  (my/shell-buffer-replace "xmllint --format -"))

(global-set-key (kbd "C-c b x") 'my/xml-pretty-print)

(defun my/har-to-restclient ()
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*temp-har*"))
  (yank)
  (my/shell-buffer-replace "har-to-restclient")
  (restclient-mode))

(defun get-frame-name (frame)
  (substring-no-properties (cdr (assoc 'name (frame-parameters frame)))))

(defun my/shell-to-lines-async (command cont)
  (let* ((proc (start-process "my/shell-to-lines-async" nil "sh" "-c" command))
         (head (list))
         (tail head)
         (rest nil))
    (set-process-filter
     proc
     (lambda (process output)
       (cl-loop with start = 0
                with end = nil
                while (setq end (string-match-p "\n" output start))
                do (let ((old-tail tail)
                         (new-tail (list (let ((s (substring output start end)))
                                           (if rest
                                               (let ((with-rest (concat rest s)))
                                                 (setq rest nil)
                                                 with-rest)
                                             s)))))
                     (if old-tail
                         (setcdr old-tail new-tail)
                       (setq head new-tail))
                     (setq tail new-tail)
                     (setq start (+ end 1)))
                finally (setq rest
                              (cond
                               ((= start 0)
                                output)
                               ((< start (length output))
                                (substring output start (length output))))))))
    (set-process-sentinel
     proc
     (lambda (process event)
       (message (concat "process: event -> " event ", status -> " (symbol-name (process-status process))))
       (when (and (eq (process-status process) 'exit))
         (run-with-timer 0.01 nil
                         (lambda ()
                           (funcall cont head))))))))

(defun my/compile-from-git-toplevel ()
  (interactive)
  (compilation-start (format "cd '%s' && make" (magit-toplevel)) t))

(global-set-key (kbd "C-c c") 'my/compile-from-git-toplevel)

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))
