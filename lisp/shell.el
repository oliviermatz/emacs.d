(defun sh-send-line-or-region ()
  (interactive ())
  (let ((proc (get-process "shell"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (shell)
        (switch-to-buffer currbuff)
        (setq proc (get-process "shell"))))
    (setq pbuff (process-buffer proc))
    (if (use-region-p)
        (setq min (region-beginning)
              max (region-end))
      (setq min (point-at-bol)
            max (point-at-eol)))
    (setq command (concat (buffer-substring min max) "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (goto-char (point-max))
      (set-marker (process-mark proc) (point-max)))
    (process-send-string  proc command)
    (with-current-buffer pbuff
      (goto-char (point-max))
      (set-marker (process-mark proc) (point-max)))))

(defun sh-switch-to-process-buffer ()
  (interactive)
  (pop-to-buffer (process-buffer (get-process "shell")) t))

(defun sh-mode-initialize ()
  (interactive)
  (define-key sh-mode-map (kbd "C-c C-c") 'sh-send-line-or-region)
  (define-key sh-mode-map (kbd "C-c C-z") 'sh-switch-to-process-buffer))

(add-hook 'sh-mode-hook #'sh-mode-initialize)
