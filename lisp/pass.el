;;; -*- lexical-binding: t -*-

(defun my/pass-get-username-then (cb)
  (my/shell-to-lines-async
   "find ~/.password-store | sed -En 's/^.*\\.password-store\\/(.*)\\.gpg$/\\1/p'"
   (lambda (lines)
     (let ((to-show (completing-read-default "Pass: " lines)))
       (funcall cb to-show)))))

(defun my/pass-copy-password-to-clipboard ()
  (interactive)
  (my/pass-get-username-then
   (lambda (to-show)
     (kill-new
      (shell-command-to-string (concat "pass show " (shell-quote-argument to-show)))))))

(defun my/pass-copy-username-to-clipboard ()
  (interactive)
  (my/pass-get-username-then
   (lambda (to-show)
     (string-match "[^/]+/\\(.*\\)" to-show)
     (kill-new (match-string 1 to-show)))))

(global-set-key (kbd "C-c k p") 'my/pass-copy-password-to-clipboard)
(global-set-key (kbd "C-c k u") 'my/pass-copy-username-to-clipboard)
