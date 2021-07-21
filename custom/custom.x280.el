;;; -*- lexical-binding: t -*-

(server-start)

(custom-set-faces
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 143 :width normal)))))

(add-to-list 'load-path "~/.emacs.d.private/external-lisp/")
(dolist (d (seq-filter #'file-directory-p
                       (append (file-expand-wildcards "~/.emacs.d.private/external-lisp/*")
                               (file-expand-wildcards "~/.emacs.d.private/external-lisp/*/lisp"))))
  (add-to-list 'load-path d))

(load "~/.emacs.d/lisp/emacs-lisp.el")
(load "~/.emacs.d/lisp/misc.el")
(load "~/.emacs.d/lisp/pass.el")
(load "~/.emacs.d/lisp/project.el")
(load "~/.emacs.d/lisp/win.el")
(load "~/.emacs.d/lisp/ruby.el")
(load "~/.emacs.d/lisp/c.el")
(load "~/.emacs.d/lisp/org.el")
(load "~/.emacs.d.private/lisp/ledger.el")
(load "~/.emacs.d.private/lisp/email.el")
(load "~/.emacs.d.private/lisp/erc.el")

(defun my/setup-windows ()
  (interactive)
  (my/switch-to-win-config 1)
  (my/rename-current-win-config "code")
  (find-file "~/dev/")
  (split-window-right)
  (windmove-right)
  (split-window-below)
  (windmove-down)
  (vterm "*vterm-git*")
  (set-window-dedicated-p (get-buffer-window) t)
  (my/switch-to-win-config 4)
  (my/rename-current-win-config "org")
  (find-file "~/org/doing.org")
  (my/switch-to-win-config 6)
  (my/rename-current-win-config "el")
  (find-file "~/.emacs.d/init.el")
  (my/switch-to-win-config 9)
  (my/rename-current-win-config "term")
  (vterm "*vterm-nas*")
  (vterm "*vterm-shell*")
  (my/switch-to-win-config 1))

(my/setup-windows)
