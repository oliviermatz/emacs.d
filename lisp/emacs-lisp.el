;;; -*- lexical-binding: t -*-

(defun my/emacs-lisp-mode-initialize ()
  (paredit-mode)
  (rainbow-delimiters-mode)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)
  (define-key emacs-lisp-mode-map (kbd "C-c C-e C-d") 'edebug-defun))

(add-hook 'emacs-lisp-mode-hook #'my/emacs-lisp-mode-initialize)
