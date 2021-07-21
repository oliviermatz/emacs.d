;;; -*- lexical-binding: t -*-

(require 'gtags nil t)

(defun c-mode-initialize ()
  (gtags-mode)
  (setq c-default-style "linux")
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (define-key c-mode-map (kbd "C-c c r") (lambda () (interactive) (compile "make run")))
  (define-key c-mode-map (kbd "C-c c c") (lambda () (interactive) (compile "make clean")))
  (define-key c-mode-map (kbd "C-c c m") (lambda () (interactive) (compile "make"))))

(add-hook 'c-mode-hook #'c-mode-initialize)
