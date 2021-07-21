;;; -*- lexical-binding: t -*-

(toggle-frame-maximized)

(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024))

(add-to-list 'load-path "~/.emacs.d/external-lisp/")
(dolist (d (seq-filter #'file-directory-p
                       (append (file-expand-wildcards "~/.emacs.d/external-lisp/*")
                               (file-expand-wildcards "~/.emacs.d/external-lisp/*/lisp"))))
  (add-to-list 'load-path d))

(setq temporary-file-directory "~/.emacs.d.run")

(require 's)
(require 'dash)
(require 'f)
(require 'async)

(require 'material-theme)
(load-theme 'material t)

(require 'which-key)
(which-key-mode)

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(setenv "NIX_SSL_CERT_FILE" "/Users/oliviermatz/.nix-profile/etc/ssl/certs/ca-bundle.crt")

(require 'markdown-mode)
(require 'graphql-mode)
(require 'rainbow-delimiters)
(require 'vterm)
(require 'paredit)
(require 'magit)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml.rb\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(setq transient-levels-file "~/.emacs.d.run/transient/levels.el")
(setq transient-values-file "~/.emacs.d.run/transient/values.el")
(setq transient-history-file "~/.emacs.d.run/transient/history.el")

(require 'restclient)
(add-to-list 'auto-mode-alist '("\\.http$" . restclient-mode))
(setq restclient-inhibit-cookies t)

;; vertico ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/backward-delete-sexp ()
  (interactive)
  (let ((opoint (point)))
    (forward-sexp -1)
    (delete-region opoint (point))))

(require 'vertico)
(vertico-mode)
(setq vertico-count 25)

(define-key vertico-map (kbd "C-l") #'my/backward-delete-sexp)

(advice-add 'vertico--display-candidates
            :after (lambda (_)
                     (vertico--resize-window vertico-count)))

(require 'orderless)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))

(setq resize-mini-windows t)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq enable-recursive-minibuffers t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(column-number-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode nil)
;; (setq-default cursor-type 'bar)
(show-paren-mode t)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines t)
(delete-selection-mode t)
(setq scroll-step 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-auto-revert-mode 1)
(setq tool-bar-mode nil)
(setq menu-bar-mode nil)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-list-file-prefix nil)

(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(windmove-up windmove-down windmove-left windmove-right))
  (advice-add command :after #'pulse-line))

(setq linum-format "%d ")
(global-linum-mode)

(global-set-key (kbd "C-s-<up>")    'windmove-up)
(global-set-key (kbd "C-s-<down>")  'windmove-down)
(global-set-key (kbd "C-s-<left>")  'windmove-left)
(global-set-key (kbd "C-s-<right>") 'windmove-right)

(global-set-key (kbd "\e[1;P12") 'windmove-up)
(global-set-key (kbd "\e[1;P11") 'windmove-down)
(global-set-key (kbd "\e[1;P10") 'windmove-left)
(global-set-key (kbd "\e[1;P9") 'windmove-right)

(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-x c c") 'comment-region)
(global-set-key (kbd "C-x c u") 'uncomment-region)

(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'bookmark+)
(setq bmkp-last-as-first-bookmark-file "~/.emacs.bookmarks")

;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

(defun my/vterm (name)
  (interactive "sPrompt: ")
  (vterm (format "*vterm-%s*" name)))

(defun my/vterm-switch-to-buffer ()
  (interactive)
  (switch-to-buffer
   (completing-read "Buffer: "
                    (->> (buffer-list)
                      (-map #'buffer-name)
                      (-filter (-partial #'s-prefix? "*vterm-"))))))

(global-set-key (kbd "C-c t n") 'my/vterm)
(global-set-key (kbd "C-c t b") 'my/vterm-switch-to-buffer)

(setq-default mode-line-format
              (list
               (propertize "%m" 'face '(:weight thin :foreground "grey"))
               " "
               ;; value of current buffer name
               "%b@%l:%c "
               ;; value of current line number
               '(:eval (my/win-config-status-line))))

(load "~/.emacs.d/custom.el")
