(require 'paredit)
(require 'rainbow-delimiters)

(defun chicken-initialize-scheme-mode ()
  (enable-paredit-mode)
  (rainbow-delimiters-mode)
  (define-key scheme-mode-map (kbd "C-c C-c") 'scheme-send-definition)
  (define-key scheme-mode-map (kbd "C-c C-z") 'run-scheme-other-window-or-go-back)
  (define-key scheme-mode-map (kbd "C-c z") 'reset-scheme)
  (define-key scheme-mode-map (kbd "C-c C") 'compile))

(defun chicken-initialize-inferior-scheme-mode ()
  (define-key inferior-scheme-mode-map (kbd "C-c C-z") 'run-scheme-other-window-or-go-back)
  (define-key inferior-scheme-mode-map (kbd "C-c C") 'compile))

(add-hook 'scheme-mode-hook #'chicken-initialize-scheme-mode)
(add-hook 'inferior-scheme-mode-hook #'chicken-initialize-inferior-scheme-mode)

(defvar chicken-install-prefix "/usr")

(setq scheme-program-name (concat chicken-install-prefix "/bin/csi -:d"))

(setq scheme-special-forms
      '(module if and-let* parameterize handle-exceptions
        when unless match bind-lambda bind-lambda*
        match-let match-let* match-letrec if-let if-let*))

(dolist (special-form scheme-special-forms)
  (put special-form 'scheme-indent-function 1))

(defvar scheme-previous-buffer-name nil)

(defun run-scheme-other-window-or-go-back ()
  (interactive)
  (if (equal (buffer-name (current-buffer)) "*scheme*")
      (let ((dest scheme-previous-buffer-name))
        (setq scheme-previous-buffer-name nil)
        (switch-to-buffer-other-window dest))
    (progn
      (if (not (comint-check-proc "*scheme*"))
          (let ((cmdlist (split-string-and-unquote scheme-program-name)))
            (set-buffer (apply 'make-comint "scheme" (car cmdlist)
                               (scheme-start-file (car cmdlist)) (cdr cmdlist)))
            (inferior-scheme-mode)))
      (setq scheme-previous-buffer-name (buffer-name (current-buffer)))
      (setq scheme-buffer "*scheme*")
      (pop-to-buffer "*scheme*"))))

(defun reset-scheme ()
  (interactive)
  (setq kill-buffer-query-functions nil)
  (kill-buffer "*scheme*")
  (run-scheme-other-window-or-go-back))
