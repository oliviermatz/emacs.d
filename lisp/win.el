;;; -*- lexical-binding: t -*-

(global-unset-key (kbd "C-x C-w"))

(defstruct win-config
  label
  value)

(setq my/win-config-store (make-vector 10 nil))
(setq my/win-config-current nil)
(setq my/win-config-previous nil)

(defun my/open-default-win-config ()
  (delete-other-windows)
  (switch-to-buffer (startup--get-buffer-create-scratch)))

(defun my/save-current-win-config ()
  (when my/win-config-current
    (let ((wc (aref my/win-config-store my/win-config-current)))
      (setf (win-config-value wc) (current-window-configuration)))
    (setq my/win-config-previous my/win-config-current)))

(defun my/switch-to-win-config (n)
  (my/save-current-win-config)
  (-if-let (selected-config (win-config-value (or (aref my/win-config-store n)
                                                  (let ((wc (make-win-config :label (concat "noname" (number-to-string n))
                                                                             :value nil)))
                                                    (aset my/win-config-store n wc)
                                                    wc))))
      (set-window-configuration selected-config)
    (my/open-default-win-config))
  (setq my/win-config-current n))

(defun my/close-current-win-config ()
  (interactive)
  (my/save-current-win-config)
  (my/open-default-win-config)
  (setq my/win-config-current nil))

(defun my/back-to-previous-win-config ()
  (interactive)
  (let ((previous my/win-config-previous))
    (my/save-current-win-config)
    (my/switch-to-win-config previous)))

(defun my/rename-current-win-config (name)
  (interactive "sName: ")
  (if my/win-config-current
      (let* ((cur-win-config (aref my/win-config-store my/win-config-current)))
        (setf (win-config-label cur-win-config) name))
    (message "No window configuration selected")))

(defun my/prompt-win-config-and-switch ()
  (interactive)
  (my/switch-to-win-config
   (let ((selected-label (completing-read-default
                          "Name: "
                          (-map #'win-config-label (-non-nil (append my/win-config-store '()))))))
     (-find-index
      (lambda (win-config)
        (and win-config
             (string= selected-label (win-config-label win-config))))
      (append my/win-config-store nil)))))

(defun my/win-config-status-line ()
  (let ((parts '()))
    (-dotimes 10
      (lambda (n)
        (-if-let (wc (aref my/win-config-store n))
            (add-to-list 'parts
                         (format
                          (if (= n my/win-config-current) "*%d:%s" "%d:%s")
                          n
                          (win-config-label wc))
                         t))))
    (string-join parts " ")))

(-dotimes 10
  (lambda (n)
    (global-set-key (kbd (concat "C-x C-w " (number-to-string n)))
                    (lambda ()
                      (interactive)
                      (my/switch-to-win-config n)))))

(global-set-key (kbd (concat "C-x C-w d")) #'my/close-current-win-config)
(global-set-key (kbd (concat "C-x C-w b")) #'my/back-to-previous-win-config)
(global-set-key (kbd (concat "C-x C-w r")) #'my/rename-current-win-config)
(global-set-key (kbd (concat "C-x C-w l")) #'my/prompt-win-config-and-switch)
(global-set-key (kbd "M-s-<f10>") #'my/prompt-win-config-and-switch)
