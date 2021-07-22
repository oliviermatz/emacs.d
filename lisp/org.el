;;; -*- lexical-binding: t -*-

(require 'org-bullets)

(defvar org-notes-dir (concat (getenv "HOME") "/org"))

(setq org-agenda-start-with-follow-mode t)

(setq org-bullets-bullet-list '("•" "►" "⁃" "◎" "◉" "◎" "◇"))
;; (setq org-bullets-bullet-list '("✸" "★" "•"))

(setq org-capture-templates
      '(("t" "doing" entry (file "~/org/doing.org")
         "* TODO %?")
        ("d" "drop" entry (file "~/org/drop.org")
         "* %?")))

(setq org-todo-keywords
      '((sequence "TODO(t)" "|"
                  "INPROGRESS(p!)" "|"
                  "ONHOLD(h@/!)" "|"
                  "DONE(d!)" "|"
                  "IDEA(i)")))

(setq org-todo-keyword-faces
      '(("TODO" . "red")
        ("INPROGRESS" . "green")
        ("ONHOLD" . "cyan")))

(setq org-archive-location "archive/%s::")

(setq org-agenda-files
      '("~/org/doing.org"
        "~/org/drop.org"
        "~/org/radar.org"
        "~/org/shopify.org"))

(defun my/org-mode-open-notes ()
  (interactive)
  (let ((default-directory org-notes-dir))
   (call-interactively #'find-file)))

(defun my/org-mode-sync ()
  (interactive)
  (start-process "Org Mode Sync"
                 (get-buffer-create "*org-mode-sync*")
                 "sync.sh"))

(defun my/org-mode-initialize ()
  ;; (linum-mode -1)
  ;; (setq mode-line-format nil)
  (org-bullets-mode 1)
  (add-hook 'after-save-hook 'my/org-mode-sync t t))

(defun my/org-mode-new-loose-sheet ()
  (interactive)
  (let* ((dir (pcase (calendar-current-date)
                (`(,day ,month ,year)
                 (format "%s/notes/%d/%02d/%02d" org-notes-dir year month day))))
         (file (concat dir "/"
                       (car (s-match "[0-9]+:[0-9]+:[0-9]+" (current-time-string)))
                       ".org")))
    (shell-command-to-string (concat "mkdir -p " dir))
    (find-file file)
    (add-hook 'after-save-hook
              'my/org-mode-rename-file-from-title
              0
              t)))

(defun my/org-mode-rename-file-from-title ()
  (interactive)
  (let ((title (save-excursion
                 (ignore-errors
                   (outline-up-heading 100 t))
                 (cl-loop while (outline-previous-heading))
                 (buffer-substring (+ (line-beginning-position) 2)
                                   (line-end-position)))))
    (rename-file-and-buffer
     (concat
      (->> title
           (downcase)
           (s-replace-all '((" " . "_")
                            ("/" . "_"))))
      "_" (file-name-base (buffer-file-name)) ".org"))))

(add-hook 'org-mode-hook 'my/org-mode-initialize)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c n s") 'my/org-mode-open-notes)
(global-set-key (kbd "C-c n n") 'my/org-mode-new-loose-sheet)
;; (global-set-key (kbd "M-s-<f10>") 'org-capture)

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.0 :weight normal :underline nil :box nil :background nil))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0 :weight normal :underline nil :box nil :background nil))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0 :weight normal :underline nil :box nil :background nil))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0 :weight normal :underline nil :box nil :background nil))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0 :weight normal :underline nil :box nil :background nil))))
 '(org-todo ((t (:background nil))))
 '(org-done ((t (:background nil)))))

;; (setq org-agenda-exporter-settings
;;       '((ps-number-of-columns 2)
;;         (ps-landscape-mode t)
;;         (org-agenda-add-entry-text-maxlines 5)
;;         (htmlize-output-type 'css)))

;; (setq org-agenda-span 21)

;; (setq org-agenda-custom-commands
;;       `(("X" agenda "" nil ,(concat org-notes-dir "agenda.html"))
;;         ("Y" alltodo "" nil ,(concat org-notes-dir "todo.html"))))

(defun recoll-search (query)
  (interactive "sPrompt")
  (let* ((proc
          (start-process "*recoll*"
                         (get-buffer-create "*recoll results*")
                         "recoll" "-t" "-A" "-q" query))
         (state (make-hash-table))
         (sentinel (lambda (proc output)
                     (with-current-buffer (process-buffer proc)
                      (insert output)))))
    (set-process-filter proc sentinel)))
