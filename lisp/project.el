;; -*- lexical-binding: t -*-

(defun my/run-in-term (command)
  (let ((vterm-shell command))
    (vterm)))

(defun my/git-find-file-toplevel ()
  (interactive)
  (my/run-in-term "~/.emacs.d/bin/fzf-git-ls-then-open.sh"))

(defun my/git-find-file-toplevel ()
  (interactive)
  (my/run-in-term "~/.emacs.d/bin/fzf-git-ls-toplevel-then-open.sh"))

(defun my/git-find-file-toplevel-filtered (filter)
  (interactive)
  (let* ((toplevel (magit-toplevel)))
    (my/shell-to-lines-async
     (format "cd '%s' && git ls-files -co --exclude-standard --full-name %s" toplevel filter)
     (lambda (lines)
       (find-file
        (concat toplevel "/" (completing-read-default "Open File: " lines)))))))

(defun my/git-grep-filtered (filter pattern)
  (interactive "sPattern: ")
  (compilation-start (concat (format "git ls-files -coz --exclude-standard %s" filter)
                             " | "
                             "xargs -0 rg --no-heading "
                             "'" pattern "'")
                     'grep-mode))

(defun my/git-grep (pattern)
  (interactive "sPattern: ")
  (my/git-grep-filtered "" pattern))

(defun my/git-grep-toplevel (pattern)
  (interactive "sPattern: ")
  (my/git-grep-toplevel-filtered "" pattern))

(defun my/git-grep-toplevel-filtered (filter pattern)
  (interactive "sPattern: ")
  (compilation-start (concat "cd " (magit-toplevel) " && "
                             (format "git ls-files -co --exclude-standard %s" filter)
                             " | "
                             "xargs rg --no-heading "
                             "'" pattern "'")
                     'grep-mode))

(defun my/git-grep (pattern)
  (interactive "sPattern: ")
  (my/git-grep-filtered "" pattern))

(defun my/git-grep-toplevel (pattern)
  (interactive "sPattern: ")
  (my/git-grep-toplevel-filtered "" pattern))

(defun my/git-sed (pattern)
  (interactive "sPattern: ")
  (compilation-start (concat "git ls-files -coz --exclude-standard"
                             " | "
                             "xargs -0 sed -Ei "
                             "'" pattern "'")))

(defun my/git-sed-toplevel (pattern)
  (interactive "sPattern: ")
  (compilation-start (concat "cd " (magit-toplevel) " && "
                             "git ls-files -coz --exclude-standard"
                             " | "
                             "xargs -0 sed -Ei "
                             "'" pattern "'")))

(global-set-key (kbd "C-c p f") 'my/git-find-file-toplevel)
(global-set-key (kbd "C-c p F") 'my/git-find-file)
(global-set-key (kbd "C-c p g") 'my/git-grep-toplevel)
(global-set-key (kbd "C-c p G") 'my/git-grep)
(global-set-key (kbd "C-c p s") 'my/git-sed-toplevel)
(global-set-key (kbd "C-c p S") 'my/git-sed)
