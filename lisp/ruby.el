;;; -*- lexical-binding: t -*-

(require 'inf-ruby)

(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)

(setq my/grep-executable
      (if (string= system-type "gnu/linux")
          "grep"
        "ggrep"))

(setq my/sed-executable
      (if (string= system-type "gnu/linux")
          "sed"
        "gsed"))

(defun my/ruby-is-shopify-path (s)
  (->> s
    (s-split "/")
    (-map #'downcase)
    (member "shopify")))

(defun my/rspec-at-point ()
  (interactive)
  (let* ((file-name (buffer-file-name))
         (line-num (number-to-string (line-number-at-pos)))
         (path (concat file-name ":" line-num)))
    (compilation-start (format "cd '%s' && bundle exec rspec '%s'" (magit-toplevel) path))))

(defun my/rspec-path ()
  (interactive)
  (let* ((path (buffer-file-name)))
    (compilation-start (format "cd '%s' && bundle exec rspec '%s'" (magit-toplevel) path))))

(defun my/ruby-test-at-point ()
  (interactive)
  (if (my/ruby-is-shopify-path (buffer-file-name))
      (my/run-test-at-point)
    (my/rspec-at-point)))

(defun my/ruby-test-path ()
  (interactive)
  (if (my/ruby-is-shopify-path (buffer-file-name))
      (my/run-test-path)
    (my/rspec-path)))

(defun --my/ruby-symbol-char-p (c)
  (or (and (>= c ?A) (<= c ?Z))
      (and (>= c ?a) (<= c ?z))
      (and (>= c ?0) (<= c ?9))
      (and (member c '(?? ?_)))))

(defun --my/ruby-symbol-char-qualified-p (c)
  (or (--my/ruby-symbol-char-p c) (= c ?:)))

(defun --my/ruby-accumulate-symbol-char-around (dir point pred)
  (cl-loop with acc = nil
           with p = point
           while (funcall pred (char-after p))
           do (progn
                (setq acc (cons (char-after p) acc))
                (setq p (+ p dir)))
           finally return acc))

(defun --my/ruby-symbol-at-point (char-pred)
  (save-excursion
    (let ((char-at-point (char-after))
          (left (--my/ruby-accumulate-symbol-char-around -1 (- (point) 1) char-pred))
          (right (reverse (--my/ruby-accumulate-symbol-char-around +1 (+ (point) 1) char-pred))))
      (apply #'string (append left (list char-at-point) right)))))

(defun my/uppercase-p (c)
  (and (>= c ?A) (<= c ?Z)))

(defun my/lowercase-p (c)
  (and (>= c ?a) (<= c ?z)))

(defun --my/ruby-camel-to-snake-case (s)
  (cl-loop with acc = nil
           with last-c = nil
           for c in (string-to-list s)
           do (progn
                (when (and last-c (my/lowercase-p last-c) (my/uppercase-p c))
                  (push ?_ acc))
                (push (downcase c) acc)
                (setq last-c c))
           finally return (let ((acc-rev (reverse acc)))
                            (when (= (car acc-rev) ?_)
                              (pop acc-rev))
                            (apply #'string acc-rev))))

(defun my/ruby-search-definition-of-symbol-at-point ()
  (interactive)
  (let ((sym (--my/ruby-symbol-at-point #'--my/ruby-symbol-char-p)))
    (my/git-grep-toplevel-filtered
     (s-format "| ${grep} -E '\\.rb$'"
               'aget `(("grep" . ,my/grep-executable)))
     (concat "^\\s*(def|class|module).*" (regexp-quote sym)))))

(defun my/ruby-search-reference-of-symbol-at-point ()
  (interactive)
  (let ((sym (--my/ruby-symbol-at-point #'--my/ruby-symbol-char-p)))
    (my/git-grep-toplevel-filtered
     (s-format "| ${grep} -E '\\.rb$'"
               'aget `(("grep" . ,my/grep-executable)))
     (regexp-quote sym))))

(defun my/ruby-find-file-for-constant-at-point ()
  (interactive)
  (let* ((sym (--my/ruby-symbol-at-point #'--my/ruby-symbol-char-qualified-p))
         (parts (->> sym
                  (s-split "::")
                  (-map #'--my/ruby-camel-to-snake-case))))
    (my/git-find-file-toplevel-filtered
     (s-format "| ${grep} -E '${expression}\\.rb$'"
               'aget `(("grep" . ,my/grep-executable)
                       ("expression" . ,(s-join "/" parts)))))))

(defun my/ruby-find-file-for-unit-test ()
  (interactive)
  (let* ((file-path (buffer-file-name))
         (file-name (car (last (s-split "/" file-path))))
         (filename-no-ext (s-chop-suffix ".rb" file-name)))
    (my/git-find-file-toplevel-filtered
     (s-format "| ${grep} -E '${filename-no-ext}_(test|spec).rb'"
               'aget `(("grep" . ,my/grep-executable)
                       ("filename-no-ext" . ,filename-no-ext))))))

(defun my/ruby-open-files-in-project ()
  (interactive)
  (let* ((toplevel (magit-toplevel))
         (buffers (->> (buffer-list)
                    (-filter
                     (lambda (buf)
                       (and (eq (buffer-local-value 'major-mode buf) 'ruby-mode)
                            (s-prefix? toplevel (buffer-file-name buf)))))
                    (-map #'buffer-name))))
    (switch-to-buffer (completing-read "Buffer: " buffers))))

(defun ruby-mode-setup ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4)

  (define-key ruby-mode-map (kbd "C-c T") 'my/ruby-test-path)
  (define-key ruby-mode-map (kbd "C-c t") 'my/ruby-test-at-point)

  (define-key ruby-mode-map (kbd "C-c p c") 'my/ruby-find-file-for-constant-at-point)
  (define-key ruby-mode-map (kbd "C-c p b") 'my/ruby-open-files-in-project)
  (define-key ruby-mode-map (kbd "C-c p d") 'my/ruby-search-definition-of-symbol-at-point)
  (define-key ruby-mode-map (kbd "C-c p s") 'my/ruby-search-reference-of-symbol-at-point)
  (define-key ruby-mode-map (kbd "C-c p u") 'my/ruby-find-file-for-unit-test))

(add-hook 'ruby-mode-hook 'ruby-mode-setup)

(defun my/find-non-test-ruby-files ()
  (interactive)
  (my/run-in-term "~/.emacs.d/bin/fzf-git-ls-toplevel-non-test-ruby-files-then-open.sh"))

(global-set-key (kbd "C-c p r f") 'my/find-non-test-ruby-files)

(defun my/find-test-files ()
  (interactive)
  (my/run-in-term "~/.emacs.d/bin/fzf-git-ls-toplevel-test-files-then-open.sh"))

(global-set-key (kbd "C-c p t f") 'my/find-test-files)

(defun my/grep-non-test-ruby-files (pattern)
  (interactive "sPattern: ")
  (my/git-grep-toplevel-filtered (s-format "| ${sed} -z -E '/^test\\/|^sorbet\\/|\\/test\\//d' | ${grep} -z -E '(\\.rb|\\.rake)$'"
                                           'aget `(("grep" . ,my/grep-executable)
                                                   ("sed" . ,my/sed-executable)))
                                 pattern))

(global-set-key (kbd "C-c p r g") 'my/grep-non-test-ruby-files)

(defun my/run-toplevel (command)
  (compilation-start
   (s-format "cd ${git-toplevel} && ${command}"
             'aget `(("git-toplevel" . ,(magit-toplevel))
                     ("command" . ,command)))))

(defun my/run-test-path ()
  (interactive)
  (my/run-toplevel (s-format "./bin/test ${filename}"
                             'aget `(("filename" . ,(buffer-file-name))))))

(defun my/run-test-at-point ()
  (interactive)
  (my/run-toplevel (s-format "./bin/test ${filename}:${line-num}"
                             'aget `(("filename" . ,(buffer-file-name))
                                     ("line-num" . ,(number-to-string (line-number-at-pos)))))))

(defun my/rubocop-check-current-file ()
  (interactive)
  (compilation-start
   (concat "cd '" (magit-toplevel) "' && "
           "./bin/rubocop " (buffer-file-name))))

(defun my/sorbet-typecheck-current-file ()
  (interactive)
  (compilation-start
   (concat "cd '" (magit-toplevel) "' && "
           "./bin/srb typecheck " (buffer-file-name))))

(defun my/rubocop-check-modified-files ()
  (interactive)
  (compilation-start
   (concat "cd '" (magit-toplevel) "' && "
           "git status -s | sed -En 's/^.. (.*)/\\1/p' | xargs ./bin/rubocop")))

(defun my/sorbet-typecheck-modified-files ()
  (interactive)
  (compilation-start
   (concat "cd '" (magit-toplevel) "' && "
           "git status -s | sed -En 's/^.. (.*)/\\1/p' | xargs ./bin/srb typecheck")))

(global-set-key (kbd "C-c p r r") 'my/rubocop-check-current-file)
(global-set-key (kbd "C-c p r R") 'my/rubocop-check-modified-files)
(global-set-key (kbd "C-c p r t") 'my/sorbet-typecheck-current-file)
(global-set-key (kbd "C-c p r T") 'my/sorbet-typecheck-modified-files)
