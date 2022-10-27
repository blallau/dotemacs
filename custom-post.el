;;
;; usefull custo taken from: https://github.com/ianyepan/yay-evil-emacs
;;

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l"
              lsp-keep-workspace-alive nil
              lsp-signature-auto-activate nil
              lsp-modeline-code-actions-enable nil
              lsp-modeline-diagnostics-enable nil
              lsp-modeline-workspace-status-enable nil
              lsp-headerline-breadcrumb-enable t

              lsp-semantic-tokens-enable t
              lsp-progress-spinner-type 'progress-bar-filled

              lsp-enable-file-watchers nil
              lsp-enable-folding nil
              lsp-enable-symbol-highlighting nil
              lsp-enable-text-document-color nil

              lsp-enable-indentation nil
              lsp-enable-on-type-formatting nil

              ;; For diagnostics
              lsp-diagnostics-disabled-modes '(markdown-mode gfm-mode)

              ;; For clients
              lsp-clients-python-library-directories '("/usr/local/" "/usr/")

              lsp-ansible-ansible-path "/usr/local/bin/ansible"
              lsp-ansible-python-interpreter-path "/usr/bin/python3"
              lsp-ansible-validation-lint-path "/usr/local/bin/ansible-lint"

              lsp-auto-guess-root t
              lsp-go-gopls-server-path "/home/T0125936/go/bin/gopls"))

;; Zoom in/out
;;;;;;;;;;;;;;;;
(use-package emacs
  :ensure nil
  :after (face-remap)
  :preface
  (define-globalized-minor-mode
    global-text-scale-mode
    text-scale-mode
    (lambda () (text-scale-mode 1)))

  (defun global-text-scale-adjust (inc)
    (interactive)
    (text-scale-set 1)
    (kill-local-variable 'text-scale-mode-amount)
    (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
    (global-text-scale-mode 1))
  :config
  (global-set-key (kbd "M-0")
                  '(lambda ()
                     (interactive)
                     (global-text-scale-adjust (- text-scale-mode-amount))
                     (global-text-scale-mode -1)))
  (global-set-key (kbd "M-+")
                  '(lambda ()
                     (interactive)
                     (global-text-scale-adjust 1)))
  (global-set-key (kbd "M--")
                  '(lambda ()
                     (interactive)
                     (global-text-scale-adjust -1))))

;; Modify default window split
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emacs
  :ensure nil
  :preface
  (defun ian/split-and-follow-horizontally ()
    "Split window below."
    (interactive)
    (split-window-below)
    (other-window 1))
  (defun ian/split-and-follow-vertically ()
    "Split window right."
    (interactive)
    (split-window-right)
    (other-window 1))
  :bind
  (("C-x 2" . 'ian/split-and-follow-horizontally)
   ("C-x 3" . 'ian/split-and-follow-vertically)))

;; File-related tweaks
;; Don’t bother confirming killing processes and don’t let backup~ files scatter around.
(use-package files
  :ensure nil
  :config
  (setq confirm-kill-processes nil
        create-lockfiles nil))

;; Automatically refreshes the buffer for changes outside of Emacs
;; Auto refreshes every 2 seconds. Don’t forget to refresh the version control status as well.
;; Note(bl): override autorevert from centaur
(use-package autorevert
  :ensure nil
  :config
  (global-auto-revert-mode +1)
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

;; Clean up whitespace on save
(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

;; Mouse wheel (track-pad) scroll speed
;; By default, the scrolling is way too fast to be precise and helpful.
(use-package mwheel
  :ensure nil
  :config (setq mouse-wheel-scroll-amount '(2 ((shift) . 1))
                mouse-wheel-progressive-speed nil))

;;
;; https://git.sr.ht/~sirn/dotfiles/tree/be8905bf233e10acd5083c82ca538eaf6045279d/item/etc/emacs/packages/tool-ansible.el
(use-package ansible
  :after yaml-mode
  :commands ansible
  :preface
  (setq bl/ansible-filename-re
        ".*\\(main\.yml\\|site\.yml\\|encrypted\.yml\\|roles/.+\.yml\\|group_vars/.+\\|host_vars/.+\\)")
  (defun bl/ansible-maybe-enable ()
    (and (stringp buffer-file-name)
         (string-match bl/ansible-filename-re buffer-file-name)))
  (defun bl/setup-ansible-maybe ()
    (when (bl/ansible-maybe-enable)
      (ansible t)))
  :init
  (add-hook 'yaml-mode-hook 'bl/setup-ansible-maybe))

(use-package ansible-doc
  :after ansible
  :diminish ansible-doc-mode
  :bind (:map ansible-doc-mode-map
         ("<f1>" . ansible-doc))
  :commands
  (ansible-doc
   ansible-doc-mode)
  :preface
  (defun bl/setup-ansible-doc ()
    (ansible-doc-mode t))
  :init
  (add-hook 'ansible-hook 'bl/setup-ansible-doc))

(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items 50))

(use-package markdown-mode
  :bind (:map markdown-mode-map
         ("<f2>" . markdown-outline-previous)
         ("<f3>" . markdown-outline-next)))

;; Display line changes in gutter based on git history. Enable it everywhere.
;; Gutter Icons indicating inserted, modified or deleted lines
;; (use-package git-gutter
;;   :init
;;   (setq git-gutter:handled-backends '(git)
;;         git-gutter:hide-gutter t
;;         git-gutter:update-interval 0.02)
;;   (global-git-gutter-mode +1)
;;   :config
;;   (defun git-gutter-reset-to-head-parent()
;;     (interactive)
;;     (let (parent (filename (buffer-file-name)))
;;       (if (eq git-gutter:vcs-type 'svn)
;;           (setq parent "PREV")
;;         (setq parent (if filename (concat (shell-command-to-string (concat "git --no-pager log --oneline -n1 --pretty='format:%H' " filename)) "^") "HEAD^")))
;;       (git-gutter:set-start-revision parent)
;;       (message "git-gutter:set-start-revision HEAD^")))
;;   (defun git-gutter-reset-to-default ()
;;     (interactive)
;;     (git-gutter:set-start-revision nil)
;;     (message "git-gutter reset"))
;;   :bind
;;   (
;;    ;; ("C-x p" . git-gutter:previous-hunk)
;;    ;; ("C-x n" . git-gutter:next-hunk)
;;    ;; Stage current hunk
;;    ("C-x v s" . git-gutter:stage-hunk)
;;    ;; Revert current hunk
;;    ("C-x v r" . git-gutter:revert-hunk)
;;    ;;
;;    ("<C-f5>" . git-gutter-reset-to-head-parent)
;;    ("<C-S-f5>" . git-gutter-reset-to-default)
;;    ;; Jump to next/previous hunk
;;    ("<C-f2>" . git-gutter:previous-hunk)
;;    ("<C-f3>" . git-gutter:next-hunk)))
;; (use-package git-gutter-fringe
;;   :config
;;   (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
;;   (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;; fzf is a fuzzy file finder which is very quick.
(use-package fzf)

;; ;; deadgrep uses rg to search for strings, project.el means it will automatically use the project root
;; ;; if (e.g.) it's a git repository, which is my usual use case.
;; (use-package deadgrep)

;; Dumb Jump uses The Silver Searcher ag, ripgrep rg, or grep to find potential definitions of a function or variable under point.
;; It uses a set of regular expressions based on the file extension, or major-mode, of the current buffer.
;; The matches are run through a shared set of heuristic methods to find the best candidate to jump to.
;; If it can't decide it will present the user with a list in a pop-menu, helm, or ivy (see dumb-jump-selector).
(use-package dumb-jump)
;; :bind (("C-M-g" . dumb-jump-go)
;;        ("C-M-p" . dumb-jump-back)
;;        ("C-M-q" . dumb-jump-quick-look)))
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate t)

(use-package flycheck
  :custom
  (flycheck-disabled-checkers '(python-flake8)))

;; (use-package git-commit-insert-issue)

;; Easier Window Switching using meta key
(windmove-default-keybindings 'meta)

;;;;::::::::::
;; SHORTKEYS
;;;;;;;;;;;;;;
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "<f1>") 'eldoc)
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))
(global-set-key (kbd "<C-f1>") 'show-file-name)
(global-set-key (kbd "<f2>") 'xref-pop-marker-stack)
(global-set-key (kbd "<f3>") 'xref-find-definitions)
(global-set-key (kbd "<C-f3>") 'xref-find-references)
;; To look for a regex pattern we use
(global-set-key (kbd "<C-S-f3>") 'xref-find-apropos)

(global-set-key (kbd "<f4>") 'counsel-grep)
(global-set-key (kbd "<C-f4>") 'projectile-ripgrep)
(global-set-key (kbd "<C-S-f4>") 'rgrep)

(global-set-key (kbd "<f5>") 'magit-blame)
(global-set-key (kbd "<f8>") 'magit-status)
(global-set-key (kbd "<C-f8>") 'magit-checkout)
(global-set-key (kbd "<C-S-f8>") 'magit-log-buffer-file)

(global-set-key (kbd "<f9>") 'flyspell-auto-correct-word)
(global-set-key (kbd "<C-f9>") 'flyspell-correct-word-before-point)

(setq eglot-extend-to-xref t)

;; (use-package jinja2-mode :defer t)

;; (use-package plantuml
;;   :ensure t
;;   :init
;;   (setq plantuml-jar-path "~/work/plantuml-1.2022.1.jar"
;;         plantuml-default-exec-mode 'jar)
;;   (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))
(setq org-plantuml-jar-path "~/work/plantuml-1.2022.1.jar")

;; Given ~/Projects/FOSS/emacs/lisp/comint.el
;; auto => emacs/l/comint.el (in a project) or comint.el
;; truncate-upto-project => ~/P/F/emacs/lisp/comint.el
;; truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
;; truncate-with-project => emacs/l/comint.el
;; truncate-except-project => ~/P/F/emacs/l/comint.el
;; truncate-upto-root => ~/P/F/e/lisp/comint.el
;; truncate-all => ~/P/F/e/l/comint.el
;; truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
;; relative-from-project => emacs/lisp/comint.el
;; relative-to-project => lisp/comint.el
;; file-name => comint.el
;; buffer-name => comint.el<2> (uniquify buffer name)
(setq doom-modeline-buffer-file-name-style 'relative-from-project)
(setq doom-modeline-project-detection 'projectile)

;; ;; Copy the path to your kill ring instead of placing it into your buffer
;; (defun my/filename ()
;;   "Copy the full path of the current buffer."
;;   (interactive)
;;   (kill-new (buffer-file-name (window-buffer (minibuffer-selected-window)))))

;; (global-set-key (kbd "<C-S-f1>") 'my/filename)
(use-package emacs
  :ensure nil
  :after (projectile)
  :config
  (defvar jira-url "https://thales-factory.atlassian.net/" "Jira URL")
  (defun open-jira-ID-at-point ()
    (interactive)
    (when (projectile-project-p)
      (let ((project-name (projectile-project-name))
            (identifiant (buffer-substring (line-beginning-position) (line-end-position)))
            (case-fold-search t))
        (when (string-equal project-name "kast")
          (cond
           ((string-match "kas-\\([[:alnum:]]+\\)[[:space:]]*" identifiant)
            (browse-url (concat jira-url "browse/KAS-" (match-string 1 identifiant))))
           (t
            (message "Jira ID (KAS-XXXX) not found")))))))
  :bind
  (("<f12>" . 'open-jira-ID-at-point)))
