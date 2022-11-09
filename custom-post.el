;; usefull custo taken from: https://github.com/ianyepan/yay-evil-emacs

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

(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (setq centaur-tabs-set-bar 'under
        ;; Navigate through tab groups only
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-show-navigation-buttons nil
        ;; Note: If you're not using Spacmeacs, in order for the underline to display
        ;; correctly you must add the following line:
        x-underline-at-descent-line t
        centaur-tabs-adjust-buffer-order t
        centaur-tabs-height 32)
  (centaur-tabs-mode t)
  (centaur-tabs-enable-buffer-alphabetical-reordering)
  ;; group your tabs by Projectile’s project
  (centaur-tabs-group-by-projectile-project)
  :bind
  ("<C-S-f11>" . centaur-tabs-mode)
  ("C-s-<left>" . centaur-tabs-backward)
  ("C-s-<right>" . centaur-tabs-forward))

;; note(bl) add centaur-tab and minimap
(pretty-hydra-define+ toggles-hydra ()
  (;; these heads are added to the existing "Windows" column
   "Basic"
   (("u" centaur-tabs-mode "centaur mode" :toggle t)
    ("x" minimap-mode "minimap mode" :toggle t))))

;; Dumb Jump uses The Silver Searcher ag, ripgrep rg, or grep to find potential definitions of a function or variable under point.
;; It uses a set of regular expressions based on the file extension, or major-mode, of the current buffer.
;; The matches are run through a shared set of heuristic methods to find the best candidate to jump to.
;; If it can't decide it will present the user with a list in a pop-menu, helm, or ivy (see dumb-jump-selector).
(use-package dumb-jump)

;; Remap <f5> key set by LSP dab-mode
;; note(bl) conflict keymap with LSP dap-mode
(use-package emacs
  :ensure nil
  :config
  (dolist (key '("<f5>" "M-<f5>"))
    (unbind-key key lsp-mode-map)))

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
(use-package emacs
  :ensure nil
  :config
  (setq doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-project-Easier 'detection))

;; Drag stuff (lines, words, region, etc...) around
;; note(bl) change drag-stuff-modifier to avoid conflict with windmove shortcuts
(use-package drag-stuff
  :diminish
  :commands drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (setq drag-stuff-modifier 'hyper)
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

(use-package windmove
  :ensure t
  :config
  ;; use command key on Mac
  (setq windmove-default-keybindings 'meta)
  ;; wrap around at edges
  (setq windmove-wrap-around t))

;; Zoom in/out
;; note(bl) zoom in/out with M-+/M--
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
;; note(bl) give focus to new window
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

;; Increase selected region by semantic units
;; note(bl) add contract-region keymap
(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

;; File-related tweaks
;; Don’t bother confirming killing processes and don’t let backup~ files scatter around.
(use-package files
  :ensure nil
  :config
  (setq confirm-kill-processes nil
        create-lockfiles nil))

;; note(bl) add flycheck-inline to display error in buffer
(use-package flycheck-inline
  :init (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

(use-package flycheck
  :custom
  (flycheck-disabled-checkers '(python-flake8)))

(use-package flycheck-yamllint
  :ensure t
  :defer t
  :init
  (progn
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))))

;; fzf is a fuzzy file finder which is very quick.
(use-package fzf)

;; Search tool
;; note(bl) to fix find-grep command
(use-package grep
  :ensure nil
  :commands grep-apply-setting
  :init
  ;; note(bl) to fix grep command
  (setq grep-use-null-device nil)
  :config
  (cond
   ((executable-find "ugrep")
    (grep-apply-setting
     'grep-command "ugrep --color=auto -0In -e ")
    (grep-apply-setting
     'grep-template "ugrep --color=auto -0In -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("ugrep --color=auto -0Inr -e ''" . 30))
    (grep-apply-setting
     'grep-find-template "ugrep <C> -0Inr -e <R> <D>"))
   ((executable-find "rg")
    (grep-apply-setting
     'grep-command "rg --color=auto --null -nH --no-heading -e ")
    (grep-apply-setting
     'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
    (grep-apply-setting
     ;; note(bl) to fix find-grep command
     'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 45))
    (grep-apply-setting
     'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>"))))

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l"
              lsp-keep-workspace-alive nil
              lsp-signature-auto-activate nil
              lsp-modeline-code-actions-enable nil
              lsp-modeline-diagnostics-enable nil
              lsp-modeline-workspace-status-enable nil
              lsp-headerline-breadcrumb-enable t
              lsp-headerline-breadcrumb-segments '(project file symbols)
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

(use-package markdown-mode
  :bind (:map markdown-mode-map
         ("<f2>" . markdown-outline-previous)
         ("<f3>" . markdown-outline-next)))

(use-package minimap
  :diminish minimap-mode
  :bind
  ("C-<f11>" . minimap-mode)
  :init
  (setq minimap-window-location 'right
        minimap-width-fraction 0.15
        minimap-hide-scroll-bar nil
        minimap-hide-fringes t
        minimap-dedicated-window t
        minimap-always-recenter t
        minimap-minimum-width 20)
  :custom-face
  (minimap-font-face ((t (:height 13 :weight bold :width condensed
                          :spacing dual-width :family "VT323"))))
  (minimap-active-region-background ((t (:extend t :background "gray24")))))

;; Mouse wheel (track-pad) scroll speed
;; By default, the scrolling is way too fast to be precise and helpful.
(use-package mwheel
  :ensure nil
  :config (setq mouse-wheel-scroll-amount '(2 ((shift) . 1))
                mouse-wheel-progressive-speed nil))

;; Search tools
;; Writable `grep' buffer
;; note(bl) change enable key to 'w'
(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t
        wgrep-enable-key "w"))

;; Clean up whitespace on save
(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

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

;; ;; deadgrep uses rg to search for strings, project.el means it will automatically use the project root
;; ;; if (e.g.) it's a git repository, which is my usual use case.
;; (use-package deadgrep)

;; :bind (("C-M-g" . dumb-jump-go)
;;        ("C-M-p" . dumb-jump-back)
;;        ("C-M-q" . dumb-jump-quick-look)))
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate t)

;; (use-package git-commit-insert-issue)

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
(global-set-key (kbd "<C-S-f4>") 'counsel-projectile-git-grep)

(global-set-key (kbd "<f5>") 'magit-blame)
(global-set-key (kbd "<f8>") 'magit-status)
(global-set-key (kbd "<f8> c") 'magit-checkout)
(global-set-key (kbd "<f8> l") 'magit-log-buffer-file)

(global-set-key (kbd "<f9>") 'flyspell-auto-correct-word)
(global-set-key (kbd "<C-f9>") 'flyspell-correct-word-before-point)

;; ;; Copy the path to your kill ring instead of placing it into your buffer
;; (defun my/filename ()
;;   "Copy the full path of the current buffer."
;;   (interactive)
;;   (kill-new (buffer-file-name (window-buffer (minibuffer-selected-window)))))

;; (use-package jinja2-mode :defer t)

;; (use-package plantuml
;;   :ensure t
;;   :init
;;   (setq plantuml-jar-path "~/work/plantuml-1.2022.1.jar"
;;         plantuml-default-exec-mode 'jar)
;;   (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))
(setq org-plantuml-jar-path "~/work/plantuml-1.2022.1.jar")
