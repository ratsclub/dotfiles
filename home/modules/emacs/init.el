;; -*- lexical-binding: t; -*-

;; disable impure packages
(setq package-archives nil
      package-enable-at-startup nil)

;; eval use-package as fast as possible
(eval-when-compile
  (require 'use-package))

;; always ensure that use-package will download the needed packages
(setq use-package-always-ensure nil)

;; ensure bind-key is available
(use-package bind-key)

(use-package emacs
  :init
  ;; xdg directories
  (setq user-emacs-config-directory (concat (getenv "HOME") "/.config/emacs")
	user-emacs-data-directory (concat (getenv "HOME") "/.local/share/emacs")
	user-emacs-cache-directory (concat (getenv "HOME") "/.cache/emacs"))

  ;; set font
  (set-face-attribute 'default nil :font "JetBrains Mono")

  ;; remove useless welcome screen
  (setq inhibit-startup-screen t
	inhibit-splash-screen t
	inhibit-startup-message t)

  ;; remove ring bell sound and activate visual bell
  (setq ring-bell-function 'ignore
	visible-bell t)

  ;; Use 80 columns to keep things readable with split windows.
  (setq whitespace-style '(trailing lines space-before-tab)
        whitespace-line-column 80
        default-fill-column 80)

  ;; Use utf-8 by default
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  ;; Consider a period followed by a single space to be end of
  ;; sentence.
  (setq sentence-end-double-space nil)

  ;; Show stray whitespaces.
  (setq-default show-trailing-whitespace t
		indicate-empty-lines t)

  ;; Automatically add a new whiteline at the end of the file while saving
  (setq require-final-newline t)

  ;; Use ~y~ and ~n~ instead of long ~yes~ and ~no~
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; remove scratch initial message
  (setq initial-scratch-message nil)

  ;; case-insensitive completion and search
  (setq case-fold-search t
	completion-ignore-case t
	read-file-name-completion-ignore-case t
	read-buffer-completion-ignore-case t)

  ;; backup and lock files
  ;; Instead of littering the current project's directory, we can use
  ;; the xdg variables we defined to improve things up.
  (let ((backup-dir (concat user-emacs-data-directory "/backup/")))
    (unless (file-directory-p backup-dir)
      (mkdir backup-dir t))

    (setq auto-save-file-name-transforms `((".*" ,backup-dir t))
	  backup-directory-alist `(("." . ,backup-dir))
	  create-lockfiles nil
	  backup-by-copying t))

  :config
  ;; ui
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)

  (column-number-mode)
  (display-time)

  ;; theme
  (load-theme 'modus-vivendi t)
  (setq modus-themes-region '(accented)
	modus-themes-org-blocks 'gray-background
	modus-themes-fringes 'subtle
	modus-themes-italic-constructs t
	modus-themes-bold-constructs t
	modus-themes-syntax '(green-strings)
	modus-themes-hl-line '(intense)
	modus-themes-paren-match '(intense)
	modus-themes-mode-line '(moody borderless)
	modus-themes-headings (quote ((1 . (overline variable-pitch 1.4))
                                      (2 . (overline variable-pitch 1.25))
                                      (3 . (overline 1.1))
                                      (t . (monochrome)))))

  ;; smoother scrolling
  (pixel-scroll-precision-mode)

  :hook
  ((prog-mode . display-line-numbers-mode)
   (org-mode . display-line-numbers-mode)

   ;; colors in compilation-mode
   (compilation-filter . ansi-color-compilation-filter))

  :mode
  (("\\.go\\'" . go-ts-mode)
   ("/go\\.mod\\'" . go-mod-ts-mode)
   ("\\.ya?ml$" . yaml-ts-mode)
   ("\\.rs$" . rust-ts-mode)
   ("\\.toml$" . toml-ts-mode)))

(use-package windmove
  :bind
  (("C-c <left>" .  'windmove-left)
   ("C-c <right>" . 'windmove-right)
   ("C-c <up>" .    'windmove-up)
   ("C-c <down>" .  'windmove-down)))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package corfu
  :init
  (setq tab-always-indent 'complete
	completion-cycle-threshold nil)

  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)
  (corfu-popupinfo-delay corfu-auto-delay)
  (corfu-min-width 40)
  (corfu-max-width 80)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle t)
  (corfu-quit-at-boundary nil)
  (corfu-preselect-first t)
  (corfu-popupinfo-mode)
  :hook
  '((prog-mode . corfu-mode)
    (shell-mode . corfu-mode)
    (eshell-mode . corfu-mode))

  :config
  (use-package corfu-terminal :defer t)
  (unless (display-graphic-p)
    (corfu-terminal-mode +1))
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package consult
  :hook ((completion-list-mode . consult-preview-at-point-mode))
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'project-find-regexp :override #'consult-ripgrep)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :bind
  (("C-s" . consult-line))

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<"))

(use-package vertico
  :init
  (vertico-mode))

(use-package marginalia
  :defer t
  :bind
  (("M-A" . marginalia-cycle)
   :map minibuffer-local-map
   ("M-A" . marginalia-cycle))

  :init
  (marginalia-mode))

(use-package magit
  :defer t
  :config
  (use-package forge :defer t)
  (use-package magit-todos
    :defer t
    :hook (magit-mode . magit-todos-mode))

  ;; makes magit fullscreen and restore the windows when closing
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1
	magit-bury-buffer-function 'magit-restore-window-configuration))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package eglot
  :hook ((go-ts-mode . eglot-ensure)
	 (fsharp-mode . eglot-ensure)
	 (rust-ts-mode . eglot-ensure)
	 (typescript-mode . eglot-ensure)
	 (nix-mode . eglot-ensure)
	 (sml-mode . eglot-ensure)
	 (eglot-managed-mode . (lambda ()
				 ;; Show flymake diagnostics first.
				 (setq eldoc-documentation-functions
				       (cons #'flymake-eldoc-function
					     (remove #'flymake-eldoc-function eldoc-documentation-functions)))
				 ;; Show all eldoc feedback.
				 (setq eldoc-documentation-strategy #'eldoc-documentation-compose))))
  :bind (:map eglot-mode-map
	      ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l h" . eldoc)
              ("C-c l f" . eglot-format)
              ("C-c l F" . eglot-format-buffer)
              ("C-c l d" . xref-find-definitions-at-mouse)
	      ;; sometimes ionide acts up
	      ("C-c l R" . eglot-reconnect))
  :config
  (use-package eglot-fsharp :defer t)
  (use-package typescript-mode :defer t)
  (require 'eglot-fsharp)
  (add-to-list 'eglot-server-programs '((rust-ts-mode) "rust-analyzer"))
  (add-to-list 'eglot-server-programs '((sml-mode) "millet-ls"))
  (add-to-list 'eglot-server-programs
	       '((javascript-mode typescript-ts-mode) "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil"))))

(use-package org
  :ensure org-contrib
  :defines org-element-use-cache
  :config
  (use-package citeproc)
  (use-package ox-hugo :defer t)
  (use-package org-drill
    :defer t
    :config
    (setq org-drill-spaced-repetition-algorithm 'sm2))

  (use-package org-super-agenda
    :after org-agenda
    :config (org-super-agenda-mode))

  ;; add items to structure template list
  (add-to-list 'org-structure-template-alist '("d" . "description"))

  (setq org-directory "~/org"
	org-log-done 'time

	org-element-use-cache nil
	org-startup-indented t

	;; use the language's major mode indentation
	org-src-tab-acts-natively t

	;; configure reference file
	org-cite-global-bibliography (list (concat org-directory "/references.bib"))

	;; configure cite export
	org-cite-export-processors '((latex biblatex)
                                     (moderncv basic)
				     (md csl)
                                     (html csl)
                                     (t csl))

	;; set source block indentation to 0
	org-edit-src-content-indentation 0

	;; todo keywords to cycle through
	org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "IDEA(i)" "|" "DONE(d)"))

	;; todo file used on org-capture for org-agenda
	+org-capture-todo-file (concat org-directory "/todo.org")

	;; org-drill file used for SRS
	+org-capture-drill-file (concat org-directory "/drill.org")
	org-capture-templates `(("p" "Personal")
				("pt" "Personal todo" entry
				 (file+headline +org-capture-todo-file "Personal")
				 "* TODO %?  :personal:\n" :prepend t)
				("pi" "Personal idea" entry
				 (file+headline +org-capture-todo-file "Personal")
				 "* IDEA %? \n" :prepend t)
				("pn" "Personal note" entry
				 (file+headline +org-capture-todo-file "Personal")
				 "* TODO %?  :personal:\n%i\n%a" :prepend t)
				("w" "Work")
				("wt" "Work todo" entry
				 (file+headline +org-capture-todo-file "Work")
				 "* TODO %?  :work:\n%i\n" :prepend t)
				("wn" "Work note" entry
				 (file+headline +org-capture-todo-file "Work")
				 "* TODO %?  :work:\n%i\n%a" :prepend t)
				("d" "Drill")
				("dd" "Drill simple" entry
				 (file +org-capture-drill-file)
				 "* Item :drill:\n%?\n")
				("dc" "Drill cloze 2" entry
				 (file +org-capture-drill-file)
				 ,(concat "* Item           :drill:\n"
					  ":PROPERTIES:\n"
					  ":drill_card_type: hide2cloze\n\n"
					  ":END:\n"
					  "%?\n")))

	;; org-agenda
	org-agenda-files (list org-directory)
	org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-tags-column 100 ;; from testing this seems to be a good value
        org-agenda-compact-blocks t
        org-agenda-custom-commands
        '(("o" "Overview"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
				  :time-grid t
				  :date today
				  :todo "TODAY"
				  :scheduled today
				  :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Important"    :tag "Important" :priority "A" :order 6)
                            (:name "Due Today"    :deadline today  :order 2)
                            (:name "Due Soon"     :deadline future :order 8)
                            (:name "Overdue"      :deadline past   :face error :order 7)

                            (:name "To read"      :tag "read"      :order 30)
                            (:name "Waiting"      :todo "WAIT"     :order 20)
                            (:name "Work"         :tag "work"      :order 32)
                            (:name "Personal"     :tag "personal"  :order 14)
			    (:name "Future Ideas" :todo "IDEA"     :order 32)))))))))
  :hook
  ((org-capture-mode . org-align-all-tags))

  :bind
  (("C-c c" .  'org-capture)
   ("C-c a" . 'org-agenda)))

(use-package fsharp-mode :defer t)
(use-package sml-mode
  :defer t
  :mode ("\\.\\(sml\\|sig\\)\\'" . sml-mode))

(use-package nix-mode
  :defer t
  :mode "\\.nix\\'"
  :config
  (setq nix-nixfmt-bin "nixpkgs-fmt"))

(use-package direnv
  :config (direnv-mode)
  :custom (direnv-always-show-summary nil))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package treemacs
  :defer t
  :config
  (use-package treemacs-projectile :defer t)
  (setq treemacs-no-png-images t)
  (treemacs-git-mode 'extended))

(use-package treesit
  :preface
  (dolist (mapping '((python-mode . python-ts-mode)
                     (css-mode . css-ts-mode)
                     (typescript-mode . tsx-ts-mode)
		     (rust-mode . rust-ts-mode)
		     (go-mode . go-ts-mode)
                     (js-mode . js-ts-mode)
		     (json-mode . json-ts-mode)
		     (toml-mode . toml-ts-mode)
                     (css-mode . css-ts-mode)
		     (java-mode . java-ts-mode)
                     (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping)))

(use-package switch-window
  :config
  (setq switch-window-shortcut-style 'qwerty)
  :bind (("C-x o" . switch-window)))

(use-package gnus
  :ensure nil
  :demand t

  :custom
  (mail-use-agent 'gnus-user-agent)
  (gnus-novice-user nil)

  :config

  ;; declutter $HOME
  (setq gnus-directory (concat user-emacs-data-directory "/gnus")
	;; all of them depend on the `gnus-directory` variable
	gnus-startup-file (concat gnus-directory "/.newsrc")
	gnus-cache-directory (concat gnus-directory  "/news/cache")
	gnus-article-save-directory (concat gnus-directory "/news")
	gnus-kill-files-directory (concat gnus-directory "/news")
	nndraft-directory (concat gnus-directory "/mail/draft")
	nnfolder-directory (concat gnus-directory "/mail/archive"))

  ;; set smtp as the default way of sending messages
  (setq message-send-mail-function 'message-use-send-mail-function
	send-mail-function 'smtpmail-send-it)

  ;; remove backends in order to make secondary methods the default
  (setq gnus-select-method '(nnnil nil))
  (setq gnus-secondary-select-methods
	'((nnimap "personal"
                  (nnimap-address "imap.mailbox.org")
                  (nnimap-server-port "imaps")
                  (nnimap-stream ssl)
                  (nnir-search-engine imap)
                  (nnmail-expiry-target "nnimap+home:[Mailbox]/Trash")
                  (nnmail-expiry-wait 'immediate))))

  (setq gnus-posting-styles
	'(("personal"
	   (address "victor@freire.dev.br")
	   (signature "Victor Freire")
	   ("X-Message-SMTP-Method" "smtp smtp.mailbox.org 587 victor@freire.dev.br"))))

  (setq gnus-parameters
	'(("personal"
	   (gcc-self . "nnimap+personal:Sent"))))

  (setq gnus-asynchronous t
	gnus-use-cache t
	gnus-use-header-prefetch t)

  ;; summary
  (setq gnus-sum-thread-tree-false-root " "
	gnus-sum-thread-tree-indent "  "
	gnus-sum-thread-tree-root "r "
	gnus-sum-thread-tree-single-indent "◎ "
	gnus-sum-thread-tree-vertical        "|"
	gnus-sum-thread-tree-leaf-with-other "├─► "
	gnus-sum-thread-tree-single-leaf     "╰─► "

	;; │06-Jan│  Sender Name  │ Email Subject
	gnus-summary-line-format (concat "%0{%U%R%z%}"
					 "%3{│%}" "%1{%d%}" "%3{│%}"
					 "  "
					 "%4{%-20,20f%}"
					 "  "
					 "%3{│%}"
					 " "
					 "%1{%B%}"
					 "%s\n"))

  :hook ((gnus-group-mode . gnus-topic-mode)
	 (message-mode . flyspell-mode)
	 (gnus-group-mode . hl-line-mode)))

(use-package proced
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 3)
  (proced-enable-color-flag t)
  (proced-show-remote-processes t))

;; my functions
(defun my/slugify-hugo-heading ()
  "Gets the current heading title, slugifies it and sets the
`EXPORT_FILE_NAME` and `EXPORT_HUGO_SLUG` properties with its
value."
  (interactive)
  (let* ((title (org-entry-get nil "ITEM"))
	 (slug (org-hugo-slug title)))
    (org-entry-put nil "EXPORT_FILE_NAME" slug)
    (org-entry-put nil "EXPORT_HUGO_SLUG" slug)
    (message "Successfully added '%s' slug." slug)))
