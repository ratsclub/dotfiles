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

  ;; set command as meta key on macOS
  (if (eq system-type 'darwin)
      (setq mac-command-modifier      'meta
	    mac-option-modifier       'alt
	    mac-function-modifier     'control
	    mac-right-option-modifier 'alt))

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
  (corfu-quit-no-match t)
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
	 (rust-ts-mode . eglot-ensure)
	 (typescript-ts-mode . eglot-ensure)
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
  (add-to-list 'eglot-server-programs '((rust-ts-mode) "rust-analyzer"))
  (add-to-list 'eglot-server-programs '((sml-mode) "millet-ls"))
  (add-to-list 'eglot-server-programs
	       '((javascript-mode typescript-ts-mode) "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil"))))

(use-package org
  :ensure org-contrib
  :defines org-element-use-cache
  :config

  ;; add items to structure template list
  (add-to-list 'org-structure-template-alist '("d" . "description"))

  (setq org-directory "~/org"
	org-log-done 'time

	org-element-use-cache nil
	org-startup-indented t

	;; use the language's major mode indentation
	org-src-tab-acts-natively t

	;; set source block indentation to 0
	org-edit-src-content-indentation 0))

(use-package sml-mode
  :defer t
  :mode ("\\.\\(sml\\|sig\\)\\'" . sml-mode))

(use-package nix-mode
  :defer t
  :mode "\\.nix\\'"
  :config
  (setq nix-nixfmt-bin "nixfmt"))

(use-package direnv
  :config (direnv-mode)
  :custom (direnv-always-show-summary nil))

(use-package markdown
  :defer t
  :custom
  (markdown-fontify-code-block-natively t))

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
  :mode
  (("\\.tsx\\'"  . tsx-ts-mode)
   ("\\.js\\'"   . typescript-ts-mode)
   ("\\.mjs\\'"  . typescript-ts-mode)
   ("\\.mts\\'"  . typescript-ts-mode)
   ("\\.cjs\\'"  . typescript-ts-mode)
   ("\\.ts\\'"   . typescript-ts-mode)
   ("\\.jsx\\'"  . tsx-ts-mode)
   ("\\.json\\'" . json-ts-mode))
  :preface
  (dolist (mapping '((python-mode . python-ts-mode)
                     (css-mode . css-ts-mode)
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

(use-package eshell
  :ensure nil
  :defer t
  :init
  (setq eshell-hist-ignoredups t
	eshell-history-size 10000)
  (defun p/setup-eshell ()
    (keymap-set eshell-mode-map "C-r" 'consult-history))
  :hook ((eshell-mode . p/setup-eshell)))

(use-package proced
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 3)
  (proced-enable-color-flag t)
  (proced-show-remote-processes t))
