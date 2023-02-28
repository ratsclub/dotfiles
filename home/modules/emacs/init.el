;; -*- lexical-binding: t; -*-

;; disable impure packages
(setq package-archives nil
      package-enable-at-startup nil)

;; eval use-package as fast as possible
(eval-when-compile
  (require 'use-package))

;; always ensure that use-package will download the needed packages
(setq use-package-always-ensure t)

(use-package emacs
  :init
  ;; xdg directories
  (setq user-emacs-config-directory (concat (getenv "HOME") "/.config/emacs")
	user-emacs-data-directory (concat (getenv "HOME") "/.local/share/emacs")
	user-emacs-cache-directory (concat (getenv "HOME") "/.cache/emacs"))

  ;; remove useless welcome screen
  (setq inhibit-startup-screen t)

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
  (setq show-trailing-whitespace t
        indicate-empty-lines t)

  ;; Automatically add a new whiteline at the end of the file while saving
  (setq require-final-newline t)

  ;; Use ~y~ and ~n~ instead of long ~yes~ and ~no~
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; remove scratch initial message
  (setq initial-scratch-message nil)

  ;; backup and lock files
  ;; Instead of littering the current project's directory, we can use
  ;; the xdg variables we defined to improve things up.
  (let ((backup-dir (concat user-emacs-data-directory "/backup")))
    (unless (file-directory-p backup-dir)
      (mkdir backup-dir t))

    (setq backup-directory-alist (cons (cons "." backup-dir) nil)
	  create-lockfiles nil
	  backup-by-copying t))

  :config
  ;; ui
  (menu-bar-mode 0)
  (when (display-graphic-p)
    (tool-bar-mode 0)
    (scroll-bar-mode 0))

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
   (compilation-filter . ansi-color-compilation-filter)))

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
  :ensure t
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
  :ensure t
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
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia
  :ensure t
  :defer t
  :bind
  (("M-A" . marginalia-cycle)
   :map minibuffer-local-map
   ("M-A" . marginalia-cycle))
  
  :init
  (marginalia-mode))

(use-package magit
  :ensure t
  :defer t
  :config
  (use-package forge :defer t))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package eglot
  :ensure t
  :hook ((go-mode . eglot-ensure)
	 (fsharp-mode . eglot-ensure)
	 (rust-mode . eglot-ensure)
	 (typescript-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-c h" . eldoc)
              ("C-c f" . eglot-format)
              ("C-c F" . eglot-format-buffer)
              ("C-c d" . xref-find-definitions-at-mouse))
  :config
  (use-package rust-mode :defer t)
  (use-package eglot-fsharp :defer t)
  (use-package typescript-mode :defer t)
  (require 'eglot-fsharp))

(use-package org
  :ensure org-contrib
  :defines org-element-use-cache
  :config
  (use-package ox-hugo :defer t)
  (use-package org-drill :defer t)
  (use-package org-roam
    :defer t
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n i" . org-roam-node-insert)
	   :map org-roam-dailies-map
           ("Y" . org-roam-dailies-capture-yesterday)
           ("T" . org-roam-dailies-capture-tomorrow))
    :bind-keymap
    ("C-c n d" . org-roam-dailies-map)
    :config
    (require 'org-roam-dailies)
    (org-roam-db-autosync-mode)

    (setq org-roam-capture-templates
	  '(("d" "default" plain "%?" :target
	     (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			;; using `LASTMOD` just so this gets exported with ox-hugo
			"#+TITLE: ${title}\n#+DATE: %U\n#+LASTMOD: %U\n\n")
	     :unnarrowed t)))
    :hook
    ((org-mode . (lambda ()
                        (setq-local time-stamp-active t
				    time-stamp-start "#\\+LASTMOD:[ \t]*"
				    time-stamp-end "$"
				    time-stamp-format "\[%Y-%02m-%02d %3a %02H:%02M\]")
                             (add-hook 'before-save-hook 'time-stamp nil 'local)))))

  ;; here to remove `unsafe local variables` warning on org-roam's
  ;; `org-roam-directory`, this should probably go away... someday
  (setq-default enable-local-variables :all)

  ;; create org directory if not exists
  (setq org-directory "~/org"
	org-roam-directory "~/org/notes")
  (unless (file-directory-p org-directory)
    (mkdir org-directory t)
    (mkdir org-roam-directory t))

  (setq org-element-use-cache nil
	org-startup-indented t

	;; use the language's major mode indentation
	org-src-tab-acts-natively t

	;; set source block indentation to 0
	org-edit-src-content-indentation 0

	;; org-drill
	org-drill-spaced-repetition-algorithm 'sm2))

(use-package fsharp-mode :defer t)
(use-package nix-mode
  :defer t
  :mode "\\.nix\\'"
  :config
  (setq nix-nixfmt-bin "nixpkgs-fmt"))

(use-package direnv
  :config (direnv-mode)
  :custom (direnv-always-show-summary nil))

(use-package treemacs
  :defer t
  :config
  (use-package treemacs-projectile :defer t)
  (treemacs-hide-gitignored-files-mode t))

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
                     (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping)))
