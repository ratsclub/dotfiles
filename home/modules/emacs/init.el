;; -*- lexical-binding: t; -*-

;; disable impure packages
(setq package-archives nil
      package-enable-at-startup nil)

;; eval use-package as fast as possible
(eval-when-compile
  (require 'use-package))

(use-package emacs
  :init
  ;; remove useless welcome screen
  (setq inhibit-startup-screen t)

  ;; remove bell ring
  (setq ring-bell-function 'ignore)

  ;; Use utf-8 by default
  (setq coding-system-for-read 'utf-8)
  (setq coding-system-for-write 'utf-8)

  ;; Sentence SHOULD end with only a point.
  (setq sentence-end-double-space nil)

  ;; Toggle wrapping text at the 80th character
  (setq default-fill-column 80)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq initial-scratch-message nil)

  ;; backup files
  (setq make-backup-files nil
	auto-save-default nil)

  :config
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
  (pixel-scroll-precision-mode))

(use-package diplay-line-numbers
  :hook
  ((prog-mode . display-line-numbers-mode)))

(use-package windmove
  :bind
  (("C-c <left>" .  'windmove-left)
   ("C-c <right>" . 'windmove-right)
   ("C-c <up>" .    'windmove-up)
   ("C-c <down>" .  'windmove-down)))

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
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package consult
  :ensure t
  :hook ((completion-list-mode . consult-preview-at-point-mode))
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  
  (advice-add #'register-preview :override #'consult-register-window)
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
  :defer t)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package eglot
  :ensure t
  :hook ((go-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-c h" . eldoc)
              ("C-c f" . eglot-format)
              ("C-c F" . eglot-format-buffer)))

(use-package org
  :ensure t
  :defines org-element-use-cache
  :config
  (setq org-element-use-cache nil
	org-startup-indented t))
