(require 'package)

;; makes unpure packages archives unavailable
(setq package-archives nil)

(setq package-enable-at-startup nil)
(package-initialize)

;; UI
(menu-bar-mode -1)
(toggle-scroll-bar -1) 		     
(tool-bar-mode -1)		     

;; remove startup things
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; overwrite selected text
(delete-selection-mode t)

;; remove that fricking sound
(setq ring-bell-function 'ignore)

;; remove backup files
(setq make-backup-files nil)

;; Enable transient mark mode
(transient-mark-mode 1)

;; themes
(setq custom-safe-themes t) ; trust custom themes
(load-theme 'doom-laserwave) ; theme

;; keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c f p") 'my/open-config-files)

;; company
(use-package company
  :defer 1
  :config
  :diminish (company-mode)
  :init (global-company-mode)
  :commands company-mode)

(use-package magit
  :bind
  ("C-c g g" . 'magit-status)
  ("C-c g F" . 'magit-fetch)
  ("C-c g f" . 'magit-find-file))

;; ivy
(use-package ivy
  :init (ivy-mode 1)
  :bind
  (("C-s"     . 'swiper)
   ("C-c s f" . 'counsel-rg)
   ("M-x"     . 'counsel-M-x)))

;; org-mode
(use-package org
  :bind
  (("C-c a" . org-agenda))
  :config
  ;; adds symmetric encryption
  (setq epa-file-select-keys nil)
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
  (setq org-directory "~/org")

  ;; org-agenda
  (setq org-agenda-files (list my/org-agenda-file))
  (setq org-capture-templates
	'(("t" "TODO" entry (file+headline my/org-agenda-file "Agenda")
         "* TODO %?\n  %i\n  %a"))))

(use-package which-key
  :defer 1
  :diminish
  :commands (which-key-mode)
  :config
  (setq which-key-idle-delay 1.0
	which-key-idle-secondary-delay 0.05)
  (which-key-setup-side-window-bottom)
  (which-key-mode))

;; my variables
(defvar my/org-directory "~/org")
(defvar my/org-agenda-file (concat my/org-directory "/agenda.org"))

;; my functions
(defun my/open-config-files ()
  "Open my config file.
   This function opens my configuration file"
  (interactive)
  (counsel-fzf nil (locate-dominating-file "~/dotfiles" ".git")))



