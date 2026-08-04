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

  ;; smoother scrolling
  (pixel-scroll-precision-mode)

  ;; remember point position in visited files
  (save-place-mode 1)

  ;; persist minibuffer history across sessions
  (savehist-mode 1)

  :hook
  ((prog-mode . display-line-numbers-mode)
   (org-mode . display-line-numbers-mode)

   ;; colors in compilation-mode
   (compilation-filter . ansi-color-compilation-filter))

  :bind
  ;; kill up to, but not including, the next occurrence of a char
  (("M-z" . zap-up-to-char))

  :mode
  (("\\.go\\'" . go-ts-mode)
   ("/go\\.mod\\'" . go-mod-ts-mode)
   ("\\.ya?ml$" . yaml-ts-mode)
   ("\\.rs$" . rust-ts-mode)
   ("\\.toml$" . toml-ts-mode)))

(use-package modus-themes
  :ensure nil
  :no-require t
  :demand t
  :init
  (setq modus-themes-italic-constructs t
	modus-themes-bold-constructs t
	modus-themes-mixed-fonts t
	modus-themes-disable-other-themes t
	modus-themes-to-toggle '(modus-vivendi modus-operandi)
	modus-themes-prompts '(bold)
	modus-themes-completions '((matches . (extrabold))
				   (selection . (semibold)))
	modus-themes-headings '((1 . (overline variable-pitch 1.4))
				(2 . (overline variable-pitch 1.25))
				(3 . (overline 1.1))
				(t . (monochrome))))

  (setq modus-themes-common-palette-overrides
	'((bg-region bg-lavender)
	  ;; Keep syntax highlighting visible through the selection.
	  (fg-region unspecified)
	  (string green-cooler)
	  (bg-hl-line bg-cyan-nuanced)
	  (bg-paren-match bg-magenta-intense)
	  (border-mode-line-active unspecified)
	  (border-mode-line-inactive unspecified)))

  :config
  (load-theme 'modus-vivendi t)

  :bind
  (("<f5>" . modus-themes-toggle)))

;; A windowed Emacs is started by launchd (Spotlight, Dock), so it inherits
;; the system default PATH instead of the one zsh builds, leaving everything
;; installed through nix invisible to magit, eglot and friends.
(use-package exec-path-from-shell
  ;; `window-system' is nil while a daemon's init runs, so guarding on it
  ;; silently skips this in `emacs --daemon' and every emacsclient frame
  ;; inherits the bare launchd PATH.
  :if (and (eq system-type 'darwin)
	   (or (display-graphic-p) (daemonp)))
  :init
  (setq exec-path-from-shell-arguments '("-l" "-i")
	exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(use-package server
  :ensure nil
  :preface
  ;; The daemon is not the frontmost app, so its frames open behind whatever
  ;; is on screen. Focusing reaches NS's `x-focus-frame', which activates emacs.
  (defun p/focus-client-frame ()
    "Raise and focus the frame `emacsclient' just created."
    (when (display-graphic-p)
      (select-frame-set-input-focus (selected-frame))))

  :hook (server-after-make-frame . p/focus-client-frame))

(use-package uniquify
  :ensure nil
  :custom
  ;; disambiguate same-named buffers by path instead of <2> suffixes
  (uniquify-buffer-name-style 'forward))

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
  (corfu-popupinfo-delay 0.25)
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
  (global-corfu-mode)
  (corfu-popupinfo-mode)

  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :config
  (corfu-terminal-mode))

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
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
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
  :after project
  :config
  (use-package forge :defer t)
  (use-package magit-todos
    :defer t
    :hook (magit-mode . magit-todos-mode))

  ;; makes magit fullscreen and restore the windows when closing
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1
	magit-bury-buffer-function 'magit-restore-window-configuration))

(use-package project
  :config
  (add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m))
  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel" ?t) t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers" ?T) t))

(use-package ediff
  :ensure nil
  :custom
  ;; keep ediff in a single frame instead of spawning a control frame
  (ediff-window-setup-function 'ediff-setup-windows-plain))

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

(defvar p/org-vault-root (expand-file-name "~/org")
  "Working tree of the git-backed org vault.")

(defvar p/org-sync-process nil
  "Handle of the in-flight sync, so that runs cannot stack up.")

(defun p/org-vault-repo-p ()
  "Return non-nil when the vault exists and is a git repository."
  (file-directory-p (expand-file-name ".git" p/org-vault-root)))

;; Separate from `org-roam-directory' so it can be read before org-roam loads.
(defvar p/org-roam-directory (expand-file-name "roam" p/org-vault-root)
  "Directory holding the roam notes inside the vault.")

(defun p/org-vault-ensure ()
  "Refuse to proceed unless the vault exists as a git repository."
  (unless (p/org-vault-repo-p)
    (user-error "No git repository at %s; create or clone it first"
		p/org-vault-root))
  (make-directory p/org-roam-directory t)
  (when (and (fboundp 'org-roam-db-autosync-mode)
	     (not (bound-and-true-p org-roam-db-autosync-mode)))
    (org-roam-db-autosync-mode 1)))

(defun p/org-sync--steps ()
  "The git invocations making up one round-trip, in order.
Without an upstream there is nothing to pull and the push must create it."
  (let* ((default-directory (file-name-as-directory p/org-vault-root))
	 (upstream (magit-get-upstream-branch)))
    `(,@(and upstream '(("pull" "--rebase" "--autostash")))
      ("add" "-A")
      ("commit" "-m" ,(format "sync(%s): %s"
			      (system-name)
			      (format-time-string "%Y-%m-%d %H:%M:%S")))
      ,(if upstream
	   '("push")
	 '("push" "--set-upstream" "origin" "HEAD")))))

(defun p/org-sync--skip-p (step)
  "Return non-nil when STEP has nothing to do.
Both cases exit non-zero, which would read as failure and abort the rest.
Checked per step, not upfront: the commit creates the HEAD the push wants."
  (pcase (car step)
    ("commit" (not (magit-anything-staged-p)))
    ;; An unchanged vault would otherwise pay a network round-trip every
    ;; five idle minutes to be told nothing happened.
    ("push" (or (not (magit-rev-verify "HEAD"))
		(when-let* ((upstream (magit-get-upstream-branch)))
		  (magit-rev-ancestor-p "HEAD" upstream))))
    (_ nil)))

(defun p/org-sync--report-failure (step)
  "Report STEP as the point where the sync gave up."
  (message "org: vault sync failed on `git %s'" (string-join step " ")))

(defun p/org-sync--failed (step)
  "Report STEP as the point of failure and open the vault in magit."
  (setq p/org-sync-process nil)
  (p/org-sync--report-failure step)
  (magit-status-setup-buffer p/org-vault-root))

(defun p/org-sync--run (steps)
  "Run STEPS in order, each starting only once its predecessor succeeded."
  (if (null steps)
      (progn (setq p/org-sync-process nil)
	     (message "org: vault synced"))
    (let* ((default-directory (file-name-as-directory p/org-vault-root))
	   ;; Save unasked, or an unsaved capture is committed in its old state.
	   (magit-save-repository-buffers 'dontask)
	   ;; Magit would otherwise refresh whichever repository was current when
	   ;; the timer fired, once per step, reverting every file buffer with it.
	   (magit-inhibit-refresh t)
	   (step (car steps)))
      (if (p/org-sync--skip-p step)
	  (p/org-sync--run (cdr steps))
	(let ((process (apply #'magit-run-git-async step)))
	  (setq p/org-sync-process process)
	  ;; `magit-start-process' documents replacing the sentinel before the
	  ;; process runs; magit's own still runs first, keeping its bookkeeping.
	  (set-process-sentinel
	   process
	   (lambda (proc event)
	     (magit-process-sentinel proc event)
	     ;; A pty sentinel can fire twice for one exit, so only the call
	     ;; still owning `p/org-sync-process' may advance or fail the chain.
	     (when (and (memq (process-status proc) '(exit signal))
			(eq p/org-sync-process proc))
	       (if (zerop (process-exit-status proc))
		   (p/org-sync--run (cdr steps))
		 (p/org-sync--failed step))))))))))

(defun p/org-sync-start ()
  "Begin a sync unless one is already in flight."
  (require 'magit)
  (unless (process-live-p p/org-sync-process)
    (p/org-sync--run (p/org-sync--steps))))

(defun p/org-sync ()
  "Pull, commit and push the org vault."
  (interactive)
  (p/org-vault-ensure)
  (p/org-sync-start))

(defun p/org-sync-idle ()
  "Sync from the idle timer, staying quiet when there is no repository yet.
Runs even on a clean worktree, or a machine only read from never pulls."
  (when (p/org-vault-repo-p)
    (p/org-sync-start)))

(defun p/org-sync-on-exit ()
  "Synchronously sync the vault while leaving Emacs.
Blocks because the async chain would be killed mid-flight; prompting is off
so quitting cannot hang on a passphrase nobody can see."
  (when (p/org-vault-repo-p)
    (require 'magit)
    (when (process-live-p p/org-sync-process)
      (kill-process p/org-sync-process))
    (let ((default-directory (file-name-as-directory p/org-vault-root))
	  (magit-save-repository-buffers 'dontask))
      (with-environment-variables (("GIT_TERMINAL_PROMPT" "0")
				   ("GIT_SSH_COMMAND" "ssh -o BatchMode=yes"))
	(catch 'p/org-sync-failed
	  (dolist (step (p/org-sync--steps))
	    (unless (p/org-sync--skip-p step)
	      (unless (zerop (apply #'magit-call-git step))
		(p/org-sync--report-failure step)
		(throw 'p/org-sync-failed nil)))))))))

;; Cancel first, so re-evaluating this file does not stack up timers.
(cancel-function-timers #'p/org-sync-idle)
(run-with-idle-timer (* 5 60) t #'p/org-sync-idle)

(add-hook 'kill-emacs-hook #'p/org-sync-on-exit)

(use-package org
  :ensure org-contrib
  :defines org-element-use-cache
  :config

  ;; add items to structure template list
  (add-to-list 'org-structure-template-alist '("d" . "description"))

  (setq org-directory p/org-vault-root
	org-log-done 'time

	org-id-locations-file (expand-file-name "org-id-locations"
						user-emacs-data-directory)

	org-element-use-cache nil
	org-startup-indented t

	;; use the language's major mode indentation
	org-src-tab-acts-natively t

	;; set source block indentation to 0
	org-edit-src-content-indentation 0))

(use-package org-roam
  :preface
  (defun p/org-vault-ensure-advice (&rest _)
    "Swallow arguments so `p/org-vault-ensure' can advise other commands."
    (p/org-vault-ensure))

  (defun p/org-roam-capture-note ()
    "Capture a new note."
    (interactive)
    (org-roam-capture nil "n"))

  :init
  (setq org-roam-directory p/org-roam-directory
	org-roam-dailies-directory "daily/"
	org-roam-db-location (expand-file-name "org-roam.db" user-emacs-cache-directory))

  (setq org-roam-capture-templates
	'(("n" "note" plain "%?"
	   :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			      "#+title: ${title}\n#+filetags:\n\n")
	   :unnarrowed t)))

  (setq org-roam-dailies-capture-templates
	'(("d" "entry" entry "* %<%H:%M> %?"
	   :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

  :config
  (when (file-directory-p org-roam-directory)
    (org-roam-db-autosync-mode))

  ;; Every path creating a note funnels through `org-roam-capture-': captures,
  ;; dailies, and `org-roam-node-insert' on a node that does not exist yet.
  (advice-add 'org-roam-capture- :before #'p/org-vault-ensure-advice)

  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n l" . org-roam-buffer-toggle)
   ("C-c n a" . org-roam-alias-add)
   ("C-c n c" . p/org-roam-capture-note)
   ("C-c n s" . p/org-sync)
   ("C-c n d t" . org-roam-dailies-goto-today)
   ("C-c n d y" . org-roam-dailies-goto-yesterday)
   ("C-c n d d" . org-roam-dailies-goto-date)
   ("C-c n d n" . org-roam-dailies-capture-today)))

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

(use-package ghostel
  :after project
  :config
  ;; Don't paint whitespaces on screen
  (defun p/setup-ghostel ()
    (setq-local show-trailing-whitespace nil
		indicate-empty-lines nil))

  :hook ((ghostel-mode . p/setup-ghostel))

  :bind
  (("C-x m" . ghostel)
   :map ghostel-semi-char-mode-map
   ("C-s" . consult-line)
   :map project-prefix-map
   ("t" . ghostel-project)
   ("T" . ghostel-project-list-buffers)))

(use-package agent-shell
  :defer t
  :init
  (setq agent-shell-preferred-agent-config 'claude-code)

  ;; Transcripts and screenshots go under `user-emacs-data-directory`
  ;; not the project's folder.
  (defun p/agent-shell-dot-subdir (subdir)
    "Resolve agent-shell's SUBDIR under `user-emacs-data-directory'.
Keyed by the project's full path, so same-named checkouts stay distinct."
    (file-name-concat user-emacs-data-directory
		      "agent-shell"
		      (replace-regexp-in-string
		       "/" "-" (string-remove-prefix
				"/" (directory-file-name (agent-shell-cwd))))
		      subdir))

  (setq agent-shell-dot-subdir-function #'p/agent-shell-dot-subdir)

  :config
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))

  ;; Inherit environment ($PATH and others) from GNU Emacs
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables :inherit-env t))

  ;; Don't paint whitespaces on screen
  (defun p/setup-agent-shell ()
    (setq-local show-trailing-whitespace nil
		indicate-empty-lines nil))

  :hook ((agent-shell-mode . p/setup-agent-shell))

  :bind
  (("C-c a a" . agent-shell)
   ("C-c a t" . agent-shell-toggle)
   ("C-c a c" . agent-shell-prompt-compose)))

(use-package proced
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 3)
  (proced-enable-color-flag t)
  (proced-show-remote-processes t))

(use-package multiple-cursors
  :bind
  (("C-M-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))
