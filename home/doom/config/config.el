;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-
;;; Code:

(setq user-full-name "Victor Freire"
      user-mail-address "victor@freire.dev.br")

;;; :ui doom
(setq doom-font (font-spec :family "JetBrainsMono" :size 15)
      ivy-posframe-font (font-spec :family "JetBrainsMono" :size 15)
      doom-theme 'doom-tomorrow-night)

;;; :ui treemacs
(use-package! treemacs
  :config
  (treemacs-follow-mode t))

;;; :lang org
(setq org-directory "~/Projects/notes"

      ;; org-agenda
      org-log-done 'time
      org-agenda-start-with-log-mode t
      org-agenda-files (list
                        (concat org-directory "/private/tasks.org"))

      ;; roam
      org-roam-directory org-directory
      org-roam-capture-templates '(("d" "default" plain "%?" :target
                                    (file+head "%<%Y%m%d%H%M%S>.org" "#+title: ${title}\n")
                                    :unnarrowed t)))

(after! org
  (add-to-list 'org-modules 'org-habit))

;;; :lang nix
(setq nix-nixfmt-bin "nixpkgs-fmt")

;;; :tools rss
(setq elfeed-search-filter "@6-month-ago"
      rmh-elfeed-org-files (list
                            (concat org-directory "/private/feeds.org")))

;;; :app erc
(setq erc-nick "bookdoorstop")

;;; :email notmuch
(setq mail-signature "Victor Freire")

;;; config.el ends here
