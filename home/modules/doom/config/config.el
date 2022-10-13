;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Code:

(persp-mode)

(setq user-full-name "Victor Freire"
      user-mail-address "victor@freire.dev.br")

;; go
(setq lsp-go-use-gofumpt t)

;; nix
(setq nix-nixfmt-bin "nixpkgs-fmt")

;; notmuch
(setq +notmuch-home-function (lambda () (notmuch-search "tag:inbox"))
      +notmuch-sync-backend 'mbsync)

;; org
(use-package! org
  :hook ((org-capture-mode . org-align-all-tags)))

(after! org
  (setq org-capture-templates
               '(("t" "Personal")
                 ("tt" "Personal todo" entry
                  (file+headline +org-capture-todo-file "Personal")
                  "* TODO %?  :personal:\n" :prepend t)
                 ("tn" "Personal note" entry
                  (file+headline +org-capture-todo-file "Personal")
                  "* TODO %?  :personal:\n%i\n%a" :prepend t)
                 ("w" "Work")
                 ("ww" "Work todo" entry
                  (file+headline +org-capture-todo-file "Work")
                  "* TODO %?  :work:\n%i\n" :prepend t)
                 ("wn" "Work note" entry
                  (file+headline +org-capture-todo-file "Work")
                  "* TODO %?  :work:\n%i\n%a" :prepend t)
                 ("p" "Templates for projects")
                 ("pt" "Project-local todo" entry
                  (file+headline +org-capture-todo-file "Project")
                  "* TODO %?\n%i\n%a" :prepend t))))

(setq org-directory "~/org"
      org-log-done 'time
      org-id-link-to-org-use-id t
      org-agenda-files (list org-directory))

;; org-agenda
(use-package! org-super-agenda
  :after org-agenda
  :commands org-super-agenda-mode)

(after! evil-org-agenda
  (setq org-super-agenda-header-map (copy-keymap evil-org-agenda-mode-map)))

(after! org-agenda
  (org-super-agenda-mode)
  (setq org-agenda-skip-scheduled-if-done t
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
                          '((:name "Important"
                             :tag "Important"
                             :priority "A"
                             :order 6)
                            (:name "Due Today"
                             :deadline today
                             :order 2)
                            (:name "Due Soon"
                             :deadline future
                             :order 8)
                            (:name "Overdue"
                             :deadline past
                             :face error
                             :order 7)
                            (:name "Future Ideas"
                             :todo "IDEA"
                             :order 14)
                            (:name "To read"
                             :tag "read"
                             :order 30)
                            (:name "Waiting"
                             :todo "WAIT"
                             :order 20)
                            (:name "Work"
                             :tag "work"
                             :order 32)
                            (:name "Personal"
                             :tag "personal"
                             :order 32))))))))))

(map! :leader
      (:prefix "n"
       (:prefix "r"
        :desc "Search Roam" "S" (lambda ()
                                  (interactive)
                                  (consult-ripgrep org-roam-directory)))))

;; treemacs
(after! treemacs
  (treemacs-follow-mode)
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

;; ui
(setq doom-theme 'modus-vivendi
      display-line-numbers-type t

      ;; modus
      modus-themes-region '(accented)
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

;;; config.el ends here
