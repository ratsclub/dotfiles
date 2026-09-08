;; -*- lexical-binding: t; -*-

;; Collecting during startup is wasted work, since almost everything init.el
;; conses is still live when it finishes. Defer it completely, then settle at a
;; threshold low enough that steady-state pauses stay short.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Every `load' and `require' matches the filename against every regexp in
;; here. Nothing during startup needs TRAMP or jka-compr, so empty it and put
;; it back once init is done.
(let ((cached-file-name-handler-alist file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold (* 16 1024 1024)
                    gc-cons-percentage 0.1)
              ;; Append rather than assign, so a handler registered during init
              ;; survives. The cache predates it.
              (setq file-name-handler-alist
                    (delete-dups (append file-name-handler-alist
                                         cached-file-name-handler-alist))))))
