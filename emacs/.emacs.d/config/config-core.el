(setenv "LSP_USE_PLISTS" "true")
(setq auth-sources '("~/.authinfo.gpg"))
(setq native-comp-async-report-warnings-errors 'silent)

(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(server-start)

(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(set-frame-font "Hack-15")
(ido-mode 1)
(setq visible-bell nil
      ring-bell-function 'ignore
      blink-cursor-blinks 0)

(global-set-key (kbd "M-/") 'xref-go-forward)

;; Preview files in dired
(use-package dired-preview
  :ensure t
  ;; :hook (dired-mode . (lambda ()
  ;;                       (when (string-match-p "Pictures" default-directory)
  ;;                         (dired-preview-mode 1))))
  :defer 1
  :hook (after-init . dired-preview-global-mode)
  :config
  (setq dired-preview-max-size (* (expt 2 20) 10))
  (setq dired-preview-delay 0.5)
  (setq dired-preview-ignored-extensions-regexp
        (concat "\\."
                "\\(gz\\|"
                "zst\\|"
                "tar\\|"
                "xz\\|"
                "rar\\|"
                "zip\\|"
                "iso\\|"
                "epub"
                "\\)")))

;; Enable for all dired buffers
(dired-preview-global-mode 1)

; Make sure recentf mode works, for recent file switching with C-c p e
(recentf-mode 1)
(setq recentf-max-menu-items 25)  ;; Modify this as needed to set the number of items
(setq recentf-auto-cleanup 'never) ;; Keep recent files around forever and ever and ever and ever and....

(winner-mode 1)

;; Force gpg to ask in minibuffer
(setq epg-pinentry-mode 'loopback)

(provide 'config-core)
