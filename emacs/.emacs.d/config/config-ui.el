;; This makes scroll a bit nicer, but it can be annoying... not decided whether to keep it or not yet
(setq scroll-margin 3
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

(setq create-lockfiles nil)

(unless (file-exists-p "~/.saves")
  (make-directory "~/.saves"))

(setq auto-save-file-name-transforms `((".*" "~/.saves" t)))

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

(provide 'config-ui)
