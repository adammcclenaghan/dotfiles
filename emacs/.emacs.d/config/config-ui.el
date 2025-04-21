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
