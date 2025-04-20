(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list)
         ("M-s o" . helm-occur)
         ("M-s i" . helm-imenu)
         ("M-s I" . helm-imenu-in-all-buffers)
         ("M-s m" . helm-mini)
         ("M-s b" . helm-bookmarks))
  :config
  (require 'helm-autoloads)
  (setq helm-split-window-inside-p t
        helm-use-frame-when-more-than-two-windows nil
        imenu-max-item-length 120
        helm-buffer-max-length nil)
  (helm-mode 1)
  (helm-autoresize-mode 1)) ;; TODO: Note sure if want this.

(use-package helm-lsp
  :after (helm lsp-mode lsp-ui)
  :bind (("C-c h f" . helm-lsp-workspace-symbol)
         ("C-c h g f" . helm-lsp-global-workspace-symbol)
         ("C-c h s" . helm-lsp-switch-project)
         ("C-c h d" . helm-lsp-diagnostics)
         (:map lsp-mode-map
               ([remap xref-find-apropos] . helm-lsp-workspace-symbol)))
  :config
  (setq helm-lsp-prefix-key "C-c h"))

(use-package helm-projectile)
(use-package projectile)
(projectile-mode +1)

(use-package which-key)
(which-key-mode)

(provide 'config-projects)
