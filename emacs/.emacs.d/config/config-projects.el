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
  (setq imenu-max-item-length 120
        helm-buffer-max-length nil)
  (helm-mode 1)
  (helm-autoresize-mode 1)) ;; TODO: Note sure if want this.

;; This is a bit janky, it breaks helm-occur
;; Maybe it breaks other stuff too, we'll see
;; However I am struggling to get helm to actually do this 
;; in a way that has the frame pop up at the cursor with
;; the builtin via something like:
;;(setq helm-display-function 'helm-display-buffer-in-own-frame
;;      helm-display-buffer-reuse-frame t
;;      helm-use-undecorated-frame-option t)
(use-package helm-posframe
  :after helm
  :config
  (setq helm-posframe-poshandler 'posframe-poshandler-point-bottom-left-corner
        helm-display-function 'helm-posframe-display
        helm-posframe-border-width 5)
  (helm-posframe-enable))

;; Helm swoop is a nice way to search within current/all buffers
;; M-i / M-I to activative, avy supports too so can quickly jump
;; with M-j afterwards, making avy searching across the file somewhat
;; possible.
(use-package helm-swoop
  :ensure t
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-multi-swoop-all)))

(use-package helm-lsp
  :after (helm lsp-mode lsp-ui)
  :bind (("C-c h f" . helm-lsp-workspace-symbol)
         ("C-c h g f" . helm-lsp-global-workspace-symbol)
         ("C-c h s" . helm-lsp-switch-project)
         ("C-c h d" . helm-lsp-diagnostics)
	 ("C-c h a" . helm-lsp-code-actions)
         (:map lsp-mode-map
               ([remap xref-find-apropos] . helm-lsp-workspace-symbol)))
  :config
  (setq helm-lsp-prefix-key "C-c h"))

(use-package helm-projectile)
(use-package projectile)
; Start projectile commands with C-c p
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)
(helm-projectile-on)
;; I would expect this to work if I call C-u C-c pp instead of C-c pp , however it doesn't, so just override it...
(setq projectile-switch-project-action 'projectile-commander)

(use-package which-key)
(which-key-mode)

(use-package magit)
(use-package forge
  :after magit)

(provide 'config-projects)
