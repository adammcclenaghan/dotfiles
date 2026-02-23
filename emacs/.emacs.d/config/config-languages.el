(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package company)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

;; Template expansion, see https://emacsconf.org/2019/talks/31/ - use "M-x yas-describle-tables" to see available snippets in the buffer's mode. Essentially, adds handy auto complete expansion for code
(use-package yasnippet)
(yas-global-mode 1)
;; Bundled snippets from https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets)

;; To use without internet connection, download all of this into ~/rfc : https://www.rfc-editor.org/retrieve/bulk
(use-package rfc-mode)

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
;; I am often editing c++ and not so often editing c so default to c++ can override on a per-project basis
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(use-package go-mode)
(add-hook 'go-mode-hook #'lsp-deferred) ; This will take care of starting gopls, lsp-ui, company etc
(add-hook 'go-mode-hook #'yas-minor-mode)
;; Set up before-save hooks, these will do auto formatting and add/delete imports in the go file on save
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook #'auto-highlight-symbol-mode) ; Seems to be a bug where global doesn't work: https://github.com/elp-revive/auto-highlight-symbol/issues/22

;; go ts mode is not working on mac for me and I haven't got time to fix it 
(setq major-mode-remap-alist
       '((go-ts-mode . go-mode)))


(use-package terraform-mode)
;; Enables using C-c C-f to toggle visbility of a block
(add-hook 'terraform-mode-hook #'outline-minor-mode)
(setq terraform-format-on-save t)

(use-package protobuf-mode)

(use-package pyenv-mode)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))

(use-package rustic)

(use-package clang-format
  :hook ((c-mode          . clang-format-on-save-mode)
         (c++-mode        . clang-format-on-save-mode)
         (c-ts-mode       . clang-format-on-save-mode)
         (c++-ts-mode     . clang-format-on-save-mode)))

(use-package dired-preview)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; DAP for debugging in programming modes
(use-package dap-mode
    :bind
  (:map dap-mode-map
   ("C-c d b" . dap-breakpoint-toggle)
   ("C-c d r" . dap-debug-restart)
   ("C-c d l" . dap-debug-last)
   ("C-c d d" . dap-debug)
   ("C-c d u" . dap-ui-locals)
   ("C-c d c" . dap-continue)
   ("C-c d n" . dap-next)
   ("C-c d i" . dap-step-in)
   ("C-c d o" . dap-step-out)
   ("C-c d e" . dap-eval)
   ("C-c d q" . dap-disconnect)
   ("C-c d C-b" . dap-ui-breakpoints)
   ("C-c d f" . dap-breakpoint-condition)
  )
)

(require 'dap-dlv-go) ; Require the submode for go

; Setup some default features
(setq dap-auto-configure-features '(sessions locals controls tooltip))

(provide 'config-languages)
