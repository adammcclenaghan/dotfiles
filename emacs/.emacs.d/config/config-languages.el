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

(use-package rfc-mode)
(use-package go-mode)
(use-package terraform-mode)
(use-package pyenv-mode)
(use-package lsp-pyright)
(use-package rustic)
(use-package clang-format)
(use-package dired-preview)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(provide 'config-languages)
