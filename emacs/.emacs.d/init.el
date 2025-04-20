(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'config-core)
(require 'config-packages)
(require 'config-ui)
(require 'config-editing)
(require 'config-lsp)
(require 'config-projects)
(require 'config-languages)
(require 'config-irc)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(erc-image treesit-auto dired-preview clang-format rustic lsp-pyright pyenv-mode terraform-mode go-mode rfc-mode yasnippet-snippets yasnippet company flycheck which-key helm-projectile helm-lsp lsp-ui lsp-mode exec-path-from-shell auto-highlight-symbol ace-jump-helm-line ace-window hc-zenburn-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
