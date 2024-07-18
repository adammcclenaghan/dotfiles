; Enable melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

;; --- THEME ---

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-window terraform-mode helm exec-path-from-shell go-mode magit yasnippet company lsp-ui lsp use-package solarized-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 (load-theme 'solarized-light t)

;; --- General emacs configuration ---

(package-initialize)

;; Prevent backup ~ files from being stored alongside originals. Keeps directories tidy.
;; Thanks to : https://stackoverflow.com/questions/2680389/how-to-remove-all-files-ending-with-made-by-emacs/25692389
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; Disable lockfiles
(setq create-lockfiles nil)

; Enable server on startup
(server-start)

; Screens and layouts - Requires Hack font: https://github.com/source-foundry/Hack
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(set-frame-font "Hack-13")
(ido-mode 1)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; I like my cursor to blink forever
(setq blink-cursor-blinks 0)

;; Incremental completion
(package-install 'helm)
(require 'helm)
;; Get use-package, nice wrapper around require
(package-install 'use-package)
(require 'use-package) ; Ensure that use-package always tries to install if the package is missing
(setq use-package-always-ensure t)

; ----- Packages and configurations used by mutliple programming languages -----
; Enable line numbers in all programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

; This is a nice package for switching around windows. When >2 windows it lets you pick a window with a key specifier.
(use-package ace-window)
(global-set-key (kbd "M-o") 'ace-window)

;; Configure dired mode on darwin - ls doesn't support --dired on OSx
(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))

;; Increase threshold as lsp-mode suffers otherwise
(setq gc-cons-threshold 100000000)

;; Increase for lsp mode as some language servers send large responsees
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Make sure env in emacs looks the same as shell. OS x sets different env when opened in shell vs UI. Annoying.
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Language server protocol support
(use-package lsp-mode)

;; UI packages for lsp mode
(use-package lsp-ui)

;; Company mode adds a text completion framework, allows us to see auto completion popups as we type
(use-package company)
(setq company-idle-delay 0) 
(setq company-minimum-prefix-length 1) ; Immediately show popup after one character

;; Template expansion, see https://emacsconf.org/2019/talks/31/ - use "M-x yas-describle-tables" to see available snippets in the buffer's mode. Essentially, adds handy auto complete expansion for code
(use-package yasnippet)

;; Git in emacs
(use-package magit)

;; ----- Configuration for GO -----
;; Major mode for go - required for highlighting, indentation etc
(use-package go-mode)
;; Hooks for go mode
;; NOTE: For this to work need to run: go install golang.org/x/tools/gopls@latest
;; And ensure that $PATH has $GOPATH set in it.
;; Major mode for go - required for highlighting, indentation etc
;; Start LSP Mode and YASnippet mode when in go-mode
(add-hook 'go-mode-hook #'lsp-deferred) ; This will take care of starting gopls, lsp-ui, company etc
(add-hook 'go-mode-hook #'yas-minor-mode)
;; Set up before-save hooks, these will do auto formatting and add/delete imports in the go file on save
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


;; ----- Configuration for Terraform -----
(use-package terraform-mode)
;; Enables using C-c C-f to toggle visbility of a block
(add-hook 'terraform-mode-hook #'outline-minor-mode)
