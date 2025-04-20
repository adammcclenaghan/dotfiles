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

(provide 'config-core)
