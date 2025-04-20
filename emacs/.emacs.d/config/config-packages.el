(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives
                 (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(unless (package-installed-p 'hc-zenburn-theme)
  (package-refresh-contents)
  (package-install 'hc-zenburn-theme))

(load-theme 'hc-zenburn t)

(provide 'config-packages)
