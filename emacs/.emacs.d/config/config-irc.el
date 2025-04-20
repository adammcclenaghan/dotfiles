(use-package erc
  :custom
  (erc-autojoin-channels-alist '(("libera.chat" "#emacs" "#C++" "#C++-general" "#C++-basic" "##rust")))
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  (erc-prompt-for-nickserv-password nil)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT" "324" "329" "332" "333" "353" "477"))
  :config
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (erc-services-mode 1)
  (erc-spelling-mode 0)
  (erc-update-modules))

(defun my/get-erc-creds ()
  (let ((cred-file "~/.erc-credentials.el"))
    (when (file-exists-p cred-file)
      (load cred-file))))

(defun my/erc-start-or-switch ()
  (interactive)
  (my/get-erc-creds)
  (if (get-buffer "*erc-libera.chat:6697*")
      (erc-track-switch-buffer 1)
    (when (y-or-n-p "Start ERC? ")
      (erc-tls :server "irc.libera.chat"
               :port 6697
               :nick erc-nick))))

(use-package erc-image
  :after erc)

(provide 'config-irc)
