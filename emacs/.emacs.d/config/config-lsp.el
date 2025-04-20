(setq max-lisp-eval-depth 20000)
(setq read-process-output-max (* 1024 1024))

(use-package exec-path-from-shell)
(dolist (var '("LSP_USE_PLISTS"))
  (add-to-list 'exec-path-from-shell-variables var))
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package lsp-mode
  :bind ("C-c l g i" . lsp-ui-peek-find-implementation)
  :config
  (setq lsp-use-plists t
        lsp-response-timeout 10
        lsp-warn-no-matched-clients nil
        gc-cons-threshold (* 100 1024 1024)
        lsp-idle-delay 0.1))

(add-hook 'prog-mode-hook 'lsp-deferred)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "test-fixtures"))

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  (or (when (equal (following-char) ?#)
        (let ((bytecode (read (current-buffer))))
          (when (byte-code-function-p bytecode)
            (funcall bytecode))))
      (apply old-fn args)))
(advice-add (if (fboundp 'json-parse-buffer) 'json-parse-buffer 'json-read)
            :around #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)
             (not (file-remote-p default-directory))
             lsp-use-plists
             (not (functionp 'json-rpc-connection))
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-use-webkit t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay '1
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-peek-fontify 'always
        lsp-ui-peek-highlight 'highlight
        lsp-ui-peek-list-width '100)
  (add-hook 'lsp-ui-doc-mode-hook #'lsp-ui-doc-frame-mode)
  (define-key lsp-ui-doc-frame-mode-map (kbd "C-c f") 'lsp-ui-doc-focus-frame)
  ;; prevent lsp-ui-doc-focus from stealing the q key in all lsp enabled files... :) 
  (with-eval-after-load 'lsp-ui-doc
    (define-key lsp-ui-doc-frame-mode-map [?q] nil)
    (define-key lsp-ui-doc-frame-mode-map [?q] 'self-insert-command))
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(provide 'config-lsp)
