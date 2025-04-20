(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-dispatch-always t)
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(defun find-file-ace-window ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (> (length (aw-window-list)) 1)
        (aw-select "" (lambda (window)
                        (aw-switch-to-window window)
                        (find-file file)))
      (find-file-other-window file))))

(eval-after-load "dired"
  '(define-key dired-mode-map (kbd "o") 'find-file-ace-window))

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(use-package avy)
(avy-setup-default)
(global-set-key (kbd "C-c C-j") 'avy-resume)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-j") 'avy-goto-char-timer)

(use-package ace-jump-helm-line
  :after helm)
(eval-after-load "helm"
  '(define-key helm-map (kbd "M-j") 'ace-jump-helm-line))

(use-package auto-highlight-symbol
  :bind (:map auto-highlight-symbol-mode-map
              ("M-p" . ahs-backward)
              ("M-n" . ahs-forward)))
(global-auto-highlight-symbol-mode t)

(provide 'config-editing)
