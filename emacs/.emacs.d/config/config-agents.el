;; See https://github.com/xenodium/agent-shell for pre-requisite setup
(use-package agent-shell
    :ensure t)

(setq agent-shell-anthropic-claude-environment
      (agent-shell-make-environment-variables
       "ANTHROPIC_API_KEY" (auth-source-pass-get "secret" "anthropic-api-key"))
      )

;; authentication setups for various agents

;; Anthropic (CLAUDE)
(setq agent-shell-anthropic-authentication
      (agent-shell-anthropic-make-authentication :login t))

;; inherit environment variables from system
(setq agent-shell-anthropic-claude-environment
      (agent-shell-make-environment-variables :inherit-env t))
 
;; Google (GEMINI)
(setq agent-shell-google-authentication
      (agent-shell-google-make-authentication :login t))

;; inherit environment variables from system
(setq agent-shell-google-gemini-environmen
      (agent-shell-make-environment-variables :inherit-env t))

;; OpenAI (CODEX)
;; default login auth
(setq agent-shell-openai-authentication
      (agent-shell-openai-make-authentication :login t))

;; inherit environment variables from system
(setq agent-shell-openai-codex-environment
      (agent-shell-make-environment-variables :inherit-env t))

;; If I find myself using a particular agent then set this variable, like so, to
;; automatically default to preferred agent
;; (setq agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config))

(provide 'config-agents)
