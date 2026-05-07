;; AI and coding assistant packages

(straight-use-package 'vterm)
(straight-use-package '(claude-code-ide
                        :type git
                        :host github
                        :repo "manzaltu/claude-code-ide.el"))

(require 'claude-code-ide)
(claude-code-ide-emacs-tools-setup)

(provide 'packages-ai)
