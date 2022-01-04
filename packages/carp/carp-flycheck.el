(require 'flycheck)

(flycheck-define-checker carp-checker
  "Syntax checker for Carp"
  :command ("carp" "--check" source)
  :error-patterns ((error line-start space (message)
                          "\n  At " line " : " column " in '" (file-name) "'" line-end)
                   (error line-start (file-name) ":" line ":" column " " (message) line-end))
  :modes (carp-mode))

(eval-after-load 'flycheck
  '(add-to-list 'flycheck-checkers 'carp-checker))

(provide 'carp-flycheck)
