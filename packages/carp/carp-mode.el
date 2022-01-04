;;;###autoload
(define-derived-mode carp-mode clojure-mode "Carp"
  "Major mode for Carp")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.carp\\'" . carp-mode))

(provide 'carp-mode)
