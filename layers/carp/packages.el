(defconst carp-packages '(clojure-mode evil-cleverparens smartparens
                                       flycheck carp-mode))

(defun carp/init-carp-mode ()
  (use-package carp-mode :defer t
    :mode "\\.carp\\'"))

(defun carp/init-inf-carp-mode ()
  (use-package inf-carp-mode
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode
            'carp-mode
            ;; shortcuts "'"
            ;; 'sesman-start
            ;; ;; help / documentation
            ;; "ha"
            ;; 'cider-apropos
            ;; "hc"
            ;; 'cider-cheatsheet
            ;; "hd"
            ;; 'cider-clojuredocs
            ;; "hj"
            ;; 'cider-javadoc
            ;; "hn"
            ;; 'cider-browse-ns
            ;; "hN"
            ;; 'cider-browse-ns-all
            ;; "hs"
            ;; 'cider-browse-spec
            ;; "hS"
            ;; 'cider-browse-spec-all
            ;; ;; evaluate in source code buffer
            ;; "e;"
            ;; 'cider-eval-defun-to-comment
            ;; "e$"
            ;; 'spacemacs/cider-eval-sexp-end-of-line
            ;; "e("
            ;; 'cider-eval-list-at-point
            "eb"
            'inf-carp-eval-buffer
            "ee"
            'inf-carp-eval-last-sexp
            "ef"
            'inf-carp-eval-defun ;; at point
            ;; "ei"
            ;; 'cider-interrupt
            ;; "el"
            ;; 'spacemacs/cider-eval-sexp-end-of-line
            "em"
            'inf-carp-macroexpand ;; -1
            ;; "eM"
            ;; 'cider-macroexpand-all
            ;; "ena"
            ;; 'cider-ns-reload-all
            ;; "enn"
            ;; 'cider-eval-ns-form
            ;; "enr"
            ;; 'cider-ns-refresh
            ;; "enl"
            ;; 'cider-ns-reload ;; SPC u for cider-ns-reload-all
            ;; "ep;"
            ;; 'cider-pprint-eval-defun-to-comment
            ;; "ep:"
            ;; 'cider-pprint-eval-last-sexp-to-comment
            ;; "epf"
            ;; 'cider-pprint-eval-defun-at-point
            ;; "epe"
            ;; 'cider-pprint-eval-last-sexp
            "er"
            'inf-carp-eval-region
            ;; "eu"
            ;; 'cider-undef
            ;; "ev"
            ;; 'cider-eval-sexp-at-point
            ;; "eV"
            ;; 'cider-eval-sexp-up-to-point
            ;; "ew"
            ;; 'cider-eval-last-sexp-and-replace
            ;; ;; format code style
            ;; "=="
            ;; 'cider-format-buffer
            ;; "=eb"
            ;; 'cider-format-edn-buffer
            ;; "=ee"
            ;; 'cider-format-edn-last-sexp
            ;; "=er"
            ;; 'cider-format-edn-region
            ;; "=f"
            ;; 'cider-format-defun
            ;; ;; goto
            ;; "gb"
            ;; 'cider-pop-back
            ;; "gc"
            ;; 'cider-classpath
            ;; "gg"
            ;; 'spacemacs/clj-find-var
            ;; "gn"
            ;; 'cider-find-ns
            ;; ;; manage cider connections / sesman
            ;; "mb"
            ;; 'sesman-browser
            ;; "mi"
            ;; 'sesman-info
            ;; "mg"
            ;; 'sesman-goto
            ;; "mlb"
            ;; 'sesman-link-with-buffer
            ;; "mld"
            ;; 'sesman-link-with-directory
            ;; "mlu"
            ;; 'sesman-unlink
            ;; "mqq"
            ;; 'sesman-quit
            ;; "mqr"
            ;; 'sesman-restart
            ;; "mlp"
            ;; 'sesman-link-with-project
            ;; "mSj"
            ;; 'cider-connect-sibling-clj
            ;; "mSs"
            ;; 'cider-connect-sibling-cljs
            ;; "ms"
            ;; 'sesman-start
            ;; ;; send code - spacemacs convention
            "sa"
            'inf-carp-switch-to-repl
            ;; "sb"
            ;; 'cider-load-buffer
            ;; "sB"
            ;; 'spacemacs/cider-send-buffer-in-repl-and-focus
            ;; "scj"
            ;; 'cider-connect-clj
            ;; "scm"
            ;; 'cider-connect-clj&cljs
            ;; "scs"
            ;; 'cider-connect-cljs
            ;; "se"
            ;; 'spacemacs/cider-send-last-sexp-to-repl
            ;; "sE"
            ;; 'spacemacs/cider-send-last-sexp-to-repl-focus
            ;; "sf"
            ;; 'spacemacs/cider-send-function-to-repl
            ;; "sF"
            ;; 'spacemacs/cider-send-function-to-repl-focus
            ;; "si"
            ;; 'sesman-start
            ;; "sjj"
            ;; 'cider-jack-in-clj
            ;; "sjm"
            ;; 'cider-jack-in-clj&cljs
            ;; "sjs"
            ;; 'cider-jack-in-cljs
            "sl"
            'inf-carp-clear-repl-buffer
            ;; "sL"
            ;; 'cider-find-and-clear-repl-output
            ;; "sn"
            ;; 'spacemacs/cider-send-ns-form-to-repl
            ;; "sN"
            ;; 'spacemacs/cider-send-ns-form-to-repl-focus
            ;; "so"
            ;; 'cider-repl-switch-to-other
            ;; "sqq"
            ;; 'cider-quit
            ;; "sqr"
            ;; 'cider-restart
            ;; "sqn"
            ;; 'cider-ns-reload
            ;; "sqN"
            ;; 'cider-ns-reload-all
            ;; "sr"
            ;; 'spacemacs/cider-send-region-to-repl
            ;; "sR"
            ;; 'spacemacs/cider-send-region-to-repl-focus
            ;; "su"
            ;; 'cider-repl-require-repl-utils
            ;; ;; toggle options
            ;; "Te"
            ;; 'cider-enlighten-mode
            ;; "Tf"
            ;; 'spacemacs/cider-toggle-repl-font-locking
            ;; "Tp"
            ;; 'spacemacs/cider-toggle-repl-pretty-printing
            ;; "Tt"
            ;; 'cider-auto-test-mode
            ;; ;; cider-tests
            ;; "ta"
            ;; 'spacemacs/cider-test-run-all-tests
            ;; "tb"
            ;; 'cider-test-show-report
            ;; "tl"
            ;; 'spacemacs/cider-test-run-loaded-tests
            ;; "tn"
            ;; 'spacemacs/cider-test-run-ns-tests
            ;; "tp"
            ;; 'spacemacs/cider-test-run-project-tests
            ;; "tr"
            ;; 'spacemacs/cider-test-rerun-failed-tests
            ;; "tt"
            ;; 'spacemacs/cider-test-run-focused-test
            ;; ;; cider-debug and inspect
            ;; "db"
            ;; 'cider-debug-defun-at-point
            ;; "de"
            ;; 'spacemacs/cider-display-error-buffer
            ;; "dve"
            ;; 'cider-inspect-last-sexp
            ;; "dvf"
            ;; 'cider-inspect-defun-at-point
            ;; "dvi"
            ;; 'cider-inspect
            ;; "dvl"
            ;; 'cider-inspect-last-result
            ;; "dvv"
            ;; 'cider-inspect-expr
            ;; ;; profile
            ;; "p+"
            ;; 'cider-profile-samples
            ;; "pc"
            ;; 'cider-profile-clear
            ;; "pn"
            ;; 'cider-profile-ns-toggle
            ;; "ps"
            ;; 'cider-profile-var-summary
            ;; "pS"
            ;; 'cider-profile-summary
            ;; "pt"
            ;; 'cider-profile-toggle
            ;; "pv"
            ;; 'cider-profile-var-profiled-p
            )))

(defun carp/pre-init-evil-cleverparens ()
  (spacemacs|use-package-add-hook evil-cleverparens
    :pre-init (add-to-list 'evil-lisp-safe-structural-editing-modes
                           'carp-mode)))
(defun carp/post-init-clojure-mode ()
  (remove-hook 'clojure-mode-hook #'spacemacs//clojure-setup-backend)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (when (not (eq 'carp-mode major-mode))
                (spacemacs//clojure-setup-backend)))))

(defun carp/post-init-smartparens ()
  (add-hook 'carp-mode-hook #'spacemacs//activate-smartparens))

(defun carp/post-init-flycheck ()
  (spacemacs/enable-flycheck 'carp-mode))
