(with-eval-after-load 'cider-mode
  (load (dsppath "cider.el")))
;;
;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-clojure-indent (defrecord '(2 nil
                                      nil
                                      (1)))
  (deftype '(2 nil
               nil
               (1)))
  (do-nil 0)
  (do-true 0)
  (do-false 0)
  (cond! 0)
  (>defn :defn)
  (>defn- :defn)
  (case-eval :defn)
  (manifold.deferred/catch 0)
  (let-flow 1)
  (let-flow '1)
  (handler-case :defn)
  (restart-case :defn)
  (handler-bind 1)
  (restart-bind 1)
  (farolero.core/block 1)
  (farolero.core/block* 1)
  (farolero.core/return-from 1)
  (tagbody 1)
  (catching 1)
  (match 1))
(setq clojure-special-arg-indent-factor 1)

;;
;; Clojure keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; toggle reader macro sexp comment
;; toggles the #_ characters at the start of an expression
(defun clojure-toggle-reader-comment-sexp ()
  (interactive)
  (let* ((point-pos1 (point)))
    (evil-insert-line 0)
    (let* ((point-pos2 (point))
           (cmtstr "#_")
           (cmtstr-len (length cmtstr))
           (line-start (buffer-substring-no-properties point-pos2
                                                       (+ point-pos2 cmtstr-len)))
           (point-movement (if (string= cmtstr line-start)
                               -2
                             2))
           (ending-point-pos (+ point-pos1 point-movement 1)))
      (if (string= cmtstr line-start)
          (delete-char cmtstr-len)
        (insert cmtstr))
      (goto-char ending-point-pos)))
  (evil-normal-state))

;; Assign keybinding to the toggle-reader-comment-sexp function
(spacemacs|forall-clojure-modes m
  (spacemacs/set-leader-keys-for-major-mode
    m "#" 'clojure-toggle-reader-comment-sexp
    "gg" 'lsp-find-definition))

;; Toggle view of a clojure `(comment ,,,) block'
(defun clojure-hack/toggle-comment-block (arg)
  "Close all top level (comment) forms. With universal arg, open all."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "^(comment\\>" nil
                                  'noerror)
      (call-interactively (if arg 'evil-open-fold 'evil-close-fold)))))

(evil-define-key 'normal
  clojure-mode-map
  "zC"
  'clojure-hack/toggle-comment-block
  "zO"
  (lambda ()
    (interactive)
    (clojure-hack/toggle-comment-block 'open)))


;;
;; zprint formatter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'clojure-mode
  (require 'zprint-mode))
(with-eval-after-load 'zprint-mode
  (spacemacs|forall-clojure-modes m
    (spacemacs/set-leader-keys-for-major-mode
      m "-b" 'zprint "-f" 'zprint-defun)))

;;
;; Kaocha runner
;; see https://github.com/magnars/kaocha-runner.el for more configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(spacemacs|forall-clojure-modes m
  (spacemacs/set-leader-keys-for-major-mode
    m "kt" 'kaocha-runner-run-test-at-point "kr"
    'kaocha-runner-run-tests "ka" 'kaocha-runner-run-all-tests
    "kw" 'kaocha-runner-show-warnings "kh" 'kaocha-runner-hide-windows))

;;
;; clj-decompiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(spacemacs|forall-clojure-modes m
  (spacemacs/set-leader-keys-for-major-mode
    m "ed" 'clj-decompiler-decompile "eD" 'clj-decompiler-disassemble))
