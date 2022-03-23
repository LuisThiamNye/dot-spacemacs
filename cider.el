;;
;; Clojure Data visualisation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun rebl-interactive-eval (s bounds)
;;   (let* ((reblized (concat "(cognitect.rebl/inspect " s ")")))
;;     (cider-interactive-eval reblized nil bounds (cider--nrepl-pr-request-map))))

;; (defun rebl-eval-cider-loc (f)
;;   (rebl-interactive-eval (funcall f) (funcall f 'bounds)))

;; (defun rebl-eval-last-sexp ()
;;   (interactive)
;;   (rebl-eval-cider-loc 'cider-last-sexp))

;; (defun rebl-eval-defun-at-point ()
;;   (interactive)
;;   (rebl-eval-cider-loc 'cider-defun-at-point))

;; (defun rebl-eval-list-at-point ()
;;   (interactive)
;;   (rebl-eval-cider-loc 'cider-list-at-point))

;; (defun rebl-eval-sexp-at-point ()
;;   (interactive)
;;   (rebl-eval-cider-loc 'cider-sexp-at-point))

;; (defun rebl-eval-region (start end)
;;   (interactive "r")
;;   (rebl-interactive-eval (buffer-substring start end) (list start end)))

;; Use LSP completions instead when at the REPL
(add-hook 'cider-mode-hook
          (lambda ()
            (remove-hook 'completion-at-point-functions #'cider-complete-at-point)))

(defvar me-hijack-cider-eval-wrap-form nil)

(defun cider-interactive-eval (formin &optional callback bounds additional-params)
  "Evaluate FORM and dispatch the response to CALLBACK.
If the code to be evaluated comes from a buffer, it is preferred to use a
nil FORM, and specify the code via the BOUNDS argument instead.
This function is the main entry point in CIDER's interactive evaluation
API.  Most other interactive eval functions should rely on this function.
If CALLBACK is nil use `cider-interactive-eval-handler'.
BOUNDS, if non-nil, is a list of two numbers marking the start and end
positions of FORM in its buffer.
ADDITIONAL-PARAMS is a map to be merged into the request message.
If `cider-interactive-eval-override' is a function, call it with the same
arguments and only proceed with evaluation if it returns nil."
  (let* ((form  (or formin (apply #'buffer-substring-no-properties bounds)))
         (form (if me-hijack-cider-eval-wrap-form
                   (funcall me-hijack-cider-eval-wrap-form form)
                 form))
         (start (car-safe bounds))
         (end   (car-safe (cdr-safe bounds))))
    (when (and start end)
      (remove-overlays start end 'cider-temporary t))
    (unless (and cider-interactive-eval-override
                 (functionp cider-interactive-eval-override)
                 (funcall cider-interactive-eval-override form callback bounds))
      (cider-map-repls :auto
        (lambda (connection)
          (cider--prep-interactive-eval form connection)
          (cider-nrepl-request:eval
           form
           (or callback (cider-interactive-eval-handler nil bounds))
           ;; always eval ns forms in the user namespace
           ;; otherwise trying to eval ns form for the first time will produce an error
           (if (cider-ns-form-p form) "user" (cider-current-ns))
           (when start (line-number-at-pos start))
           (when start (cider-column-number-at-pos start))
           (seq-mapcat #'identity additional-params)
           connection))))))

(defvar rebl-hijack-cider-eval nil)
(defvar rebl-enable-hijack-cider-eval nil)
(defun rebl-toggle-hijack-cider-eval ()
  (interactive)
  (let ((r (not rebl-enable-hijack-cider-eval)))
    (message (format "Set REBL status: %s" r))
    (setq rebl-enable-hijack-cider-eval r)))
(defvar rebl-cider-hide-eval-output nil)
(defvar rebl-cider-tap-to-portal nil)
(defvar rebl-cider-tap-to-reveal nil)

(defun rebl-hijack-cider-eval-wrap-form (form)
  (let* ((form (if rebl-hijack-cider-eval (concat "(cognitect.rebl/inspect " form ")") form))
         (form (if rebl-cider-tap-to-portal (concat "(let[result " form "](when-some[submit(resolve 'portal.api/submit)](submit result))result)") form))
         (form (if rebl-cider-tap-to-reveal (concat "(let[result " form "](when-some[reveal(resolve 'dev/reveal)](reveal result))result)") form))
         (form (if rebl-cider-hide-eval-output (concat "(do " form " :cider/hidden)") form)))
    form))

(defun rebl-wrap-eval-main (cmd)
  (let ((me-hijack-cider-eval-wrap-form 'rebl-hijack-cider-eval-wrap-form)
        (rebl-hijack-cider-eval rebl-enable-hijack-cider-eval))
    (funcall cmd)))

(defun rebl-wrap-eval-alt (cmd)
  (let ((me-hijack-cider-eval-wrap-form 'rebl-hijack-cider-eval-wrap-form)
        (rebl-cider-tap-to-reveal t)
        (rebl-cider-tap-to-portal nil))
    (funcall cmd)))

(defun rebl-eval-sexp-end-of-line ()
  (interactive)
  (rebl-wrap-eval-main 'spacemacs/cider-eval-sexp-end-of-line))

(defun rebl-open ()
  (interactive)
  (setq rebl-enable-hijack-cider-eval t)
  (cider-nrepl-sync-request:eval
   "((requiring-resolve 'cognitect.rebl/ui))"))

(defun vlaaad.reveal/open ()
  (interactive)
  (cider-nrepl-sync-request:eval
   "(create-ns 'dev)(intern 'dev 'reveal((requiring-resolve 'vlaaad.reveal/ui) :title \"Emacs General\" :close-difficulty :normal))"))

(defun vlaaad.reveal/clear ()
  (interactive)
  (cider-nrepl-sync-request:eval
   "(when (resolve 'dev/reveal) (dev/reveal {:vlaaad.reveal/command '(clear-output)}))"))

(defun vlaaad.reveal/open-tap-log ()
  (interactive)
  (cider-nrepl-sync-request:eval "(vlaaad.reveal/tap-log)"))

(defun vlaaad.reveal/open-ui-tap-log ()
  (interactive)
  (cider-nrepl-sync-request:eval "(add-tap (vlaaad.reveal/ui :title \"Taps\" :close-difficulty :normal))"))

(defun portal.api/open ()
  (interactive)
  (setq rebl-cider-tap-to-portal t)
  (cider-nrepl-sync-request:eval
   "(require 'portal.api) (add-tap #'portal.api/submit) (portal.api/open)"))

;; (defun portal.api/clear ()
;;   (interactive)
;;   (cider-nrepl-sync-request:eval "(portal.api/clear)"))

;; (defun portal.api/close ()
;;   (interactive)
;;   (cider-nrepl-sync-request:eval "(portal.api/close)"))

(defmacro rebl-make-cider-eval-fun (group quoted-cmd)
  (let* ((cmd-sym (second quoted-cmd))
         (rebl-sym (intern (concat "rebl-" group "-"
                                   (substring (symbol-name cmd-sym) 5))))
         (rebl-eval-sym (intern (concat "rebl-wrap-eval-" group))))
    (list 'progn
          (list 'defun rebl-sym '()
                (list 'interactive)
                (list rebl-eval-sym quoted-cmd))
          (list 'quote rebl-sym))))

(spacemacs|forall-clojure-modes m
  (spacemacs/set-leader-keys-for-major-mode m
    "dp" 'portal.api/open
    "dr" 'vlaaad.reveal/open
    "dtr" 'vlaaad.reveal/open-ui-tap-log
    "dtR" 'vlaaad.reveal/open-tap-log
    "dcr" 'vlaaad.reveal/clear
    "eF" 'cider-eval-defun-up-to-point
    ": o" 'rebl-toggle-hijack-cider-eval
    "SPC e" (rebl-make-cider-eval-fun "main" 'cider-eval-last-sexp)
    "SPC f" (rebl-make-cider-eval-fun "main" 'cider-eval-defun-at-point)
    "SPC x" (rebl-make-cider-eval-fun "main" 'cider-eval-defun-up-to-point)
    "SPC v" (rebl-make-cider-eval-fun "main" 'cider-eval-sexp-at-point)
    "SPC (" (rebl-make-cider-eval-fun "main" 'cider-eval-list-at-point)
    "SPC r" (rebl-make-cider-eval-fun "main" 'cider-eval-region)
    "SPC V" (rebl-make-cider-eval-fun "main" 'cider-eval-sexp-up-to-point)
    "v e" (rebl-make-cider-eval-fun "alt" 'cider-eval-last-sexp)
    "v f" (rebl-make-cider-eval-fun "alt" 'cider-eval-defun-at-point)
    "v x" (rebl-make-cider-eval-fun "alt" 'cider-eval-defun-up-to-point)
    "v v" (rebl-make-cider-eval-fun "alt" 'cider-eval-sexp-at-point)
    "v (" (rebl-make-cider-eval-fun "alt" 'cider-eval-list-at-point)
    "v r" (rebl-make-cider-eval-fun "alt" 'cider-eval-region)
    "v V" (rebl-make-cider-eval-fun "alt" 'cider-eval-sexp-up-to-point)
    "SPC l" 'rebl-eval-sexp-end-of-line
    "do" 'rebl-open))

(defun me-cider-wrap-eval-add-libs (form)
  (concat "(require '[clojure.tools.deps.alpha.repl])"
          "(clojure.tools.deps.alpha.repl/add-libs (quote " form "))"))

(defun me-cider-eval-list-add-libs ()
  (interactive)
  (let ((me-hijack-cider-eval-wrap-form 'me-cider-wrap-eval-add-libs))
    (cider-eval-list-at-point)))

(spacemacs|forall-clojure-modes m
  (spacemacs/set-leader-keys-for-major-mode m
    "end" 'me-cider-eval-list-add-libs))


;;
;; CIDER NREPL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make repl more dynamic - download cider specific deps on connection to nrepl
;; see https://lambdaisland.com/blog/2021-11-24-making-nrepl-cider-more-dynamic-2

;; remove the previous one if necessary
;; (pop 'cider-connected-hook)

(add-hook 'cider-connected-hook
          (lambda ()
            (cider-sync-tooling-eval
             (parseedn-print-str
              `(.addURL (loop
                         [loader (.getContextClassLoader (Thread/currentThread))]
                         (let [parent (.getParent loader)]
                           (if (instance? clojure.lang.DynamicClassLoader parent)
                               (recur parent)
                             loader)))
                        (java.net.URL. ,(concat "file:" (cider-jar-find-or-fetch "cider" "cider-nrepl" cider-injected-nrepl-version))))))
            (cider-add-cider-nrepl-middlewares)))
