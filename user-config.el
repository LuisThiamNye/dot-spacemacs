(load (dsppath "theme.el"))
(load (dsppath "shell.el"))
(with-eval-after-load 'lsp-mode
  (load (dsppath "lsp.el")))
(with-eval-after-load 'clojure-mode
  (load (dsppath "clojure.el")))

(with-eval-after-load 'carp-mode
  (require 'inf-carp-mode)
  (add-to-list 'evil-lisp-safe-structural-editing-modes
               'inf-carp-mode)
  ;; (remove-hook 'carp-mode-hook #'inf-carp-mode)
  )

;;
;; Projectile
;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default projectile-indexing-method 'hybrid)

(when (eq system-type 'windows-nt)
  ;; requires https://lib.rs/crates/coreutils
  (setq projectile-git-submodule-command "git submodule --quiet foreach 'echo $displaypath' | coreutils tr '\\n' '\\0'"))

;;
;; Solve the problem of deadly slow autocomplete when using semantic mode
;; https://github.com/syl20bnr/spacemacs/issues/1907
;; https://github.com/syl20bnr/spacemacs/issues/11058#issuecomment-407269954
;;;;;;;;;;;;;;;;;;;;;
(defun et/semantic-remove-hooks ()
  (remove-hook 'completion-at-point-functions
               'semantic-analyze-completion-at-point-function)
  (remove-hook 'completion-at-point-functions
               'semantic-analyze-notc-completion-at-point-function)
  (remove-hook 'completion-at-point-functions
               'semantic-analyze-nolongprefix-completion-at-point-function))

(add-hook 'semantic-mode-hook #'et/semantic-remove-hooks)

;;;;;;;;;;
;;
;; rebindings
;;
(global-set-key (kbd "M-3")
                '(lambda ()
                   (interactive)
                   (insert "£")))
(global-set-key (kbd "M-8")
                '(lambda ()
                   (interactive)
                   (insert "•")))
(define-key winum-keymap (kbd "M-0") nil)
(define-key winum-keymap (kbd "M-1") nil)
(define-key winum-keymap (kbd "M-2") nil)
(define-key winum-keymap (kbd "M-3") nil)
(define-key winum-keymap (kbd "M-4") nil)
(define-key winum-keymap (kbd "M-5") nil)
(define-key winum-keymap (kbd "M-6") nil)
(define-key winum-keymap (kbd "M-7") nil)
(define-key winum-keymap (kbd "M-8") nil)
(define-key winum-keymap (kbd "M-9") nil)
(define-key global-map (kbd "<magnify-up>") nil)
(define-key global-map (kbd "<magnify-down>") nil)

(with-eval-after-load 'treemacs-mode
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
  (setq treemacs-collapse-dirs 7))

;;
;; Editing
;;;;;;;;;;;;;;;;;;;;;;;;;

(spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hooks)
(setq-default evil-escape-key-sequence nil)

;; disable the annoying completion sometimes when hitting TAB
(setq tab-always-indent t)

;; Disable obnoxious auto-highlight-syntax
(remove-hook 'prog-mode-hook 'auto-highlight-symbol-mode)

;;
;; Emacs text rendering optimizations
;; https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Only render text left to right
(setq-default bidi-paragraph-direction 'left-to-right)

(if (version<= "27.1" emacs-version)
    (progn
      ;; Disable Bidirectional Parentheses Algorithm
      (setq bidi-inhibit-bpa t)
      ;; So long mode when Emacs thinks a file would affect performance
      (global-so-long-mode 1)))

;; Files with known long lines
;; SPC f l to open files literally to disable most text processing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of: Emacs text rendering optimizations

;;
;; Override Spacemacs defaults
;;
;; Set new location for file bookmarks, SPC f b
;; Default: ~/.emacs.d/.cache/bookmarks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq bookmark-default-file "~/.spacemacs.d/bookmarks")
;;
;; Set new location for recent save files
;; Default: ~/.emacs.d/.cache/recentf
(setq recentf-save-file "~/.spacemacs.d/recentf")
;;
;; native line numbers taking up lots of space?
(setq-default display-line-numbers-width nil)

;;
;; RUST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'toml-mode-hook #'spacemacs//activate-smartparens)

;;
;; LEDGER
;;;;;;;;;;;;;;;;;
(add-hook 'ledger-mode-hook #'spacemacs//activate-smartparens)
