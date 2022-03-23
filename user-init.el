(setq package-native-compile t)

;; (setq cljr-ignore-analyzer-errors nil)

;; Visual line navigation for textual modes
;; takes effect when wrapping/truncating text
;; (add-hook 'text-mode-hook 'spacemacs/toggle-visual-line-navigation-on)

(let* ((clr-comment ;"#d47a63"
        "#c6a358")
       (comment-background ;"#292524"
        "#2e2c29")
       (clr-comment-delimiter "#cbb567") ;; the semicolons or slashes
       (clr-constant "#d3869b")) ;; inline quoting of variables, clojure keywords
  (setq-default
   theming-modifications
   `((doom-one-light
      (mode-line :height 0.92)
      (mode-line-inactive :height 0.92))
     (doom-gruvbox-light
      (lsp-face-highlight-read :background nil :weight bold)
      (command-log-command :foreground "firebrick")
      (command-log-key :foreground "dark magenta"))
     (doom-gruvbox
      (font-lock-comment-face :foreground ,clr-comment :background ,comment-background)
      (font-lock-comment-delimiter-face :foreground ,clr-comment-delimiter  :background ,comment-background)
      (font-lock-constant-face :foreground ,clr-constant)))))
