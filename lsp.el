(add-to-list 'lsp-language-id-configuration '(carp-mode . "carp"))

(add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.clj-kondo")
(add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.shadow-cljs")
(add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.lsp")
(add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.DS_Store")
(add-to-list 'lsp-file-watch-ignored "[/\\\\]resources[/\\\\]public")
(add-to-list 'lsp-file-watch-ignored "[/\\\\]public[/\\\\]css")
(add-to-list 'lsp-file-watch-ignored "[/\\\\]public[/\\\\]js")
(add-to-list 'lsp-file-watch-ignored "[/\\\\]dist[/\\\\]")
(add-to-list 'lsp-file-watch-ignored "[/\\\\]app-output")
(add-to-list 'lsp-file-watch-ignored "[/\\\\]yarn\\.lock")

(spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
  "gr" #'lsp-find-references)
