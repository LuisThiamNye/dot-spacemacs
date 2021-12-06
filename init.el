;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(setq home-dir
      (pcase system-type
        ('windows-nt "~/")
        ('darwin "/Users/luis/")
        ('gnu/linux "/home/luis/")))

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     osx

     (shell :variables
            terminal-here-windows-terminal-command '("pwsh.exe" "-c" "start" "pwsh.exe")
            shell-default-shell 'eshell
            spacemacs-vterm-history-file-location "~/.zsh_history")

     (spacemacs-layouts :variables
                        spacemacs-layouts-restrict-spc-tab t
                        persp-autokill-buffer-on-remove 'kill-weak)

     (auto-completion :variables
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete
                      ;; auto-completion-complete-with-key-sequence "kj"
                      ;; auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-minimum-prefix-length 1
                      auto-completion-idle-delay 0
                      auto-completion-private-snippets-directory nil
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip nil
                      auto-completion-use-company-box nil
                      ;; Beware: Sorting completion results is often done already by the
                      ;; completion backend, doing it again in company may degrate performance.
                      auto-completion-enable-sort-by-usage t)

     (ivy :variables
          ivy-wrap t
          ivy-height 35)

     (git :variables
          git-magit-status-fullscreen t
          magit-diff-refine-hunk t
          git-enable-magit-todos-plugin t)
     github

     (ranger :variables
             ranger-show-preview t
             ranger-show-hidden t
             ranger-cleanup-eagerly t
             ranger-cleanup-on-disable t
             ranger-ignored-extensions '("mkv" "flv" "iso" "mp4"))

     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)
     (treemacs :variables
               treemacs-indentation 1
               treemacs-width 28
               ;; Efficient use of space in treemacs-lsp display
               treemacs-space-between-root-nodes nil
               treemacs-use-scope-type 'Perspectives
               treemacs-use-filewatch-mode t
               treemacs-use-follow-mode t)
     graphviz

     ;; Editing

     (syntax-checking :variables syntax-checking-use-original-bitmaps t)

     ;; C-a to toggle start of line/code
     better-defaults
     multiple-cursors

     (lsp :variables
          ;; lsp-clojure-custom-server-command "/Volumes/House/script/clojure-lsp/clojure-lsp-entry"
          ;; Formatting and indentation
          ;; setting to nil to increase responsiveness when editing
          lsp-enable-on-type-formatting nil ;; clojure-lsp currently does not support onTypeFormatting
          lsp-enable-indentation nil ;; must restart the backend to take effect

          lsp-enable-snippet t
          ;; symbol highlighting - `lsp-toggle-symbol-highlight` toggles highlighting
          lsp-enable-symbol-highlighting t
          ;; Show lint error indicator in the mode-bar (tested on doom-modeline)
          lsp-modeline-diagnostics-enable t
          lsp-idle-delay 0.5

          ;; popup documentation boxes
          ;; lsp-ui-doc-enable nil          ;; disable all doc popups
          lsp-ui-doc-show-with-cursor nil   ;; doc popup for cursor
          lsp-ui-doc-show-with-mouse nil   ;; doc popup for mouse
          ;; lsp-ui-doc-delay 2                ;; delay in seconds for popup to display
          lsp-ui-doc-include-signature t    ;; include function signature
          ;; lsp-ui-doc-position 'at-point  ;; top bottom at-point
          lsp-ui-doc-alignment 'frame      ;; `frame' or `window'

          lsp-headerline-breadcrumb-enable nil

          ;; code actions and diagnostics text as right-hand side of buffer
          lsp-ui-sideline-enable nil
          lsp-ui-sideline-show-code-actions nil

          ;; reference count for functions (assume their maybe other lenses in future)
          lsp-lens-enable t
          lsp-lens-debounce-interval 0.1 ;; 0.001
          ;; lsp-lens-place-position 'end-of-line

          ;; Optimization for large files
          lsp-file-watch-threshold 10000
          lsp-log-io nil)

     ;; Languages

     (clojure :variables
              ;; clojure-backend 'cider
              clojure-enable-clj-refactor t

              ;; https://docs.cider.mx/cider/1.1/usage/pretty_printing.html
              cider-print-fn 'fipp
              cider-print-quota 500000
              cider-print-options '(("length" 50)
                                    ("level" 7)
                                    ("right-margin" 72) ;; pprint default: 72
                                    )


              ;; auto trim the repl (number of characters)
              cider-repl-buffer-size-limit (* 500 100)
              cider-repl-display-help-banner nil
              cider-offer-to-open-cljs-app-in-browser nil
              cider-overlays-use-font-lock t

              clojure-align-forms-automatically nil
              clojure-enable-fancify-symbols nil
              clojure-toplevel-inside-comment-form t
              ;;
              )
     (rust :variables
           lsp-rust-server 'rust-analyzer
           rust-format-on-save t
           rust-indent-offset 2
           cargo-process-reload-on-modify t)
     (dart :variables
           lsp-dart-sdk-dir (concat home-dir "installations/flutter/bin/cache/dart-sdk/")
           lsp-dart-flutter-sdk-dir (concat home-dir "installations/flutter/")
           flutter-sdk-path (concat home-dir "installations/flutter/"))
     emacs-lisp
     semantic ;; provides elisp formatting:

     (nixos :variables nixos-format-on-save t)

     org
     asciidoc
     (markdown :variables markdown-live-preview-engine 'vmd)

     python
     yaml
     java
     html
     javascript
     typescript

     finance
     latex

     ;; Themes

     (spacemacs-modeline :variables
                         doom-modeline-height 12
                         doom-modeline-major-mode-color-icon nil
                         doom-modeline-buffer-file-name-style 'relative-to-project
                         doom-modeline-display-default-persp-name t
                         doom-modeline-minor-modes nil
                         doom-modeline-modal-icon nil)
     (unicode-fonts :variables
                                        ; bug in 27.2 and older causes hang when opening file with ligatures
                    unicode-fonts-enable-ligaftures t
                    unicode-fonts-ligature-modes '(prog-mode))
     theming
     ;;
     )


   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(kaocha-runner clj-decompiler)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; aggressive-indent-mode readme says it is more reliable than electric indent mode

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024 3)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style '(hybrid :variables
                                       hybrid-mode-enable-evilified-state t
                                       hybrid-mode-enable-hjkl-bindings t
                                       hybrid-mode-default-state 'normal)

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents-by-project . (7 . 5)))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(doom-gruvbox
                         doom-Iosvkem
                         doom-molokai
                         doom-monokai-pro

                         doom-monokai-spectrum
                         doom-dark+
                         doom-vibrant
                         doom-challenger-deep
                         cyberpunk
                         doom-laserwave
                         ;; light
                         doom-one-light

                         doom-opera-light
                         doom-acario-light
                         doom-solarized-light
                         doom-gruvbox-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(doom)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font `("Fira Code"
                               :size ,(if (eq system-type 'windows-nt) 13 12)
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state t

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup (when (eq system-type 'darwin) t)

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling nil

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format nil

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'all

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  ;; (setq cljr-ignore-analyzer-errors nil)

   ;; Visual line navigation for textual modes
   (add-hook 'text-mode-hook 'spacemacs/toggle-visual-line-navigation-on)

   (setq-default
    theming-modifications
    '((doom-one-light
       (mode-line :height 0.92)
       (mode-line-inactive :height 0.92))
      (doom-gruvbox-light
       (lsp-face-highlight-read :background nil :weight bold)
       (command-log-command :foreground "firebrick")
       (command-log-key :foreground "dark magenta"))))

  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;;
  ;; zprint formatter
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (add-to-list 'load-path (concat home-dir ".spacemacs.d/packages/zprint-mode/"))
  (require 'zprint-mode)

  (spacemacs|forall-clojure-modes m
    (spacemacs/set-leader-keys-for-major-mode m
      "-b" 'zprint
      "-f" 'zprint-defun))

  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Kaocha runner
  ;; see https://github.com/magnars/kaocha-runner.el for more configuration

  (spacemacs|forall-clojure-modes m
    (spacemacs/set-leader-keys-for-major-mode m
      "kt" 'kaocha-runner-run-test-at-point
      "kr" 'kaocha-runner-run-tests
      "ka" 'kaocha-runner-run-all-tests
      "kw" 'kaocha-runner-show-warnings
      "kh" 'kaocha-runner-hide-windows))

  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; clj-decompiler
  (spacemacs|forall-clojure-modes m
    (spacemacs/set-leader-keys-for-major-mode m
      "ed" 'clj-decompiler-decompile
      "eD" 'clj-decompiler-disassemble))

  ;; disable the annoying completion sometimes when hitting TAB
  (setq tab-always-indent t)

  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Clojure Data visualisation
  ;;

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

  (defvar me-hijack-cider-eval-wrap-form nil)

  (require 'cider-mode)

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

  ;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;; PR
  ;;;;;;;;;;;;

  (defun cider--insert-closing-delimiters (code)
  "Closes all open parenthesized or bracketed expressions."
  (with-temp-buffer
    (insert code)
    (goto-char (point-max))
    (let ((matching-delimiter nil))
      (while (ignore-errors
               (save-excursion
                 (backward-up-list 1)
                 (setq matching-delimiter (cdr (syntax-after (point)))))
               t)
        (insert-char matching-delimiter)))
    (buffer-string)))

(defun cider-eval-defun-up-to-point (&optional output-to-current-buffer)
  "Evaluate the current toplevel form up to point.
If invoked with OUTPUT-TO-CURRENT-BUFFER, print the result in the current
buffer.  It constructs an expression to eval in the following manner:

- It find the code between the point and the start of the toplevel expression;
- It balances this bit of code by closing all open expressions;
- It evaluates the resulting code using `cider-interactive-eval'."
  (interactive "P")
  (let* ((beg-of-defun (save-excursion (beginning-of-defun) (point)))
         (code (buffer-substring-no-properties beg-of-defun (point)))
         (code (cider--insert-closing-delimiters code)))
    (cider-interactive-eval code
                            (when output-to-current-buffer
                              (cider-eval-print-handler))
                            nil
                            (cider--nrepl-pr-request-map))))

  ;;;;;;;;;;;;
  ;;;;;;;;;;;; PR
  ;;;;;;;;;;;;;;;;;;;;;;

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
     "(create-ns 'dev)(intern 'dev 'reveal((requiring-resolve 'vlaaad.reveal/ui)))"))

  (defun vlaaad.reveal/clear ()
    (interactive)
    (cider-nrepl-sync-request:eval
     "(when (resolve 'dev/reveal) (dev/reveal {:vlaaad.reveal/command '(clear-output)}))"))

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


  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Clojure keybindings

  ;; toggle reader macro sexp comment
  ;; toggles the #_ characters at the start of an expression
  (defun clojure-toggle-reader-comment-sexp ()
    (interactive)
    (let* ((point-pos1 (point)))
      (evil-insert-line 0)
      (let* ((point-pos2 (point))
             (cmtstr "#_")
             (cmtstr-len (length cmtstr))
             (line-start (buffer-substring-no-properties point-pos2 (+ point-pos2 cmtstr-len)))
             (point-movement (if (string= cmtstr line-start) -2 2))
             (ending-point-pos (+ point-pos1 point-movement 1)))
        (if (string= cmtstr line-start)
            (delete-char cmtstr-len)
          (insert cmtstr))
        (goto-char ending-point-pos)))
    (evil-normal-state))
  ;;
  ;; Assign keybinding to the toggle-reader-comment-sexp function
  (spacemacs|forall-clojure-modes m
    (spacemacs/set-leader-keys-for-major-mode m
      "#" 'clojure-toggle-reader-comment-sexp))
  ;;
  ;;
  ;; Toggle view of a clojure `(comment ,,,) block'
  (defun clojure-hack/toggle-comment-block (arg)
    "Close all top level (comment) forms. With universal arg, open all."
    (interactive "P")
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "^(comment\\>" nil 'noerror)
        (call-interactively
         (if arg 'evil-open-fold
           'evil-close-fold)))))
  ;;
  (evil-define-key 'normal clojure-mode-map
    "zC" 'clojure-hack/toggle-comment-block
    "zO" (lambda () (interactive) (clojure-hack/toggle-comment-block 'open)))

  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; LSP
  ;;
  (require 'lsp-mode)
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

  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Projectile
  ;;
  ;;
  (setq-default projectile-indexing-method 'hybrid)

  (when (eq system-type 'windows-nt)
    ;; requires https://lib.rs/crates/coreutils
    (setq projectile-git-submodule-command "git submodule --quiet foreach 'echo $displaypath' | coreutils tr '\\n' '\\0'"))

  ;;;;;;
  ;; this does not work, needs a hook
  ;; (spacemacs/toggle-truncate-lines-on)

  ;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Solve the problem of deadly slow autocomplete when using semantic mode
  ;; https://github.com/syl20bnr/spacemacs/issues/1907
  ;; https://github.com/syl20bnr/spacemacs/issues/11058#issuecomment-407269954
  (defun et/semantic-remove-hooks ()
    (remove-hook 'completion-at-point-functions
                 'semantic-analyze-completion-at-point-function)
    (remove-hook 'completion-at-point-functions
                 'semantic-analyze-notc-completion-at-point-function)
    (remove-hook 'completion-at-point-functions
                 'semantic-analyze-nolongprefix-completion-at-point-function))

  (add-hook 'semantic-mode-hook #'et/semantic-remove-hooks)

  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Disable obnoxious auto-highlight-syntax
  (remove-hook 'prog-mode-hook 'auto-highlight-symbol-mode)

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

  (require 'treemacs-mode)
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)

  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; THEME
  ;;
  (setq doom-gruvbox-dark-variant "medium")
  (setq doom-gruvbox-light-variant "hard")

  ;; Fira Code Emacs workaround
  (when (eq 'windows-nt system-type)
    (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                  (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                  (36 . ".\\(?:>\\)")
                  (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                  (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                  (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                  (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                  ;; (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                                        ; fix attempt shape unibyte error
                                        ;(46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                  ;; (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                  (48 . ".\\(?:x[a-zA-Z]\\)")
                  (58 . ".\\(?:::\\|[:=]\\)")
                  (59 . ".\\(?:;;\\|;\\)")
                  (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                  (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                  (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                  (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                  (91 . ".\\(?:]\\)")
                  (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                  (94 . ".\\(?:=\\)")
                  (119 . ".\\(?:ww\\)")
                  (123 . ".\\(?:-\\)")
                  (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                  (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                  )))
     (dolist (char-regexp alist)
       (set-char-table-range composition-function-table (car char-regexp)
                             `([,(cdr char-regexp) 0 font-shape-gstring])))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Editing
  ;;

  (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hooks)
  ;;;;;;;;;;;;
  ;;
  ;; Indentation
  ;;
  (require 'clojure-mode)

  (define-clojure-indent
    (defrecord '(2 nil nil (1)))
    (deftype '(2 nil nil (1)))
    (do-nil 0)
    (do-true 0)
    (do-false 0)
    (cond! 0)
    (>defn :defn)
    (>defn- :defn)
    (case-eval :defn)
    (manifold.deferred/catch 0)
    (let-flow 1)
    (let-flow' 1)
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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Emacs text rendering optimizations
  ;; https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html

  ;; Only render text left to right
  (setq-default bidi-paragraph-direction 'left-to-right)

  ;; Disable Bidirectional Parentheses Algorithm
  (if (version<= "27.1" emacs-version)
      (setq bidi-inhibit-bpa t))

  ;; Files with known long lines
  ;; SPC f l to open files literally to disable most text processing

  ;; So long mode when Emacs thinks a file would affect performance
  (if (version<= "27.1" emacs-version)
      (global-so-long-mode 1))

  ;; End of: Emacs text rendering optimizations
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Override Spacemacs defaults
  ;;
  ;; Set new location for file bookmarks, SPC f b
  ;; Default: ~/.emacs.d/.cache/bookmarks
  (setq bookmark-default-file "~/.spacemacs.d/bookmarks")
  ;;
  ;; Set new location for recent save files
  ;; Default: ~/.emacs.d/.cache/recentf
  (setq recentf-save-file  "~/.spacemacs.d/recentf")
  ;;
  ;; native line numbers taking up lots of space?
  (setq-default display-line-numbers-width nil)
  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Eshell visual enhancements
  ;;
  ;; Add git status visual labels
  ;;
  (require 'dash)
  (require 's)
  ;;
  (defmacro with-face (STR &rest PROPS)
    "Return STR propertized with PROPS."
    `(propertize ,STR 'face (list ,@PROPS)))
  ;;
  (defmacro esh-section (NAME ICON FORM &rest PROPS)
    "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
    `(setq ,NAME
           (lambda () (when ,FORM
                        (-> ,ICON
                            (concat esh-section-delim ,FORM)
                            (with-face ,@PROPS))))))
  ;;
  (defun esh-acc (acc x)
    "Accumulator for evaluating and concatenating esh-sections."
    (--if-let (funcall x)
        (if (s-blank? acc)
            it
          (concat acc esh-sep it))
      acc))
  ;;
  (defun esh-prompt-func ()
    "Build `eshell-prompt-function'"
    (concat esh-header
            (-reduce-from 'esh-acc "" eshell-funcs)
            "\n"
            eshell-prompt-string))
  ;;
  ;;
  ;; Unicode icons on Emacs
  ;; `list-character-sets' and select unicode-bmp
  ;; scroll through bitmaps list to find the one you want
  ;; some bitmaps seem to change
  ;;
  (esh-section esh-dir
               "\xf07c"  ;  (faicon folder)
               (abbreviate-file-name (eshell/pwd))
               '(:foreground "olive" :bold bold :underline t))
  ;;
  (esh-section esh-git
               "\xf397"  ;  (git branch icon)
               (magit-get-current-branch)
               '(:foreground "maroon"))
  ;;
  ;; (esh-section esh-python
  ;;              "\xe928"  ;  (python icon)
  ;;              pyvenv-virtual-env-name)
  ;;
  (esh-section esh-clock
               ""  ;  (clock icon)
               (format-time-string "%H:%M" (current-time))
               '(:foreground "forest green"))
  ;;
  ;; Below I implement a "prompt number" section
  (setq esh-prompt-num 0)
  (add-hook 'eshell-exit-hook (lambda () (setq esh-prompt-num 0)))
  (advice-add 'eshell-send-input :before
              (lambda (&rest args) (setq esh-prompt-num (incf esh-prompt-num))))
  ;;
  ;;
  ;; "\xf0c9"  ;  (list icon)
  (esh-section esh-num
               "\x2130"  ;  ℰ (eshell icon)
               (number-to-string esh-prompt-num)
               '(:foreground "brown"))
  ;;
  ;; Separator between esh-sections
  (setq esh-sep " ")  ; or " | "
  ;;
  ;; Separator between an esh-section icon and form
  (setq esh-section-delim "")
  ;;
  ;; Eshell prompt header
  (setq esh-header "\n ")  ; or "\n┌─"
  ;;
  ;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
  ;; your login, these can be the same.
  (setq eshell-prompt-regexp " \x2130 ")   ; or "└─> "
  (setq eshell-prompt-string " \x2130 ")   ; or "└─> "
  ;;
  ;; Choose which eshell-funcs to enable
  ;; (setq eshell-funcs (list esh-dir esh-git esh-python esh-clock esh-num))
  ;; (setq eshell-funcs (list esh-dir esh-git esh-clock esh-num))
  (setq eshell-funcs (list esh-dir esh-git))

  ;; Enable the new eshell prompt
  (setq eshell-prompt-function 'esh-prompt-func)

  ;; End of Eshell
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;

  ;; (setq cider-format-code-options '(("indents" ((">defn" (("inner" 1)))))))

  ;; disable LSP indent on paste
  ;; https://github.com/syl20bnr/spacemacs/issues/14488
  ;; (dolist (func '(yank yank-pop evil-paste-before evil-paste-after))
  ;;   (advice-remove func #'spacemacs//yank-indent-region))


  ;; indentinator ------------
  ;; (add-to-list 'load-path "/Users/luis/.spacemacs.d/packages/indentinator")
  ;; (require 'indentinator)
  ;; (add-hook 'emacs-lisp-mode-hook #'indentinator-mode)
  ;; (add-hook 'clojure-mode-hook #'indentinator-mode)
  ;; (setq indentinator-initial-idle-time 0.6)
  ;; (setq indentinator-idle-time 0.6)
  ;;
  ;;

  ;; auto indent code. too aggressive with lsp
  ;; seems to cause massive slowdown with lsp:
  ;; (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  ;; (add-hook 'emacs-elisp-mode-hook #'aggressive-indent-mode)

  ;; (defun indent-clojure ()
  ;;   (when (eq major-mode 'clojure-mode)
  ;;     (clojure-align)))
  ;; (add-hook 'after-save-hook #'indent-clojure)

  ;; fix way too eager aggressive indent with clojure lsp
  ;; (require 'aggressive-indent)
  ;; (add-to-list
  ;;  'aggressive-indent-dont-indent-if
  ;;  '(and (derived-mode-p 'clojure-mode)
  ;;        (or (thing-at-point 'whitespace)
  ;;            (string= ":" (thing-at-point 'sexp)) )
  ;;        (null (string-blank-p (thing-at-point 'line)))
  ;;        ))
  ;; (add-to-list 'aggressive-indent-dont-indent-if
  ;;              '(eq (char-before) ?\s))

  ;; for testing:
  ;;(setq aggressive-indent-dont-indent-if nil)

  ;;
  ;; RUST
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (require 'toml-mode)
  (add-hook 'toml-mode #'smartparens-mode)

  ;;;;;;;;;;;;;;;;;
  ;;
  ;; LEDGER
  ;;
  (add-hook 'ledger-mode #'smartparens-mode)

  ;;;;;;;;;;;;;;;;;
  ;;
  ;;
  ;;
  ;; (setq xbuff (generate-new-buffer "*my output*"))

  ;; (evil-window-right 1)
  ;; (print (string= ":" (thing-at-point 'sexp)) xbuff)

  ;; (print (null (string-blank-p (thing-at-point 'line))) xbuff)

  ;; (print (let (ret)
  ;;          (mapatoms (lambda (s)
  ;;                      (when (cl-loop for prop in '(beginning-op end-op bounds-of-thing-at-point thing-at-point forward-op)
  ;;                                     thereis (get s prop))
  ;;                        (push s ret))))
  ;;          ret)
  ;;        xbuff)

  ;; (switch-to-buffer xbuff)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-scale-factor 1.2)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(auth-source-save-behavior nil)
 '(compilation-message-face 'default)
 '(compilation-scroll-output 'first-error)
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(emms-mode-line-icon-color "#1ba1a1")
 '(evil-want-C-u-scroll t)
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#404040")
 '(gnus-logo-colors '("#4c8383" "#bababa") t)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };") t)
 '(helm-completion-style 'emacs)
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(hl-sexp-background-color "#1c1f26")
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(mac-mouse-wheel-smooth-scroll nil)
 '(magit-diff-use-overlays nil)
 '(mouse-wheel-flip-direction t)
 '(mouse-wheel-tilt-scroll t)
 '(ns-use-native-fullscreen t t)
 '(package-selected-packages
   '(nix-mode company-nixos-options nixos-options zprint-mode yaml-mode lsp-dart flutter dart-server dart-mode mvn maven-test-mode lsp-java dap-mode bui groovy-mode groovy-imports orgit-forge orgit org-rich-yank org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download org-contrib org org-cliplink org-brain gnuplot evil-org adoc-mode markup-faces magit-todos wgrep unicode-fonts ucs-utils font-utils smex ranger persistent-soft lsp-ivy ligature ivy-yasnippet ivy-xref ivy-purpose ivy-hydra ivy-avy graphviz-dot-mode counsel-projectile counsel-css counsel swiper ivy company-box frame-local doom-themes cyberpunk-theme flycheck-ledger evil-ledger ledger-mode stickyfunc-enhance srefactor vmd-mode mmm-mode markdown-toc gh-md cfrs posframe origami lsp-mode dash-functional unfill mwim clj-refactor inflections xterm-color vterm terminal-here shell-pop multi-term eshell-z eshell-prompt-extras esh-help monokai-theme gruvbox-theme autothemer color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized ample-theme material-theme reverse-theme grandshell-theme flycheck-joker flycheck-clj-kondo sublime-themes lush-theme doom-modeline shrink-path alect-themes afternoon-theme yasnippet-snippets ws-butler writeroom-mode winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe valign uuidgen use-package undo-tree treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil toc-org tagedit symon symbol-overlay string-inflection spaceline-all-the-icons smeargle slim-mode scss-mode sass-mode reveal-in-osx-finder restart-emacs rainbow-delimiters pug-mode prettier-js popwin pcre2el password-generator paradox overseer osx-trash osx-dictionary osx-clipboard org-superstar open-junk-file nodejs-repl nameless move-text magit-svn magit-section magit-gitflow macrostep lsp-ui lsp-treemacs lsp-origami lorem-ipsum livid-mode lispy link-hint launchctl json-navigator json-mode js2-refactor js-doc indent-guide impatient-mode hybrid-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-purpose helm-projectile helm-org helm-mode-manager helm-make helm-lsp helm-ls-git helm-gitignore helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-cider helm-c-yasnippet helm-ag google-translate golden-ratio gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ fuzzy forge font-lock+ flycheck-pos-tip flycheck-package flycheck-elsa flx-ido fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-easymotion evil-cleverparens evil-args evil-anzu emr emmet-mode elisp-slime-nav editorconfig dumb-jump dotenv-mode dired-quick-sort diminish devdocs company-web column-enforce-mode clojure-snippets clean-aindent-mode cider-eval-sexp-fu centered-cursor-mode browse-at-remote auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent ace-link ace-jump-helm-line ac-ispell))
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(safe-local-variable-values
   '((cider-clojure-cli-aliases . "test:dev")
     (elisp-lint-indent-specs
      (if-let* . 2)
      (when-let* . 1)
      (let* . defun)
      (nrepl-dbind-response . 2)
      (cider-save-marker . 1)
      (cider-propertize-region . 1)
      (cider-map-repls . 1)
      (cider--jack-in . 1)
      (cider--make-result-overlay . 1)
      (insert-label . defun)
      (insert-align-label . defun)
      (insert-rect . defun)
      (cl-defun . 2)
      (with-parsed-tramp-file-name . 2)
      (thread-first . 1)
      (thread-last . 1))
     (checkdoc-package-keywords-flag)
     (cider-clojure-cli-aliases . "provided:test:datahike")
     (cider-known-endpoints
      ("electron-hub" "localhost" "5030"))
     (cider-known-endpoints
      ("electron-hub" "5030"))
     (cider-known-endpoints quote
                            (("electron-hub" "5030")))
     (cider-known-endpoints
      '(("electron-hub" "5030")))
     (cider-clojure-cli-aliases . "test:cljs")
     (cider-clojure-cli-aliases . "test:dev:electron-hub")
     (cider-clojure-cli-aliases . "test:dev:workspaces")
     (cider-clojure-cli-aliases . "snoop:test:dev")
     (cider-clojure-cli-aliases . "debug:test")
     (cider-clojure-cli-aliases . "debug")
     (options-global-cli-aliases . "cljs:dev")
     (javascript-backend . tide)
     (javascript-backend . tern)
     (javascript-backend . lsp)))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 101)
 '(scroll-preserve-screen-position t)
 '(spaceline-all-the-icons-hide-long-buffer-path t)
 '(vc-annotate-background "#404040")
 '(vc-annotate-color-map
   '((20 . "#ea4141")
     (40 . "#db4334")
     (60 . "#e9e953")
     (80 . "#c9d617")
     (100 . "#dc7700")
     (120 . "#bcaa00")
     (140 . "#29b029")
     (160 . "#47cd57")
     (180 . "#60a060")
     (200 . "#319448")
     (220 . "#078607")
     (240 . "#1ec1c4")
     (260 . "#1ba1a1")
     (280 . "#26d5d5")
     (300 . "#58b1f3")
     (320 . "#00a2f5")
     (340 . "#1e7bda")
     (360 . "#da26ce")))
 '(vc-annotate-very-old-color "#da26ce")
 '(warning-suppress-log-types '((comp)))
 '(weechat-color-list
   '(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:height 0.92))))
 '(mode-line-inactive ((t (:height 0.92)))))
)
