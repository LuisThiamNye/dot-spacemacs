;; (setq shell-command-switch "-ic") ;; -i to load .zshrc for `shell-command'
;; (setenv "PATH" (concat "~/installations/carp/bin:" (getenv "PATH")))
;; (setenv "PATH" "/Users/luis/installations/carp/bin:/Users/luis/.emacs.d/bin:/usr/local/Cellar/emacs-plus/26.3/Emacs.app/Contents/MacOS:/Users/luis/scripts:/Library/Frameworks/Python.framework/Versions/3.8/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/TeX/texbin:/usr/local/share/dotnet:~/.dotnet/tools:/Library/Apple/usr/bin:/Users/luis/.local/bin:/usr/local/MacGPG2/bin:/Users/luis/.cargo/bin:/usr/local/Cellar/emacs-mac/emacs-27.2-mac-8.2/libexec/emacs/27.2/x86_64-apple-darwin20.2.0")

;; (getenv "PATH")
(setenv "CARP_DIR" "/Users/luis/installations/carp")
(add-to-list 'exec-path "/Users/luis/installations/carp/bin")

;;
;; Eshell visual enhancements
;;
;; Add git status visual labels
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'dash)
;; (require 's)

(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  `(propertize ,STR
               'face
               (list ,@PROPS)))

(defmacro esh-section (NAME ICON FORM &rest PROPS)
  "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
  `(setq ,NAME (lambda ()
                 (when ,FORM
                   (-> ,ICON
                       (concat esh-section-delim ,FORM)
                       (with-face ,@PROPS))))))

(defun esh-acc (acc x)
  "Accumulator for evaluating and concatenating esh-sections."
  (--if-let (funcall x)
      (if (s-blank? acc)
          it
        (concat acc esh-sep it))
    acc))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of Eshell
