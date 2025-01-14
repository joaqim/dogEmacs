;;; -*- lexical-binding: t -*-

(straight-use-package 'meow)

(defun meow-insert-right ()
  (interactive)
  (meow-right)
  (meow-insert))


(defun meow-insert-after-cursor ()
  (interactive)
  (if meow--temp-normal
      (progn
        (message "Quit temporary normal mode")
        (meow--switch-state 'motion))
    (meow--cancel-selection)
    (meow-right)
    (meow--switch-state 'insert)))

(defun meow-negative-find ()
  (interactive)
  (let ((current-prefix-arg -1))
    (call-interactively 'meow-find)))

(defun meow-setup ()
  ;; Programmer Dvorak layout on ansi keyboard
  (setq meow-cheatsheet-physical-layout meow-cheatsheet-physical-layout-ansi
        meow-cheatsheet-layout meow-cheatsheet-layout-dvp)
  ;; it's not a good idea to have a complex leader keymap
  ;; here we create bindings for necessary, high frequency commands
  (meow-leader-define-key
   ;; reverse command query
   ;; cheatsheet
   '("?" . meow-cheatsheet)
   ;; high frequency keybindings
   '("e" . "C-x C-e")
   '(")" . "C-)")
   '("}" . "C-}")
   '("," . xref-pop-marker-stack)
   '("." . xref-find-definitions)
   ;; window management
   '("w" . other-window)
   '("W" . window-swap-states)
   '("o" . delete-other-windows)
   '("s" . split-window-right)
   '("-" . split-window-below)
   ;; overwrited motion key
   '("$" . "H-$")
   ;; high frequency commands
   '("&" . +change-theme)
   '(";" . comment-dwim)
   '("k" . kill-this-buffer)
   '("d" . dired)
   '("b" . switch-to-buffer)
   '("r" . rg-project)
   '("f" . find-file)
   '("i" . imenu)
   '("a" . execute-extended-command)
   '("=" . smerge-basic-map)
   '("p" . project-find-file)
   '("j" . project-switch-to-buffer)
   '("t" . tab-bar-switch-to-tab)
   '("l" . project-switch-project)
   '("y" . magit)
   '("n" . org-roam-keymap)
   ;; toggles
   '("L" . display-line-numbers-mode)
   '("S" . smartparens-strict-mode)
   ;'("t" . telega)
   '("P" . pass)
   '("A" . org-agenda)
   '("D" . docker)
   '("E" . elfeed)
   '("F" . flymake-mode)
   '("\\" . dired-sidebar-toggle-sidebar)
   '("#" . +project-previous-buffer))
  (meow-motion-overwrite-define-key
   '("$" . repeat)
   '("'" . repeat)
   '("1" . select-window-1)
   '("2" . select-window-2)
   '("3" . select-window-3)
   '("4" . select-window-4)
   '("5" . select-window-5)
   '("6" . select-window-6)
   '("7" . select-window-7)
   '("8" . select-window-8)
   '("9" . select-window-9)
   '("0" . select-window-0)
   '("<escape>" . ignore))
  (meow-normal-define-key
   '("?" . meow-keypad-describe-key)
   '("*" . meow-expand-0)
   '("=" . meow-expand-9)
   '("!" . meow-expand-8)
   '("[" . meow-expand-7)
   '("]" . meow-expand-6)
   '("{" . meow-expand-5)
   '("+" . meow-expand-4)
   '("}" . meow-expand-3)
   '(")" . meow-expand-2)
   '("(" . meow-expand-1)
   '("1" . select-window-1)
   '("2" . select-window-2)
   '("3" . select-window-3)
   '("4" . select-window-4)
   '("5" . select-window-5)
   '("6" . select-window-6)
   '("7" . select-window-7)
   '("8" . select-window-8)
   '("9" . select-window-9)
   '("0" . select-window-0)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-line)
   '("E" . meow-goto-line)
   '("f" . meow-find)
   '("F" . meow-negative-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("/" . meow-insert-after-cursor)
   '("I" . meow-open-above)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . meow-till)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-search)
   '("t" . meow-right)
   '("T" . meow-right-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-save)
   '("X" . meow-sync-grab)
   '("y" . meow-yank)
   '("z" . meow-pop-selection)
   '("$" . repeat)
   '("'" . repeat)
   '("&" . meow-query-replace-regexp)
   '("%" . meow-query-replace)
   '("<escape>" . ignore)))

(setq
 meow-visit-sanitize-completion nil
 meow-keypad-describe-delay 1.0
 meow-replace-state-name-list '((normal . "N")
                                (motion . "M")
                                (keypad . "K")
                                (insert . "I")
                                (beacon . "B")))

(require 'meow)

(meow-global-mode 1)

(with-eval-after-load "meow"
  ;; disable expand hint in telega
  (add-to-list 'meow-expand-exclude-mode-list 'telega-chat-mode)

  ;; make Meow usable in TUI Emacs
  (add-to-list 'meow-mode-state-list '(inf-iex-mode . normal))
  (add-to-list 'meow-mode-state-list '(haskell-interactive-mode . normal))
  (add-to-list 'meow-mode-state-list '(erc-mode . normal))

  ;; use << and >> to select to bol/eol
  (add-to-list 'meow-char-thing-table '(?> . line))
  (add-to-list 'meow-char-thing-table '(?< . line))
  (add-to-list 'meow-char-thing-table '(?o . do/end))

  (meow-thing-register 'do/end
                       '(pair ("do" "fn") ("end"))
                       '(pair ("do" "fn") ("end")))
  (meow-thing-register 'quoted
                       '(regexp "`" "`\\|'")
                       '(regexp "`" "`\\|'"))

  (setq meow-grab-fill-commands '(meow-query-replace
                                  meow-query-replace-regexp
                                  eval-expression)
        meow-esc-delay 0.001)


  ;; define our command layout
  (meow-setup))

(provide 'init-modal)
