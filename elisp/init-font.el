;;; -*- lexical-binding: t; -*-

(defvar +font-ligature (not (null window-system))
  "If ligature is supported by current font.")

(when +font-ligature
  (straight-use-package '(ligature :type git :host github :repo "mickeynp/ligature.el"))

  (require 'ligature)

  (global-ligature-mode t)

  (with-eval-after-load "ligature"
    (ligature-set-ligatures 'emacs-lisp-mode
                            '("->" "->>" "<=" ">="))
    (ligature-set-ligatures 'elixir-mode
                            '("->" "=>" "|>" "<-" ">=" "<=" "!=" "!==" "===" "==" "::" "++" "&&" "||" "<<" ">>"))
    (ligature-set-ligatures 'clojure-mode
                            '("->" "->>" ">=" "<="  ".-"))
    (ligature-set-ligatures 'web-mode
                            '("</" "<!--" "-->" "/>"))))

;;; Fonts
;; Custom fonts can be set in ~/.emacs.d/private.el

;(defvar +font-wide-family "PxPlus IBM VGA 8x16")
;(defvar +font-tall-family "PxPlus IBM VGA 9x14")
(defvar +font-wide-family "PxPlus IBM VGA8")
(defvar +font-tall-family "PxPlus IBM VGA8")
(defvar +font-unicode-family "Unifont")
(defvar +fixed-pitch-family "Unifont")
(defvar +variable-pitch-family "Unifont")
(defvar +font-wide-or-tall 'tall)
(defvar +font-size-list '(12 13 14 15 16 17 18))
(defvar +font-size 12)

(defun +get-base-font ()
  (if (eq 'tall +font-wide-or-tall) +font-tall-family +font-wide-family))

(defun +load-base-font ()
  (let* ((font-spec (format "%s-%d" +font-family +font-size)))
    (set-frame-parameter nil 'font font-spec)
    (add-to-list 'default-frame-alist `(font . ,font-spec))))

(defun +load-face-font (&optional frame)
  (let ((font-spec (format "%s-%d" +font-family +font-size))
        (line-font-spec (format "%s-%d" +modeline-font-family +font-size))
        (variable-pitch-font-spec (format "%s-%d" +variable-pitch-family +font-size))
        (fixed-pitch-font-spec (format "%s-%d" +fixed-pitch-family +font-size)))
    (set-face-attribute 'variable-pitch frame
                        :font variable-pitch-font-spec
                        :height 1.2)
    (set-face-attribute 'fixed-pitch frame :font fixed-pitch-font-spec)
    (set-face-attribute 'fixed-pitch-serif frame :font fixed-pitch-font-spec)
    (set-face-attribute 'tab-bar frame :font font-spec :height 1.0)
    (set-face-attribute 'mode-line frame :font line-font-spec)
    (set-face-attribute 'mode-line-inactive frame :font line-font-spec)))

(defun +load-ext-font ()
  (when window-system
    (let ((font (frame-parameter nil 'font))
          (font-spec (font-spec :family +font-unicode-family)))
      (dolist (charset '(kana han hangul cjk-misc bopomofo symbol))
        (set-fontset-font font charset font-spec)))))

(defun +load-font ()
  (+load-base-font)
  (+load-face-font)
  (+load-ext-font))

(defun +larger-font ()
  (interactive)
  (if-let ((size (--find (> it +font-size) +font-size-list)))
      (progn (setq +font-size size)
             (+load-font)
             (message "Font size: %s" +font-size))
    (message "Using largest font")))

(defun +smaller-font ()
  (interactive)
  (if-let ((size (--find (< it +font-size) (reverse +font-size-list))))
      (progn (setq +font-size size)
             (message "Font size: %s" +font-size)
             (+load-font))
    (message "Using smallest font")))

;; Helper function to enable fixed pitch in buffer
(defun +use-fixed-pitch ()
  (interactive)
  (setq buffer-face-mode-face `(:family ,+fixed-pitch-family))
  (buffer-face-mode +1))

(global-set-key (kbd "M-+") #'+larger-font)
(global-set-key (kbd "M--") #'+smaller-font)

(+load-font)

(add-hook 'after-make-frame-functions
          (lambda (f)
            (+load-face-font f)
            (+load-ext-font)))

(provide 'init-font)
