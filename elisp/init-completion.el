;;; -*- lexical-binding: t -*-

(straight-use-package 'vertico)
(straight-use-package 'orderless)
(straight-use-package 'marginalia)
(straight-use-package 'company)
(straight-use-package 'company-posframe)
(straight-use-package 'rg)
(straight-use-package 'prescient)
(straight-use-package 'yasnippet)

(defun +complete ()
  (interactive)
  (or (yas/expand)
      (company-indent-or-complete-common nil)))

;;; yasnippet

(autoload #'yas-minor-mode "yasnippet")

(add-hook 'prog-mode-hook 'yas-minor-mode)

(with-eval-after-load "yasnippet"
  (let ((inhibit-message t))
    (yas-reload-all))

  (define-key yas-keymap [escape] nil)
  (define-key yas-keymap [tab] nil)
  (define-key yas-keymap (kbd "S-<tab>") nil)
  (define-key yas-keymap (kbd "TAB") nil)
  (define-key yas-keymap [return] 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "RET") 'yas-next-field-or-maybe-expand)
  (define-key yas-keymap (kbd "S-<return>") 'yas-prev-field))

;;; company

(setq
 company-tng-auto-configure nil
 company-frontends '(company-tng-frontend
                     company-pseudo-tooltip-frontend
                     company-echo-metadata-frontend)
 company-begin-commands '(self-insert-command
                          backward-delete-char
                          sp-backward-delete-char)
 company-idle-delay 0
 company-tooltip-limit 10
 company-tooltip-align-annotations t
 company-tooltip-width-grow-only t
 company-tooltip-idle-delay 0.4
 company-minimum-prefix-length 3
 company-dabbrev-downcase nil
 company-abort-manual-when-too-short t
 company-require-match nil
 company-global-modes '(not dired-mode dired-sidebar-mode)
 company-tooltip-margin 0
 company-format-margin-function nil)

(autoload #'company-mode "company")

(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'conf-mode-hook 'company-mode)
(add-hook 'eshell-mode-hook 'company-mode)

(with-eval-after-load "company"
  (require 'company-tng)
  (require 'company-template)
  (require 'company-posframe)

  (add-hook 'company-mode-hook 'company-tng-mode)
  ;; (company-posframe-mode 1)

  (define-key company-mode-map [tab] '+complete)
  (define-key company-mode-map (kbd "TAB") '+complete)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map [escape] nil)
  (define-key company-active-map [return] nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "SPC") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)

  (define-key company-template-nav-map (kbd "RET") 'company-template-forward-field)
  (define-key company-template-nav-map [return] 'company-template-forward-field)
  (define-key company-template-nav-map (kbd "TAB") nil)
  (define-key company-template-nav-map [tab] nil))

(with-eval-after-load "company-posframe"
  (setq company-posframe-show-indicator nil
        company-posframe-quickhelp-delay nil
        company-posframe-show-metadata nil))

;;; vertico

(require 'vertico)
(vertico-mode 1)

(defun +vertico-init-minibuffer ()
  (setq-local completion-styles '(basic orderless)))

(defun +backward-delete-sexp ()
  (interactive)
  (save-restriction
    (narrow-to-region (minibuffer-prompt-end) (point-max))
    (delete-region
     (save-mark-and-excursion
       (backward-sexp)
       (point))
     (point))))

(with-eval-after-load "vertico"
  (require 'orderless)
  (setq orderless-skip-highlighting t)
  (add-hook 'minibuffer-setup-hook '+vertico-init-minibuffer)
  ;; https://lists.gnu.org/archive/html//help-gnu-emacs/2006-03/msg00609.html
  (local-set-key (kbd "M-DEL") #'+backward-delete-sexp))

;;; rg

(autoload #'rg-project "rg" nil t)

(require 'wgrep)
(with-eval-after-load "wgrep"
  (define-key wgrep-mode-map (kbd "C-c C-c") #'wgrep-finish-edit))

;; marginalia

(require 'marginalia)

(marginalia-mode 1)

(provide 'init-completion)
