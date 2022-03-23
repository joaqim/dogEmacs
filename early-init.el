(setq gc-cons-threshold 50000000)

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))

(require 'init-straight)
(require 'init-gc)

(let ((private-conf (expand-file-name "private.el" user-emacs-directory)))
  (when (file-exists-p private-conf)
    (load-file private-conf)))

(let ((my-conf (expand-file-name "my-conf.el" user-emacs-directory)))
  (when (file-exists-p my-conf)
    (load-file my-conf)))


(defun measure-require (pkg)
  (message "require: %s" pkg)
  (+measure-time-1
   (require pkg)))

(require 'init-util)
(require 'init-defaults)
(require 'init-modeline)
(require 'init-laf)
(require 'init-tab-bar)
