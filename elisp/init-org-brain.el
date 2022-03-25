(straight-use-package 'org-brain)
(straight-use-package 'polymode)

(with-eval-after-load "org-brain"
  (setq org-brain-path "~/org/brain")
  ;(define-key ("C-c b" 'org-brain-prefix-map org-mode-map)
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer)
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12)
  (setq org-brain-include-file-entries nil
        org-brain-file-entries-use-title nil))

;; Allows you to edit entries directly from org-brain-visualize
(with-eval-after-load "polymode"
  (add-hook 'org-brain-visualize-mode-hook #'org-brain-polymode))


(provide 'init-org-brain)
