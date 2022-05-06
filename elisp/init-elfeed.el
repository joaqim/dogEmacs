;;; -*- lexical-binding: t; -*-

(straight-use-package 'elfeed)
(straight-use-package 'elfeed-goodies)
(straight-use-package 'elfeed-org)


(require 'cl-lib)
(cl-defun +elfeed-dead-feeds (&optional (years 1.0))
          "Return a list of feeds that haven't posted en entry in YEARS years."
          (interactive)
          (let* ((living-feeds (make-hash-table :test 'equal))
                 (seconds (* years 365.0 24 60 60))
                 (threshold (- (float-time) seconds)))
            (with-elfeed-db-visit (entry feed)
                                  (let ((date (elfeed-entry-date entry)))
                                    (when (> date threshold)
                                      (setf (gethash (elfeed-feed-url feed) living-feeds) t))))
            (cl-loop for url in (elfeed-feed-list)
                     unless (gethash url living-feeds)
                     collect url)))

;; face for starred articles
(defface elfeed-search-starred-title-face
  '((t :foreground "#f77"))
  "Marks a starred Elfeed entry.")

(defvar +youtube-feed-format
  '(("^UC" . "https://www.youtube.com/feeds/videos.xml?channel_id=%s")
    ("^PL" . "https://www.youtube.com/feeds/videos.xml?playlist_id=%s")
    (""    . "https://www.youtube.com/feeds/videos.xml?user=%s")))

(defun +elfeed-mark-read ()
    (interactive)
    (elfeed-search-untag-all 'unread)
    (previous-line)
    (elfeed-search-tag-all 'read))

(defun +elfeed-visit-entry-dwim (&optional arg)
  (interactive "P")
  (if arg
      (elfeed-search-browse-url)
    (-let [entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))]
      (if (s-matches? (rx "https://www.youtube.com/watch" (1+ any))
                      (elfeed-entry-link entry))
          (let* ((quality (completing-read "Max height resolution (0 for unlimited): " '("0" "480" "720" "1080")))
                 (format (if (= 0 (string-to-number quality)) "" (format "--ytdl-format=[height<=?%s]" quality))))
            (message "Opening %s with height ≤ %s with mpv..."
                     (elfeed-entry-link entry) quality)
            ;(elfeed-untag entry 'unread)
            (+elfeed-mark-read)
            (start-process "elfeed-mpv" nil "mpv" format (elfeed-entry-link entry))
            (elfeed-search-update :force))
        (if (eq major-mode 'elfeed-search-mode)
            (elfeed-search-browse-url)
          (elfeed-show-visit))))))

(defun +elfeed-hint-open-link ()
  "Open link with avy-goto-line hint"
  (interactive)
  (avy-goto-line)
  (+elfeed-visit-maybe-external)
  ;(avy-pop-mark)
  )
(defun +elfeed-hint-copy-link ()
  "Open link with avy-goto-line hint"
  (interactive)
  (avy-goto-line)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
    (setq entry-link (elfeed-entry-link entry))
    (when entry-link
      (x-set-selection 'PRIMARY entry-link)
      (message "Yanked: %s" entry-link)))
  ;(avy-pop-mark)
  )


(defun +elfeed-play-with-mpv (&optional speed)
  "Play entry link with mpv."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
    (setq entry-link (elfeed-entry-link entry))
    ;; TODO: Make cleaner conditionals, (if (<0 speed))... see [[https://github.com/Ambrevar/dotfiles/blob/master/.emacs.d/lisp/init-elfeed.el#L5]]
    (cond ((stringp speed)
           (message "Opening %s Speed: %s" entry-link speed))
          (t
            (message "Opening %s" entry-link))
          )
    ;(cond ((stringp speed) (message "Speed %s" speed)))
    (progn (+elfeed-mark-read)
           (elfeed-search-update-entry entry)
           ;(shell-command (concat " "  "/usr/bin/mpv" " '" entry-link "' 2>&1 > /dev/null & disown") nil nil))
           (cond ((stringp speed)
                  (start-process "elfeed-mpv" nil "mpv" (format "--speed=%s" speed) (elfeed-entry-link entry)))
                 (t
                   (start-process "elfeed-mpv" nil "mpv" (elfeed-entry-link entry)))
                 )
           )))

(defun +elfeed-play-with-mpvctl ()
  "Play entry link with mpvctl."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
    (setq entry-link (elfeed-entry-link entry))
    (message "Opening %s" entry-link)
    (progn (+elfeed-mark-read)
           (elfeed-search-update-entry entry)
           (shell-command (concat " " (expand-file-name "/home/jq/scripts/mpv/mpvctl") " add '" entry-link "' 2>&1 > /dev/null & disown") nil nil))
    ))


(defun +elfeed-open-with-eww ()
  "Open in eww with `eww-readable'."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
    (eww  (elfeed-entry-link entry))
    (add-hook 'eww-after-render-hook 'eww-readable nil t)))

;(defalias '+elfeed-toggle-star (elfeed-expose #'+elfeed-search-toggle-all 'star))

;(defun +elfeed-play-with-mpv-fast () (interactive) (+elfeed-play-with-mpv "0.77"))

;TODO: Match elfeed entry names/tags to different mpv "speeds" (slight/medium/fast/ludicrious)
; Might have: :northernlion:twitchvod:speed=fast: or speed=1.77 or s=f
(defvar +elfeed-visit-patterns
  '(
    ("youtu\\.?be" . +elfeed-play-with-mpvctl)
    ("twitch.tv/videos" . +elfeed-play-with-mpv-fast)
    ("twitch" . +elfeed-play-with-mpv)
    ("." . +elfeed-open-with-eww))
  "List of (regexps . function) to match against elfeed entry link to know
whether how to visit the link.")

(defun +elfeed-visit-maybe-external ()
  "Visit with external function if entry link matches `+elfeed-visit-patterns',
  visit otherwise."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode)
                 elfeed-show-entry
                 (elfeed-search-selected :single)))
        (patterns +elfeed-visit-patterns))
    (while (and patterns (not (string-match (caar patterns) (elfeed-entry-link entry))))
           (setq patterns (cdr patterns)))
    (cond
      (patterns
        (funcall (cdar patterns)))
      ((eq major-mode 'elfeed-search-mode)
       (call-interactively 'elfeed-search-show-entry))
      (t (elfeed-show-visit)))))


(defun +elfeed-read-tag (filter tag)
  "Template for filtering feed categories.

  FILTER is the filter string to apply, and TAG is a short name of
  the displayed category.

  The cursor is moved to the beginning of the first feed line."
  (setq elfeed-search-filter filter)
  (elfeed-search-update :force)
  (goto-char (point-min))
  (forward-line)
  (message (concat "elfeed: show " tag)))

(defun +elfeed-search-toggle-tag(tag)
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries do
             (if (elfeed-tagged-p tag entry)
               (elfeed-untag entry tag)
               (elfeed-tag entry tag)))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun +elfeed-toggle-unread (&optional use-generic-p)
  "Toggle 'unread' tag on current entry"
  (interactive)
  (+elfeed-search-toggle-tag 'unread)
  (previous-line)
  (+elfeed-search-toggle-tag 'read)
  ;TODO: make sure that the tags are correct, i.e: 'if(unread) entry=read' and vice versa
  ;(let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
    ;(if (elfeed-tagged-p 'unread)
      ;)
    ;)
  )

(defun +yt-dl-it (url)
  "Downloads the URL in an async shell"
  ;(let ((default-directory "~/vids"))
  ;  (async-shell-command (format "youtube-dl '%s'" url)))
  (let ((default-directory "/mnt/tb/music/youtube/"))
    ;(async-shell-command (format "youtube-dl '%s'" url)))
    (async-shell-command (format "youtube-dl --audio-quality 0 --ignore-errors --extract-audio --audio-format mp3 -o '%%(title)s.%%(ext)s' '%s'" url)))
  )

(defun +elfeed-youtube-dl (&optional use-region-p)
  "Youtube-DL link"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (+elfeed-mark-read); (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (+yt-dl-it it)
             )
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))


;(require 'cl-lib)
;(cl-defun elfeed-dead-feeds (&optional (years 0.1))
;"Return a list of feeds that haven't posted en entry in YEARS years."
;(interactive)
;(let* ((living-feeds (make-hash-table :test 'equal))
;(seconds (* years 365.0 24 60 60))
;(threshold (- (float-time) seconds)))
;(with-elfeed-db-visit (entry feed)
;(let ((date (elfeed-entry-date entry)))
;(when (> date threshold)
;(setf (gethash (elfeed-feed-url feed) living-feeds) t))))
;(cl-loop for url in (elfeed-feed-list)
;unless (gethash url living-feeds)
;collect url)))


(defun +elfeed-expand (listing)
  "Expand feed URLs depending on their tags."
  (cl-destructuring-bind (url . tags) listing
                         (cond
                           ((member 'youtube tags)
                            (let* ((case-fold-search nil)
                                   (test (lambda (s r) (string-match-p r s)))
                                   (format (cl-assoc url youtube-feed-format :test test)))
                              (cons (format (cdr format) url) tags)))
                           (listing))))

(defun +elfeed-save-db-and-bury ()
  "Wrapper to save the elfeed db to disk before burying buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))
;TODO: not working yet, need to kill frame/window and buffer
(defun +elfeed-save-db-and-kill ()
  "Wrapper to save the elfeed db to disk before killing window/frame and buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window)
  )
(defun +elfeed-search-quit-and-kill-buffers ()
  "Save the database, then kill elfeed buffers, asking the user
  for confirmation when needed."
  (interactive)
  (elfeed-db-save)
  (let (buf)
    (dolist (file rmh-elfeed-org-files)
      (setq buf (get-file-buffer file))
      (when (and (buffer-modified-p buf)
                 file
                 (y-or-n-p (format "Save file %s? " file)))
        (with-current-buffer buf (save-buffer)))
      (kill-buffer buf)))
  (kill-buffer "*elfeed-log*")
  (kill-buffer (current-buffer)))

(defun +elfeed-load-db-and-open ()
  "Wrapper to load the elfeed db from disk before opening"
  (interactive)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force)
  (elfeed-update))

(defun +elfeed-updater ()
  (interactive)
  (elfeed-db-save)
  (quit-window)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force)
  (elfeed-update))



(require 'timer)
(defvar +elfeed--timer nil)
(defun +elfeed-timer-activate ()
  (when +elfeed--timer
    (cancel-timer +elfeed--timer)
    (setq +elfeed--timer nil))
  ;(unless elfeed--timer
  (setq +elfeed--timer
        (run-with-timer "10 min" nil #'elfeed--update)))

;(with-eval-after-load "elfeed"
;(run-at-time nil (* 1 5 60) '+elfeed-updater)
;(run-at-time nil (* 1 25 60) #'elfeed--update)
;(elfeed-timer-activate))

;; Load elfeed-web
;(use-package elfeed-web
;:ensure t)
;(setq httpd-port 8020)
;(elfeed-web-start)
;(autoload #'elfeed-goodies/setup "elfeed-search-mode")
;(autoload #'elfeed-org "elfeed-search-mode")
;(with-eval-after-load "elfeed-goodies" (elfeed-goodies/setup))
;(autoload #'elfeed-goodies/setup "elfeed" nil t)
;(autoload #'elfeed-org "elfeed" nil t)

(elfeed-goodies/setup)
(elfeed-org)

(with-eval-after-load "elfeed-org"
                      (setq rmh-elfeed-org-files (list "~/org/elfeed.org"))
                      ;; New entry hook allows meta information manipulation
                      ;; without directly having to change elfeed-feeds
                      ;(add-hook 'elfeed-new-entry-hook
                      ;(elfeed-make-tagger :feed-url "youtube\\.com"
                      ;:add '(video youtube)))
                      ;(add-hook 'elfeed-new-entry-hook
                      ;(elfeed-make-tagger :feed-url "vimeo\\.com"
                      ;:add '(video vimeo)))
                      )

(defun elfeed-tag-selection-as (mytag)
  "Returns a function that tags an elfeed entry or selection as
  MYTAG"
  (lambda ()
    "Toggle a tag on an Elfeed search selection"
    (interactive)
    (elfeed-search-toggle-all mytag)))

;; Stars/favorites
;; based on http://matt.hackinghistory.ca/2015/11/22/elfeed/

;; add a star
(defun +elfeed-star ()
  "Apply starred to all selected entries."
  (interactive )
  (let* ((entries (elfeed-search-selected))
         (tag (intern "starred")))

    (cl-loop for entry in entries do (elfeed-tag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;; remove a star
(defun +elfeed-unstar ()
  "Remove starred tag from all selected entries."
  (interactive )
  (let* ((entries (elfeed-search-selected))
         (tag (intern "starred")))

    (cl-loop for entry in entries do (elfeed-untag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;;shortcut to jump to starred bookmark
(defun +elfeed-show-starred ()
  (interactive)
  (bookmark-jump "elfeed-starred"))

;; Elfeed lazy tweak navigation ( doom scroll ) from https://karthinks.com/software/lazy-elfeed/
(defun +elfeed-scroll-up-command (&optional arg)
  "Scroll up or go to next feed item in Elfeed"
  (interactive "^P")
  (let ((scroll-error-top-bottom nil))
    (condition-case-unless-debug nil
                                 (scroll-up-command arg)
                                 (error (elfeed-show-next)))))

(defun +elfeed-scroll-down-command (&optional arg)
  "Scroll up or go to next feed item in Elfeed"
  (interactive "^P")
  (let ((scroll-error-top-bottom nil))
    (condition-case-unless-debug nil
                                 (scroll-down-command arg)
                                 (error (elfeed-show-prev)))))

(defun +elfeed-display-buffer (buf &optional act)
  "Display elfeed entry in seperate buffer"
  (pop-to-buffer buf)
  (set-window-text-height (get-buffer-window) (round (* 0.7 (frame-height)))))

(defun +elfeed-search-show-entry-pre (&optional lines)
  "Returns a function to scroll forward or back in the Elfeed
  search results, displaying entries without switching to them."
  (lambda (times)
    (interactive "p")
    (forward-line (* times (or lines 0)))
    (recenter)
    (call-interactively #'elfeed-search-show-entry)
    (select-window (previous-window))
    (unless elfeed-search-remain-on-entry (forward-line -1))))

(defun +elfeed-split-pane-setup()
  (interactive)
  (setq elfeed-show-entry-switch #'+elfeed-display-buffer)

  (define-key elfeed-search-mode-map (kbd ">") (+elfeed-search-show-entry-pre +1))
  (define-key elfeed-search-mode-map (kbd "<") (+elfeed-search-show-entry-pre -1))
  (define-key elfeed-search-mode-map (kbd "M-RET") (+elfeed-search-show-entry-pre)))

(defun +elfeed-show-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((browse-url-browser-function #'eww-browse-url))
    (elfeed-show-visit use-generic-p)))

(defun +elfeed-search-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((browse-url-browser-function #'eww-browse-url))
    (elfeed-search-browse-url use-generic-p)))

;(map! :map (elfeed-search-mode-map elfeed-show-mode-map)
;(define-key! :keymaps (elfeed-search-mode-map elfeed-show-mode-map)
(autoload #'elfeed-search-map-mode "elfeed" nil t)


(with-eval-after-load "elfeed"
                      (setq elfeed-use-curl t)
                      (setq-default elfeed-search-filter "@1-days-ago +unread") ; +unread -reddit -music

                      ;; Stars/favorites
                      (push '(starred elfeed-search-starred-title-face) elfeed-search-face-alist)
                      (defalias '+elfeed-toggle-star (elfeed-expose #'+elfeed-search-toggle-all 'star))


                      ;; Mark all YouTube entries
                      (add-hook 'elfeed-new-entry-hook
                                (elfeed-make-tagger :feed-url "youtube\\.com"
                                                    :add 'youtube))

                      ;; TODO: Make theme-ing nicer ( macros galore )
                      ;; see: [[https://gist.github.com/alphapapa/80d2dba33fafcb50f558464a3a73af9a#file-elfeed-config-el-L522]]

                      (defface elfeed-comic-face
                               '((t :foreground "purple"))
                               "Webcomic feed"
                               :group 'elfeed)

                      (defface elfeed-twitch-face
                               '((t :foreground "white"
                                    :background "purple"))
                               "Twitch feed"
                               :group 'elfeed)

                      (defface elfeed-mustread-face
                               '((t :foreground "blue"))
                               "Important feed"
                               :group 'elfeed)

                      (defface elfeed-youtube-face
                               '((t :foreground "red"))
                               "YouTube feed"
                               :group 'elfeed)

                      (defface elfeed-blender-face
                               '((t :foreground "darkblue"
                                    :background "orange"
                                    ))
                               "Blender feed"
                               :group 'elfeed)

                      (defface elfeed-comedy-face
                               '((t :foreground "lightblue"))
                               "Comedy feed"
                               :group 'elfeed)

                      (defface elfeed-gaming-face
                               '((t :foreground "lightgreen"))
                               "Comedy feed"
                               :group 'elfeed)

                      (defface elfeed-ttt-face
                               '((t :foreground "white"
                                    :background "blue"))
                               "TTT feed"
                               :group 'elfeed)





                      (push '(unread elfeed-search-unread-title-face) elfeed-search-face-alist)
                      (push '(busy shadow) elfeed-search-face-alist)

                      (push '(youtube elfeed-youtube-face) elfeed-search-face-alist)
                      (push '(twitch elfeed-twitch-face) elfeed-search-face-alist)
                      (push '(mustread elfeed-mustread-face) elfeed-search-face-alist)
                      (push '(comic elfeed-comic-face) elfeed-search-face-alist)
                      (push '(youtube elfeed-youtube-face) elfeed-search-face-alist)
                      (push '(blender elfeed-blender-face) elfeed-search-face-alist)
                      (push '(comedy elfeed-comedy-face) elfeed-search-face-alist)
                      (push '(gaming elfeed-gaming-face) elfeed-search-face-alist)
                      (push '(gaming elfeed-ttt-face) elfeed-search-face-alist)

                      (defface elfeed-read-faceoo
                               '((((class color) (min-colors 88) (background light))
                                  :background "darkseagreen2")
                                 (((class color) (min-colors 88) (background dark))
                                  :background "darkolivegreen")
                                 (((class color) (min-colors 16) (background light))
                                  :background "darkseagreen2")
                                 (((class color) (min-colors 16) (background dark))
                                  :background "darkolivegreen")
                                 (((class color) (min-colors 8))
                                  :background "green" :foreground "black")
                                 (t :inverse-video t))
                               "Basic face for highlighting."
                               :group 'elfeed)

                      (defface elfeed-read-face '((t :background "#000")
                                                  :foreground "#444444")
                               "Read tagged"
                               :group 'elfeed)

                      (push '(read elfeed-read-face) elfeed-search-face-alist)

                      (defalias '+elfeed-play-with-mpv-slightest (elfeed-expose #'+elfeed-play-with-mpv "1.10"))
                      (defalias '+elfeed-play-with-mpv-slight (elfeed-expose #'+elfeed-play-with-mpv "1.33"))
                      (defalias '+elfeed-play-with-mpv-medium (elfeed-expose #'+elfeed-play-with-mpv "1.46"))
                      (defalias '+elfeed-play-with-mpv-fast (elfeed-expose #'+elfeed-play-with-mpv "1.77"))

                      ; Vim-like aliases
                      (define-key elfeed-search-mode-map (kbd "j") (kbd "n"))
                      (define-key elfeed-search-mode-map (kbd "k") (kbd "p"))
                      (define-key elfeed-search-mode-map (kbd "g") (kbd "<"))
                      (define-key elfeed-search-mode-map (kbd "G") (kbd ">"))

                      ;(define-key elfeed-show-mode-map (kbd "j") (kbd "n"))
                      ;(define-key elfeed-show-mode-map (kbd "k") (kbd "p"))
                      ;(define-key elfeed-show-mode-map (kbd ">") (kbd "n"))
                      ;(define-key elfeed-show-mode-map (kbd "<") (kbd "p"))


                      (define-key elfeed-search-mode-map (kbd "u") 'elfeed-update)
                      (define-key elfeed-search-mode-map (kbd "t") '+elfeed-toggle-unread)
                      (define-key elfeed-search-mode-map (kbd "s") '+elfeed-youtube-dl)
                      (define-key elfeed-search-mode-map (kbd "q") '+elfeed-save-db-and-kill)
                      (define-key elfeed-search-mode-map (kbd "e") '+elfeed-play-with-mpv)
                      (define-key elfeed-search-mode-map (kbd "o") '+elfeed-visit-maybe-external)
                      (define-key elfeed-search-mode-map [enter] '+elfeed-visit-maybe-external)
                      (define-key elfeed-search-mode-map (kbd "w") '+elfeed-star)
                      (define-key elfeed-search-mode-map (kbd "W") '+elfeed-unstar)
                      (define-key elfeed-search-mode-map (kbd "S") '+elfeed-show-starred)
                      (define-key elfeed-search-mode-map (kbd "m") '+elfeed-toggle-star)


                      (define-key elfeed-show-mode-map (kbd "B") '+elfeed-show-eww-open)
                      (define-key elfeed-search-mode-map (kbd "B") '+elfeed-search-eww-open)

                      (define-key elfeed-search-mode-map (kbd "a") '+elfeed-hint-open-link)
                      (define-key elfeed-search-mode-map (kbd ",") '+elfeed-hint-copy-link)

                      (define-key elfeed-show-mode-map (kbd "<") '+elfeed-scroll-up-command)
                      (define-key elfeed-show-mode-map (kbd ">") '+elfeed-scroll-down-command)
                      )

(global-set-key (kbd "C-x w") 'elfeed)

(provide 'init-elfeed)
