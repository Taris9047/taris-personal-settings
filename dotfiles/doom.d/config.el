;;; ./config.el -*- lexical-binding: t; -*-

;; Fallback buffer names
(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")

(map! :leader
      :desc "List bookmarks"
      "b L" #'list-bookmarks
      :leader
      :desc "Save current bookmarks to bookmark file"
      "b w" #'bookmark-save)

(map! :leader
      :desc "Dired"
      "d d" #'dired
      :leader
      :desc "Dired jump to current"
      "d j" #'dired-jump
      (:after dired
       (:map dired-mode-map
        :leader
        :desc "Peep-dired image previews"
        "d p" #'peep-dired
        :leader
        :desc "Dired view file"
        "d v" #'dired-view-file)))
(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(setq centaur-tabs-set-bar 'over
      centaur-tabs-set-icons t
      centaur-tabs-gray-out-icons 'buffer
      centaur-tabs-height 24
      centaur-tabs-set-modified-marker t
      centaur-tabs-style "bar"
      centaur-tabs-modified-marker "•")
(map! :leader
      :desc "Toggle tabs globally"
      "t c" #'centaur-tabs-mode
      :leader
      :desc "Toggle tabs local display"
      "t C" #'centaur-tabs-local-mode)
(evil-define-key 'normal centaur-tabs-mode-map (kbd "g <right>") 'centaur-tabs-forward        ; default Doom binding is 'g t'
                                               (kbd "g <left>")  'centaur-tabs-backward       ; default Doom binding is 'g T'
                                               (kbd "g <down>")  'centaur-tabs-forward-group
                                               (kbd "g <up>")    'centaur-tabs-backward-group)

;; Custom functions to detect linux distro
(defun guess-linux-release(regexp)
  "Guess linux release"
  (let ((maybe-get-dis-str (shell-command-to-string "cat /etc/*release")))
    (with-temp-buffer
      (insert maybe-get-dis-str)
      (beginning-of-buffer)
      (condition-case nil
          (progn
            (search-forward-regexp regexp)
            (downcase (buffer-substring (match-beginning 1) (match-end 1))))
        (search-failed nil)))))

(defun guess-linux-based-distribution()
  "Guess linux distribution family"
  (guess-linux-release "^ID_LIKE=\"?\\([a-zA-Z ]*\\)\"?$"))

(defun guess-linux-distribution()
  "Guess linux distribution"
  (guess-linux-release "^ID=\"?\\(\\w*\\)\"?$"))

(if (string= (guess-linux-distribution) "elementary")
    (setq doom-theme 'doom-solarized-light)
  (setq doom-theme 'doom-palenight))

(map! :leader
      :desc "Load new theme"
      "h t" #'counsel-load-theme)

(map! :leader
      :desc "Winner redo"
      "w <right>" #'winner-redo
      :leader
      :desc "Winner undo"
      "w <left>" #'winner-undo)

(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; Navigation
      "<left>"  #'evil-window-left
      "<down>"  #'evil-window-down
      "<up>"    #'evil-window-up
      "<right>" #'evil-window-right
      ;; Swapping Windows
      "C-<left>"        #'+evil/window-move-left
      "C-<down>"        #'+evil/window-move-down
      "C-<up>"          #'+evil/window-move-up
      "C-<right>"       #'+evil/window-move-right)

(setq display-line-numbers-type t)
(map! :leader
      :desc "Comment or uncomment lines"
      "TAB TAB" #'comment-line
      :leader
      :desc "Toggle line numbers"
      "t l" #'doom/toggle-line-numbers
      :leader
      :desc "Toggle line highlight in frame"
      "t h" #'hl-line-mode
      :leader
      :desc "Toggle line highlight globally"
      "t H" #'global-hl-line-mode
      :leader
      :desc "Toggle truncate lines"
      "t t" #'toggle-truncate-lines)

(setq display-line-numbers-type t)
(map! :leader
      :desc "Comment or uncomment lines"
      "TAB TAB" #'comment-line
      :leader
      :desc "Toggle line numbers"
      "t l" #'doom/toggle-line-numbers
      :leader
      :desc "Toggle line highlight in frame"
      "t h" #'hl-line-mode
      :leader
      :desc "Toggle line highlight globally"
      "t H" #'global-hl-line-mode
      :leader
      :desc "Toggle truncate lines"
      "t t" #'toggle-truncate-lines)

(map! :leader
      :desc "Edit agenda file"
      "- a" #'(lambda () (interactive) (find-file "~/Org/agenda.org"))
      :leader
      :desc "Edit doom config.org"
      "- c" #'(lambda () (interactive) (find-file "~/.doom.d/config.org"))
      :leader
      :desc "Edit eshell aliases"
      "- e" #'(lambda () (interactive) (find-file "~/.doom.d/aliases"))
      :leader
      :desc "Edit doom init.el"
      "- i" #'(lambda () (interactive) (find-file "~/.doom.d/init.el"))
      :leader
      :desc "Edit doom packages.el"
      "- p" #'(lambda () (interactive) (find-file "~/.doom.d/packages.el")))

(after! neotree
  (setq neo-smart-open t
        neo-window-fixed-size nil))
(after! doom-themes
  (setq doom-neotree-enable-variable-pitch t))
(map! :leader
      :desc "Toggle neotree file viewer"
      "t n" #'neotree-toggle
      :leader
      :desc "Open directory in neotree"
      "d n" #'neotree-dir)

(after! org
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-directory "~/Org/"
        org-agenda-files '("~/Org/agenda.org")
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-log-done 'time
        org-journal-dir "~/Org/journal/"
        org-journal-date-format "%B %d, %Y (%A) "
        org-journal-file-format "%Y-%m-%d.org"
        org-hide-emphasis-markers t
        ;; ex. of org-link-abbrev-alist in action
        ;; [[arch-wiki:Name_of_Page][Description]]
        org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
          '(("google" . "http://www.google.com/search?q=")
            ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
            ("ddg" . "https://duckduckgo.com/?q=")
            ("wiki" . "https://en.wikipedia.org/wiki/"))
        org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
          '((sequence
             "TODO(t)"           ; A task that is ready to be tackled
             "BLOG(b)"           ; Blog writing assignments
             "GYM(g)"            ; Things to accomplish at the gym
             "PROJ(p)"           ; A project that contains other tasks
             "VIDEO(v)"          ; Video assignments
             "WAIT(w)"           ; Something is holding up this task
             "|"                 ; The pipe necessary to separate "active" states and "inactive" states
             "DONE(d)"           ; Task has been completed
             "CANCELLED(c)" )))) ; Task has been cancelled

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(setq user-full-name "Taylor Shin"
      user-mail-address "talezshin@gmail.com")

;; (add-hook! 'org-mode-hook #'mixed-pitch-mode)
;; (setq mixed-pitch-variable-pitch-cursor nil)
;; (use-package! mixed-pitch
;;   :hook (org-mode . mixed-pitch-mode)
;;   :config
;;   (setq mixed-pitch-set-height t)
;;   (set-face-attribute 'variable-pitch nil :height 1.2))

;; (setq doom-font (font-spec :family "mononoki Nerd Font Mono" :size 14 :height 1.0)
;;       doom-big-font (font-spec :family "mononoki Nerd Font Mono" :size 26)
;;       doom-variable-pitch-font (font-spec :family "NanumSquareRound" :size 14 :height 1.2)
;;       doom-serif-font (font-spec :family "NanumMyeongjo" :size 14))
(setq doom-font (font-spec :family "mononoki Nerd Font Mono" :size 16)
      doom-big-font (font-spec :family "mononoki Nerd Font Mono" :size 26)
      doom-variable-pitch-font (font-spec :family "NanumSquareRound" :size 16)
      doom-serif-font (font-spec :family "NanumMyeongjo" :size 16))

(map! :leader
      :desc "Copy to register"
      "r c" #'copy-to-register
      :leader
      :desc "Frameset to register"
      "r f" #'frameset-to-register
      :leader
      :desc "Insert contents of register"
      "r i" #'insert-register
      :leader
      :desc "Jump to register"
      "r j" #'jump-to-register
      :leader
      :desc "List registers"
      "r l" #'list-registers
      :leader
      :desc "Number to register"
      "r n" #'number-to-register
      :leader
      :desc "Interactively choose a register"
      "r r" #'counsel-register
      :leader
      :desc "View a register"
      "r v" #'view-register
      :leader
      :desc "Window configuration to register"
      "r w" #'window-configuration-to-register
      :leader
      :desc "Increment register"
      "r +" #'increment-register
      :leader
      :desc "Point to register"
      "r SPC" #'point-to-register)

(setq default-input-method "korean-hangul")
(global-set-key (kbd "S-SPC") 'toggle-input-method)
;; Be sure to enable hardware Hangul key from Keyboard(XKB) Options to use this.
(global-set-key (kbd "<Hangul>") 'toggle-input-method)
;; (global-set-key (kbd "<Ctrl_R>") 'toggle-input-method)
(global-set-key (kbd "<Alt_R>") 'toggle-input-method)
(when (eq system-type 'gnu/linux)
  (set-fontset-font t 'hangul (font-spec :family "NotoSans CJK KR" :size 16))
  )

(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)
(map! :leader
      :desc "Clone indirect buffer other window"
      "b c" #'clone-indirect-buffer-other-window)

(map! :leader
      :desc "Winner redo"
      "w <right>" #'winner-redo
      :leader
      :desc "Winner undo"
      "w <left>" #'winner-undo)

(custom-set-variables
 '(elfeed-feeds
   (quote
    (("https://www.reddit.com/r/linux.rss" reddit linux)
     ("https://www.gamingonlinux.com/article_rss.php" gaming linux)
     ("https://hackaday.com/blog/feed/" hackaday linux)
     ("https://opensource.com/feed" opensource linux)
     ("https://linux.softpedia.com/backend.xml" softpedia linux)
     ("https://itsfoss.com/feed/" itsfoss linux)
     ("https://www.zdnet.com/topic/linux/rss.xml" zdnet linux)
     ("https://www.phoronix.com/rss.php" phoronix linux)
     ("http://feeds.feedburner.com/d0od" omgubuntu linux)
     ("https://www.computerworld.com/index.rss" computerworld linux)
     ("https://www.networkworld.com/category/linux/index.rss" networkworld linux)
     ("https://www.techrepublic.com/rssfeeds/topic/open-source/" techrepublic linux)
     ("https://betanews.com/feed" betanews linux)
     ("http://lxer.com/module/newswire/headlines.rss" lxer linux)
     ("https://distrowatch.com/news/dwd.xml" distrowatch linux)))))

(emms-all)
(emms-default-players)
(emms-mode-line 1)
(emms-playing-time 1)
(setq emms-source-file-default-directory "~/Music/"
      emms-playlist-buffer-name "*Music*"
      emms-info-asynchronously t
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
(map! :leader
      :desc "Go to emms playlist"
      "a a" #'emms-playlist-mode-go
      :leader
      :desc "Emms pause track"
      "a x" #'emms-pause
      :leader
      :desc "Emms stop track"
      "a s" #'emms-stop
      :leader
      :desc "Emms play previous track"
      "a p" #'emms-previous
      :leader
      :desc "Emms play next track"
      "a n" #'emms-next)

(map! :leader
      :desc "Evaluate elisp in buffer"
      "e b" #'eval-buffer
      :leader
      :desc "Evaluate defun"
      "e d" #'eval-defun
      :leader
      :desc "Evaluate elisp expression"
      "e e" #'eval-expression
      :leader
      :desc "Evaluate last sexpression"
      "e l" #'eval-last-sexp
      :leader
      :desc "Evaluate elisp in region"
      "e r" #'eval-region)

(setq browse-url-browser-function 'eww-browse-url)
(map! :leader
      :desc "Eww web browser"
      "e w" #'eww
      :leader
      :desc "Eww reload page"
      "e R" #'eww-reload
      :leader
      :desc "Search web for text between BEG/END"
      "s w" #'eww-search-words)

(cond (IS-MAC
       (setq mac-command-modifier 'meta
             mac-option-modifier 'alt
             mac-right-option-modifier 'alt)))

;; File management stuff
(setq-default
 delete-by-moving-to-trash t
 window-combination-resize t
 x-stretch-cursor t)

;; Moar undos!
(setq undo-limit 800000000
      evil-want-fine-undo t
      truncate-string-ellipsis "…")

;; Tile mode!
(display-time-mode 1)

(if (equal "Batter status not available"
           (battery))
    (display-battery-mode 1)
  (setq password-cache-expiry nil))
(global-subword-mode 1)

;; Smaller default window size
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 80))

(use-package! graphviz-dot-mode)

(use-package! iedit
  :defer
  :config
  (set-face-background 'iedit-occurrence "Magneta")
  :bind
  ("C-;" . iedit-mode))

(defmacro ts/measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))
