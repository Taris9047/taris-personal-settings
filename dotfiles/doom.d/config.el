;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "List bookmarks"
      "b L" #'list-bookmarks
      :leader
      :desc "Save current bookmarks to bookmark file"
      "b w" #'bookmark-save)

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
;; Make 'h' and 'l' go back and forward in dired. Much faster to navigate the directory structure!
(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
;; If peep-dired is enabled, you will get image previews as you go up/down with 'j' and 'k'
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))



;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Taylor Shin"
      user-mail-address "talezshin@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "mononoki Nerd Font Mono" :size 16)
      doom-big-font (font-spec :family "mononoki Nerd Font Mono" :size 26)
      doom-variable-pitch-font (font-spec :family "NanumGothic" :size 16)
      doom-serif-font (font-spec :family "NanumGothic" :weight 'light))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(if (string= (guess-linux-distribution) "elementary")
  (setq doom-theme 'doom-solarized-light)
  (setq doom-theme 'doom-palenight))

(map! :leader
      :desc "Load new theme"
      "h t" #'counsel-load-theme)

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

;; emms
(emms-all)
(emms-default-players)
(emms-mode-line 1)
(emms-playing-time 1)
(setq emms-source-file-default-directory "~/Music/Non-Classical/70s-80s/"
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

;; Eww web browser
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


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
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

(after! mastodon
  (setq mastodon-instance-url "https://mastodon.technology/"))


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

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

;; Battery status show
(if (equal "Batter status not available"
           (battery))
    (display-battery-mode 1)
  (setq password-cache-expiry nil))

(global-subword-mode 1)

;; Smaller default window size
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 80))

;; Setting up windows stuff
; (setq evil-vsplit-window-right t
;       evil-split-window-below t)
; (defadvice! prompt-for-buffer (&rest _)
;   :after '(evil-window-split evil-window-vsplit)
;   (+ivy/switch-buffer))
; (setq +ivy-buffer-preview t)
(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)
(map! :leader
      :desc "Clone indirect buffer other window"
      "b c" #'clone-indirect-buffer-other-window)

;; Undo redo
(map! :leader
      :desc "Winner redo"
      "w <right>" #'winner-redo
      :leader
      :desc "Winner undo"
      "w <left>" #'winner-undo)


;; Hotkeys for window navigation
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

;; Hangul stuff
(setq default-input-method "korean-hangul")
(global-set-key (kbd "S-SPC") 'toggle-input-method)
(global-set-key (kbd "<Hangul>") 'toggle-input-method)
(global-set-key (kbd "<Ctrl_R>") 'toggle-input-method)
(global-set-key (kbd "<Alt_R>") 'toggle-input-method)
(when (eq system-type 'gnu/linux)
  (set-fontset-font t 'hangul (font-spec :family "NotoSans CJK KR" :size 16))
  )

;; neotree
(after! neotree
  (setq neo-smart-open t
        neo-window-fixed-size nil
        neo-window-width 25))
(after! doom-themes
  (setq doom-neotree-enable-variable-pitch t))
(map! :leader
      :desc "Toggle neotree file viewer"
      "t n" #'neotree-toggle
      :leader
      :desc "Open directory in neotree"
      "d n" #'neotree-dir)

;; Quick settings edit
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

;; Winner mode
(map! :leader
      :desc "Winner redo"
      "w <right>" #'winner-redo
      :leader
      :desc "Winner undo"
      "w <left>" #'winner-undo)


(provide 'config)
;;; config.el ends here
