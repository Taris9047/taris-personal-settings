;; [[file:config.org::*My credentials][My credentials:1]]
(setq user-full-name "Taylor Shin"
      user-mail-address "talezshin@gmail.com")
;; My credentials:1 ends here

;; [[file:config.org::*Header][Header:1]]
;;; ./config.el -*- lexical-binding: t; -*-
;; Header:1 ends here

;; [[file:config.org::*Window title][Window title:1]]
;; Fallback buffer names
(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")
;; Window title:1 ends here

;; [[file:config.org::*Bookmarking][Bookmarking:1]]
(map! :leader
      :desc "List bookmarks"
      "b L" #'list-bookmarks
      :leader
      :desc "Save current bookmarks to bookmark file"
      "b w" #'bookmark-save)
;; Bookmarking:1 ends here

;; [[file:config.org::*Dired directory manager][Dired directory manager:1]]
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
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))
;; Dired directory manager:1 ends here

;; [[file:config.org::*Centaur tabs][Centaur tabs:1]]
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
;; Centaur tabs:1 ends here

;; [[file:config.org::*Theme!][Theme!:1]]
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
;; Theme!:1 ends here

;; [[file:config.org::*Theme!][Theme!:2]]
;; Set different theme per distribution...
(cond
 ((string= (guess-linux-distribution) "elementary")
  (setq doom-theme 'doom-solarized-light))
 ((string= (guess-linux-distribution) "hamonikr")
  (setq doom-theme 'doom-moonlight))
 ((string= (guess-linux-distribution) "linuxmint")
  (setq doom-theme 'doom-henna))
 ((string= (guess-linux-distribution) "rhel")
  (setq doom-theme 'doom-horizon))
 ((string= (guess-linux-distribution) "opensuse-leap")
  (setq doom-theme 'doom-oceanic-next))
 ((string= (guess-linux-distribution) "debian")
  (setq doom-theme 'doom-monokai-pro))
 (t (setq doom-theme 'doom-palenight)))
;; Theme!:2 ends here

;; [[file:config.org::*Theme!][Theme!:3]]
(map! :leader
      :desc "Load new theme"
      "h t" #'counsel-load-theme)
;; Theme!:3 ends here

;; [[file:config.org::*Window Navigation][Window Navigation:1]]
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
;; Window Navigation:1 ends here

;; [[file:config.org::*Line number manipulation and Comment][Line number manipulation and Comment:1]]
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
;; Line number manipulation and Comment:1 ends here

;; [[file:config.org::*Window Splits][Window Splits:1]]
(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
;; Window Splits:1 ends here

;; [[file:config.org::*Winner Mode][Winner Mode:1]]
(map! :leader
      :desc "Winner redo"
      "w <right>" #'winner-redo
      :leader
      :desc "Winner undo"
      "w <left>" #'winner-undo)
;; Winner Mode:1 ends here

;; [[file:config.org::*Quick Settings Edit Shortcuts][Quick Settings Edit Shortcuts:1]]
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
;; Quick Settings Edit Shortcuts:1 ends here

;; [[file:config.org::*Basic Org mode setup][Basic Org mode setup:1]]
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
        org-list-allow-alphabetical t
        org-export-in-background t
        org-catch-invisible-edits 'smart
        org-export-with-sub-superscripts '{}
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
;; Basic Org mode setup:1 ends here

;; [[file:config.org::*Shortcut for SRC blocks][Shortcut for SRC blocks:1]]
;; Setup code block templates.
;; For Org-mode < 9.2
;;(setq old-structure-template-alist
;;      '(("py" "#+BEGIN_SRC python :results output\n?\n#+END_SRC" "")
;;        ("ipy" "#+BEGIN_SRC ipython :results output\n?\n#+END_SRC" "")
;;        ("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC" "")
;;        ("hs" "#+BEGIN_SRC haskell\n?\n#+END_SRC" "")
;;        ("laeq" "#+BEGIN_LaTeX\n\\begin{equation} \\label{eq-sinh}\ny=\\sinh x\n\\end{equation}\n#+END_LaTeX" "")
;;        ("sh" "#+BEGIN_SRC shell\n?\n#+END_SRC" "")
;;        ("r" "#+BEGIN_SRC R\n?\n#+END_SRC" "")
;;        ("js" "#+BEGIN_SRC js\n?\n#+END_SRC" "")
;;        ("http" "#+BEGIN_SRC http\n?\n#+END_SRC" "")
;;        ("ditaa" "#+BEGIN_SRC ditaa :file\n?\n#+END_SRC" "")
;;        ("dot" "#+BEGIN_SRC dot :file\n?\n#+END_SRC" "")
;;        ("rp" "#+BEGIN_SRC R :results output graphics :file \n?\n#+END_SRC" "")
;;        ("plantuml" "#+BEGIN_SRC plantuml :file\n?\n#+END_SRC" "")
;;        ("n" "#+NAME: ?")
;;        ("cap" "#+CAPTION: ?")))
;; For Org-mode >= 9.2
(setq org-structure-template-alist
      '(("py" . "src python :results output")
        ("ipy" . "src ipython :results output")
        ("el" . "src emacs-lisp")
        ("hs" . "src haskell")
        ("laeq" . "latex \n\\begin{equation} \\label{eq-sinh}\ny=\\sinh x\n\\end{equation}")
        ("sh" . "src shell")
        ("r" . "src R")
        ("js" . "src js")
        ("http" . "src http")
        ("ditaa" . "src ditaa :file")
        ("dot" . "src dot :file")
        ("rp" . "src R :results output graphics :file ")
        ("plantuml" . "src plantuml :file")
        ))
;; Keyword expansion also changed in 9.2
(setq my-tempo-keywords-alist
      '(("n" . "NAME")
        ("cap" . "CAPTION")))

(when (version< (org-version) "9.2")
  (add-to-list 'org-modules 'org-tempo))
(require 'org-tempo)
(if (version<  (org-version) "9.2")
    (dolist (ele old-structure-template-alist)
      (add-to-list 'org-structure-template-alist ele))
  (dolist (ele org-structure-template-alist)
    (add-to-list 'org-structure-template-alist ele))
  (dolist (ele my-tempo-keywords-alist)
    (add-to-list 'org-tempo-keywords-alist ele))
  )
;; Shortcut for SRC blocks:1 ends here

;; [[file:config.org::*Header arguments][Header arguments:1]]
(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "replace")
        (:exports . "code")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "yes")
        (:comments . "link")))
;; Header arguments:1 ends here

;; [[file:config.org::*Buffer Creation][Buffer Creation:1]]
(evil-define-command evil-buffer-org-new (count file)
  "Creates a new ORG buffer replacing the current window, optionally
   editing a certain FILE"
  :repeat nil
  (interactive "P<f>")
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new org*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (org-mode)))))
(map! :leader
      (:prefix "b"
       :desc "New empty ORG buffer" "o" #'evil-buffer-org-new))
;; Buffer Creation:1 ends here

;; [[file:config.org::*Bullet mode sequence][Bullet mode sequence:1]]
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))
;; Bullet mode sequence:1 ends here

;; [[file:config.org::*View Exported File][View Exported File:1]]
(map! :map org-mode-map
      :localleader
      :desc "View exported file" "v" #'org-view-output-file)

(defun org-view-output-file (&optional org-file-path)
  "Visit buffer open on the first output file (if any) found, using `org-view-output-file-extensions'"
  (interactive)
  (let* ((org-file-path (or org-file-path (buffer-file-name) ""))
         (dir (file-name-directory org-file-path))
         (basename (file-name-base org-file-path))
         (output-file nil))
    (dolist (ext org-view-output-file-extensions)
      (unless output-file
        (when (file-exists-p
               (concat dir basename "." ext))
          (setq output-file (concat dir basename "." ext)))))
    (if output-file
        (if (member (file-name-extension output-file) org-view-external-file-extensions)
            (browse-url-xdg-open output-file)
          (pop-to-buffer (or (find-buffer-visiting output-file)
                             (find-file-noselect output-file))))
      (message "No exported file found"))))

(defvar org-view-output-file-extensions '("pdf" "md" "rst" "txt" "tex" "html")
  "Search for output files with these extensions, in order, viewing the first that matches")
(defvar org-view-external-file-extensions '("html")
  "File formats that should be opened externally.")
;; View Exported File:1 ends here

;; [[file:config.org::*HTML Export][HTML Export:1]]
(define-minor-mode org-fancy-html-export-mode
  "Toggle my fabulous org export tweaks. While this mode itself does a little bit,
the vast majority of the change in behaviour comes from switch statements in:
 - `org-html-template-fancier'
 - `org-html--build-meta-info-extended'
 - `org-html-src-block-collapsable'
 - `org-html-block-collapsable'
 - `org-html-table-wrapped'
 - `org-html--format-toc-headline-colapseable'
 - `org-html--toc-text-stripped-leaves'
 - `org-export-html-headline-anchor'"
  :global t
  :init-value t
  (if org-fancy-html-export-mode
      (setq org-html-style-default org-html-style-fancy
            org-html-meta-tags #'org-html-meta-tags-fancy
            org-html-checkbox-type 'html-span)
    (setq org-html-style-default org-html-style-plain
          org-html-meta-tags #'org-html-meta-tags-default
          org-html-checkbox-type 'html)))
;; HTML Export:1 ends here

;; [[file:config.org::*TOC As a Collapsable Tree][TOC As a Collapsable Tree:1]]
(defadvice! org-html--format-toc-headline-colapseable (orig-fn headline info)
  "Add a label and checkbox to `org-html--format-toc-headline's usual output,
to allow the TOC to be a collapseable tree."
  :around #'org-html--format-toc-headline
  (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
      (funcall orig-fn headline info)
    (let ((id (or (org-element-property :CUSTOM_ID headline)
                  (org-export-get-reference headline info))))
      (format "<input type='checkbox' id='toc--%s'/><label for='toc--%s'>%s</label>"
              id id (funcall orig-fn headline info)))))

(defadvice! org-html--toc-text-stripped-leaves (orig-fn toc-entries)
  "Remove label"
  :around #'org-html--toc-text
  (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
      (funcall orig-fn toc-entries)
    (replace-regexp-in-string "<input [^>]+><label [^>]+>\\(.+?\\)</label></li>" "\\1</li>"
                              (funcall orig-fn toc-entries))))
;; TOC As a Collapsable Tree:1 ends here

;; [[file:config.org::*Tangle Hotkey][Tangle Hotkey:1]]
(map! :leader
      (:desc "Tangle the buffer!!"
             "t t" #'org-babel-tangle))
;; Tangle Hotkey:1 ends here

;; [[file:config.org::*Doom Font config.][Doom Font config.:1]]
(setq doom-font (font-spec :family "Mononoki Nerd Font" :size 16)
      doom-big-font (font-spec :family "Mononoki Nerd Font" :size 26)
      doom-variable-pitch-font (font-spec :family "NanumSquare" :size 16)
      doom-serif-font (font-spec :family "Mononoki Nerd Font" :size 16))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq global-prettify-symbols-mode t)
;; Doom Font config.:1 ends here

;; [[file:config.org::*Ivy-Posframe][Ivy-Posframe:1]]
(setq ivy-posframe-display-functions-alist
      '((swiper                     . ivy-posframe-display-at-point)
        (complete-symbol            . ivy-posframe-display-at-point)
        (counsel-M-x                . ivy-display-function-fallback)
        (counsel-esh-history        . ivy-posframe-display-at-window-center)
        (counsel-describe-function  . ivy-display-function-fallback)
        (counsel-describe-variable  . ivy-display-function-fallback)
        (counsel-find-file          . ivy-display-function-fallback)
        (counsel-recentf            . ivy-display-function-fallback)
        (counsel-register           . ivy-posframe-display-at-frame-bottom-window-center)
        (dmenu                      . ivy-posframe-display-at-frame-top-center)
        (nil                        . ivy-posframe-display))
      ivy-posframe-height-alist
      '((swiper . 20)
        (dmenu . 20)
        (t . 10)))
(ivy-posframe-mode 1) ; 1 enables posframe-mode, 0 disables it.
;; Ivy-Posframe:1 ends here

;; [[file:config.org::*Ivy Keybindings][Ivy Keybindings:1]]
(map! :leader
      (:prefix ("v" . "Ivy")
       :desc "Ivy push view" "v p" #'ivy-push-view
       :desc "Ivy switch view" "v s" #'ivy-switch-view))
;; Ivy Keybindings:1 ends here

;; [[file:config.org::*Registers (clipboard system?)][Registers (clipboard system?):1]]
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
;; Registers (clipboard system?):1 ends here

;; [[file:config.org::*Emacs shell settings.][Emacs shell settings.:1]]
(setq shell-file-name "/bin/bash"
      eshell-aliases-file "~/.doom.d/aliases"
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh")
      vterm-max-scrollback 5000)
(map! :leader
      :desc "Eshell" "e s" #'eshell
      :desc "Counsel eshell history" "e h" #'counsel-esh-history)
;; Emacs shell settings.:1 ends here

;; [[file:config.org::*Editor Font Settings][Editor Font Settings:1]]
(setq default-input-method "korean-hangul")
(global-set-key (kbd "S-SPC") 'toggle-input-method)
;; Be sure to enable hardware Hangul key from Keyboard(XKB) Options to use this.
(global-set-key (kbd "<Hangul>") 'toggle-input-method)
;; (global-set-key (kbd "<Ctrl_R>") 'toggle-input-method)
(global-set-key (kbd "<Alt_R>") 'toggle-input-method)
(when (eq system-type 'gnu/linux)
  (set-fontset-font t 'hangul (font-spec :family "Noto Sans CJK KR" :size 16))
  )
;; Editor Font Settings:1 ends here

;; [[file:config.org::*Font Size Matching][Font Size Matching:1]]
(setq face-font-rescale-alist
      '((".*hiragino.*" . 1.25)
        ("Noto Sans CJK KR" . 1.25)))
;; Font Size Matching:1 ends here

;; [[file:config.org::*Evaluate Elisp expressions.][Evaluate Elisp expressions.:1]]
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
;; Evaluate Elisp expressions.:1 ends here

;; [[file:config.org::*Elfeed stuff][Elfeed stuff:1]]
;; (custom-set-variables
;;  '(elfeed-feeds
;;    (quote
;;     (("https://www.reddit.com/r/linux.rss" reddit linux)
;;      ("https://www.gamingonlinux.com/article_rss.php" gaming linux)
;;      ("https://hackaday.com/blog/feed/" hackaday linux)
;;      ("https://opensource.com/feed" opensource linux)
;;      ("https://linux.softpedia.com/backend.xml" softpedia linux)
;;      ("https://itsfoss.com/feed/" itsfoss linux)
;;      ("https://www.zdnet.com/topic/linux/rss.xml" zdnet linux)
;;      ("https://www.phoronix.com/rss.php" phoronix linux)
;;      ("http://feeds.feedburner.com/d0od" omgubuntu linux)
;;      ("https://www.computerworld.com/index.rss" computerworld linux)
;;      ("https://www.networkworld.com/category/linux/index.rss" networkworld linux)
;;      ("https://www.techrepublic.com/rssfeeds/topic/open-source/" techrepublic linux)
;;      ("https://betanews.com/feed" betanews linux)
;;      ("http://lxer.com/module/newswire/headlines.rss" lxer linux)
;;      ("https://distrowatch.com/news/dwd.xml" distrowatch linux)))))
;; Elfeed stuff:1 ends here

;; [[file:config.org::*EMMS music player stuff][EMMS music player stuff:1]]
;; (emms-all)
;; (emms-default-players)
;; (emms-mode-line 1)
;; (emms-playing-time 1)
;; (setq emms-source-file-default-directory "~/Music/"
;;       emms-playlist-buffer-name "*Music*"
;;       emms-info-asynchronously t
;;       emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)
;; (map! :leader
;;       :desc "Go to emms playlist"
;;       "a a" #'emms-playlist-mode-go
;;       :leader
;;       :desc "Emms pause track"
;;       "a x" #'emms-pause
;;       :leader
;;       :desc "Emms stop track"
;;       "a s" #'emms-stop
;;       :leader
;;       :desc "Emms play previous track"
;;       "a p" #'emms-previous
;;       :leader
;;       :desc "Emms play next track"
;;       "a n" #'emms-next)
;; EMMS music player stuff:1 ends here

;; [[file:config.org::*EWS Web browser][EWS Web browser:1]]
;; (setq browse-url-browser-function 'eww-browse-url)
;; (map! :leader
;;       :desc "Eww web browser"
;;       "e w" #'eww
;;       :leader
;;       :desc "Eww reload page"
;;       "e R" #'eww-reload
;;       :leader
;;       :desc "Search web for text between BEG/END"
;;       "s w" #'eww-search-words)
;; EWS Web browser:1 ends here

;; [[file:config.org::*File management stuff][File management stuff:1]]
;; File management stuff
(setq-default
 delete-by-moving-to-trash t
 window-combination-resize t
 x-stretch-cursor t)
;; File management stuff:1 ends here

;; [[file:config.org::*More Undos!][More Undos!:1]]
;; Moar undos!
(setq undo-limit 800000000
      evil-want-fine-undo t
      truncate-string-ellipsis "…")
;; More Undos!:1 ends here

;; [[file:config.org::*Window Split Behaviors][Window Split Behaviors:1]]
(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)
(map! :leader
      :desc "Clone indirect buffer other window" "b c" #'clone-indirect-buffer-other-window)
;; Window Split Behaviors:1 ends here

;; [[file:config.org::*Show battery status][Show battery status:1]]
(if (equal "Battery status not available"
           (battery))
    (display-battery-mode 1)
  (setq password-cache-expiry nil))
(global-subword-mode 1)
;; Show battery status:1 ends here

;; [[file:config.org::*Graphviz][Graphviz:1]]
(use-package! graphviz-dot-mode)
;; Graphviz:1 ends here

;; [[file:config.org::*iEdit settings][iEdit settings:1]]
(use-package! iedit
  :defer
  :config
  (set-face-background 'iedit-occurrence "Magneta")
  :bind
  ("C-;" . iedit-mode))
;; iEdit settings:1 ends here

;; [[file:config.org::*measure-time macro][measure-time macro:1]]
(defmacro ts/measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))
;; measure-time macro:1 ends here

;; [[file:config.org::*Initial Window Size][Initial Window Size:1]]
(add-to-list 'default-frame-alist '(height . 60) )
(add-to-list 'default-frame-alist '(width . 100) )
;; Initial Window Size:1 ends here

;; [[file:config.org::*Exit messages are... Annoying][Exit messages are... Annoying:1]]
(defun save-buffers-kill-emacs-with-confirm ()
 "jsled's special save-buffers-kill-emacs, but with confirm"
 (interactive)
 (if (null current-prefix-arg)
     (if (y-or-n-p "Are you sure you want to quit?")
         (save-buffers-kill-emacs))
     (save-buffers-kill-emacs)))
(global-set-key "\C-x\C-c" 'save-buffers-kill-emacs-with-confirm)
;; Exit messages are... Annoying:1 ends here

;; [[file:config.org::*Lisp Eval Depth Crap][Lisp Eval Depth Crap:1]]
(setq max-lisp-eval-depth 10000)
;; Lisp Eval Depth Crap:1 ends here
