(use-package emojify
  :hook (after-init . global-emojify-mode))

(map! :leader
      (:prefix ("=" . "open file")
       :desc "Edit agenda file" "=" #'(lambda () (interactive) (find-file "~/.org/start.org"))
       :desc "Edit agenda file" "a" #'(lambda () (interactive) (find-file "~/.org/agenda.org"))
       :desc "Edit doom config.org" "c" #'(lambda () (interactive) (find-file "~/.settings/dotfiles/doom.d/config.org"))
       :desc "Edit doom init.el" "i" #'(lambda () (interactive) (find-file "~/.settings/dotfiles/doom.d/init.el"))
       :desc "Edit doom packages.el" "p" #'(lambda () (interactive) (find-file "~/.settings/dotfiles/doom.d/packages.el")) ))

(map! :leader
      (:prefix ("= e" . "open eshell settings files")
       :desc "Edit eshell aliases" "a" #'(lambda () (interactive) (find-file "~/.settings/dotfiles/doom.d/aliases"))
       :desc "Edit eshell profile" "p" #'(lambda () (interactive) (find-file "~/.settings/dotfiles/doom.d/profile"))        ))

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
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "Peep-dired image previews" "d p" #'peep-dired
        :desc "Dired view file"           "d v" #'dired-view-file)))

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "Z") 'dired-do-compress
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-do-kill-lines
  (kbd "% l") 'dired-downcase
  (kbd "% m") 'dired-mark-files-regexp
  (kbd "% u") 'dired-upcase
  (kbd "* %") 'dired-mark-files-regexp
  (kbd "* .") 'dired-mark-extension
  (kbd "* /") 'dired-mark-directories
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
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

(defun uname-s-output()
  "returns the uname -s command output"
  (let ((output (shell-command-to-string "uname -s")))
    (replace-regexp-in-string "\n$" "" output)))

;; Set different theme per distribution...
(pcase system-type

  ;; Linux
  (gnu/linux
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
;; For some strange reason, emacs-plus' system-type returns 'gnu/linux' instead of 'darwin'... huh...?
    ((string= (uname-s-output) "Darwin")
     (setq doom-theme 'doom-homage-white))
;; For any other cases
    (t (setq doom-theme 'doom-palenight)) ) )

  ;; MacOS
  (darwin
    (setq doom-theme 'doom-homage-white))

;; BSD?
  (berkeley-unix
   (setq doom-theme 'doom-one-light))

  ;; Windows
  (windows-nt
    (setq doom-theme 'doom-material))    )

(map! :leader
      :desc "Load new theme"
      "h t" #'counsel-load-theme)

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

(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed

(map! :leader
      :desc "Winner redo"
      "w <right>" #'winner-redo
      :leader
      :desc "Winner undo"
      "w <left>" #'winner-undo)

(after! org
  (setq org-directory "~/.org/"
        org-agenda-files '("~/.org/agenda.org")
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-log-done 'time
        org-journal-dir "~/.org/journal/"
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

(after! org
  (setq org-agenda-files '("~/.org/agenda.org")))

(setq
   ;; org-fancy-priorities-list '("[A]" "[B]" "[C]")
   ;; org-fancy-priorities-list '("❗" "[B]" "[C]")
   org-fancy-priorities-list '("🟥" "🟧" "🟨")
   org-priority-faces
   '((?A :foreground "#ff6c6b" :weight bold)
     (?B :foreground "#98be65" :weight bold)
     (?C :foreground "#c678dd" :weight bold))
   org-agenda-block-separator 8411)

(setq org-agenda-custom-commands
      '(("v" "A better agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (tags "PRIORITY=\"B\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Medium-priority unfinished tasks:")))
          (tags "PRIORITY=\"C\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Low-priority unfinished tasks:")))
          (tags "customtag"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Tasks marked with customtag:")))

          (agenda "")
          (alltodo "")))))

(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "replace")
        (:exports . "code")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "yes")
        (:comments . "link")))

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

(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

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

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

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

(setq shell-file-name "/bin/bash"
      eshell-aliases-file "~/.settings/dotfiles/doom.d/aliases"
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "zsh" "htop" "ssh" "top" "fish")
      vterm-max-scrollback 5000)
(map! :leader
      :desc "Eshell" "e s" #'eshell
      :desc "Counsel eshell history" "e h" #'counsel-esh-history)

(setq default-input-method "korean-hangul")
(global-set-key (kbd "S-SPC") 'toggle-input-method)
;; Be sure to enable hardware Hangul key from Keyboard(XKB) Options to use this.
(global-set-key (kbd "<Hangul>") 'toggle-input-method)
;; (global-set-key (kbd "<Ctrl_R>") 'toggle-input-method)
(global-set-key (kbd "<Alt_R>") 'toggle-input-method)
(when (eq system-type 'gnu/linux)
  (set-fontset-font t 'hangul (font-spec :family "Noto Sans CJK KR" :size 16))
  )

(setq face-font-rescale-alist
      '((".*hiragino.*" . 1.25)
        ("Noto Sans CJK KR" . 1.25)))

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

;; File management stuff
(setq-default
 delete-by-moving-to-trash t
 window-combination-resize t
 x-stretch-cursor t)

;; Moar undos!
(setq undo-limit 800000000
      evil-want-fine-undo t
      truncate-string-ellipsis "…")

(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)
(map! :leader
      :desc "Clone indirect buffer other window" "b c" #'clone-indirect-buffer-other-window)

(if (equal "Battery status not available"
           (battery))
    (display-battery-mode 1)
  (setq password-cache-expiry nil))
(global-subword-mode 1)

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

(add-to-list 'default-frame-alist '(height . 60) )
(add-to-list 'default-frame-alist '(width . 100) )

(defun save-buffers-kill-emacs-with-confirm ()
 "jsled's special save-buffers-kill-emacs, but with confirm"
 (interactive)
 (if (null current-prefix-arg)
     (if (y-or-n-p "Are you sure you want to quit?")
         (save-buffers-kill-emacs))
     (save-buffers-kill-emacs)))
(global-set-key "\C-x\C-c" 'save-buffers-kill-emacs-with-confirm)

(setq max-lisp-eval-depth 10000)
