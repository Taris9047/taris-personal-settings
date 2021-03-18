;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! all-the-icons-dired)
(package! flycheck-aspell)
(package! async)
(package! dired-open)
; (package! dmenu)
(package! elfeed)
(package! elpher)
(package! emms)
(package! emojify)
(package! evil-tutor)
(package! ivy-posframe)
(package! mastodon)
(package! org-bullets)
(package! org-ql)
(package! peep-dired)
(package! pianobar)
(package! rainbow-mode)
(package! resize-window)
(package! tldr)
(package! wc-mode)
(package! writeroom-mode)

;; Window pin
(package! rotate)

;; Pretty manual
(package! info-colors)

;; Open biglly files
(package! vlf :recipe (:host github :repo "m00natic/vlfi" :files ("*.el"))
  :pin "cc02f25337..." :disable t)
(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)


;; Editing systemd unit files
(package! systemd :pin "b6ae63a236...")

;; graphviz
(package! graphviz-dot-mode :pin "3642a0a5f4...")

;; org tangle
(package! org-auto-tangle)


;; Emoji
;; (setq emojify-emoji-set "twemoji-v2")
;; (defvar emojify-disabled-emojis
;;   '(;; Org
;;     "◼" "☑" "☸" "⚙" "⏩" "⏪" "⬆" "⬇" "❓"
;;     ;; Terminal powerline
;;     "✔"
;;     ;; Box drawing
;;     "▶" "◀")
;;   "Charachters that should never be affected by `emojify-mode'.")

;; (defadvice! emojify-delete-from-data ()
;;   "Ensure `emojify-disabled-emojis' don't appear in `emojify-emojis'."
;;   :after #'emojify-set-emoji-data
;;   (dolist (emoji emojify-disabled-emojis)
;;     (remhash emoji emojify-emojis)))
;; (add-hook! '(mu4e-compose-mode org-msg-edit-mode circe-channel-mode) (emoticon-to-emoji 1))

;; Info colors
;; (use-package! info-colors
;;   :commands (info-colors-fontify-node))
;; (add-hook 'Info-selection-hook 'info-colors-fontify-node)
;; (add-hook 'Info-mode-hook #'mixed-pitch-mode)

;; Ivy
;; (setq ivy-read-action-function #'ivy-hydra-read-action)
;; (setq ivy-sort-max-size 50000)

;; Smart Parentheses
;; (sp-local-pair
 ;; '(org-mode)
 ;; "<<" ">>"
 ;; :actions '(insert))

;; Ignore some files in tree
;; (after! treemacs
;;   (defvar treemacs-file-ignore-extensions '()
;;     "File extension which `treemacs-ignore-filter' will ensure are ignored")
;;   (defvar treemacs-file-ignore-globs '()
;;     "Globs which will are transformed to `treemacs-file-ignore-regexps' which `treemacs-ignore-filter' will ensure are ignored")
;;   (defvar treemacs-file-ignore-regexps '()
;;     "RegExps to be tested to ignore files, generated from `treeemacs-file-ignore-globs'")
;;   (defun treemacs-file-ignore-generate-regexps ()
;;     "Generate `treemacs-file-ignore-regexps' from `treemacs-file-ignore-globs'"
;;     (setq treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp treemacs-file-ignore-globs)))
;;   (if (equal treemacs-file-ignore-globs '()) nil (treemacs-file-ignore-generate-regexps))
;;   (defun treemacs-ignore-filter (file full-path)
;;     "Ignore files specified by `treemacs-file-ignore-extensions', and `treemacs-file-ignore-regexps'"
;;     (or (member (file-name-extension file) treemacs-file-ignore-extensions)
;;         (let ((ignore-file nil))
;;           (dolist (regexp treemacs-file-ignore-regexps ignore-file)
;;             (setq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
;;   (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter))
;; (setq treemacs-file-ignore-extensions
;;       '(;; LaTeX
;;         "aux"
;;         "ptc"
;;         "fdb_latexmk"
;;         "fls"
;;         "synctex.gz"
;;         "toc"
;;         ;; LaTeX - glossary
;;         "glg"
;;         "glo"
;;         "gls"
;;         "glsdefs"
;;         "ist"
;;         "acn"
;;         "acr"
;;         "alg"
;;         ;; LaTeX - pgfplots
;;         "mw"
;;         ;; LaTeX - pdfx
;;         "pdfa.xmpi"
;;         ))
;; (setq treemacs-file-ignore-globs
;;       '(;; LaTeX
;;         "*/_minted-*"
;;         ;; AucTeX
;;         "*/.auctex-auto"
;;         "*/_region_.log"
;;         "*/_region_.tex"))

;; ;; Popup faster
;; (setq which-key-idle-delay 0.5)

;; ;; Popup cleaner
;; (setq which-key-allow-multiple-replacements t)
;; (after! which-key
;;   (pushnew!
;;    which-key-replacement-alist
;;    '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
;;    '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
;;    ))

;; More org mode
;; (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))
;; (use-package! org-ref
;;   :after org
;;   :config
;;   (setq org-ref-completion-library 'org-ref-ivy-cite))


(provide 'packages)
;;; packages.el ends here
