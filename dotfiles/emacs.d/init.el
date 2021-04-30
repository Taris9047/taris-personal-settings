(setq vc-follow-symlinks nil)
(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(neotree fish-mode writeroom-mode which-key vterm use-package unicode-fonts toc-org smex ruby-electric projectile perspective peep-dired org-bullets org-auto-tangle ob-ipython markdown-mode magit-todos lua-mode js-comint ivy-rich ivy-posframe iedit haskell-mode graphviz-dot-mode general flycheck evil-collection eshell-syntax-highlighting elpy doom-themes doom-modeline dired-open dashboard counsel all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
