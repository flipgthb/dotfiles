
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'ob-tangle)
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
;; (mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))
(org-babel-load-file (expand-file-name "settings.org" dotfiles-dir))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indicate-empty-lines t)
 '(org-reveal-title-slide (quote auto))
 '(package-selected-packages
   (quote
    (zenburn-theme wrap-region which-key use-package try spaceline smartparens rainbow-delimiters racket-mode paradox ox-reveal org-ref org-bullets ob-ipython monokai-theme magit live-py-mode julia-shell ivy-hydra hy-mode htmlize helm-descbinds haskell-mode flycheck flatui-theme fish-mode expand-region ess elpy darktooth-theme counsel ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
