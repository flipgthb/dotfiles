;; package manager
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indicate-empty-lines t)
 '(package-selected-packages
   (quote
    (zenburn-theme wrap-region which-key use-package undo-tree try spacemacs-theme spaceline spacegray-theme solarized-theme smartparens rainbow-delimiters racket-mode paradox ox-reveal org-ref org-bullets ob-ipython mode-line-bell magit live-py-mode julia-shell jedi iedit hy-mode hungry-delete htmlize haskell-mode github-modern-theme flycheck fish-mode expand-region ess elpy doom-themes counsel beacon ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
