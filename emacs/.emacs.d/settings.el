(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package paradox
  :ensure t
  :commands paradox-list-packages
  :config
  (setq paradox-github-token t
        paradox-automatically-star nil
        paradox-execute-asynchronously nil))

(setq inhibit-startup-screen t       ;; don't show startup screen
        visible-bell nil               ;; no visual bell
        apropos-do-all t               ;; more options on aproppos search (C-h a)
        shift-select-mode nil)         ;; no region selecting with shift arrows

  (fset 'yes-or-no-p 'y-or-n-p)        ;; use y/n instead of yes/no
  (defalias 'list-buffers 'ibuffer)    ;; use ibuffer as default buffer list (C-x C-b)

  (menu-bar-mode 1)                   ;; ~no~ menu bar
  (tool-bar-mode -1)                   ;; no tool bar
  (scroll-bar-mode -1)                 ;; no scroll bar
  (blink-cursor-mode -1)               ;; no cursor blinking
  (fringe-mode '(10 . 1))              ;; 10 pixel window fringes
  (toggle-indicate-empty-lines t)      ;; display dashes on left border end of buffer
;;  (highlight-indentation-mode -1)     ;; do not highlight indentation

  (setq-default indent-tabs-mode nil)  ;; never use hard tabs

  (setq show-paren-delay 0)            ;; immediatly show matching delimiter
  (show-paren-mode t)                  ;; turn of highlighting of matching delimiters

  (column-number-mode 1)               ;; show column number in mode line
  (setq-default fill-column 80)        ;; number of characters before line wrap
  (global-visual-line-mode 1)          ;; turn on line wrap on window border
  (diminish 'visual-line-mode "w↩")   ;; diminish the bulky "Wrap"

  (require 'cl)
  ;; function to test fonts installed before setting some as default
  (defun font-candidate (&rest fonts)
    "Return existing font which first match."
    (find-if (lambda (f) (find-font (font-spec :name f))) fonts))

  ;; try to set some fonts as default
  (set-face-attribute 'default nil :font (font-candidate "Hack-14:weight=normal"
                                                         "Droid Sans Mono-14:weight=normal"
                                                         "DejaVu Sans Mono-14:weight=normal"))
  (setq linum-format " %3d ")           ;; linum format to keep line numbers 2 spaces from border and text
  (global-linum-mode 0)                  ;; don't display lateral line numbers
  (global-set-key (kbd "M-n") 'global-linum-mode) ;; toggle lateral line numbers

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t      ;; backup files are copies of the original
      delete-old-versions t    ;; no point in keeping all backaups
      kept-new-versions 6      ;; but keep some of them
      kept-old-versions 2
      version-control t)       ;; and use version control

(use-package github-modern-theme
  :ensure t)
(use-package zenburn-theme
  :ensure t)

(setq theme-moods '(zenburn
                    github-modern))

(load-theme (car theme-moods) t)

(defun toggle-theme-mood ()
  (interactive)
  (disable-theme (car theme-moods))
  (setq theme-moods (reverse theme-moods))
  (load-theme (car theme-moods) t))

(use-package spaceline
  :ensure t
  :defer 0.2
  :init
  (progn
    (require 'spaceline-config)
    (setq powerline-default-separator 'box))
  :config
  (progn
    (spaceline-emacs-theme)
    (spaceline-helm-mode)))

(use-package counsel
  :ensure t)

(use-package swiper
  :ensure try
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key (kbd "C-s") 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  ))

;; (use-package ivy :ensure t
  ;; :diminish (ivy-mode . "")
  ;; :bind
  ;; (:map ivy-mode-map
   ;; ("C-'" . ivy-avy))
  ;; :config
  ;; (ivy-mode 1))
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  ;; (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  ;; (setq ivy-height 10))
  ;; does not count candidates
  ;; (setq ivy-count-format "")
  ;; no regexp by default
  ;; (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  ;; (setq ivy-re-builders-alist
  ;;	;; allow input not in order
  ;     '((t   . ivy--regex-ignore-order))))

;; (use-package ivy-hydra
  ;; :ensure t)

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package ace-window
  :ensure t
  :config
  (progn
    (ace-window-display-mode 0)
    (setq aw-keys '(?a ?s ?d ?f ?z ?x ?c ?v))
    (global-set-key (kbd "M-'") 'ace-window)))

(use-package try
  :ensure t)

(defun comment-line-or-region (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above.
If region is active, apply to active region instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region
       (region-beginning) (region-end))
    (let ((range
           (list (line-beginning-position)
                 (goto-char (line-end-position n)))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    (forward-line 1)
    (back-to-indentation)))

(global-set-key (kbd "C-;")
                'comment-line-or-region)

(use-package expand-region
  :ensure t
  :bind(("M-@" . er/expand-region)))

(use-package wrap-region
  :ensure   t
  :config
  (wrap-region-global-mode t)
  (wrap-region-add-wrappers
   '(("(" ")")
     ("[" "]")
     ("{" "}")
     ("<" ">")
     ("'" "'")
     ("\"" "\"")
     ("‘" "’"   "q")
     ("“" "”"   "Q")
     ("*" "*"   "b"   org-mode)                 ; bolden
     ("*" "*"   "*"   org-mode)                 ; bolden
     ("/" "/"   "i"   org-mode)                 ; italics
     ("/" "/"   "/"   org-mode)                 ; italics
     ("~" "~"   "c"   org-mode)                 ; code
     ("~" "~"   "~"   org-mode)                 ; code
     ("=" "="   "v"   org-mode)                 ; verbatim
     ("=" "="   "="   org-mode)                 ; verbatim
     ("_" "_"   "u" '(org-mode markdown-mode))  ; underline
     ("**" "**" "b"   markdown-mode)            ; bolden
     ("*" "*"   "i"   markdown-mode)            ; italics
     ("`" "`"   "c" '(markdown-mode ruby-mode)) ; code
     ("`" "'"   "c"   lisp-mode)                ; code
     ))
  :diminish wrap-region-mode)

(use-package company
  :ensure t
  :init (setq company-require-match 'never)
  :bind ("C-|" . company-complete)
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :diminish "FC"
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package magit
  :ensure t
  :commands magit-status magit-blame
  :init
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  :config
  (setq magit-branch-arguments nil
        ;; use ido to look for branches
        magit-completing-read-function 'magit-ido-completing-read
        ;; don't put "origin-" in front of new branch names by default
        magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
        magit-push-always-verify nil
        ;; Get rid of the previous advice to go into fullscreen
        magit-restore-window-configuration t)
  :bind ("C-x g" . magit-status))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (setq sp-highlight-pair-overlay nil)
    (show-smartparens-global-mode t)
    (smartparens-global-mode t)))

(use-package rainbow-delimiters
  :ensure t)
(use-package racket-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
    (add-hook 'racket-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)
    (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :init
  (setq-default indent-tabs-mode nil)
  :config
  (setq python-indent 4)
  (add-hook 'python-mode-hook 'color-identifiers-mode))
  ;;(highlight-indentation-mode -1))

(use-package elpy
  :ensure t
  :commands elpy-enable
  :init (with-eval-after-load 'python (elpy-enable))

  :config
  (progn
    (elpy-use-ipython)
    ; use flycheck not flymake with elpy
    (when (require 'flycheck nil t)
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (add-hook 'elpy-mode-hook 'flycheck-mode))))

(use-package live-py-mode
  :ensure t)

(use-package fish-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode)))

(use-package julia-mode
  :ensure t)

(use-package julia-shell
  :ensure t)

(use-package ess
  :ensure t
  :defer t
  :init (setq inferior-julia-program-name "/home/felippe/.local/bin/julia"))

(use-package hy-mode
  :ensure t)

(use-package haskell-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
    (add-hook 'haskell-mode-hook 'haskell-indent-mode)
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
    (add-hook 'haskell-mode-hook 'haskell-doc-mode)))

(use-package yasnippet
  :ensure t
  :defer t
  :config 'yas-global-mode 1)

(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string
               (regexp-quote isearch-string))))))

(global-set-key (kbd "M-/") 'hippie-expand)

(use-package ox-reveal
:ensure ox-reveal)

(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-mathjax t)

(use-package htmlize
:ensure t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; (use-package org-beautify-theme
;;   :ensure t)

(setq org-src-fontify-natively t
      org-src-preserve-indentation t
      org-list-allow-alphabetical t
      org-completion-use-ido t)

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(setq org-confirm-babel-evaluate nil
      org-confirm-elisp-link-function nil
      org-confirm-shell-link-function nil)

(use-package ob-ipython
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (python . t)
   (org . t)))

(use-package org-ref
  :ensure t)

(fset 'typical-window-session
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([24 51 134217959 24 50 134217848 101 115 104 101 108 108 13 134217959 100 24 6 6 13 134217959 97] 0 "%d")) arg)))
(global-set-key (kbd "M-z") 'typical-window-session)
(fset 'create-3-windows-session
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217767 97 67108911 24 51 134217767 115 67108911 134217848 101 115 104 101 108 108 return 24 50 134217767 100 24 6 6 return 134217767 97] 0 "%d")) arg)))
(global-set-key (kbd "M-z") 'create-3-windows-session)
