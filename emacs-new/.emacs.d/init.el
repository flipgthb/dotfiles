;;; init.el -*- lexical-binding: t; -*-
;;
;; Author: Felippe Alves <flipgm@gmail.com>
;; URL:    https://github.com/flipgthb/dotfiles

(defvar startup--init-el-start-time (current-time)
  "Time when init.el was started")

;; following https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar startup--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup--reset-gc-and-file-name-handler-values ()
  (setq gc-cons-threshold 16777216
	gc-cons-percentage 0.1
	file-name-handler-alist startup--file-name-handler-alist))

(add-hook 'emacs-startup-hook 'startup--reset-gc-and-file-name-handler-values)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Package Manager

;; Bootstrap straight.el as in https://github.com/raxod502/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; In case straight.el doesn't work
;; (require 'package)
;; (setq package-enable-at-startup nil)
;; (setq package-archives '(("ELPA"  . "http://tromey.com/elpa/")
;; 			 ("gnu"   . "http://elpa.gnu.org/packages/")
;; 			 ("melpa" . "https://melpa.org/packages/")
;; 			 ("org"   . "https://orgmode.org/elpa/")))
;; (package-initialize)

;; ;; Bootstrap `use-package'
;; (unless (package-installed-p 'use-package)
;; 	(package-refresh-contents)
;; 	(package-install 'use-package))



;;; Load config file

(let ((user-emacs-directory "~/.emacs.d/")
      (orgfile (concat user-emacs-directory "config.org"))
      (elfile (concat user-emacs-directory "config.el")))
  (if (or (not (file-exists-p elfile))
          (file-newer-than-file-p orgfile elfile))
      (org-babel-load-file orgfile)
    (load-file elfile)))

;; when config.org is saved, re-generate config.el:
(defun tangle-config-org-hook-func ()
  (when (string= "config.org" (buffer-name))
    (let ((user-emacs-directory "~/.emacs.d/")
	  (orgfile (concat user-emacs-directory "config.org"))
	  (elfile (concat user-emacs-directory "config.el")))
      (org-babel-tangle-file orgfile)
      (load-file elfile)
      (find-file orgfile))))

(add-hook 'after-save-hook 'tangle-config-org-hook-func)


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

(message " -- loading init.el in %.2fs" (float-time (time-subtract (current-time) startup--init-el-start-time)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; from: http://stackoverflow.com/questions/251908/how-can-i-insert-current-date-and-time-into-a-file-using-emacs
;; (defvar current-date-time-format "%a %b %d %Y-%m-%dT%H:%M:%S "
;;   "Format of date to insert with `insert-current-date-time' funcSee help of `format-time-string' for possible replacements")

;; ;; from: http://stackoverflow.com/questions/251908/how-can-i-insert-current-date-and-time-into-a-file-using-emacs
;; (defvar current-time-format "%a %H:%M:%S"
;;   "Format of date to insert with `insert-current-time' func.Note the weekly scope of the command's precision.")

;; (defun startup-tangle-config-org ()
;;   "This function will write all source blocks from =config.org= into =config.el= that are ...
;; - not marked as =tangle: no=
;; - doesn't have the TODO state =DISABLED=
;; - have a source-code of =emacs-lisp="
;;   (require 'org)
;;   (let* ((body-list ())
;; 	 (user-emacs-directory "~/.emacs.d/")
;;          (output-file (concat user-emacs-directory "config.el"))
;;          (org-babel-default-header-args (org-babel-merge-params org-babel-default-header-args
;;                                                                 (list (cons :tangle output-file)))))
;;     (message "—————• Re-generating %s …" output-file)
;;     (save-restriction
;;       (save-excursion
;;         (org-babel-map-src-blocks (concat user-emacs-directory "config.org")
;; 	  (let* (
;; 		 (org_block_info (org-babel-get-src-block-info 'light))
;; 		 ;;(block_name (nth 4 org_block_info))
;; 		 (tfile (cdr (assq :tangle (nth 2 org_block_info))))
;; 		 (match_for_TODO_keyword)
;; 		 )
;; 	    (save-excursion
;; 	      (catch 'exit
;; 		;;(when (string= "" block_name)
;; 		;;  (message "Going to write block name: " block_name)
;; 		;;  (add-to-list 'body-list (concat "message(\"" block_name "\")"));; adding a debug statement for named blocks
;; 		;;  )
;; 		(org-back-to-heading t)
;; 		(when (looking-at org-outline-regexp)
;; 		  (goto-char (1- (match-end 0))))
;; 		(when (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
;; 		  (setq match_for_TODO_keyword (match-string 1)))))
;; 	    (unless (or (string= "no" tfile)
;; 			(string= "DISABLED" match_for_TODO_keyword)
;; 			(not (string= "emacs-lisp" lang)))
;; 	      (add-to-list 'body-list (concat "\n\n;; #####################################################################################\n"
;; 					      "(message \"config • " (org-get-heading) " …\")\n\n")
;; 			   )
;; 	      (add-to-list 'body-list body)
;; 	      ))))
;;       (with-temp-file output-file
;;         (insert ";; ============================================================\n")
;;         (insert ";; Don't edit this file, edit config.org' instead ...\n")
;;         (insert ";; Auto-generated at " (format-time-string current-date-time-format (current-time)) " on host " system-name "\n")
;;         (insert ";; ============================================================\n\n")
;;         (insert (apply 'concat (reverse body-list))))
;;       (message "—————• Wrote %s" output-file))))
