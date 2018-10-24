;; I've copied a lot from https://github.com/novoid/dot-emacs/blob/master/init.el

(defvar startup-init-el-start-time (current-time) "Time when init.el was started")


;; following https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar startup-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; package manager
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("ELPA"  . "http://tromey.com/elpa/")
			 ("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

;; from: http://stackoverflow.com/questions/251908/how-can-i-insert-current-date-and-time-into-a-file-using-emacs
(defvar current-date-time-format "%a %b %d %Y-%m-%dT%H:%M:%S "
  "Format of date to insert with `insert-current-date-time' funcSee help of `format-time-string' for possible replacements")

;; from: http://stackoverflow.com/questions/251908/how-can-i-insert-current-date-and-time-into-a-file-using-emacs
(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.Note the weekly scope of the command's precision.")


(defun startup-tangle-config-org ()
  "This function will write all source blocks from =config.org= into =config.el= that are ...
- not marked as =tangle: no=
- doesn't have the TODO state =DISABLED=
- have a source-code of =emacs-lisp="
  (require 'org)
  (let* ((body-list ())
	 (user-emacs-directory "~/.emacs.d/")
         (output-file (concat user-emacs-directory "config.el"))
         (org-babel-default-header-args (org-babel-merge-params org-babel-default-header-args
                                                                (list (cons :tangle output-file)))))
    (message "—————• Re-generating %s …" output-file)
    (save-restriction
      (save-excursion
        (org-babel-map-src-blocks (concat user-emacs-directory "config.org")
	  (let* (
		 (org_block_info (org-babel-get-src-block-info 'light))
		 ;;(block_name (nth 4 org_block_info))
		 (tfile (cdr (assq :tangle (nth 2 org_block_info))))
		 (match_for_TODO_keyword)
		 )
	    (save-excursion
	      (catch 'exit
		;;(when (string= "" block_name)
		;;  (message "Going to write block name: " block_name)
		;;  (add-to-list 'body-list (concat "message(\"" block_name "\")"));; adding a debug statement for named blocks
		;;  )
		(org-back-to-heading t)
		(when (looking-at org-outline-regexp)
		  (goto-char (1- (match-end 0))))
		(when (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
		  (setq match_for_TODO_keyword (match-string 1)))))
	    (unless (or (string= "no" tfile)
			(string= "DISABLED" match_for_TODO_keyword)
			(not (string= "emacs-lisp" lang)))
	      (add-to-list 'body-list (concat "\n\n;; #####################################################################################\n"
					      "(message \"config • " (org-get-heading) " …\")\n\n")
			   )
	      (add-to-list 'body-list body)
	      ))))
      (with-temp-file output-file
        (insert ";; ============================================================\n")
        (insert ";; Don't edit this file, edit config.org' instead ...\n")
        (insert ";; Auto-generated at " (format-time-string current-date-time-format (current-time)) " on host " system-name "\n")
        (insert ";; ============================================================\n\n")
        (insert (apply 'concat (reverse body-list))))
      (message "—————• Wrote %s" output-file))))


;; following lines are executed only when tangle-config-org-hook-func()
;; was not invoked when saving config.org which is the normal case:
(let ((user-emacs-directory "~/.emacs.d/")
      (orgfile (concat user-emacs-directory "config.org"))
      (elfile (concat user-emacs-directory "config.el")))
;;      (gc-cons-threshold most-positive-fixnum))
  (when (or (not (file-exists-p elfile))
            (file-newer-than-file-p orgfile elfile))
    (startup-tangle-config-org))
    ;;(save-buffers-kill-emacs);; TEST: kill Emacs when config has been re-generated due to many issues when loading newly generated config.el
  (load-file elfile))

;; when config.org is saved, re-generate config.el:
(defun tangle-config-org-hook-func ()
  (when (string= "config.org" (buffer-name))
    (let ((user-emacs-directory "~/.emacs.d/")
	  (orgfile (concat user-emacs-directory "config.org"))
	  (elfile (concat user-emacs-directory "config.el")))
	  (startup-tangle-config-org))))
(add-hook 'after-save-hook 'tangle-config-org-hook-func)


;; (let ((fname (expand-file-name "~/.emacs.d/config.org")))
;;   (if (file-exists-p fname)
;;       (org-babel-load-file fname)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(require 'server)
(unless (server-running-p)
  (server-start))


;; following https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast

(defun startup-reset-values ()
  (setq gc-cons-threshold 16777216
	gc-cons-percentage 0.1
	file-name-handler-alist startup-file-name-handler-alist))

(add-hook 'emacs-startup-hook 'startup-reset-values)

(message "→★ loading init.el in %.2fs" (float-time (time-subtract (current-time) startup-init-el-start-time)))