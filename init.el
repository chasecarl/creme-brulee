(eval-and-compile
  ;; master seems to throw native-comp related errors on Emacs 29
  (setq straight-repository-branch "develop")

  ;; all of the below should be at the top as 'use-package is a macro https://emacs.stackexchange.com/a/70424
  (setq straight-use-package-by-default t)
  ;; bootstrapping code from https://github.com/radian-software/straight.el#getting-started
  (defvar bootstrap-version)
  (let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
	  (url-retrieve-synchronously
	   "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	   'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))


(defun cb--setup-custom ()
  "Configures custom."
  (setq custom-file
	(concat (file-name-as-directory user-emacs-directory) "custom.el"))
  (load custom-file))


(defun cb-visit-init ()
  "Opens personal init.el"
  (interactive)
  (find-file (concat (file-name-as-directory user-emacs-directory) "init.el")))


(defun cb--setup-use-package ()
  "Configures use-package."
  (straight-use-package 'use-package))


(defun cb--adjust-visuals ()
  "Hides some unneeded things from GUI."
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 10))


(defun cb--minimize-frame ()
  "Minimizes the initial and startup frames."
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))


(defun cb--setup-line-numbers ()
  "Configures line numbers in windows."
  (column-number-mode)
  (global-display-line-numbers-mode t)
  (setq display-line-numbers-type 'relative)

  ;; TODO: it should actually be after all the packages are loaded
  (dolist (mode '(term-mode-hook
		  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))


(defun cb--open-agenda-only ()
  "Opens the agenda view for the current day."
  (org-agenda-list 1)
  (delete-other-windows))


(defun cb-startup ()
  "Stuff that runs on startup."
  (setq inhibit-startup-message t)
  (cb--adjust-visuals)
  (cb--minimize-frame)
  (cb--setup-line-numbers))


(defun cb-package-management ()
  "Configures package manager and package loading."
  (cb--setup-use-package))


(defun cb--setup-vertico ()
  "Configures vertico."
  (use-package vertico
    :init
    (vertico-mode)
    (setq vertico-cycle t)
    ;; Add prompt indicator to `completing-read-multiple'.
    ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
    (defun crm-indicator (args)
      (cons (format "[CRM%s] %s"
		      (replace-regexp-in-string
		       "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		       crm-separator)
		      (car args))
	      (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    ;; Do not allow the cursor in the minibuffer prompt
    (setq minibuffer-prompt-properties
	  '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

    ;; Enable recursive minibuffers
    (setq enable-recursive-minibuffers t))

  ;; Persist history over Emacs restarts. Vertico sorts by history position.
  (use-package savehist
    :init
    (savehist-mode))

  ;; Use the `orderless' completion style.
  (use-package orderless
    :init
    ;; Configure a custom style dispatcher (see the Consult wiki)
    ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
    ;;       orderless-component-separator #'orderless-escapable-split-on-space)
    (setq completion-styles '(orderless basic)
	  completion-category-defaults nil
	  completion-category-overrides '((file (styles partial-completion))))))


(defun cb--setup-company ()
  "Setups company-mode."
  (use-package company
    :hook (after-init . global-company-mode)))


(defun cb-completion ()
  "Loads and configures completion backend."
  (cb--setup-vertico)
  (cb--setup-company))


(defun cb-org ()
  "Loads and configures org-mode and related packages."
  (use-package org
    :config
    (setq org-log-done 'time
	  org-todo-keywords
	  '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)"))
	  org-element-use-cache nil)  ; orj-journal: solves https://github.com/bastibe/org-journal/issues/406
    :hook (org-mode . auto-fill-mode)
    :straight (:type built-in)
    :bind (("C-c a" . org-agenda)))  ; global

  (let ((my-org-journal-prefix-key "C-c j"))
    (use-package org-journal
      :init
      (setq org-journal-prefix-key my-org-journal-prefix-key)
      :bind
      :config
      (setq org-journal-dir
	    (concat (file-name-as-directory org-directory) "journal")
	    org-journal-date-prefix "#+TITLE: "
	    org-journal-time-prefix "* "
	    org-journal-file-format "%Y-%m-%d.org"
	    org-journal-enable-agenda-integration t)
      (define-key org-journal-mode-map
		  (kbd (concat my-org-journal-prefix-key " j"))
		  'org-journal-open-current-journal-file)
      (define-key org-journal-mode-map
		  (kbd (concat my-org-journal-prefix-key " n"))
		  'org-journal-new-entry)
      (global-set-key (kbd my-org-journal-prefix-key)
		      (cdr (car (cdr (cdr (car (cdr org-journal-mode-map))))))))))


(defun cb-global-bindings ()
  "Binds commands that aren't part of a package."
  (global-set-key (kbd "C-c b i") 'cb-visit-init))


(defun cb-misc ()
  "Miscellaneous stuff."
   (cb--setup-custom))


(defun cb--setup-magit ()
  (use-package magit
    :bind (:map prog-mode-map ("C-c g" . magit-status))))


(defun cb-dev ()
  "Development stuff."
  (cb--setup-magit))


(defun cb-load-packages ()
  "Loads all specified packages."
  (cb-completion)
  (cb-org)
  (cb-dev))


(cb-startup)
(cb-package-management)
(cb-global-bindings)
(cb-misc)
(cb-load-packages)
(cb--open-agenda-only)
