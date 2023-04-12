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
  (load custom-file t))


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
		  eshell-mode-hook
		  nov-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))


(defun cb--open-agenda-only ()
  "Opens the agenda view for the current day."
  (org-agenda-list)
  (delete-other-windows))


(defun cb-startup ()
  "Stuff that runs on startup."
  (setq inhibit-startup-message t)
  (cb--adjust-visuals)
  (cb--minimize-frame)
  (pixel-scroll-precision-mode)
  (save-place-mode)
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
	  org-agenda-span 'fortnight
	  org-agenda-start-on-weekday 0
	  org-edit-src-content-indentation 0
	  org-element-use-cache nil)  ; orj-journal: solves https://github.com/bastibe/org-journal/issues/406
    :hook (org-mode . auto-fill-mode)
    :straight (:type built-in)
    :bind (("C-c a" . org-agenda)))  ; global

  (let ((my-org-journal-prefix-key "C-c j"))
    (use-package org-journal
      :init
      (setq org-journal-prefix-key my-org-journal-prefix-key)
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
		      (cdr (car (cdr (cdr (car (cdr org-journal-mode-map)))))))))

  (use-package org-download
    :config
    (setq org-download-method 'directory
	  org-download-image-org-width 400
	  org-image-actual-width nil)
    (setq-default org-download-image-dir
		  (concat (file-name-as-directory org-directory) "download"))
    :bind (:map org-mode-map (("C-c i y" . org-download-yank)
			      ("C-c i r" . org-display-inline-images)))))


(defun cb-global-bindings ()
  "Binds commands that aren't part of a package."
  (global-set-key (kbd "C-c b i") 'cb-visit-init))


(defun cb-misc ()
  "Miscellaneous stuff."
   (cb--setup-custom))


(defun cb--setup-magit ()
  (use-package magit
    :bind (:map prog-mode-map ("C-c g" . magit-status))))


(defun cb--setup-eglot ()
  ;; TODO: configure the language server here. If there are many,
  ;;       see eglot-server-programs variable
  (use-package eglot
    :straight (:type built-in)
    :hook ((python-mode . eglot-ensure)
	   (python-ts-mode . eglot-ensure)
	   (prog-mode . hs-minor-mode))
    :bind (:map eglot-mode-map ("C-c r" . eglot-rename)))

  (defun cb-get-current-server ()
    "Returns the run command for the current language server."
    (interactive)
    (message "%s" (process-command
		   (jsonrpc--process (eglot-current-server))))))


(defun cb-setup-lsp ()
  "Setups the chosen LSP client."
  (cb--setup-eglot))


(defun cb--setup-tree-sitter ()
  (use-package treesit
    :straight (:type built-in)
    ;; Based on https://github.com/mickeynp/combobulate
    :preface
    (require 'cl-lib)
    (cl-defun cb--make-treesit-lang-list (lang
				          &key
					  (org "tree-sitter")
					  (repo (concat "tree-sitter-" (symbol-name lang)))
					  sourcedir
					  branch)
      (let ((result (list lang (concat "https://github.com/" org "/" repo))))
	(if (or sourcedir branch)
	    `(,@result
	      ;; We do need to include 'master' if we specify a sourcedir!
	      ,(when sourcedir (if branch branch "master"))
	      ,(if sourcedir sourcedir "src"))
	  result)))

    (defun cb-setup-install-grammars ()
      "Install Tree-sitter grammars if they are absent."
      (interactive)
      ;; the grammar list is almost entirely taken from
      ;; https://github.com/casouri/tree-sitter-module
      (dolist (grammar
	       '((clojure :org "dannyfreeman")
		 (cmake :org "uyha")
		 (css)
		 (dart :org "ast-grep")
		 (dockerfile :org "camdencheek")
		 (elisp :org "Wilfred")
		 (elixir :org "elixir-lang")
		 (glsl :org "theHamsta")
		 (go :org "camdencheek")
		 (heex :org "phoenixframework")
		 ;; (janet :org "sogaiu" :repo "tree-sitter-janet-simple")
		 (janet :org "GrayJack")
		 (javascript :sourcedir "src")
		 (make :org "alemuller")
		 (markdown :org "ikatyang")
		 (org :org "milisims")
		 (perl :org "ganezdragon")
		 (proto :org "mitchellh")
		 (python)
		 (scss :org "serenadeai")
		 (sql :org "DerekStride" :branch "gh-pages")
		 (surface :org "connorlay")
		 (toml :org "ikatyang")
		 (tsx :repo "tree-sitter-typescript" :sourcedir "tsx/src")
		 (typescript :sourcedir "typescript/src")
		 (vhdl :org "alemuller")
		 (wgsl :org "mehmetoguzderin")
		 (yaml :org "ikatyang")))
	(add-to-list 'treesit-language-source-alist
		     (apply 'cb--make-treesit-lang-list grammar))
	;; Only install `grammar' if we don't already have it
	;; installed. However, if you want to *update* a grammar then
	;; this obviously prevents that from happening.
	(unless (treesit-language-available-p (car grammar))
	  (treesit-install-language-grammar (car grammar)))))
    ;; You can remap major modes with `major-mode-remap-alist'. Note
    ;; that this does *not* extend to hooks! Make sure you migrate them
    ;; also
    (dolist (mapping '((c-mode . c-ts-mode)
		       (c++-mode . c++-ts-mode)
		       (conf-toml-mode . toml-ts-mode)
		       (css-mode . css-ts-mode)
		       (js-mode . js-ts-mode)
		       (python-mode . python-ts-mode)
		       (typescript-mode . tsx-ts-mode)))
      (add-to-list 'major-mode-remap-alist mapping))

    :config
    (cb-setup-install-grammars)

    :mode
    ("\\.ya?ml\\'" . yaml-ts-mode)))


(defun cb-dev ()
  "Development stuff."
  (cb--setup-magit)
  (cb--setup-tree-sitter)
  (cb-setup-lsp))


(defun cb-reading ()
  "Reading stuff."
  (use-package nov
    :mode ("\\.epub\\'" . nov-mode)))


(defun cb-load-packages ()
  "Loads all specified packages."
  (cb-completion)
  (cb-org)
  (cb-dev)
  (cb-reading))


(cb-startup)
(cb-package-management)
(cb-global-bindings)
(cb-misc)
(cb-load-packages)
(cb--open-agenda-only)
