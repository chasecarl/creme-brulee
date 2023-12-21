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
    (load bootstrap-file nil 'nomessage))

  (defvar cb-after-packages-load ()
    "A list of all functions that run after all packages are loaded.")
  (defvar cb-no-line-number-modes ()
    "A list of modes without line numbers.")
  (defvar cb--warning-type 'creme-brulee
    "The warning type that appears in warnings from this file.")
  (defvar cb-agenda-category-width 18
    "The width of the category column in the agenda view.")
  )


;; if my calculations are right, this is the screen width for Acer Predator 21X
(setq max-laptop-screen-mm-width 492)


(defun cb--setup-custom ()
  "Configures custom."
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file t))


(defun cb-visit-init ()
  "Opens personal init.el"
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))


(defun cb--setup-use-package ()
  "Configures use-package."
  (straight-use-package 'use-package))


(defun cb-setup-theme ()
  "Setups the color theme."
  (use-package emacs
    :config
    (require-theme 'modus-themes)
    (setq modus-themes-completions '((selection . (accented intense semibold))
				     (popup . (accented semibold)))
	  modus-themes-fringes nil
	  modus-themes-inhibit-reload nil
	  modus-themes-links '(neutral-underline)
	  modus-themes-mode-line '(accented borderless)
	  modus-themes-markup '(background intense italic)
	  modus-themes-paren-match '(intense)
	  modus-themes-region '(accented bg-only)
	  modus-themes-org-blocks 'tinted-background
	  modus-themes-org-agenda '((header-date . (bold-today))))
    (load-theme 'modus-vivendi)))


(defun cb--adjust-visuals ()
  "Hides some unneeded things from GUI."
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 10)
  (add-to-list 'default-frame-alist '(undecorated . t))
  (cb-setup-theme)
  )


(defun cb--minimize-frame ()
  "Minimizes the initial and startup frames."
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))


(defun cb--setup-line-numbers ()
  "Configures line numbers in windows."
  (column-number-mode)
  (global-display-line-numbers-mode t)
  (setq display-line-numbers-type 'relative)

  (defun cb--hook-from-mode (mode)
    ;; For some reason, `intern-soft' returns nil when the symbol is actually there...
    (intern (concat (symbol-name mode) "-hook")))
  (defun cb--remove-line-numbers ()
    (dolist (mode cb-no-line-number-modes)
      (add-hook (cb--hook-from-mode mode)
		(lambda () (display-line-numbers-mode 0)))))

  (add-to-list 'cb-after-packages-load 'cb--remove-line-numbers))


(defun cb--open-agenda-only ()
  "Opens the agenda view for the current day."
  (org-agenda-list)
  (delete-other-windows))


(defun cb-window-management ()
  "Manage windows."
  (transient-define-prefix cb-manage-windows ()
    "Manage window transient."
    :transient-suffix 'transient--do-stay
    :transient-non-suffix 'transient--do-quit-one
    ["Switch window\n"
     [("h" "Go to the window on the left" windmove-left)
      ("j" "Go to the window below" windmove-down)
      ("k" "Go to the window above" windmove-up)
      ("l" "Go to the window on the right" windmove-right)
      ("r" "Rotate windows" cb-rotate-windows)]])
  ;; TODO: merge into one function (the if-version throws error when used
  ;;       on `cb-manage-windows')
  (defun cb--ensure-transient-hidden-popup (orig-fun &rest args)
    (if (eq (oref transient--prefix command) 'cb-manage-windows)
	(let ((old-show-popup transient-show-popup))
	  (customize-set-variable 'transient-show-popup nil)
	  (funcall orig-fun)  ; assuming it takes no args
	  (customize-set-variable 'transient-show-popup old-show-popup))
      (funcall orig-fun)))
  (defun cb--ensure-transient-hidden-popup-2 (orig-fun &rest args)
    (let ((old-show-popup transient-show-popup))
      (customize-set-variable 'transient-show-popup nil)
      (let ((res (apply orig-fun args)))
	(customize-set-variable 'transient-show-popup old-show-popup)
	res)))
  (advice-add 'transient--redisplay :around #'cb--ensure-transient-hidden-popup)
  (advice-add 'cb-manage-windows :around #'cb--ensure-transient-hidden-popup-2)
  ;; (cb-remove-advice-from-functions '(transient--redisplay cb-manage-windows))

  (defun cb-remove-advice-from-functions (funcs)
    (dolist (func funcs)
      (advice-mapc (lambda (advice _props) (advice-remove func advice)) func)))

  (use-package winum
    :init (winum-set-keymap-prefix (kbd "C-c"))
    :config (winum-mode))

  (use-package window-purpose
    :config
    ;; TODO: adjust the `repl' purpose definition (e.g. now it's too python-specific)
    (dolist (purpose-mapping '((conf-mode . conf)
			       (inferior-python-mode . repl)
			       (org-mode . org)
			       (python-mode . python)
			       (python-ts-mode . python)
			       (yaml-mode . yaml)
			       (yaml-ts-mode . yaml)))
      (add-to-list 'purpose-user-mode-purposes purpose-mapping))
    (purpose-compile-user-configuration)
    (purpose-mode))
  )


(defun cb-rotate-windows (arg)
  "Rotate your windows; use the prefix argument to rotate the other direction.

Taken from https://www.emacswiki.org/emacs/TransposeWindows ('Via Robert
Bost's solution' section)."
  (interactive "P")
  (if (not (> (count-windows) 1))
      (message "You can't rotate a single window!")
    (let* ((rotate-times (prefix-numeric-value arg))
           (direction (if (or (< rotate-times 0) (equal arg '(4)))
                          'reverse 'identity)))
      (dotimes (_ (abs rotate-times))
        (dotimes (i (- (count-windows) 1))
          (let* ((w1 (elt (funcall direction (window-list)) i))
                 (w2 (elt (funcall direction (window-list)) (+ i 1)))
                 (b1 (window-buffer w1))
                 (b2 (window-buffer w2))
                 (s1 (window-start w1))
                 (s2 (window-start w2))
                 (p1 (window-point w1))
                 (p2 (window-point w2)))
            (set-window-buffer-start-and-point w1 b2 s2 p2)
            (set-window-buffer-start-and-point w2 b1 s1 p1)))))))


(defun cb-layout-management ()
  "Manage layouts."
  (use-package perspective
    :init
    (setq persp-state-default-file
	  (expand-file-name "persp-state.el" user-emacs-directory)
	  persp-mode-prefix-key (kbd "C-x l"))
    (persp-mode)
    (when (file-exists-p persp-state-default-file)
      (persp-state-load persp-state-default-file))
    (persp-switch "main")
    :bind
    (("C-x b" . persp-switch-to-buffer*)
     ("C-x k" . persp-kill-buffer*)
     :map perspective-map
     ;; TODO: consider unbinding "s"
     ("l" . persp-switch))
    :config
    ;; `:hook' with `kill-emacs' fails on missing mode called `perspective'
    (add-hook 'kill-emacs-hook #'persp-state-save)))


(defun cb-workspace-management ()
  "Manage windows, buffers, layouts, etc."
  (cb-window-management)
  (cb-layout-management)
  )


(defun cb-running-on-small-screen-p ()
  "Returns t iff running on a small screen, nil otherwise."
  (< (display-mm-width) max-laptop-screen-mm-width)
  )


(defun cb-setup-font ()
  "Setups fonts."
  ;; TODO: consider configuring modus-themes-* fonts
  (let* ((mono-font "CodeNewRoman Nerd Font Mono")
         (mono-height (if (cb-running-on-small-screen-p)
                          140
                        110)))
    (set-face-attribute 'default nil :font mono-font :height mono-height)
    (set-frame-font mono-font nil t))
  ;; (let ((variable-font "Lato"))
  (let ((variable-font "Cardo"))
    (set-face-attribute 'variable-pitch nil :font variable-font :height 140))
  (prefer-coding-system 'utf-8))


(defun cb-startup ()
  "Stuff that runs on startup.

Shouldn't have any package configuration in, only setting variables, activating
modes, etc.
"
  (cb--adjust-visuals)
  (cb--minimize-frame)
  (pixel-scroll-precision-mode)
  (save-place-mode)
  (cb-setup-font)
  (use-package emacs
    :config
    (setq inhibit-startup-message t
	  scroll-preserve-screen-position t
	  transient-enable-popup-navigation nil)
    :hook ((prog-mode
	    comint-mode)
	   . (lambda () (setq truncate-lines t)))
    )
  (cb--setup-line-numbers)
  (setq-default indent-tabs-mode nil)
  )


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
    (setq savehist-save-minibuffer-history 1
	  savehist-additional-variables
	  '(kill-ring search-ring regexp-search-ring compile-history log-edit-comment-ring))
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
    :config
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "M-n") nil)
    :hook (after-init . global-company-mode)))


(defun cb-completion ()
  "Loads and configures completion backend."
  (cb--setup-vertico)
  (cb--setup-company)
  (define-key completion-list-mode-map (kbd "C-<return>")
	      (lambda ()
		(interactive)
		(choose-completion nil nil t)
		(with-current-buffer completion-reference-buffer
		  (electric-newline-and-maybe-indent))))
  )


(defun cb-org ()
  "Loads and configures org-mode and related packages."
  (use-package org
    :demand t
    :config
    (setq org-log-done 'time
	  org-todo-keywords
	  '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)"))
	  org-agenda-span 'fortnight
	  org-agenda-start-on-weekday 0
	  org-edit-src-content-indentation 0
	  org-element-use-cache nil  ; orj-journal: solves https://github.com/bastibe/org-journal/issues/406
          org-log-redeadline 'time
          org-log-reschedule 'time
          cb-organizer-path (expand-file-name "organizer.org" org-directory)
          org-default-notes-file cb-organizer-path
          org-agenda-files (list cb-organizer-path)
          org-refile-targets '((cb-organizer-path . (:maxlevel . 6)))
          ;; like the default, but category width is `cb-agenda-category-width'
          org-agenda-prefix-format `((agenda . ,(format
                                                 " %%i %%-%d:c%%?-12t%% s"
                                                 cb-agenda-category-width))
                                     (todo . ,(format
                                               " %%i %%-%d:c"
                                               cb-agenda-category-width))
                                     (tags . ,(format
                                               " %%i %%-%d:c"
                                               cb-agenda-category-width))
                                     (search . ,(format
                                                 " %%i %%-%d:c"
                                                 cb-agenda-category-width)))
          )
    ;; see https://emacs.stackexchange.com/a/76352
    (with-eval-after-load 'org-ctags (setq org-open-link-functions nil))
    :hook (org-mode . auto-fill-mode)
    :straight (:type built-in)
    :bind (
           ("C-c a" . org-agenda)
           ("C-c c" . org-capture)  ; TODO: seems like it may be used in some keymap - check
           ))

  (let ((my-org-journal-prefix-key "C-c j"))
    (use-package org-journal
      :after (org)
      :init
      (setq org-journal-prefix-key my-org-journal-prefix-key)
      :config
      (setq org-journal-dir
	    (expand-file-name "journal" org-directory)
	    org-journal-date-prefix "#+TITLE: "
	    org-journal-time-prefix "* "
	    org-journal-file-format "%Y-%m-%d.org"
            org-journal-carryover-items ""
            )
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
		  (expand-file-name "download" org-directory))
    :bind (:map org-mode-map (("C-c i y" . org-download-yank)
			      ("C-c i r" . org-display-inline-images))))

  (use-package emacs
    :preface
    (defun org-summary-todo (n-done n-not-done)
      "Switch entry to DONE when all subentries are done, to TODO otherwise.

Taken from info:org#Breaking Down Tasks
"
      (let (org-log-done org-todo-log-states)   ; turn off logging
        (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
    :init
    (add-hook 'org-after-todo-statistics-hook #'org-summary-todo)
    )
  )


(defun cb-global-bindings ()
  "Binds commands that aren't part of a package."
  ;; TODO: implement Creme Brulee transient
  (global-set-key (kbd "C-c b i") 'cb-visit-init)
  ;; this is what you lose with the below
  ;; TODO: extend `cb-manage-windows' to include it (partially?)
  ;; C-x w -		fit-window-to-buffer
  ;; C-x w 0		delete-windows-on
  ;; C-x w 2		split-root-window-below
  ;; C-x w 3		split-root-window-right
  ;; C-x w s		window-toggle-side-windows

  ;; C-x w ^ f	tear-off-window
  ;; C-x w ^ t	tab-window-detach
  (global-set-key (kbd "C-x w") 'cb-manage-windows)
  (global-set-key (kbd "M-z") 'zap-up-to-char)
  )


(defun cb-misc ()
  "Miscellaneous stuff."
  (cb--setup-custom))


(defun cb-setup-git ()
  (use-package emacs
    :init (setq git-commit-summary-max-length 50)
    :hook (git-commit-mode . (lambda () (setq fill-column 72)))
    )
  (use-package magit
    :config (add-to-list 'magit-blame-styles
			 '(margin
			   (margin-format    . (" %C %a" " %s%f" " %H"))
			   (margin-width     . 30)
			   (margin-face      . magit-blame-margin)
			   (margin-body-face . (magit-blame-dimmed))))))


(defun cb--setup-eglot ()
  ;; TODO: configure the language server here. If there are many,
  ;;       see eglot-server-programs variable
  (use-package eglot
    :straight (:type built-in)
    :hook (((python-mode
	     python-ts-mode
	     )
	    . eglot-ensure)
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
	      ,(if branch branch (when sourcedir "master"))
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
    (cb-setup-install-grammars))

  (use-package combobulate
    :straight (combobulate :type git
			   :host github
			   :repo "mickeynp/combobulate")
    :hook ((python-ts-mode
	    js-ts-mode
	    css-ts-mode
	    yaml-ts-mode
	    typescript-ts-mode
	    tsx-ts-mode
	    )
	   . combobulate-mode))

  ;; Not using `:mode' above because yaml config is not quite `treesit' config,
  ;; and using it defers `cb-setup-install-grammars'
  ;; TODO: consider using `use-package' `:demand' or `:init'
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode)))


(defun cb-setup-shell-and-terminal ()
  "Setups shell(s) and terminal emulator(s)."
  (add-to-list 'cb-no-line-number-modes 'eshell-mode)
  (add-to-list 'cb-no-line-number-modes 'term-mode)
  (add-hook 'term-exec-hook (lambda ()
			      (set-buffer-process-coding-system 'utf-8-unix
								'utf-8-unix)))
  (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color"))))


(defun cb-read-lines (file-path)
  "Return a list of lines of a file at FILE-PATH."
  (with-temp-buffer
    (info-insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))


(defun cb-expand-project-dotenv ()
  "Sets environment variables of the current project."
  (interactive)
  (let* ((dotenv-path (expand-file-name ".env" (project-root (project-current))))
	 (dotenv (cb-read-lines dotenv-path)))
    (dolist (line dotenv)
      (apply 'setenv (split-string line "=" t)))))


(defun cb-project-poetry-venv-path ()
  "Returns the current project Poetry venv path."
  (interactive)
  (let ((path-line
	 (car (seq-filter (lambda (el) (equal (substring el 0 5) "Path:"))
			  (split-string (shell-command-to-string "poetry env info") "\n" t)))))
    (cadr (split-string path-line))))


(defun cb-setup-python ()
  "Setups all not-LSP Python-related stuff."
  (use-package emacs
    :config
    (setq python-shell-interpreter "ipython"
	  python-shell-interpreter-args "--simple-prompt -i"
	  comint-scroll-to-bottom-on-input t
	  ;; if the following are t, the wordy commands become unbearably slow
	  ;; because of redisplays
	  comint-scroll-to-bottom-on-output nil
	  comint-scroll-show-maximum-output nil
	  ;; Emacs 29!
	  python-shell-dedicated 'project)
    ;; :before doesn't work for some reason
    (advice-add #'run-python :around #'(lambda (orig-fun &rest args)
					 (cb-expand-project-dotenv)
					 (apply orig-fun args)))
    :hook
    (inferior-python-mode . (lambda () (setq tab-width 4))))

  (use-package pyvenv
    :config
    (setq pyvenv-mode-line-indicator nil)
    (advice-add #'eglot-ensure :around #'(lambda (orig-fun &rest args)
					   (let ((venv-path (cb-project-poetry-venv-path)))
					     (when venv-path
					       (pyvenv-activate venv-path)))
					   (apply orig-fun args)))
    (pyvenv-mode t))

  (let ((command-history-dirname "pyhist/"))
    (use-package python-mls
      :demand t
      :init
      (let ((command-history-dirpath (expand-file-name
				      command-history-dirname
				      user-emacs-directory)))
	(unless (file-directory-p command-history-dirpath)
	  (if (not (file-regular-p command-history-dirpath))
	      (make-directory command-history-dirpath)
	    (lwarn
	     cb--warning-type
	     :warning
	     (concat "The path %s points to a regular file"
		     "; Python history files will only use its name as a prefix")
	     command-history-dirpath))))
      :config
      (setq python-mls-command-history-file command-history-dirname
	    python-mls-save-command-history t
	    python-mls-multiline-history-modifier nil)
      (define-key python-mls-mode-map [remap next-line] nil)
      (define-key python-mls-mode-map [remap previous-line] nil)
      :bind
      (:map python-mls-mode-map (("M-p" . comint-previous-matching-input-from-input)
				 ("M-n" . comint-next-matching-input-from-input)))
      :hook
      (inferior-python-mode . python-mls-mode))
    )

  (use-package python-pytest
    :config
    (defalias 'pytest-jump-to-error
      ;; TODO: debug or find/write a better implementation
      (kmacro "C-a C-SPC M-f M-w C-s <return> : : RET C-SPC C-s <return> SPC - RET C-b C-b M-w C-r <return> = = SPC M-y C-n C-n RET RET C-s <return> M-y C-n RET RET")))
  )


(defun cb-setup-web ()
  "Setups all web-related stuff."
  (use-package emacs
    :mode ("\\.tsx?\\'" . typescript-ts-mode)
    :config
    (setq css-indent-offset 2
          sgml-basic-offset 2
          )
    :hook
    ((css-mode html-mode js-mode js-ts-mode) . (lambda () (setq fill-column 80)))
    )
  )


(defun cb-setup-debug ()
  "Setups the debugger."
  (use-package realgud)
  (use-package realgud-ipdb))


(defun cb-markup ()
  "Setups all markup languages."
  (use-package yaml-mode))


(defun cb-setup-lisp ()
  "Setups all lisp-related stuff."
  (use-package paredit
    :hook ((emacs-lisp-mode
	    eval-expression-minibuffer-setup
	    ielm
	    lisp-mode
	    lisp-interaction-mode
	    scheme-mode
	    )
	   . enable-paredit-mode)
    :config
    (defun cb--paredit-RET ()
      "Wraps `paredit-RET' to provide a sensible minibuffer exprience.

Taken from https://www.reddit.com/r/emacs/comments/101uwgd/enable_paredit_mode_for_evalexpression_mini/jjq0jen"
      (interactive)
      (if (minibufferp)
	  (read--expression-try-read)
	(paredit-RET)))
    :bind (:map paredit-mode-map ("<return>" . cb--paredit-RET))
    ))


(defun cb-setup-comint ()
  "Setups comint."
  (setq comint-input-ring-size 10000))


(defun cb-setup-search ()
  "Setups search."
  (use-package rg
    :config (rg-enable-menu))
  )


(defun cb-dev ()
  "Development stuff."
  (cb-setup-git)
  (cb--setup-tree-sitter)
  (cb-setup-shell-and-terminal)
  (cb-markup)
  (cb-setup-lisp)
  (cb-setup-lsp)
  (cb-setup-python)
  (cb-setup-web)
  ;; (cb-setup-debug)
  (cb-setup-comint)
  (cb-setup-search)
  )


(defun cb-reading ()
  "Reading stuff."
  (use-package nov
    ;; `:mode' actually defers package configuration, and then `:config'
    ;; will run only after you open an epub file, so we have to use `:init'
    :init
    (add-to-list 'cb-no-line-number-modes 'nov-mode)
    :mode ("\\.epub\\'" . nov-mode)))


(defun cb-load-packages ()
  "Loads all specified packages."
  (cb-completion)
  (cb-dev)
  (cb-reading))


(defun cb-load-packages-with-hooks ()
  "The same as `cb-load-packages', but runs all defined package hooks."
  (cb-load-packages)
  (dolist (func cb-after-packages-load)
    (funcall func)))


(cb-startup)
(cb-package-management)
;; org setup should be before all other packages
(cb-org)
(cb-global-bindings)
(cb-misc)
(cb-load-packages-with-hooks)
;; Should be actually after all the packages are loaded, because some packages
;; are deferred based on file extension, but we're visiting these files at the
;; perspective load time
(cb-workspace-management)
