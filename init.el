;; -*- lexical-binding: t -*-
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
  (defvar cb-bibliography-name "sources.bib"
    "The name of the bibliography file.")
  (defvar cb-org-roam-note-type-key-mapping '(("t" . Fleeting)
					      ("l" . Literature)
					      ("p" . Permanent))
    "The mapping of note types to keys for note type menus."))


(defun find-macro-usage ()
  (let ((result (search-forward
                 ;; so that it won't find itself
                 (format "%s%s" "(cb-make-" "note-type-menu")
                 nil t)))
    (back-to-indentation)
    result))


(defun expand-eval-unexpand ()
  "Expand the macro at point, evaluate it expanded, unexpand it.

This is achieved by killing and yanking, so the buffer will be considered modified.
"
  (kill-sexp)
  (yank)
  (backward-sexp)
  (set-mark (point))
  (emacs-lisp-macroexpand)
  (call-interactively 'eval-last-sexp)
  (kill-region (mark) (point))
  (yank 2))


(defun find-expand-properly-eval-unexpand ()
  (interactive)
  (when (find-macro-usage)
    (expand-eval-unexpand)
    t))


(defun cb-init-macro-expansion-workaround ()
  "Work around 224ce846c0827618e85fd253a4c996895dfd6439."
  (interactive)
  (save-excursion
    (cb-visit-init)
    (beginning-of-buffer)
    (while (find-expand-properly-eval-unexpand)
      t)
    (revert-buffer t t)))


(defun cb--setup-custom ()
  "Configures custom."
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file t))


(defun cb-visit-init ()
  "Opens personal init.el"
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))


(defun cb-ensure-dir-exists (dirpath)
  "Returns DIRPATH while ensuring the corresponding directory exists."
  (make-directory dirpath :parents)
  dirpath)


(defun cb--setup-use-package ()
  "Configures use-package."
  (straight-use-package 'use-package))


(defun cb-alist-unique (alist)
  "Create a new alist with the associations that have no duplicate cars."
  (let ((keys (cl-remove-duplicates (mapcar #'car alist))))
    (mapcar (lambda (key)
              (assoc key alist))
            keys)))


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
  ;; the modeline is the same as the default + winum + vc branch name are truncated + no
  ;; minor modes + purpose
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (format winum-format
                           (winum-get-number-string)))
                  mode-line-front-space
                  (:propertize
                   (""
                    mode-line-mule-info
                    mode-line-client
                    mode-line-modified
                    mode-line-remote)
                   display
                   (min-width
                    (5.0)))
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  "   "
                  mode-line-position
                  (:eval
                   (when vc-mode
                     (truncate-string-to-width vc-mode 25 nil nil "...")))
                  (:eval (purpose--modeline-string))
                  mode-line-misc-info mode-line-end-spaces
                  ))
  (use-package mini-echo
    :config (mini-echo-mode)))


(defun cb--minimize-frame ()
  "Minimizes the initial and startup frames."
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))


(defun cb--setup-line-numbers ()
  "Configures line numbers in windows."
  (column-number-mode)
  (global-display-line-numbers-mode t)
  (setq display-line-numbers-type 'relative
        display-line-numbers-width-start t
        )

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


(defun cb-remove-advice-from-functions (funcs)
  (dolist (func funcs)
    (advice-mapc (lambda (advice _props) (advice-remove func advice)) func)))


(defun cb-window-management ()
  "Manage windows."
  (use-package ace-window
    :bind ("C-x w" . ace-window))

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
	  persp-mode-prefix-key (kbd "C-x l")
          persp-modestring-short 't
          )
    (persp-mode)
    (when (file-exists-p persp-state-default-file)
      (persp-state-load persp-state-default-file))
    (persp-switch "main")
    :bind
    (("C-x b" . persp-switch-to-buffer*)
     ("C-x k" . persp-kill-buffer*)
     :map perspective-map
     ("l" . persp-switch-last))
    :config
    ;; `:hook' with `kill-emacs' fails on missing mode called `perspective'
    (add-hook 'kill-emacs-hook #'persp-state-save)))


(defun cb-workspace-management ()
  "Manage windows, buffers, layouts, etc."
  (cb-window-management)
  (cb-layout-management)
  )


(defun cb-setup-font ()
  "Setups fonts."
  ;; TODO: consider configuring modus-themes-* fonts
  (let ((mono-font "CodeNewRoman Nerd Font Mono"))
    (set-face-attribute 'default nil :font mono-font :height 110)
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

  (use-package which-key
    :config
    (which-key-mode 1))
  )


(defun cb-org ()
  "Loads and configures org-mode and related packages."
  (use-package org
    :demand t
    :config
    (setq org-log-done 'time
	  org-todo-keywords
	  '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)"))
	  org-agenda-span 2
          ;; start the agenda view from the current day, to always see the next day
          org-agenda-start-day "."
	  org-agenda-start-on-weekday 0
	  org-edit-src-content-indentation 0
          ;; orj-journal: solves https://github.com/bastibe/org-journal/issues/406
	  org-element-use-cache nil
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
          org-global-properties
          '(("Effort_ALL" . "0 0:05 0:10 0:15 0:30 0:45 1:00 1:30 2:00 2:30 3:00 4:00"))
          org-columns-default-format
          "%40ITEM(Task) %17Effort(Estimated Effort){:} %CLOCKSUM"
          org-agenda-skip-deadline-if-done t
          org-agenda-log-mode-items '(clock)
          org-agenda-start-with-log-mode t
          )
    ;; see https://emacs.stackexchange.com/a/76352
    (with-eval-after-load 'org-ctags (setq org-open-link-functions nil))
    :hook (org-mode . auto-fill-mode)
    :straight (:type built-in)
    :bind (
           ("C-c a" . org-agenda)
           ("C-c c" . org-capture)  ; TODO: seems like it may be used in some keymap - check
           ("C-c s" . org-store-link)
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

  (use-package org-roam
    :init
    (setq org-roam-directory (cb-ensure-dir-exists
                              (expand-file-name "roam" org-directory))
          ;; added empty line compared to the default
          org-roam-capture-templates
          '(("d" "default" plain "%?" :target
             (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
             :unnarrowed t
             :empty-lines-before 1)
            )
          org-roam-dailies-directory "journal/"
          org-roam-dailies-capture-templates
          '(("d" "default" entry "* %(format-time-string \"%R\" (current-time))\n%?"
             :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
             ;; I want to see the context when capturing
             :unnarrowed t
             :empty-lines-before 1))
          )
    (let ((dailies-full-path (expand-file-name
                              org-roam-dailies-directory
                              org-roam-directory)))
      (cb-ensure-dir-exists dailies-full-path)
      (add-to-list 'org-agenda-files dailies-full-path))
    :bind (
           ("C-c n l" . #'org-roam-buffer-toggle)
           ("C-c n i" . #'org-roam-node-insert)
           :map org-roam-dailies-map
           ("d" . nil)
           ("n" . nil)
           ("j" . #'org-roam-dailies-goto-today)
           ("c" . #'org-roam-dailies-capture-today)
           )
    :bind-keymap
    ("C-c n j" . org-roam-dailies-map)
    :config
    (org-roam-db-autosync-enable)
    )

  (use-package emacs
    :init
    (setq cb-org-roam-note-types '(
                                   Fleeting
                                   Literature
                                   Permanent
                                   )
          cb-org-roam-note-type-property-name "ROAM_NOTE_TYPE"
          )
    (cb-make-note-type-menu cb-org-roam-typed-node-find '(("s" . All)))

    :preface
    (defun cb-org-roam-note-type-p (note-type node-repr-type)
      "Creates a filter function by note type suitable for `org-roam-node-find'.

If NOTE-TYPE is one of `cb-org-roam-note-types', returns a function that filters by the
type, otherwise returns nil.

The primary use of the function is to create a filter function for functions like
`org-roam-node-find', and this is achieved with NODE-REPR-TYPE set to `org-roam-node'.
Otherwise, it can also be used to filter data not necessarily of type `org-roam-node',
e.g. query results from `org-roam-db-query', but then it's assumed that node properties
can be retrieved with (nth 6 node). This argument is subject to change (e.g. pass the
getter instead of the type)."
      (when (seq-contains-p cb-org-roam-note-types note-type)
          (cl-flet ((cb-properties-getter (if (eq node-repr-type 'org-roam-node)
                                              #'org-roam-node-properties
                                            #'(lambda (item) (nth 6 item)))))
            (lambda (item) (string= (cdr (assoc-string
                                          cb-org-roam-note-type-property-name
                                          (cb-properties-getter item)))
                                    note-type)))))

    (defun cb-generate-org-roam-typed-template (note-type)
      (when (seq-contains-p cb-org-roam-note-types note-type)
        `(("not used as we provide a single template" "''" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                      ,(format ":PROPERTIES:
:%s: %s
:END:
#+title: ${title}\n" cb-org-roam-note-type-property-name note-type))
           :unnarrowed t
           :empty-lines-before 1))))

    (defun cb-org-roam-typed-node-find (note-type)
      (org-roam-node-find
       nil
       ""
       (cb-org-roam-note-type-p note-type 'org-roam-node)
       nil
       :templates
       (cb-generate-org-roam-typed-template note-type)))

    (defmacro cb-make-note-type-menu (fn
                                      &optional
                                      custom-mapping
                                      desc
                                      note-type-desc-mapping
                                      name)
      "Create an interactive interface for a single-argument function FN.

The argument of FN is assumed to be a note type. The interface is created by assigning
a key binding to each note type, as per `cb-org-roam-note-type-key-mapping'. You can
provide an additional mapping in the same format via CUSTOM-MAPPING. If it has common
keys with `cb-org-roam-note-type-key-mapping', the custom ones take precedence.

DESC is a string that is used in the interface to describe the action and/or the note
type choices. Default: \"Note types\n\".

NAME is the name of the resulting interactive command. Default: the name of FN
with the \"-menu\" suffix.
"
      `(transient-define-prefix ,(or name
                                     (make-symbol (format "%s-menu" (symbol-name fn))))
         ()
         [,(or desc "Note types\n")
          [
           ,@(mapcar (pcase-lambda (`(,key . ,note-type))
                       `(,key
                         ,(or (assoc note-type note-type-desc-mapping) note-type)
                         (lambda ()
                           (interactive)
                           (,fn ',note-type))))
                     (cb-alist-unique (append (eval custom-mapping)
                                              cb-org-roam-note-type-key-mapping)))
           ]]))

    :bind (
           ("C-c n s" . cb-org-roam-typed-node-find-menu)
           )
    )

  (use-package org-roam-ui
    :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

  (use-package emacs
    :preface
    (defun citation-mentioned-by-any-of-ids-p (node-ids cite)
      (seq-contains-p node-ids (car cite)))

    (defun cb-org-roam-ui--init-filter-advices ()
      "Define advices to filter nodes in org-roam-ui by a note type.

This is done instead of advicing immediately to be able to also remove the advices (and
only the ones that are defined here) on demand.

The flow of interest for filtering appears to be in `org-roam-ui--send-graphdata', as
the whole graph is sent on each update. It would be nice if the payload creation and
sending were separated (because then we could just advice a single function), but that's
not the case, so we have to advice the functions in between.

For each note type we associate an alist of functions-to-be-adviced and their advices. We
may need to advice more functions than we do now for proper behavior, e.g.
`org-roam-ui--get-links'."
      (setq cb-org-roam-ui--filter-advices
            (seq-map (lambda (note-type)
                       `(,note-type
                         .
                         (
                          (org-roam-ui--get-nodes
                           . ,(lambda (res)
                                (seq-filter (cb-org-roam-note-type-p note-type nil)
                                            res)))
                          ;; the order of the advices matter
                          (org-roam-ui--filter-citations
                           . ,(lambda (res)
                                (let ((node-ids (seq-map (lambda (node)
                                                           (car node))
                                                         (org-roam-ui--get-nodes))))
                                  (seq-filter (lambda (cite)
                                                (citation-mentioned-by-any-of-ids-p
                                                 node-ids
                                                 cite))
                                              res)))))))
                     cb-org-roam-note-types)))

    (defun cb--org-roam-ui-filter (note-type)
      (pcase-dolist (`(,function . ,advice)
                     (cdr (assoc note-type cb-org-roam-ui--filter-advices)))
        (advice-add function :filter-return advice)))

    (defun cb-org-roam-ui-unfilter ()
      (interactive)
      (dolist (note-type cb-org-roam-note-types)
        (pcase-dolist (`(,function . ,advice)
                       (cdr (assoc note-type cb-org-roam-ui--filter-advices)))
          (advice-remove function advice))))

    (defun cb-org-roam-ui-filter (note-type)
      (if (eq note-type 'Unfilter)
          (cb-org-roam-ui-unfilter)
        (cb--org-roam-ui-filter note-type)))

    :init
    (cb-org-roam-ui--init-filter-advices)
    (cb-make-note-type-menu
        cb-org-roam-ui-filter
        '(("u" . Unfilter))
        "Filter actions\n"
        '(("u" . "Remove all filters")))

    :bind (("C-c n f" . #'cb-org-roam-ui-filter-menu)))

  (use-package ebib
    :config
    (add-to-list 'ebib-preload-bib-files
                 (expand-file-name cb-bibliography-name org-roam-directory))
    (setq ebib-bibtex-dialect 'biblatex)
    )

  (use-package org-ref
    :config
    (add-to-list 'bibtex-completion-bibliography
                 (expand-file-name cb-bibliography-name org-roam-directory))
    (setq bibtex-dialect 'biblatex)
    )

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

  (use-package org-transclusion
    :after org)

  (use-package emacs
    ;; Typed notes and transclusion integrations
    :preface
    (defun cb-org-set-note-type (note-type)
      (let* ((id (save-excursion
                   ;; copy-pasted from `org-roam-extract-subtree'
                   (org-back-to-heading-or-point-min t)
                   (when (bobp) (user-error "Already a top-level node"))
                   (org-id-get-create)))
             (note-type (if (stringp note-type)
                            (intern note-type)
                          note-type)))
        (when (seq-contains-p cb-org-roam-note-types note-type)
          (org-set-property cb-org-roam-note-type-property-name
                            (symbol-name note-type))
          id)))

    (defun cb-org-roam-get-node-title-by-id (org-id)
      (let* ((id org-id)
             (nodes (seq-filter (lambda (node) (string= (org-roam-node-id node) id))
                                (org-roam-node-list))))
        (when nodes (org-roam-node-title (car nodes)))))

    (defun cb-org-transclusion-insert-from-id (org-id &optional level exclude-title heading)
      (when level
        (unless (numberp level)
          (error "Invalid org level: %s" level)))
      (let ((title (cb-org-roam-get-node-title-by-id org-id))
            (level (if heading (1+ level) level)))
        (unless title
          (error "Node wasn't found: %s" org-id))
        (open-line 1)
        (when heading
          (insert heading)
          (newline)
          (open-line 1))
        (insert (format "#+transclude:%s%s"
                        (if level
                            (format " :level %d" level)
                          "")
                        (if exclude-title
                            " :exclude-elements \"keyword\""
                          "")))
        (org-insert-link nil (format "id:%s" org-id) title)
        (org-transclusion-add)))

    (defun cb--read-file-name-non-interactive (prompt
                                               &optional
                                               dir
                                               default-filename
                                               mustmatch
                                               initial
                                               predicate)
      default-filename)

    (defmacro cb-with-override (orig-fn temp-fn &rest body)
      (declare (indent 2))
      `(progn
         (advice-add ,orig-fn :override ,temp-fn)
         ;; TODO: handle errors in body (advice-remove isn't called right now in this
         ;; case)
         (let ((res (progn ,@body)))
           (advice-remove ,orig-fn ,temp-fn)
           res)))

    (defun cb-get-heading ()
      "Return the current heading.

Unlike `org-get-heading', include the stars."
      (save-excursion
        (org-back-to-heading)
        (let ((beg (point)))
          (outline-end-of-heading)
          (buffer-substring-no-properties beg (point)))))

    (defun cb-org-roam-factor-out-typed-note (note-type)
      "Creates a separate typed note of a heading, and transcludes it in-place."
      (let ((id (cb-org-set-note-type note-type))
            (level (org-current-level))
            (heading (cb-get-heading)))
        (cb-with-override 'read-file-name 'cb--read-file-name-non-interactive
          (org-roam-extract-subtree))
        (cb-org-transclusion-insert-from-id id level t heading)))

    :init
    (cb-make-note-type-menu cb-org-roam-factor-out-typed-note))

  (defun org-schedule-effort ()
    "Taken from https://commonplace.doubleloop.net/calculating-effort-estimates-and-scheduled-times-in-org-mode"
    (interactive)
    (save-excursion
      (org-back-to-heading t)
      (let* (
          (element (org-element-at-point))
          (effort (org-element-property :EFFORT element))
          (scheduled (org-element-property :scheduled element))
          (ts-year-start (org-element-property :year-start scheduled))
          (ts-month-start (org-element-property :month-start scheduled))
          (ts-day-start (org-element-property :day-start scheduled))
          (ts-hour-start (org-element-property :hour-start scheduled))
          (ts-minute-start (org-element-property :minute-start scheduled)) )
        (org-schedule nil (concat
          (format "%s" ts-year-start)
          "-"
          (if (< ts-month-start 10)
            (concat "0" (format "%s" ts-month-start))
            (format "%s" ts-month-start))
          "-"
          (if (< ts-day-start 10)
            (concat "0" (format "%s" ts-day-start))
            (format "%s" ts-day-start))
          " "
          (if (< ts-hour-start 10)
            (concat "0" (format "%s" ts-hour-start))
            (format "%s" ts-hour-start))
          ":"
          (if (< ts-minute-start 10)
            (concat "0" (format "%s" ts-minute-start))
            (format "%s" ts-minute-start))
          "+"
          effort)))))

  ;; allow for emphasis to spread on more than one line (20 to be precise)
  (progn
    (setcar (nthcdr 4 org-emphasis-regexp-components) 20)
    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)))


(defun cb-global-bindings ()
  "Binds commands that aren't part of a package."
  ;; TODO: implement Creme Brulee transient
  (global-set-key (kbd "C-c b i") 'cb-visit-init)
  (global-set-key (kbd "M-z") 'zap-up-to-char)
  )


(defun cb-add-entry-to-path (entry)
  "Adds ENTRY to the PATH envvar and to `exec-path'."
  (let ((path-env-name "PATH"))
    (setenv path-env-name (concat entry path-separator (getenv path-env-name)))
    (add-to-list 'exec-path entry)))


(defun cb-setup-path ()
  (dolist (entry (list (file-name-concat (getenv "HOME") ".local" "bin")
                       (file-name-concat (getenv "HOME") ".dotnet" "tools")  ;; for csharp-ls
                       ))
    (cb-add-entry-to-path entry))
)


(defun cb-misc ()
  "Miscellaneous stuff."
  (cb--setup-custom)
  (cb-setup-path)
  )


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
	     python-ts-mode  ;; requires a python language server (e.g. pyright)
             tsx-ts-mode  ;; requires typescript-language-server
             go-ts-mode ;; requires gopls
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
                 (c-sharp)
		 (css)
		 (dart :org "ast-grep")
		 (dockerfile :org "camdencheek")
		 (elisp :org "Wilfred")
		 (elixir :org "elixir-lang")
		 (glsl :org "theHamsta")
		 (go :org "camdencheek")
                 (gomod :org "camdencheek" :repo "tree-sitter-go-mod")
		 (heex :org "phoenixframework")
		 ;; (janet :org "sogaiu" :repo "tree-sitter-janet-simple")
		 (janet :org "GrayJack")
		 (javascript :sourcedir "src")
		 (make :org "alemuller")
		 (markdown :org "ikatyang")
                 (nix :org "nix-community")
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
		       (typescript-mode . tsx-ts-mode)
                       (csharp-mode . csharp-ts-mode)
                       ))
      (add-to-list 'major-mode-remap-alist mapping))

    :config
    (cb-setup-install-grammars))

  (use-package combobulate
    :straight (combobulate :type git
                           :host github
                           :repo "mickeynp/combobulate"
                           :nonrecursive t)
    :hook ((python-ts-mode
            js-ts-mode
            css-ts-mode
            yaml-ts-mode
            typescript-ts-mode
            tsx-ts-mode)
           . combobulate-mode))

  ;; Not using `:mode' above because yaml config is not quite `treesit' config,
  ;; and using it defers `cb-setup-install-grammars'
  ;; TODO: consider using `use-package' `:demand' or `:init'
  (dolist (mode-assoc '(("\\.ya?ml\\'" . yaml-ts-mode)
                        ("\\.?[Dd]ockerfile" . dockerfile-ts-mode)))
    (add-to-list 'auto-mode-alist mode-assoc)))


(defun cb-setup-shell-and-terminal ()
  "Setups shell(s) and terminal emulator(s)."
  (use-package vterm)
  (dolist (mode '(eshell-mode
                  term-mode
                  vterm-mode))
    (add-to-list 'cb-no-line-number-modes mode))
  (add-hook 'term-exec-hook (lambda ()
			      (set-buffer-process-coding-system 'utf-8-unix
								'utf-8-unix)))
  (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color"))))


(defun cb-read-lines (file-path)
  "Returns a list of lines of a file at FILE-PATH if it exists, nil otherwise."
  (when (file-exists-p file-path)
    (with-temp-buffer
      (info-insert-file-contents file-path)
      (split-string (buffer-string) "\n" t))))


(defun cb-project-root-or-nil (project)
  "Like project-root, but don't fail if PROJECT is nil."
  (when project
    (project-root project))
  )


(defun cb-expand-project-dotenv ()
  "Sets environment variables of the current project."
  (interactive)
  (let* ((dotenv-path (expand-file-name ".env"
                                        (cb-project-root-or-nil (project-current))))
	 (dotenv (cb-read-lines dotenv-path)))
    (dolist (line dotenv)
      (apply 'setenv (split-string line "=" t)))
    ))


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
	  python-shell-dedicated 'project
          toml-ts-mode-indent-offset 4
          )
    ;; :before doesn't work for some reason
    (advice-add #'run-python :around #'(lambda (orig-fun &rest args)
					 (cb-expand-project-dotenv)
					 (apply orig-fun args)))
    :hook
    (inferior-python-mode . (lambda () (setq tab-width 4))))

  (use-package pyvenv
    :config
    (setq pyvenv-mode-line-indicator nil)
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


(defun cb-setup-go ()
  "Setups go."
  (use-package emacs
    :mode (("\\.go\\'" . go-ts-mode)
           ("\\go.mod\\'" . go-mod-ts-mode)
           )
    :config
    (setq go-ts-mode-indent-offset 4)
    :hook
    (go-ts-mode . (lambda () (setq tab-width 4)))
    )
  )


(defun cb-setup-nix ()
  "Setup nix."
  (use-package nix-ts-mode
    :mode "\\.nix\\'"))


(defun cb-setup-web ()
  "Setups all web-related stuff."
  (use-package emacs
    :mode (("\\.[jt]sx?\\'" . tsx-ts-mode)
           ("\\.json\\'" . tsx-ts-mode))
    :config
    (setq css-indent-offset 2
          sgml-basic-offset 2
          )
    :hook
    ((css-mode html-mode tsx-ts-mode) . (lambda () (setq fill-column 80)))
    )

  (use-package k8s-mode
    )

  (use-package web-mode
    :mode (("\\.razor\\'" . web-mode)))
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
    :config
    (setq rg-keymap-prefix (kbd  "C-c r"))
    (rg-enable-menu)
    )
  )


(defun cb-diff ()
  "Setup diff."
  (use-package emacs
    :after ediff
    :config
    (setq ediff-window-setup-function #'ediff-setup-windows-plain
          ;; it means 'vertically' on Emacs' language :)
          ediff-split-window-function #'split-window-horizontally)))


(defun cb-dev ()
  "Development stuff."
  (cb-setup-git)
  (cb--setup-tree-sitter)
  (cb-setup-shell-and-terminal)
  (cb-markup)
  (cb-setup-lisp)
  (cb-setup-lsp)
  (cb-setup-python)
  (cb-setup-go)
  (cb-setup-nix)
  (cb-setup-web)
  ;; (cb-setup-debug)
  (cb-setup-comint)
  (cb-setup-search)
  (cb-diff))


(defun cb-reading ()
  "Reading stuff."
  (use-package nov
    ;; `:mode' actually defers package configuration, and then `:config'
    ;; will run only after you open an epub file, so we have to use `:init'
    :init
    (add-to-list 'cb-no-line-number-modes 'nov-mode)
    :config
    (setq nov-text-width 150)
    :mode ("\\.epub\\'" . nov-mode))

  (use-package olivetti
    :init
    (setq olivetti-body-width 150)
    :hook
    (nov-mode . olivetti-mode)
    )
  )


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
(cb-init-macro-expansion-workaround)
