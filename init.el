;; helpers
;; https://git.sr.ht/~technomancy/better-defaults/tree/main/item/better-defaults.el
;; https://emacs-config-generator.fly.dev/
;; https://emacs.amodernist.com/
;; https://www.patrickdelliott.com/emacs.d/

(setq frame-title-format
      '((:eval(format "emacs-%d.%d@%s:%s" emacs-major-version emacs-minor-version system-type
                      (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))))

(setq user-full-name       "JJ"
      user-real-login-name "JJ"
      user-login-name      "jj"
      user-mail-address    "temp@jnrj.test")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package exec-path-from-shell
  :ensure t
  :init
  ;; sync exec-path-from-fgshell
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Unless we've already fetched (and cached) the package archives,
;; refresh them.
(unless package-archive-contents
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Add extra context to Emacs documentation to help make it easier to
;; search and understand. This configuration uses the keybindings
;; recommended by the package author.
;; (use-package helpful
;;   :ensure t
;;   :bind (("C-h f" . #'helpful-callable)
;;          ("C-h v" . #'helpful-variable)
;;          ("C-h k" . #'helpful-key)
;;          ("C-c C-d" . #'helpful-at-point)
;;          ("C-h F" . #'helpful-function)
;;          ("C-h C" . #'helpful-command)))

(use-package windmove
  :bind (("C-M-<up>" . windmove-up)
         ("C-M-<left>" . windmove-left)
         ("C-M-<down>" .  windmove-down)
         ("C-M-<right>" . windmove-right)))

(use-package display-line-numbers
  :hook ((prog-mode . display-line-numbers-mode)
         (text-mode . display-line-numbers-mode)
         (conf-mode . display-line-numbers-mode)))

(use-package elec-pair
  :hook
  (after-init . electric-pair-mode)
  (minibuffer-setup . (lambda () (electric-pair-local-mode 0))))

(use-package
  modus-vivendi
  :defer t
  :custom
  (modus-themes-region '(bg-only))
  (modus-themes-org-blocks 'gray-background))

;; disable all themes before loading
(mapcar #'disable-theme custom-enabled-themes)
;; enable theme based on the time
;; theme-changer.el and cicardian.el are alternatives
(let ((current-hour (string-to-number (format-time-string "%H" (current-time)))))
  (if (and (> current-hour 8) (< current-hour 18))
      (modus-themes-select 'modus-operandi)
    (modus-themes-select 'modus-operandi)))
(run-at-time "05:00" nil (lambda () (modus-themes-select 'modus-operandi)))
(run-at-time "18:00" nil (lambda () (modus-themes-select 'modus-vivendi)))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c m c l") 'mc/mark-more-like-this-extended)
  (global-set-key (kbd "C-c m c a") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-c m c e") 'mc/edit-ends-of-lines)
  ;; (global-set-key (kbd "M-.") 'mc/mark-pop)
  ;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  ;; (global-set-key (kbd "C-S-w C-S-w") 'mc/mark-all-dwim)
  ;; (global-set-key (kbd "C-S-e C-S-e") 'mc/edit-ends-of-lines)
  )

(use-package emacs
  :bind (("C-o" . newline-and-indent))
  :init
  (setq enable-recursive-minibuffers  t)
  (setq backup-by-copying t)
  (setq sentence-end-double-space nil)
  (setq show-trailing-whitespace t)
  (setq visible-bell t)
  :config
  (global-auto-revert-mode 1)
  (global-subword-mode 1)
  (make-directory "backup/" t)
  (make-directory "auto-save/" t)
  (save-place-mode 1)
  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "M-z") 'zap-up-to-char)
  (global-set-key (kbd "C-S-d") 'duplicate-dwim)
  (global-set-key (kbd "C-x t <right>") 'tab-next)
  (global-set-key (kbd "C-x t <left>") 'tab-previous)
  :hook ((prog-mode . display-fill-column-indicator-mode)
         (text-mode . display-fill-column-indicator-mode)
         (conf-mode . display-fill-column-indicator-mode))
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (use-short-answers t)
  (window-combination-resize t) ; resize windows proportionally?
  (load-prefer-newer t)
  (indent-tabs-mode nil) ; no tabs
  (global-hl-line-mode t) ; highlight current line
  (truncate-lines t) ; prevent line wrap
  (use-short-answers t) ; y or n instead of yes or no
  (delete-by-moving-to-trash t) ; self explanatory
  (sentence-end-double-space nil) ;
  (apropos-do-all t) ; idk
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (require-final-newline t)
  ;; (custom-file (expand-file-name "custom.el" user-emacs-directory))
  ;; backup files
  (backup-directory-alist `(("." . ,(concat user-emacs-directory "backup/"))))
  (version-control t)
  (kept-new-versions 2)
  (kept-old-versions 2)
  (delete-old-versions t)
  (auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-save/") t)))
  ;; (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))) ;; consider using /tmp
  (kill-buffer-delete-auto-save-files t)
  (lock-file-name-transforms '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))
  (indicate-empty-lines t)
  (x-stretch-cursor t)
  (save-interprogram-paste-before-kill nil)
  ;;(select-enable-primary nil)
  (global-auto-revert-mode t)
  (global-auto-revert-non-file-buffers t)
  ;; (auto-revert-verbose nil)
  (whitespace-trailing-regexp "\\([	  ]+$\\|[^ 
]  +[^ 
]\\|^[

]+$\\)")
  (require-final-newline t)
  (js-indent-level 2)
  :custom-face
  (default((t ( :family "DejaVu Sans Mono"
                :foundry "PfEd"
                :slant normal
                :weight normal
                :height 120
                :width normal)))))

(use-package windsize
  :ensure t
  :config
  (windsize-default-keybindings))

(use-package isearch
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil)
  (search-whitespace-regexp ".*?"))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-projects-backend 'project-el)
  (dashboard-items
   '((recents . 10)
     (bookmarks . 10))))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-<up>") 'org-previous-visible-heading)
    (define-key org-mode-map (kbd "C-c C-<down>") 'org-next-visible-heading)
    (define-key org-mode-map (kbd "C-<tab>") nil)
    (define-key org-mode-map (kbd "C-S-<up>") nil)
    (define-key org-mode-map (kbd "C-S-<right>") nil)
    (define-key org-mode-map (kbd "C-S-<down>") nil)
    (define-key org-mode-map (kbd "C-S-<left>") nil))
  (with-eval-after-load 'org-agenda (define-key org-agenda-mode-map (kbd "C-t") 'org-agenda-todo-yesterday))
  ;; Org-mode src-block js fix
  (with-eval-after-load 'org
    (setq org-babel-js-function-wrapper
          "process.stdout.write(require('util').inspect(function(){\n%s\n}(), { maxArrayLength: null, maxStringLength: null, breakLength: Infinity, compact: true }))"))
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (verb . t)))
  (setq org-log-into-drawer t
        org-agenda-files '("~/Documents/habits.org"))
  (add-to-list 'org-modules 'org-habit)
  ;; :custom
  ;; (org-babel-load-languages '((verb . t)))
  )

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode)

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-quit-no-match t)
  :hook
  ((prog-mode . corfu-mode)))

(use-package dired
  :custom
  (dired-listing-switches "-Falh --group-directories-first")
  (dired-dwim-target t))

(use-package diminish :ensure t)

(use-package magit
  :ensure t
  :config
  (with-eval-after-load 'magit
    (define-key magit-hunk-section-map (kbd "C-o")
                'magit-diff-visit-file-other-window)
    (define-key magit-file-section-map (kbd "C-o")
                'magit-diff-visit-file-other-window)

    (magit-add-section-hook
     'magit-status-sections-hook
     'magit-insert-tracked-files
     nil
     'append))
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (with-eval-after-load 'magit-mode
    (add-hook 'after-save-hook 'magit-after-save-refresh-status t))
  )

(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  :config
  (diff-hl-flydiff-mode t))

(use-package recentf
  :config
  ;; (setq recentf-auto-cleanup 'never) ;; prevent issues with Tramp
  (setq recentf-max-saved-items 100)
  (setq recentf-max-menu-items 15)
  (run-at-time nil 600 'recentf-save-list)
  (recentf-mode t))

(use-package savehist
  :init
  (savehist-mode))

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-ignore-case t)
  (completion-styles '(flex partial-completion substring basic))
  (vertico-multiform-categories
   '((symbol (vertico-sort-function . vertico-sort-alpha))
     (file (vertico-sort-function . sort-directories-first)
           (+vertico-transform-functions . +vertico-highlight-directory))))
  (vertico-multiform-commands
   '((consult-line (vertico-sort-override-function . vertico-sort-alpha))
     (execute-extended-command
      (+vertico-transform-functions . +vertico-highlight-enabled-mode))))
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  (defvar +vertico-transform-functions nil)
  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
    (dolist (fun (ensure-list +vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))

  ;; function to highlight directories
  (defun +vertico-highlight-directory (file)
    "If FILE ends with a slash, highlight it as a directory."
    (if (string-suffix-p "/" file)
        (propertize file 'face 'marginalia-file-priv-dir) ; or face 'dired-directory
      file))

  ;; function to sort directories first
  (defun sort-directories-first (files)
    ;; Still sort by history position, length and alphabetically
    (setq files (vertico-sort-history-length-alpha files))
    ;; But then move directories first
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  ;; function to highlight enabled modes similar to counsel-M-x
  (defun +vertico-highlight-enabled-mode (cmd)
    "If MODE is enabled, highlight it as font-lock-constant-face."
    (let ((sym (intern cmd)))
      (if (or (eq sym major-mode)
              (and
               (memq sym minor-mode-list)
               (boundp sym)))
          (propertize cmd 'face 'font-lock-constant-face)
        cmd))))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :ensure t
  :demand t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :init
  ;; (setq consult-preview-excluded-files '("\\.pdf\\'" "\\.png\\'" "\\.jpg\\'"))
  :config
  (global-set-key (kbd "C-x b") 'consult-buffer)
  (global-set-key "\C-cy" 'consult-yank-from-kill-ring)
  )

;; TODO: embark-act on all if more than 1 selected instead of using embark-act-all?
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq embark-quit-after-action '((kill-buffer . nil)))
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package keycast :ensure t)

(use-package frameshot
  :ensure t
  :init
  (frameshot-setup
   '((name . "emacs")
     (output . "~/Downloads/"))))

(use-package ledger-mode
  :ensure t
  :custom
  (ledger-reports
   '(("budget" "%(binary) -f %(ledger-file) --budget --monthly reg expenses")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
  )

;; treesitter stuff
;; (use-package treesit-auto
;;   :ensure t
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

(use-package treesit
      :mode (("\\.tsx\\'" . tsx-ts-mode)
             ("\\.js\\'"  . typescript-ts-mode)
             ("\\.mjs\\'" . typescript-ts-mode)
             ("\\.mts\\'" . typescript-ts-mode)
             ("\\.cjs\\'" . typescript-ts-mode)
             ("\\.ts\\'"  . typescript-ts-mode)
             ("\\.jsx\\'" . tsx-ts-mode)
             ("\\.json\\'" .  json-ts-mode)
             ("\\.Dockerfile\\'" . dockerfile-ts-mode)
             ("\\.prisma\\'" . prisma-ts-mode)
             ("\\.php\\'" . php-ts-mode)
             ;; More modes defined here...
             )
      :preface
      (defun os/setup-install-grammars ()
        "Install Tree-sitter grammars if they are absent."
        (interactive)
        (dolist (grammar
                 '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                   (bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.20.0" )
                   (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                   (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
                   (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                   (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                   (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
                   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                   (make "https://github.com/alemuller/tree-sitter-make")
                   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                   (cmake "https://github.com/uyha/tree-sitter-cmake")
                   (c "https://github.com/tree-sitter/tree-sitter-c" "v0.20.0")
                   (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                   (toml "https://github.com/tree-sitter/tree-sitter-toml")
                   (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                   (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                   (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
                   (prisma "https://github.com/victorhqc/tree-sitter-prisma" "v1.4.0")
                   (php "https://github.com/tree-sitter/tree-sitter-php" "v0.23.12")))
          (add-to-list 'treesit-language-source-alist grammar)
          ;; Only install `grammar' if we don't already have it
          ;; installed. However, if you want to *update* a grammar then
          ;; this obviously prevents that from happening.
          (unless (treesit-language-available-p (car grammar))
            (treesit-install-language-grammar (car grammar)))))

      ;; Optional, but recommended. Tree-sitter enabled major modes are
      ;; distinct from their ordinary counterparts.
      ;;
      ;; You can remap major modes with `major-mode-remap-alist'. Note
      ;; that this does *not* extend to hooks! Make sure you migrate them
      ;; also
      (dolist (mapping
               '((python-mode . python-ts-mode)
                 (css-mode . css-ts-mode)
                 (typescript-mode . typescript-ts-mode)
                 (js-mode . typescript-ts-mode)
                 (js2-mode . typescript-ts-mode)
                 (c-mode . c-ts-mode)
                 (c++-mode . c++-ts-mode)
                 (c-or-c++-mode . c-or-c++-ts-mode)
                 (bash-mode . bash-ts-mode)
                 (css-mode . css-ts-mode)
                 (json-mode . json-ts-mode)
                 (js-json-mode . json-ts-mode)
                 (sh-mode . bash-ts-mode)
                 (sh-base-mode . bash-ts-mode)
                 (php-mode . php-ts-mode)))
        (add-to-list 'major-mode-remap-alist mapping))
      :config
      (os/setup-install-grammars))

;; (use-package apheleia
;;   :ensure apheleia
;;   :diminish ""
;;   :defines
;;   apheleia-formatters
;;   apheleia-mode-alist
;;   :functions
;;   apheleia-global-mode
;;   :config
;;   (setf (alist-get 'prettier-json apheleia-formatters)
;;         '("prettier" "--stdin-filepath" filepath))
;;   (apheleia-global-mode +1))

(use-package plantuml-mode
  :ensure t
  :init
  (with-eval-after-load 'plantuml-mode
    (defun hex-encode (str)
      (string-join (mapcar (lambda (c) (format "%02x" c)) (string-as-unibyte str))))

    (defun plantuml-server-encode-url (string)
      "Encode the string STRING into a URL suitable for PlantUML server interactions."
      (let* ((encoded-string (hex-encode string)))
        (concat plantuml-server-url "/" plantuml-output-type "/~h" encoded-string)))))

(use-package json-mode :ensure t)
(use-package csv-mode :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package apib-mode :ensure t)
(use-package feature-mode :ensure t)
(use-package markdown-mode
  :ensure t
  ;; These extra modes help clean up the Markdown editing experience.
  ;; `visual-line-mode' turns on word wrap and helps editing commands
  ;; work with paragraphs of text. `flyspell-mode' turns on an
  ;; automatic spell checker.
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . flyspell-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package yasnippet
  :ensure t
  :hook ((php-mode . yas-minor-mode)
         (php-ts-mode . yas-minor-mode))
  :config
  (with-eval-after-load 'yasnippet
    (yas-load-directory "~/.emacs.d/snippets")))

(use-package php-mode :ensure t)

(use-package web-mode
  :ensure t
  ;; :mode (("\\.ts\\'" . web-mode)
  ;;        ("\\.js\\'" . web-mode)
  ;;        ("\\.mjs\\'" . web-mode)
  ;;        ("\\.tsx\\'" . web-mode)
  ;;        ("\\.jsx\\'" . web-mode))
  :custom
  (web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-quoting nil))

(use-package eglot
  :ensure t
  :hook ((php-mode . eglot-ensure)
         (php-ts-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (web-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '((php-mode :language-id "php") . ("intelephense" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((python-mode) . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((tsx-ts-mode) . ("typescript-language-server" "--stdio"))))

(use-package all-the-icons :ensure t :demand t)

(use-package all-the-icons-dired
  :ensure t
  :demand t
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-completion
  :ensure t
  :demand t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode)
  :config
  ;; https://github.com/minad/marginalia/issues/175
  (advice-add #'completion-metadata-get :around #'all-the-icons-completion-completion-metadata-get)
  (advice-add (compat-function completion-metadata-get) :around #'all-the-icons-completion-completion-metadata-get))

;; (use-package ws-butler
;;   :hook ((text-mode . ws-butler-mode)
;;          (prog-mode . ws-butler-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3199be8536de4a8300eaf9ce6d864a35aa802088c0925e944e2b74a574c68fd0"
     "7dc296b80df1b29bfc4062d1a66ee91efb462d6a7a934955e94e786394d80b71"
     default))
 '(package-selected-packages nil)
 '(safe-local-variable-values
   '((org-duration-format . h:mm)
     (eval setq org-confirm-babel-evaluate nil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; wgrep
(use-package wgrep
  :ensure t
  ;; :custom
  ;; (wgrep-enable-key "e")
  ;; (wgrep-auto-save-buffer t)
  ;; (wgrep-change-readonly-file t)
  )

;; TODO: replace current response/header buffers when sending the request
;; TODO: prevent Headers window from opening if run within org source block (preferably would show as a message or smth)
(use-package verb
  :ensure t
  :custom
  (verb-auto-show-headers-buffer 'always)
  (verb-auto-kill-response-buffers t))

(use-package jinx
  :ensure t
  :config
  (global-set-key (kbd "C-M-$") #'jinx-correct)
  :custom
  (jinx-languages "en_US pt_BR"))

;; split window and move cursor
(defun my/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun my/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'my/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'my/split-window-right-and-switch)

;; Timestamp my messages buffer
(defun my/add-timestamp-message (FORMAT-STRING &rest args)
  "Advice to run before `message' that prepends a timestamp to each message.
        Activate this advice with:
          (advice-add 'message :before 'my/add-timestamp-message)
        Deactivate this advice with:
          (advice-remove 'message 'my/add-timestamp-message)"
  (if message-log-max
      (let ((deactivate-mark nil)
            (inhibit-read-only t))
        (with-current-buffer "*Messages*"
          (goto-char (point-max))
          (if (not (bolp))
              (newline))
          (insert (format-time-string "[%F %T.%3N %Z] "))))))
(advice-add 'message :before 'my/add-timestamp-message)
(advice-add 'warn :before 'my/add-timestamp-message)

;; ledger-macros //move to another file?
(fset 'my/ledger/btg/entry-from-pdf-text
   (kmacro-lambda-form [C-right ?\M-t ?\M-t ?\C-a C-right ?\M-t C-right ?  ?* ?  ?\C-s ?\M-r ?  ?\\ ?\{ ?1 ?3 ?\\ ?\} return ?\C-  ?\C-s ?- left ?\C-w ?\C-r ?* return right ?  ?\C-y ?\M-\\ return ?A ?s ?s ?e ?t ?s ?: ?B ?a ?n ?k ?s ?: ?B ?T ?G ?  ?  ?\C-  ?\C-s ?- left ?\C-w ?\C-e return ?\; ?  ?\C-y return ?E ?x ?p ?e ?n ?s ?e ?s ?: ?M ?i ?s ?c return ?\M-\\] 0 "%d"))
