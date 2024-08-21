;; https://git.sr.ht/~technomancy/better-defaults/tree/main/item/better-defaults.el
;; add melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;; Ensure use-package is installed
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
;;

;; Frame/window title
(setq frame-title-format
      '((:eval(format "emacs-%d.%d@%s:%s" emacs-major-version emacs-minor-version system-type
                      (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))))

;; Mode-line customization,,,,

;; Move between window
;; maybe check about General
(global-set-key (kbd "C-M-<up>") 'windmove-up)
(global-set-key (kbd "C-M-<left>") 'windmove-left)
(global-set-key (kbd "C-M-<down>") 'windmove-down)
(global-set-key (kbd "C-M-<right>") 'windmove-right)

;; Performance tweaks for modern machines
(setq gc-cons-threshold 100000000) ; 100 mb
(setq read-process-output-max (* 1024 1024)) ; 1mb412

;; Blink when at the top or bottom of the buffer
(setq visible-bell t)

;; Remove extra UI clutter by hiding the scrollbar, menubar, and toolbar.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode +1)

;; Enable line numbers for some modes
;; (dolist (mode '(text-mode-hook
;;                 prog-mode-hook
;;                 conf-mode-hook))
;;   (add-hook mode (lambda ()
;;                    (display-line-numbers-mode 1)
;;                    (display-fill-column-indicator-mode))))

;; Show line numbers
(use-package display-line-numbers
  :hook ((prog-mode . display-line-numbers-mode)
         (text-mode . display-line-numbers-mode)
         (conf-mode . display-line-numbers-mode)))


;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package elec-pair
  :hook
  (after-init . electric-pair-mode)
  (minibuffer-setup . (lambda () (electric-pair-local-mode 0))))

;; load theme
;; https://protesilaos.com/codelog/2022-11-30-standard-themes-emacs/
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
      (load-theme 'modus-operandi)
    (load-theme 'modus-vivendi)))

(run-at-time "08:00" nil (lambda () (modus-themes-load-operandi)))
(run-at-time "18:00" nil (lambda () (modus-themes-load-vivendi)))

;; https://github.com/dandavison/magit-delta syntax highlighting

;; Add parts of each file's directory to the buffer name if not unique
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

;; multiple cursors
(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

;; sync exec-path-from-fgshell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Setting Custom font and size
(use-package emacs
  :config
  (global-auto-revert-mode 1)
  (global-subword-mode 1)
  (make-directory "backup/" t)
  (make-directory "auto-save/" t)
  (save-place-mode 1)
  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "M-z") 'zap-up-to-char)
  ;; Remove text in active region if inserting text
  ;; (delete-selection-mode 1)
  :hook ((prog-mode . display-fill-column-indicator-mode)
         (text-mode . display-fill-column-indicator-mode)
         (conf-mode . display-fill-column-indicator-mode))
  :custom
  (window-combination-resize t) ; resize windows proportionally?
  (frame-inhibit-implied-resize t) ;; move to early init
  ;; (load-prefer-newer t)
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
  ;; auto save files
  (auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-save/") t)))
  ;; (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))) ;; consider using /tmp
  (kill-buffer-delete-auto-save-files t)
  ;; lock files
  (lock-file-name-transforms '(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t)))
  (indicate-empty-lines t)
  (x-stretch-cursor t)
  ;; (save-interprogram-paste-before-kill nil)
  ;; Don't automatically copy selected text
  ;;(select-enable-primary nil)
  (global-auto-revert-non-file-buffers t)
  ;; (auto-revert-verbose nil)
  :custom-face
  (default((t ( :family "DejaVu Sans Mono"
                :foundry "PfEd"
                :slant normal
                :weight normal
                :height 120
                :width normal)))))

;; isearch
(use-package isearch
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil)
  (search-whitespace-regexp ".*?"))

;; dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-projects-backend 'project-el)
  (dashboard-items
   '((recents . 10)
     (bookmarks . 10))))

;; org-mode
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (verb . t)
     ))
  (setq org-log-into-drawer t
        org-agenda-files '("~/Documents/habits.org"))
  (add-to-list 'org-modules 'org-habit)
  ;; :custom
  ;; (org-babel-load-languages '((verb . t)))
  )


;; requires melpa
;; (use-package dired-subtree)

;;(setq enable-recursive-minibuffers  t)
;;(minibuffer-depth-indicate-mode 1)
;; Finally, there is C-] (abort-recursive-edit) to get out of such a recursive minibuffer.

;; Revert Dired and other buffers
;; (setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
;; (global-auto-revert-mode 1)


;; Help me remember which key to press next
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode ;; whats is this? probably uses diminish package to hide from modeline
  :config
  ;; (setq which-key-idle-delay 0.3)
  )

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  ;; (corfu-popupinfo-mode)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  )


(use-package dired
  :custom
  (dired-listing-switches "-Falh --group-directories-first")
  (dired-dwim-target t))

;; hide modes i think?
(use-package diminish
  :ensure t)

(use-package magit
  :ensure t)

;; recent files
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

;; TODO: quick close a buffer with C-k https://www.reddit.com/r/emacs/comments/16g08me/killbuffer_from_the_minibuffer_after_mx/
(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
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

;; TODO: show git project name in annotation when switching buffers
(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package consult :ensure t)

;; TODO: embark-act on all if more than 1 selected instead of using embark-act-all?
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
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

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package keycast :ensure t)

;; (use-package all-the-icons
;;   :if (display-graphic-p))

;; (use-package all-the-icons-completion
;;   :after (marginalia all-the-icons)
;;   :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
;;   :init
;;   (all-the-icons-completion-mode))

;; (use-package ws-butler
;;   :hook ((text-mode . ws-butler-mode)
;;          (prog-mode . ws-butler-mode)))

;; (use-package origami
;;   :hook (yaml-mode . origami-mode))

;; (use-package savehist
;;   :init
;;   (savehist-mode))

;; eglot
;; (use-package eglot
;;   :config
;;   (add-to-list 'eglot-server-programs '(python-mode . ("pylsp"))))

;;(add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))

;; (use-package python
;;   :interpreter ("python3" . python-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(verb wgrep embark-consult embark corfu consult dashboard eglot php-mode exec-path-from-shell lv marginalia vertico magit orderless)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; split window and move cursor
(defun jj/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun jj/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

;; (defun split-window-right-and-focus ()
;;   "Spawn a new window right of the current one and focus it."
;;   (interactive)
;;   (split-window-right)
;;   (windmove-right))

;; (defun split-window-below-and-focus ()
;;   "Spawn a new window below the current one and focus it."
;;   (interactive)
;;   (split-window-below)
;;   (windmove-down))


(global-set-key (kbd "C-x 2") 'jj/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'jj/split-window-right-and-switch)

;; wgrep
(use-package wgrep
  :ensure t
  ;; :custom
  ;; (wgrep-enable-key "e")
  ;; (wgrep-auto-save-buffer t)
  ;; (wgrep-change-readonly-file t)
  )

;; veeeerb
;; TODO: replace current response/header buffers when sending the request
;; TODO: prevent Headers window from opening if run within org source block (preferably would show as a message or smth)
(use-package verb
  :ensure t
  :custom
  (verb-auto-show-headers-buffer 'always)
  (verb-auto-kill-response-buffers t))

;; my custom functions

;; languages
;; php
;;(use-package php-mode)
;; web
;; python

;; no-littering
;; TODO: prevent emacs from leaving/creating files

;; (keymap-set minibuffer-local-map "C-k" "C-. k y")

;; (defun my-embark-M-k (&optional arg)
;;   (interactive "P")
;;   (require 'embark)
;;   (if-let ((targets (embark--targets)))
;;       (let* ((target
;;               (or (nth
;;                   (if (or (null arg) (minibufferp))
;;                       0
;;                     (mod (prefix-numeric-value arg) (length targets)))
;;                   targets)))
;;             (type (plist-get target :type)))
;;         (cond
;;          ((eq type 'buffer)
;;           (let ((embark-pre-action-hooks))
;;             (embark--act 'kill-buffer target)))))))

;; (define-key minibuffer-local-map (kbd "M-k") 'my-embark-M-k)

