(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; since melpa version is breaking
(add-to-list 'package-pinned-packages
             '(modus-themes . "3.0.0"))

;; Defaults
;; Start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; (global-set-key "\C-cy" '(lambda ()
;;                            (interactive)
;;                            (popup-menu 'yank-menu)))

(global-set-key "\C-cy" 'counsel-kill-ring)

(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))

(global-set-key "\M-Y" 'yank-pop-forwards)

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(column-number-mode +1)

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; how to autoload this?
(require 'modeline-region)
(global-modeline-region-mode 1)

(autoload 'hide/show-comments-toggle "hide-comnt" "Hide/Show Comments" t)

;; (setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)
;; (setq indent-line-function 'insert-tab)

;; to add & configure
;; phpactor, dragstuff, zoom-all-windows

;; LSP
(require 'lsp-mode) ;; avoid this require?
(add-hook 'php-mode-hook #'lsp)
;; override phpactor config and set it as an add-on
(with-eval-after-load 'lsp-php
  (add-to-list 'lsp-language-id-configuration
               '(php-mode . "php"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
                   (lambda ()
                     (unless lsp-php-composer-dir
                       (setq lsp-php-composer-dir (lsp-php-get-composer-dir)))
                     (unless lsp-phpactor-path
                       (setq lsp-phpactor-path (or (executable-find "phpactor")
                                                   (f-join lsp-php-composer-dir "vendor/phpactor/phpactor/bin/phpactor"))))
                     (list lsp-phpactor-path "language-server")))
                    :major-modes '(php-mode)
                    :activation-fn (lsp-activate-on "php")
                    :add-on? t
                    :initialization-options (ht)
                    :server-id 'phpactor)))

(add-hook 'web-mode-hook #'lsp)
(add-hook 'css-mode-hook #'lsp)
;;(add-hook 'prog-mode-hook #'lsp)

;(require 'lsp-pyright)
;(add-hook 'python-mode-hook #'lsp) ; or lsp-deferred

;; Enable html-ls for twig files
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(".*\\.twig$" . "html")))

;; Enable html-ls for jinja files
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(".*\\.jinja$" . "html")))

;; Enable php ls (iph) for .php files any mode
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(".*\\.php$" . "php")))

;; Windsize & Windmove
(windsize-default-keybindings)
(global-set-key (kbd "C-M-<up>") 'windmove-up)
(global-set-key (kbd "C-M-<left>") 'windmove-left)
(global-set-key (kbd "C-M-<down>") 'windmove-down)
(global-set-key (kbd "C-M-<right>") 'windmove-right)

;; Experimental
(global-set-key (kbd "C-M-p") 'windmove-up)
(global-set-key (kbd "C-M-b") 'windmove-left)
(global-set-key (kbd "C-M-n") 'windmove-down)
(global-set-key (kbd "C-M-f") 'windmove-right)

;; Alternative stuff
;; (global-set-key (kbd "C-x M-<up>") #'windmove-up)
;; (global-set-key (kbd "C-x M-<down>") #'windmove-down)
;; (global-set-key (kbd "C-x M-<left>") #'windmove-left)
;; (global-set-key (kbd "C-x M-<right>") #'windmove-right)

;; Ivy/Swiper/Counsel
(with-eval-after-load 'ivy
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key (kbd "C-S-s") 'swiper-isearch)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (define-key ivy-minibuffer-map (kbd "M-<tab>") 'ivy-toggle-fuzzy))

;; (advice-add 'ivy-toggle-fuzzy :around
;;             (lambda (orig-fun)
;;               (let ((l (funcall orig-fun)))
;;                        (message "%s" ivy--regex-function))))

(defun ivy-curr-reb ()
  (format "[%s] "
          (or
           (cdr
            (assoc ivy--regex-function ivy-preferred-re-builders))
           "fuzzy")))

;; Swiper-isearch with moving between lines with arrow keys
(defun swiper-isearch-next-line ()
  (interactive)
  (let ((shift 1))
    (with-ivy-window
      (let ((ln (line-number-at-pos (ivy-state-current ivy-last))))
        (while (and (< (+ ivy--index shift) ivy--length)
                    (= ln (line-number-at-pos (nth (+ ivy--index shift) ivy--all-candidates))))
          (cl-incf shift))))
    (ivy-next-line shift)))

(defun swiper-isearch-prev-line ()
  (interactive)
  (let ((shift 1))
    (with-ivy-window
      (let ((ln (line-number-at-pos (ivy-state-current ivy-last))))
        (while (and (>= (- ivy--index shift) 0)
                    (= ln (line-number-at-pos (nth (- ivy--index shift) ivy--all-candidates))))
          (cl-incf shift))))
    (ivy-previous-line shift)))

(with-eval-after-load 'swiper
  (define-key swiper-isearch-map (kbd "<down>") #'swiper-isearch-next-line)
  (define-key swiper-isearch-map (kbd "<up>") #'swiper-isearch-prev-line)
  (define-key swiper-isearch-map (kbd "TAB") #'swiper-isearch-next-line)
  (define-key swiper-isearch-map (kbd "S-TAB") #'swiper-isearch-prev-line)
  (define-key swiper-isearch-map (kbd "S-SPC") nil))


;; Projectile
(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


;; Custom Shortcuts
;; Multline
(global-set-key (kbd "C-c C-d") 'mc/mark-next-like-this-word)
(global-set-key (kbd "C->") 'mc/mark-next-word-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-.") 'mc/mark-pop)

;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C-S-w C-S-w") 'mc/mark-all-dwim)
;; (global-set-key (kbd "C-S-e C-S-e") 'mc/edit-ends-of-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

;; Undo Tree
;; (defun undo-tree-split-side-by-side (original-function &rest args)
;;   "Split undo-tree side-by-side"
;;   (let ((split-height-threshold nil)
;;         (split-width-threshold 0))
;;     (apply original-function args)))

;; (advice-add 'undo-tree-visualize :around #'undo-tree-split-side-by-side)

;; Auto-Completion
(eval-after-load 'company
  '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))

;; Hooks
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(with-eval-after-load 'magit-mode
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'electric-pair-local-mode)
(add-hook 'prog-mode-hook 'yas-minor-mode)
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'org-mode-hook 'yas-minor-mode)

(with-eval-after-load 'yasnippet
  (yas-load-directory "~/.emacs.d/snippets"))

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


;; (add-hook 'flyspell-mode-hook #'flyspell-buffer)


;; Disable pairing simple quote in elisp-mode
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local electric-pair-pairs
                        (eval
                         (car
                          (get 'electric-pair-pairs 'standard-value))))
            (setq-local electric-pair-text-pairs
                        (eval
                         (car
                          (get 'electric-pair-pairs 'standard-value))))))

(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'after-init-hook (lambda () (message (concat "Startup time: " (emacs-init-time)))))

;; Set filter groups in ibuffer
(add-hook 'ibuffer-hook (lambda () (ibuffer-projectile-set-filter-groups)))

;; Mode & File Associations
;; Software Development
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts[x]?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jinja\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.sql\\'" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.restclient\\'" . restclient-mode))
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode))

;; Other
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Config/Data Files
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.env\\'" . conf-mode))


;; Things to probably move to other files
;; Enable mode-line-buffer-id states
(progn
  (defun my-buffer-identification (fmt)
    (list (propertize fmt
                      'face (if (let ((window (selected-window)))
                     (or (eq window (old-selected-window))
                         (and (minibuffer-window-active-p (minibuffer-window))
                              (with-selected-window (minibuffer-window)
                                (eq window (minibuffer-selected-window))))))
                                'mode-line-buffer-id-highlight
                              'mode-line-buffer-id)
                      'mouse-face 'mode-line-highlight
                      'local-map mode-line-buffer-identification-keymap)))
  (setq-default mode-line-buffer-identification
                '(:eval (my-buffer-identification " %12b "))))

;; Eval and Replace
;; From magnars
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


;; Regex Builder & Replace Bridge
;; https://karthinks.com/software/bridging-islands-in-emacs-1/
(defvar my/re-builder-positions nil
  "Store point and region bounds before calling re-builder")
(advice-add 're-builder
            :before
            (defun my/re-builder-save-state (&rest _)
              "Save into `my/re-builder-positions' the point and region
positions before calling `re-builder'."
              (setq my/re-builder-positions
                    (cons (point)
                          (when (region-active-p)
                            (list (region-beginning)
                                  (region-end)))))))
(defun reb-replace-regexp (&optional delimited)
  "Run `query-replace-regexp' with the contents of re-builder. With
non-nil optional argument DELIMITED, only replace matches
surrounded by word boundaries."
  (interactive "P")
  (reb-update-regexp)
  (let* ((re (reb-target-binding reb-regexp))
         (replacement (query-replace-read-to
                       re
                       (concat "Query replace"
                               (if current-prefix-arg
                                   (if (eq current-prefix-arg '-) " backward" " word")
                                 "")
                               " regexp"
                               (if (with-selected-window reb-target-window
                                     (region-active-p)) " in region" ""))
                       t))
         (pnt (car my/re-builder-positions))
         (beg (cadr my/re-builder-positions))
         (end (caddr my/re-builder-positions)))
    (with-selected-window reb-target-window
      (goto-char pnt) ; replace with (goto-char (match-beginning 0)) if you want
                                        ; to control where in the buffer the replacement starts
                                        ; with re-builder
      (setq my/re-builder-positions nil)
      (reb-quit)
      (query-replace-regexp re replacement delimited beg end))))


(with-eval-after-load 're-builder
  (define-key reb-mode-map (kbd "RET") #'reb-replace-regexp)
  (define-key reb-lisp-mode-map (kbd "RET") #'reb-replace-regexp))
(global-set-key (kbd "C-M-%") #'re-builder)

;; Custom Faces
(defface mode-line-buffer-id-highlight
 '((t (:background "white" :foreground "black" :weight normal)))
  "Face for mode-line-buffer-id buffer active"
  :group 'basic-faces )

;; Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline success warning error])
 '(ansi-color-names-vector
   ["gray35" "#ff8059" "#44bc44" "#d0bc00" "#2fafff" "#feacd0" "#00d3d0" "gray65"])
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/backup/" t)))
 '(awesome-tray-mode-line-active-color "#2fafff")
 '(awesome-tray-mode-line-inactive-color "#323232")
 '(backup-directory-alist '(("." . "~/.emacs.d/backup/")))
 '(company-dabbrev-downcase t)
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes '(modus-vivendi))
 '(custom-safe-themes
   '("3860a842e0bf585df9e5785e06d600a86e8b605e5cc0b74320dfe667bcbe816c" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "74a50f18c8c88eac44dc73d7a4c0bbe1f3e72ff5971aac38fcf354ddad0d4733" "aa72e5b41780bfff2ff55d0cc6fcd4b42153386088a4025fed606c1099c2d9b8" default))
 '(diff-hl-flydiff-mode t)
 '(dired-listing-switches "-alh")
 '(dired-mode-hook '(auto-revert-mode diff-hl-dired-mode))
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(electric-pair-pairs '((34 . 34) (8216 . 8217) (8220 . 8221) (39 . 39)))
 '(electric-pair-text-pairs '((34 . 34) (8216 . 8217) (8220 . 8221) (39 . 39)))
 '(erc-server "irc.libera.chat")
 '(erc-timestamp-format "[%H:%M:%S]")
 '(erc-timestamp-format-right " [%H:%M:%S]")
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 '(explicit-shell-file-name "/bin/bash")
 '(exwm-floating-border-color "#646464")
 '(fci-rule-color "#5B6268")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-themes-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-themes-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-themes-fringe-yellow))
 '(global-diff-hl-mode t)
 '(global-hl-line-mode t)
 '(global-undo-tree-mode t)
 '(global-whitespace-mode nil)
 '(highlight-tail-colors
   ((("#101909" "#A8FF60" "green")
     . 0)
    (("#131319" "#C6C5FE" "brightcyan")
     . 20)))
 '(hl-todo-keyword-faces
   '(("HOLD" . "#c0c530")
     ("TODO" . "#feacd0")
     ("NEXT" . "#b6a0ff")
     ("THEM" . "#f78fe7")
     ("PROG" . "#00d3d0")
     ("OKAY" . "#4ae2f0")
     ("DONT" . "#70b900")
     ("FAIL" . "#ff8059")
     ("BUG" . "#ff8059")
     ("DONE" . "#44bc44")
     ("NOTE" . "#d3b55f")
     ("KLUDGE" . "#d0bc00")
     ("HACK" . "#d0bc00")
     ("TEMP" . "#ffcccc")
     ("FIXME" . "#ff9077")
     ("XXX+" . "#ef8b50")
     ("REVIEW" . "#6ae4b9")
     ("DEPRECATED" . "#bfd9ff")))
 '(ibuffer-deletion-face 'modus-themes-mark-del)
 '(ibuffer-filter-group-name-face 'modus-themes-pseudo-header)
 '(ibuffer-marked-face 'modus-themes-mark-sel)
 '(ibuffer-title-face 'default)
 '(indent-guide-char "│")
 '(indent-guide-recursive t)
 '(isearch-lazy-count t)
 '(ispell-skip-html t)
 '(ivy-mode t)
 '(ivy-pre-prompt-function 'ivy-curr-reb)
 '(ivy-preferred-re-builders
   '((ivy--regex-plus . "regex")
     (ivy--regex-ignore-order . "order")
     (ivy--regex-fuzzy . "fuzzy")))
 '(ivy-read-action-format-function 'ivy-read-action-format-columns)
 '(ivy-rich-mode t)
 '(ivy-use-selectable-prompt t)
 '(ivy-virtual-abbreviate 'abbreviate)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#96CBFE"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#A8FF60"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(keycast-mode-line-format "%1s%k%c%r")
 '(keycast-mode-line-remove-tail-elements nil)
 '(ledger-reports
   '(("budget" "%(binary) -f %(ledger-file) --budget --monthly reg expenses")
     ("bal" "%(binary) -f %(ledger-file) bal")
     ("reg" "%(binary) -f %(ledger-file) reg")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(lsp-disabled-clients '(eslint))
 '(lsp-file-watch-threshold 10000)
 '(magit-uniquify-buffer-names nil)
 '(mini-modeline-face-attr '(:background unspecified))
 '(minimap-hide-fringes t)
 '(minimap-mode t)
 '(mlr-non-rectangle-style 'lines+words+chars)
 '(mode-line-format
   '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
     (vc-mode
      (:eval
       (format "%s[%s]" vc-mode
               (projectile-project-name))))
     "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))
 '(objed-cursor-color "#ff6c60")
 '(org-babel-load-languages '((emacs-lisp . t) (python . t) (js . t) (restclient . t)))
 '(org-fontify-quote-and-verse-blocks t)
 '(org-habit-graph-column 60)
 '(org-log-into-drawer t)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))
 '(org-src-block-faces 'nil)
 '(org-src-lang-modes
   '(("php" . php)
     ("C" . c)
     ("C++" . c++)
     ("asymptote" . asy)
     ("bash" . sh)
     ("beamer" . latex)
     ("calc" . fundamental)
     ("cpp" . c++)
     ("ditaa" . artist)
     ("dot" . fundamental)
     ("elisp" . emacs-lisp)
     ("ocaml" . tuareg)
     ("screen" . shell-script)
     ("shell" . sh)
     ("sqlite" . sql)
     ("plantuml" . plantuml)))
 '(org-todo-keyword-faces
   '(("IN-REVIEW" . org-macro)
     ("IN-PROGRESS" . org-sexp-date)))
 '(package-selected-packages
   '(yaml-mode lsp-treemacs writeroom-mode ob-restclient apib-mode restclient ox-gfm counsel-jq flycheck-plantuml plantuml-mode csv-mode feature-mode dockerfile-mode yasnippet ob-php git-link ledger-mode php-mode ivy-rich ibuffer-projectile highlight-indent-guides rainbow-delimiters lsp-pyright rg frameshot path-headerline-mode gif-screencast company-quickhelp keycast modus-themes tramp flycheck lsp-ui diff-hl multiple-cursors idle-highlight-mode company projectile counsel ivy web-mode windsize which-key uniquify-files undo-tree transpose-frame smex magit emmet-mode crux))
 '(pdf-view-midnight-colors '("#ffffff" . "#100f10"))
 '(php-mode-coding-style 'symfony2)
 '(projectile-globally-ignored-directories
   '("^\\.idea$" "^\\.vscode$" "^\\.ensime_cache$" "^\\.eunit$" "^\\.git$" "^\\.hg$" "^\\.fslckout$" "^_FOSSIL_$" "^\\.bzr$" "^_darcs$" "^\\.pijul$" "^\\.tox$" "^\\.svn$" "^\\.stack-work$" "^\\.ccls-cache$" "^\\.cache$" "^\\.clangd$" ".expo/web/cache/.*" "^\\.log" "^node_modules$"))
 '(projectile-mode t nil (projectile))
 '(reb-re-syntax 'string)
 '(recentf-max-saved-items 100)
 '(recentf-mode t)
 '(rg-command-line-flags '("-C 5"))
 '(rustic-ansi-faces
   ["#000000" "#ff6c60" "#A8FF60" "#FFFFB6" "#96CBFE" "#FF73FD" "#C6C5FE" "#f6f3e8"])
 '(show-paren-mode t)
 '(show-paren-when-point-in-periphery t)
 '(show-paren-when-point-inside-paren t)
 '(size-indication-mode t)
 '(split-width-threshold 140)
 '(sql-mysql-login-params '(user password server database port))
 '(sql-port 3306)
 '(tab-bar-close-button-show nil)
 '(tab-bar-mode t)
 '(tab-bar-new-button-show nil)
 '(tab-bar-separator "​" t)
 '(tab-bar-tab-name-function 'tab-bar-tab-name-current-with-count)
 '(text-mode-hook '(turn-on-flyspell text-mode-hook-identify))
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(undo-limit 800000000)
 '(undo-outer-limit 240000000)
 '(undo-strong-limit 2400000)
 '(undo-tree-enable-undo-in-region nil)
 '(undo-tree-history-directory-alist '(("." . "~/.emacs.d/backup")))
 '(undo-tree-visualizer-diff t)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#ff8059")
     (40 . "#feacd0")
     (60 . "#f78fe7")
     (80 . "#ef8b50")
     (100 . "#d0bc00")
     (120 . "#c0c530")
     (140 . "#f8dec0")
     (160 . "#bfebe0")
     (180 . "#44bc44")
     (200 . "#70b900")
     (220 . "#6ae4b9")
     (240 . "#4ae2f0")
     (260 . "#00d3d0")
     (280 . "#c6eaff")
     (300 . "#2fafff")
     (320 . "#79a8ff")
     (340 . "#00bcff")
     (360 . "#b6a0ff")))
 '(vc-annotate-very-old-color nil)
 '(vc-make-backup-files t)
 '(visible-bell t)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-enable-auto-pairing nil)
 '(web-mode-enable-current-element-highlight t)
 '(web-mode-enable-sql-detection t)
 '(web-mode-script-padding 4)
 '(web-mode-style-padding 4)
 '(which-key-mode t)
 '(whitespace-style
   '(face trailing tabs spaces newline empty indentation space-after-tab space-before-tab space-mark tab-mark))
 '(whitespace-trailing-regexp "\\([	  ]+$\\|[^ 
]  +[^ 
]\\|^[

]+$\\)")
 '(widget-link-prefix "[")
 '(widget-link-suffix "]")
 '(widget-mouse-face '(highlight widget-button))
 '(widget-push-button-prefix "[")
 '(widget-push-button-suffix "]")
 '(xterm-color-names
   ["black" "#ff8059" "#44bc44" "#d0bc00" "#2fafff" "#feacd0" "#00d3d0" "gray65"])
 '(xterm-color-names-bright
   ["gray35" "#ef8b50" "#70b900" "#c0c530" "#79a8ff" "#f78fe7" "#4ae2f0" "white"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(keycast-command ((t (:inherit bold :foreground "red"))))
 '(keycast-key ((t (:background "red" :foreground "white" :box (:line-width 1 :color "dark gray")))))
 '(mode-line ((t (:background "#000000" :foreground "#ffffff"))))
 '(mode-line-buffer-id ((t (:inherit bold :background "gray10" :foreground "dim gray" :weight normal))))
 '(mode-line-buffer-id-highlight ((t (:background "white" :foreground "black" :weight normal))))
 '(mode-line-highlight ((t (:background "white" :foreground "black" :box nil))))
 '(mode-line-id-inactive ((t (:background "gray10" :foreground "dim gray"))))
 '(mode-line-inactive ((t (:background "black" :foreground "dim gray" :box (:line-width 1 :color "gray40")))))
 '(tab-bar ((t (:inherit variable-pitch :background "black" :foreground "white"))))
 '(tab-bar-tab ((t (:inherit modus-themes-tab-active :background "white" :foreground "black" :box (:line-width 2 :color "white")))))
 '(tab-bar-tab-inactive ((t (:inherit modus-themes-tab-inactive :foreground "light gray" :box (:line-width 2 :color "gray25"))))))

;; Frame/window title
(setq frame-title-format
      '((:eval (format "emacs-%d.%d@%s:%s" emacs-major-version emacs-minor-version system-type
                       (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))))

;; Frameshot
(with-eval-after-load 'frameshot
  (frameshot-setup
   '((name . "emacs")
     (output . "~/Downloads/"))))


;; Tab bar
;; Remove nth element of a list
;; (defun remove-nth-element (nth list)
;;   (if (zerop nth) (cdr list)
;;     (let ((last (nthcdr (1- nth) list)))
;;       (setcdr last (cddr last))
;;       list)))

;; Remove first tab separator
;; (advice-add 'tab-bar-make-keymap-1 :around
;;             (lambda (orig-fun)
;;               (remove-nth-element 2 (funcall orig-fun))))


;; Remove first tab separator (alternative)
(advice-add 'tab-bar-make-keymap-1 :around
            (lambda (orig-fun)
                     (let ((l (funcall orig-fun)))
                       (delq (setcar (nthcdr 2 l) (cons nil nil)) l))))


;; Custom keybinds
(global-set-key (kbd "C-S-d") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-x t <right>") 'tab-next)
(global-set-key (kbd "C-x t <left>") 'tab-previous)

;; Aliases
(defalias 'counsel-kill-ring 'counsel-yank-pop)

;; Experimental stuff
;; Highlight parens when inside
(define-advice show-paren-function (:around (fn) fix)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
        (t (save-excursion
             (ignore-errors (backward-up-list))
             (funcall fn)))))


;; Custom keybindings
;;(define-key company-mode-map (kbd "<tab>") 'company-complete)

;; Disable suspend-frame keybind
(global-unset-key (kbd "C-z"))

;; Automatically wrap isearch
;; Prevents issue where you have to press backspace twice when
;; trying to remove the first character that fails a search
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

;; Prevent spell checking tags/attributes
(defun web-mode-flyspell-verify ()
  (let ((f (get-text-property (- (point) 1) 'face))
        rlt)
    (cond
     ((not (memq f '(web-mode-html-attr-value-face
                     web-mode-html-tag-face
                     web-mode-html-attr-name-face
                     web-mode-constant-face
                     web-mode-doctype-face
                     web-mode-keyword-face
                     web-mode-comment-face ;; focus on get html label right
                     web-mode-function-name-face
                     web-mode-variable-name-face
                     web-mode-css-property-name-face
                     web-mode-css-selector-face
                     web-mode-css-color-face
                     web-mode-type-face
                     web-mode-block-control-face
                     )
                 ))
      (setq rlt t))
     ((memq f '(web-mode-html-attr-value-face))
      (save-excursion
        (search-backward-regexp "=['\"]" (line-beginning-position) t)
        (backward-char)
        (setq rlt (string= (thing-at-point 'word) "value"))
        ))
     (t t))
    rlt
    ))

(put 'web-mode 'flyspell-mode-predicate 'web-mode-flyspell-verify)

;; (defun set-exec-path-from-shell-PATH ()
;;   "Set up Emacs' `exec-path' and PATH environment variable to match
;; that used by the user's shell.

;; This is particularly useful under Mac OS X and macOS, where GUI
;; apps are not started from a shell."
;;   (interactive)
;;   (let ((path-from-shell (replace-regexp-in-string
;; 			  "[ \t\n]*$" "" (shell-command-to-string
;; 					  "$SHELL --login -c 'echo $PATH'"
;; 						    ))))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))))

;; (set-exec-path-from-shell-PATH)

;; Org-mode src-block js fix
(with-eval-after-load 'org
  (setq org-babel-js-function-wrapper
      "process.stdout.write(require('util').inspect(function(){\n%s\n}(), { maxArrayLength: null, maxStringLength: null, breakLength: Infinity, compact: true }))"))

;; Disable Ctrl+tab and C-S-<arrow> org-mode
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-<tab>") nil)
  (define-key org-mode-map (kbd "C-S-<up>") nil)
  (define-key org-mode-map (kbd "C-S-<right>") nil)
  (define-key org-mode-map (kbd "C-S-<down>") nil)
  (define-key org-mode-map (kbd "C-S-<left>") nil))

;; Disable Ctrl+tab magit-status, process
(with-eval-after-load 'magit-status
  (define-key magit-status-mode-map (kbd "C-<tab>") nil))

(with-eval-after-load 'magit-process
  (define-key magit-process-mode-map (kbd "C-<tab>") nil))

;; Org-mode custom keybindings
(with-eval-after-load 'org-agenda (define-key org-agenda-mode-map (kbd "C-t") 'org-agenda-todo-yesterday))

;; PlantUML server fix (https://github.com/skuro/plantuml-mode/issues/146#issuecomment-1045289734)
(with-eval-after-load 'plantuml-mode
  (defun hex-encode (str)
    (string-join (mapcar (lambda (c) (format "%02x" c)) (string-as-unibyte str))))

  (defun plantuml-server-encode-url (string)
    "Encode the string STRING into a URL suitable for PlantUML server interactions."
    (let* ((encoded-string (hex-encode string)))
      (concat plantuml-server-url "/" plantuml-output-type "/~h" encoded-string))))
;; utilities
(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
    Prefixed with negative \\[universal-argument], sorts in reverse.

    The variable `sort-fold-case' determines whether alphabetic case
    affects the sort order.

    See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))

(defun sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
    See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

(defun execute-last-line-as-shell-command (&optional arg)
  "Excute last previous non-empty line as a shell command. Prefix to insert the output to the current buffer."
  (interactive "P")
  (shell-command
   (save-excursion
     (goto-char (line-beginning-position))
     (beginning-of-line)
     (while
         (and
          (not (bobp))
          (string-blank-p
           (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
       (forward-line -1))
     (buffer-substring-no-properties (line-beginning-position) (line-end-position))) (when arg (current-buffer))))

(defun execute-region-as-shell-command (beg end &optional arg)
  "Execute selected region as a shell command. Prefix to insert the output to the current buffer."
  (interactive "r\nP")
  (shell-command (buffer-substring beg end)
                 (when arg (current-buffer))))

(defun increase-default-face-height ()
  "Increase default face height."
  (interactive)
  (set-face-attribute 'default nil :height (+ 20 (face-attribute 'default :height))))

(defun decrease-default-face-height ()
  "Decrease the default face height."
  (interactive)
  (set-face-attribute 'default nil :height (- (face-attribute 'default :height) 20)))

(define-minor-mode sharing-screen-mode
  "Sharing screen minor mode."
  :lighter " ss"
  :global
  (if sharing-screen-mode
      (funcall 'increase-default-face-height)
    (funcall 'decrease-default-face-height)))

(defun scratch ()
  "Create and switch to a temporary scratch buffer."
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*scratch*"))
  (org-mode))

(defun bash ()
  "Run ansi-term with bash shell."
  (interactive)
  (ansi-term "/bin/bash" default-directory)
  (add-hook 'after-change-functions
            (lambda (_ _ _) (rename-buffer (format "*term: %s*" default-directory) t)) nil t))

;; (defun rename-buffer-to-default-directory ()
;;   (add-hook 'after-change-functions
;;             (lambda (_ _ _) (rename-buffer (format "*term: %s*" default-directory) t)) nil t))

;; (add-hook 'term-mode-hook 'rename-buffer-to-default-directory)

(defun cleanup-whitespace-git-modified-files ()
  "Cleanup whitespaces and empty lines of git modified files."
  (interactive)
  (mapcar (lambda (file)
            (with-current-buffer (find-file file)
              (save-excursion
                (goto-char (point-min))
                (replace-regexp "^\n$" "" nil (point-min) (point-max))
                (delete-trailing-whitespace)
                (goto-char (point-min))
                (delete-blank-lines)
                (save-buffer))))
          (magit-modified-files)))

(defun generate-uuid (&optional arg)
  "Generates a uuid using the uuidgen tool."
  (interactive "P")
  (shell-command "uuidgen" (when arg t)))

(defun json-stringify-region ()
  (interactive)
  (if (region-active-p)
  (shell-command-on-region
   (region-beginning) (region-end)
   (format "node -e 'console.log(JSON.stringify(%s))'"
           (buffer-substring (region-beginning) (region-end)))
   (current-buffer) t) (message "No active region."))
  )

(defun json-stringify-prettify-region (beg end)
  (interactive "r")
  (unless (region-active-p)) (error "No active region.")
  (replace-string
   (buffer-substring-no-properties beg end)
   (let ((output
          (shell-command-to-string
           (format "node -e 'console.log(JSON.stringify(%s))' " (buffer-substring beg end)))))
     (let ((temp-buffer-string (with-temp-buffer
                                 (insert output)
                                 (json-pretty-print-buffer)
                                 (buffer-string))))
       temp-buffer-string)
     ) nil beg end)
  )
