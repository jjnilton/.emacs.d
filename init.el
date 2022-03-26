(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


;; Defaults
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key "\C-cy" '(lambda ()
                           (interactive)
                           (popup-menu 'yank-menu)))
(menu-bar-mode -1)
(tool-bar-mode -1)
;;(toggle-scroll-bar -1)
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
(add-hook 'php-mode-hook #'lsp)
;;(add-hook 'prog-mode-hook #'lsp)

;; (with-eval-after-load 'lsp-mode
;;   (add-to-list 'lsp-language-id-configuration
;;     '(web-mode . "twig"))

;;   (lsp-register-client
;;     (make-lsp-client :new-connection (lsp-stdio-connection "html-ls")
;;                      :activation-fn (lsp-activate-on "twig")
;;                      :server-id 'html-ls)))


;; Which-Key
(which-key-mode)


;; Windsize & Windmove
(windsize-default-keybindings)
(global-set-key (kbd "C-M-<up>") 'windmove-up)
(global-set-key (kbd "C-M-<left>") 'windmove-left)
(global-set-key (kbd "C-M-<down>") 'windmove-down)
(global-set-key (kbd "C-M-<right>") 'windmove-right)
;; (global-set-key (kbd "C-x M-<up>") #'windmove-up)
;; (global-set-key (kbd "C-x M-<down>") #'windmove-down)
;; (global-set-key (kbd "C-x M-<left>") #'windmove-left)
;; (global-set-key (kbd "C-x M-<right>") #'windmove-right)



;; Ivy/Swiper/Counsel
(ivy-mode)
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
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

;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;; Custom Shortcuts
;; Multline
(global-set-key (kbd "C-c C-d") 'mc/mark-next-like-this-word)
(global-set-key (kbd "M-<down>") 'mc/mark-next-word-like-this)
(global-set-key (kbd "M-.") 'mc/mark-pop)

;; Undo Tree
;; (defun undo-tree-split-side-by-side (original-function &rest args)
;;   "Split undo-tree side-by-side"
;;   (let ((split-height-threshold nil)
;;         (split-width-threshold 0))
;;     (apply original-function args)))

;; (advice-add 'undo-tree-visualize :around #'undo-tree-split-side-by-side)

;; Auto-Completion
;; hopefully autocomplete stuff
;; (add-to-list 'company-backends 'company-css)
;; (add-to-list 'company-backends 'company-capf)
;; (add-to-list 'company-backends 'company-web-html)
;; (add-to-list 'company-backends 'company-web-jade)
;; (add-to-list 'company-backends 'company-web-slim)
;; (defun my-web-mode-hook ()
;;   (set (make-local-variable 'company-backends) '(company-capf company-css company-web-html company-yasnippet company-files))
;; )
;; (add-hook 'web-mode-hook 'my-web-mode-hook)
;;Yes. You can add hook to lsp-managed-mode and do something like (when (eq major-qmode 'css-mode) ... <change company-backends))

;;(add-hook 'lsp-managed-mode-hook (when (eq major-mode 'web-mode) (setq-local company-backends '((company-capf)))))

(eval-after-load 'company
  '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))


;; Hooks
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'after-init-hook (lambda () (message (concat "Startup time: " (emacs-init-time)))))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
;; (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)

;; Mode & File Associations
;; Web Development
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
(setq web-mode-content-types-alist '(("twig"  . "\\.twig\\'")))
(setq web-mode-content-types-alist '(("jsx"  . "\\.js[x]?\\'")))

;; Other
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Config/Data Files
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . conf-mode))
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
                '(:eval (my-buffer-identification "%12b"))))

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
 '(awesome-tray-mode-line-active-color "#2fafff")
 '(awesome-tray-mode-line-inactive-color "#323232")
 '(custom-enabled-themes '(modus-vivendi))
 '(custom-safe-themes
   '("3860a842e0bf585df9e5785e06d600a86e8b605e5cc0b74320dfe667bcbe816c" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "74a50f18c8c88eac44dc73d7a4c0bbe1f3e72ff5971aac38fcf354ddad0d4733" "aa72e5b41780bfff2ff55d0cc6fcd4b42153386088a4025fed606c1099c2d9b8" default))
 '(diff-hl-flydiff-mode t)
 '(electric-pair-mode t)
 '(electric-pair-pairs '((34 . 34) (8216 . 8217) (8220 . 8221)))
 '(electric-pair-text-pairs '((34 . 34) (8216 . 8217) (8220 . 8221) (39 . 39)))
 '(exwm-floating-border-color "#646464")
 '(fci-rule-color "#5B6268")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-themes-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-themes-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-themes-fringe-yellow))
 '(global-diff-hl-mode t)
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
 '(ivy-virtual-abbreviate 'abbreviate)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#96CBFE"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#A8FF60"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(keycast-mode-line-format "%1s%k%c%r")
 '(keycast-mode-line-remove-tail-elements nil)
 '(lsp-disabled-clients '(eslint))
 '(minimap-hide-fringes t)
 '(minimap-mode t)
 '(mlr-non-rectangle-style 'lines+words+chars)
 '(objed-cursor-color "#ff6c60")
 '(org-src-block-faces 'nil)
 '(package-selected-packages
   '(frameshot path-headerline-mode gif-screencast company-quickhelp keycast modus-themes tramp flycheck lsp-ui diff-hl multiple-cursors idle-highlight-mode company projectile counsel ivy web-mode windsize which-key uniquify-files undo-tree transpose-frame smex magit emmet-mode crux))
 '(pdf-view-midnight-colors '("#ffffff" . "#100f10"))
 '(recentf-mode t)
 '(rustic-ansi-faces
   ["#000000" "#ff6c60" "#A8FF60" "#FFFFB6" "#96CBFE" "#FF73FD" "#C6C5FE" "#f6f3e8"])
 '(show-paren-mode t)
 '(split-width-threshold 120)
 '(tab-bar-separator "" t)
 '(truncate-lines t)
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
 '(visible-bell t)
 '(web-mode-enable-auto-indentation nil)
 '(web-mode-enable-current-element-highlight t)
 '(web-mode-script-padding 4)
 '(web-mode-style-padding 4)
 '(whitespace-style
   '(face trailing tabs spaces newline empty indentation space-after-tab space-before-tab space-mark tab-mark))
 '(whitespace-trailing-regexp "\\([	  ]+\\|^[

]+\\)$")
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
 '(mode-line-buffer-id ((t (:background "gray10" :foreground "dim gray" :weight normal))))
 '(mode-line-highlight ((t (:background "white" :foreground "black" :box nil))))
 '(mode-line-id-inactive ((t (:background "gray10" :foreground "dim gray"))))
 '(mode-line-inactive ((t (:background "black" :foreground "dim gray" :box (:line-width 1 :color "gray40")))))
 '(tab-bar ((t (:inherit variable-pitch :background "black" :foreground "white"))))
 '(tab-bar-tab ((t (:background "white" :foreground "black" :box (:line-width 1 :color "white")))))
 '(tab-bar-tab-inactive ((t (:foreground "light gray")))))
(setq frame-title-format
      '((:eval (format "emacs-%d.%d@%s:%s" emacs-major-version emacs-minor-version system-type
                       (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))))

(frameshot-setup
 '((name . "emacs")
   (output . "~/Downloads/")))