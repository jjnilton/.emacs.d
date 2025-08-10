;;; early-init.el --- -*- lexical-binding: t -*-

(defvar comp-deferred-compliation)
(setq comp-deferred-compilation t)

(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)

;; increase gc threshold to speedup starting up
(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

(setq inhibit-startup-message t)

;; no menu bar, toolbar, scroll bar
;; minimal ui
(menu-bar-mode -1) ;; disables menubar
(tool-bar-mode -1) ;; disables toolbar
(scroll-bar-mode -1) ;; disables scrollbar
(column-number-mode +1) ;; enable column number
(pixel-scroll-precision-mode 1) ;; enable smooth scrolling

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t
      tab-bar-new-button-show nil ;; don't show new tab button
      tab-bar-close-button-show nil ;; don't show tab close button
      tab-line-close-button-show nil) ;; don't show tab close button

(setq native-comp-async-report-warnings-errors 'silent)

;; TODO: start dark theme if time is > 18h, else light theme

(provide 'early-init)
;;; early-init.el ends here
