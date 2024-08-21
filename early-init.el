;; https://config.phundrak.com/emacs/
(setq
 package-enable-at-startup nil
 inhibit-startup-message   t
 frame-resize-pixelwise    t  ; fine resize
 package-native-compile    t) ; native compile packages

(scroll-bar-mode -1)               ; disable scrollbar
(tool-bar-mode -1)                 ; disable toolbar
;; (tooltip-mode -1)                  ; disable tooltips
;; (set-fringe-mode 10)               ; give some breathing room
(menu-bar-mode -1)                 ; disable menubar
;; (blink-cursor-mode 0)              ; disable blinking cursor

(setq garbage-collection-messages t            ;; tell me when garbage collecting
      gc-cons-threshold (* 16 1024 1024 1024)) ;; 16GiB of RAM

(defmacro my/time (&rest body)
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(defun my/garbage-collect ()
  "Garbage collect and tell the user how much time it took."
  (message "Garbage collector ran for %.06fs"
           (my/time (garbage-collect))))

(defvar my/gc-timer nil
  "Timer for garbage collection. See
`my/garbage-collect-on-focus-lost'.")

(defun my/garbage-collect-on-focus-lost ()
  "Garbage collect when Emacs loses focus.

Garbage collection is only triggered thirty seconds after losing
focus, and only once."
  (if (frame-focus-state)
      (when (timerp my/gc-timer)
       (cancel-timer my/gc-timer))
    (setq my/gc-timer (run-with-idle-timer 30 nil #'my/garbage-collect))))

(add-function :after after-focus-change-function #'my/garbage-collect-on-focus-lost)


;; Remove command line options that aren't relevant to the current OS; this
;; results in slightly less processing at startup.
(unless (eq system-type 'darwin)
  (setq command-line-ns-option-alist nil))
(unless (eq system-type 'gnu/linux)
  (setq command-line-x-option-alist nil))


(setenv "LSP_USE_PLISTS" "true") ;; in early-init.el
