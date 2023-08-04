;;; early-init.el --- Executes before init.el  -*- lexical-binding: t -*-

;; Turn off GC when initializing. It must be re-enabled at the end of init.el.
(setq gc-cons-threshold #x40000000)

;; (package-initialize) is called later, so don't load up packages by default.
(setq package-enable-at-startup nil)

;; Use UTF-8 everywhere. This is all that's needed.
(set-language-environment "UTF-8")

;; (set-language-environment) sets default-input-method, which is unwanted.
(setq default-input-method nil)

;; Set font.
(set-face-attribute 'default nil :family "Iosevka Fixed" :height 140)

;; Turn off unneeded GUI things. These settings should be preferably set using
;; `defaults', as setting them here slows down startup considerably.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode 0))

;; `file-name-handler-alist' is consulted on each `require', `load' and other
;; path functions. Unsetting it gives us a startup speed up.
(let ((old-file-name-handler-alist file-name-handler-alist))
  (setq file-name-handler-alist nil)

  (defun vsp/restore-file-name-handler-alist ()
    (setq file-name-handler-alist old-file-name-handler-alist))

  (add-hook 'emacs-startup-hook 'vsp/restore-file-name-handler-alist))
