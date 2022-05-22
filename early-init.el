;;; early-init.el --- Executes before init.el  -*- lexical-binding: t -*-

(add-to-list 'load-path "~/.emacs.d/vendor/")
;; (package-initialize) is called later, so don't load up packages by default.
(setq package-enable-at-startup nil)

;; Use UTF-8 everywhere. This is all that's needed.
(set-language-environment "UTF-8")

;; (set-language-environment) sets default-input-method, which is unwanted.
(setq default-input-method nil)
