;;; init.el --- Vitor's .emacs file  -*- lexical-binding: t -*-

;; Make startup faster by reducing the frequency of garbage collection. The
;; default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Set my name. Used by org export and mail packages.
(setq user-full-name "Vitor M. de Sousa Pereira")

;; Turn off unneeded GUI things. These settings should be preferably set using
;; `defaults', as setting them here slows down startup considerably.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode 0))

;; Archives where packages will be downloaded from.
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;;; Install use-package. It will load up all other packages.

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Download all packages declared with use-package by default. Emacs' packages
;; must set `:ensure nil`.
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Native compile packages as part of their installation.
(setq package-native-compile t)

;; Turn off startup message.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message (user-login-name))

;; Make *scratch* buffer use fundamental instead of elisp mode.
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(setq-default indent-tabs-mode nil
              ;; Make it so a single space ends a sentence when filling.
              sentence-end-double-space nil
              fill-column 80)

;; Don't create emacs-specific files in the directory of the file that's being
;; edited.
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/saves/" t))
      create-lockfiles nil
      backup-directory-alist '(("." . "~/.emacs.d/backups/"))
      kept-old-versions 0
      kept-new-versions 10
      delete-old-versions t
      backup-by-copying t)

(setq
 ;; Always `load' most recent elisp file. Useful when byte compiled files are
 ;; not up to date.
 load-prefer-newer t
 ;; Copy from point, instead of click, when using a mouse.
 mouse-yank-at-point t
 ;; Always add a final newline to all files.
 require-final-newline t
 ;; Save existing clipboard text into kill ring before replacing it.
 save-interprogram-paste-before-kill t
 ;; Use forward slashes when uniquifying buffer names.
 uniquify-buffer-name-style 'forward
 ;; Use y/n instead of yes/no.
 use-short-answers t
 ;; Highlight current error message.
 next-error-message-highlight t)

;; Open ediff's buffers in a single frame.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; No noise.
(setq ring-bell-function 'ignore
      visible-bell nil)

;; Load custom.el if it exists.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;; Turn on/off some minor modes globally.
(save-place-mode 1)
(show-paren-mode 1)
(column-number-mode 1)
(winner-mode 1)
(global-subword-mode 1)
(global-auto-revert-mode 1)
(electric-pair-mode 1)
(blink-cursor-mode 0)

;; Add a bit of fringe so the buffer's contents aren't sticking to the window's
;; edge.
(fringe-mode '(4 . 0))

;; Transparently encrypt and decrypt GnuPG encrypted files.
(setq epa-pinentry-mode 'loopback)
(epa-file-enable)

;; Make scripts executable as soon they're saved.
(add-hook 'after-save-hook
      	  'executable-make-buffer-file-executable-if-script-p)

;; Set registers, for quick file access.
(set-register ?i '(file . "~/.emacs.d/init.el"))
(set-register ?t '(file . "~/.emacs.d/templates"))

(use-package async
  ;; Make dired and byte-compilation asynchronous.
  :defer t
  :custom (async-bytecomp-allowed-packages '(all))
  :init
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1))

;;; Better minibuffer completion UI

(use-package vertico
  :bind (:map vertico-map
              ("RET" . minibuffer-force-complete-and-exit)
              ("TAB" . minibuffer-complete))
  :custom
  ;; Enable cycling for `vertico-next' and `vertico-previous'.
  (vertico-cycle t)
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

;; Use flex matching.
(setq completion-styles '(basic substring flex))

;; Disable case-sensitivity for file and buffer matching.
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; Do not allow the cursor in the minibuffer prompt.
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq enable-recursive-minibuffers t)

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-reverse
  :after vertico
  :ensure nil
  :init (vertico-reverse-mode))

;;; Global keybinds. These won't be overriden on any mode.

(bind-key* "C-x C-b" 'ibuffer)
(bind-key* "C-x TAB" 'imenu)
(bind-key* "C--" 'undo)
(bind-key* "M-z" 'undo)
(bind-key* "C-x k" 'kill-this-buffer)
(bind-key* "M-o" 'other-window)
(bind-key* "C-x o" 'ff-find-other-file)

;; Use CMD as meta on both Cocoa and Mitsuharu builds. Option is used to insert
;; all sorts of characters.
(setq ns-command-modifier 'meta
      ns-option-modifier 'none
      ns-pop-up-frames nil)

(use-package eshell
  :bind ("C-x m" . eshell))

(use-package project
  ;; Emacs' built-in projectile-ish mode. Default prefix is C-x p.
  :ensure nil
  :bind (("M-RET" . project-find-file)
         ("M-s s" . project-find-regexp)))

(use-package ws-butler
  ;; Trim whitespace without touching the point.
  :hook (prog-mode . ws-butler-mode))

(use-package display-fill-column-indicator
  :ensure nil
  :hook ((prog-mode) . display-fill-column-indicator-mode))

(use-package nord-theme
  :config (load-theme 'nord t))

(use-package mood-line
  :hook (after-init . mood-line-mode))

(use-package mwim
  ;; Make C-a and C-e do what I actually want.
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end)))

(use-package whole-line-or-region
  ;; Allows functions to operate on the current line if they would normally
  ;; operate on a region and region is currently undefined.
  :config (whole-line-or-region-global-mode 1))

(use-package ctrlf
  :config (ctrlf-mode 1))

(use-package winum
  :bind (:map winum-keymap
              ("M-1" . winum-select-window-1)
              ("M-2" . winum-select-window-2)
              ("M-3" . winum-select-window-3)
              ("M-4" . winum-select-window-4)
              ("M-5" . winum-select-window-5)
              ("M-6" . winum-select-window-6)
              ("M-7" . winum-select-window-7)
              ("M-8" . winum-select-window-8))
  :init (winum-mode 1))

(use-package magit
  :bind ("C-x g" . magit))

(use-package exec-path-from-shell
  ;; Load up correct $PATH on macOS.
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package ls-lisp
  ;; Use an Emacs provided `ls', instead of the system's, so behavior is
  ;; consistent across all platforms.
  :ensure nil
  :custom
  (ls-lisp-dirs-first t)
  (ls-lisp-verbosity nil)
  (ls-lisp-use-insert-directory-program nil))

(use-package dired
  :ensure nil
  :defer t
  :custom
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  :hook (dired-mode . dired-hide-details-mode))

(use-package dired-x
  :ensure nil)

(use-package treemacs
  :bind (:map winum-keymap
              ("M-0" . treemacs-select-window)
              :map treemacs-mode-map
              ([mouse-1] . treemacs-single-click-expand-action))
  :custom
  (treemacs-no-png-images t)
  (treemacs-display-current-project-exclusively t)
  (treemacs-project-follow-mode t))

(use-package dash-at-point
  :if (eq system-type 'darwin)
  :bind ("C-c d" . dash-at-point))

(use-package cmake-mode
  :load-path "/usr/local/share/emacs/site-lisp/cmake/"
  :mode "CMakeLists.txt")

(use-package ninja-mode
  :load-path "/usr/local/share/emacs/site-lisp/ninja/"
  :mode "\\.ninja\\'")

(use-package bazel
  :defer t
  :hook (bazel-mode . flymake-mode)
  :mode ("\\.BUILD\\'" . bazel-build-mode))

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package sh-script
  :ensure nil
  :defer t
  :custom (sh-basic-offset 2))

(use-package fish-mode
  :mode "\\.fish\\'"
  :custom (fish-indent-offset 2))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :custom
  (python-fill-docstring-style 'pep-257-nn))

(use-package lua-mode
  :mode "\\.lua\\'"
  :custom (lua-indent-level 2))

(use-package c++-mode
  :ensure nil
  :mode "\\.h\\'"
  :bind (:map c++-mode-map
              ("C-c C-c" . compile)
              ("C-c C-b" . bazel-build)
              ("C-c C-r" . bazel-run)))

(use-package objc-mode
  :ensure nil
  :mode "\\.mm\\'")

(use-package google-c-style
  :load-path "~/.emacs.d/vendor/"
  :hook (c-mode-common . google-set-c-style))

;; Ignore GCC generate header dependency files.
(add-to-list 'completion-ignored-extensions ".d")

(use-package clang-format
  :load-path "/usr/local/opt/llvm/share/emacs/site-lisp/llvm"
  :bind (:map c++-mode-map
              ("C-c C-f" . clang-format-buffer)))

(use-package php-mode
  :mode "\\.php\\'"
  :custom (php-mode-template-compatibility nil))

(use-package go-mode
  :mode (("\\.go\\'" . go-mode)
         ("go\\.mod\\'" . go-dot-mod-mode))
  :hook ((go-mode . eglot-ensure)
         (go-mode . vsp/go-mode-hook))
  :config
  (defun vsp/go-mode-hook ()
    (display-fill-column-indicator-mode 0)))

(use-package slime
  :commands slime
  :hook (common-lisp-mode . slime-mode)
  :init
  (setq inferior-lisp-program "sbcl"
        slime-contribs '(slime-fancy slime-asdf slime-quicklisp)))

(use-package lispy
  :hook (lisp-mode . lispy-mode))

(use-package cider
  :commands cider-jack-in)

(use-package js
  :ensure nil
  :defer t
  :mode (("\\.js\\'" . js-mode)
         ("\\.json\\'" . js-mode))
  :custom
  (js-expr-indent-offset 2)
  (js-indent-level 2))

(use-package css-mode
  :ensure nil
  :defer t
  :custom (css-indent-offset 2))

(use-package web-mode
  :mode ("\\.html\\'" "\\.html.erb\\'" "\\.html.dtl\\'")
  :hook (web-mode . vsp/web-mode-hook)
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-comment-style 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-script-padding 2)
  (web-mode-style-padding 2)
  :init
  (setq web-mode-engines-alist '(("liquid" . "\\.html\\'")))
  :config
  (defun vsp/web-mode-hook ()
    (setq-local display-fill-column-indicator-column 100
                ;; Don't pair curly braces in web-mode as it already has its own
                ;; completions.
                electric-pair-inhibit-predicate
                (lambda (c)
                  (if (char-equal c ?{)
                      t
                    (electric-pair-default-inhibit c))))))

(use-package emmet-mode
  :commands emmet-expand-line
  :hook (web-mode . vsp/emmet-setup-capf)
  :init
  (defun vsp/emmet-setup-capf ()
    (add-hook 'completion-at-point-functions
              (lambda () (emmet-expand-line nil))
              0
              'local)))

(use-package eglot
  ;; Language Server Protocol client for Emacs.
  :commands (eglot eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c C-r" . eglot-rename)
              ("C-c C-f" . eglot-format)
              ("C-c o" . eglot-code-action-organize-imports)))

;;; Completion at point UI

(use-package corfu
  :hook ((prog-mode org-mode) . corfu-mode))

;; Enable indentation and completion using TAB.
(setq tab-always-indent 'complete)

;;; Templates

(use-package tempel
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert)
         :map tempel-map
         ("TAB" . tempel-next)
         ("S-TAB" . tempel-previous)
         ("C-g" . tempel-done))
  :hook ((prog-mode text-mode) . vsp/tempel-setup-capf)
  :init
  ;; Setup completion at point.
  (defun vsp/tempel-setup-capf ()
    (add-hook 'completion-at-point-functions #'tempel-expand -1 'local)))

;;; Writing

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "IBM Plex Sans" :height 180)))))

(use-package org
  :ensure t
  :pin elpa
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c b" . org-cite-insert))
  :custom
  (org-agenda-files '("~/Documents/Agenda.org"))
  (org-capture-templates
   '(("f" "Favorito" entry (file "~/Documents/Favoritos.org")
      "\n* TODO %:annotation%?\n:PROPERTIES:\n:ADDED: %U\n:END:\n\n"
      :prepend t)
     ("s" "Scratch" entry (file "~/Documents/Scratch.org")
      "* %?\n\n")))
  (org-export-time-stamp-file nil)
  (org-html-doctype "html5")
  (org-html-validation-link nil)
  (org-startup-indented t)
  (org-startup-folded t))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package auto-fill-mode
  :ensure nil
  :hook (text-mode . auto-fill-mode))

(use-package flyspell
  ;; Portuguese dictionaries can be downloaded at
  ;; https://natura.di.uminho.pt/wiki/doku.php?id=dicionarios:main
  :ensure nil
  :hook ((text-mode . flyspell-mode))
  :custom
  (ispell-really-aspell t))

(use-package flyspell-correct
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper)))

(use-package guess-language
  ;; Guess the language that's being used in the sentece.
  :hook (flyspell-mode . guess-language-mode)
  :custom
  (guess-language-languages '(en pt))
  (guess-language-min-paragraph-length 35))

(use-package server
  ;; Start a server by default so that org-protocol links work. On macOS, only
  ;; mituharu's fork registers a handler for org-protocol.
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
