;;; init.el --- Vitor's .emacs file  -*- lexical-binding: t -*-

;; Set my name. Used by org export and mail packages.
(setq user-full-name "Vitor M. de Sousa Pereira")

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

(use-package doom-themes
  :config
  (load-theme 'doom-monokai-pro t)
  (doom-themes-org-config))

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
  :defer 1
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

;; I only use git.
(setq vc-handled-backends '(Git))

(use-package vc-defer
  ;; Defer non-essential work related to the built in VC mode. VC mode is used
  ;; in `project-find-file'.
  :config
  (add-to-list 'vc-defer-backends 'Git)
  (vc-defer-mode))

(use-package magit
  :bind ("C-x g" . magit))

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
  :after dired
  :ensure nil)

(use-package dash-at-point
  :if (eq system-type 'darwin)
  :bind ("C-c d" . dash-at-point))

(use-package cmake-mode
  :load-path "/usr/local/share/emacs/site-lisp/cmake/"
  :mode "CMakeLists.txt")

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package sh-script
  :ensure nil
  :defer t
  :custom (sh-basic-offset 2)
  :hook (sh-mode . vsp/sh-mode-hook)
  :init
  (defun vsp/sh-mode-hook ()
    ;; Make scripts executable as soon they're saved.
    (add-hook 'after-save-hook
              'executable-make-buffer-file-executable-if-script-p
              0
              'local)))

(use-package fish-mode
  :mode "\\.fish\\'"
  :custom (fish-indent-offset 2))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :custom
  (python-fill-docstring-style 'pep-257-nn))

(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

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

(use-package cider
  :commands cider-jack-in)

(use-package paredit
  :hook ((lisp-mode emacs-lisp-mode) . paredit-mode))

(use-package rainbow-delimiters
  :hook ((lisp-mode emacs-lisp-mode) . rainbow-delimiters-mode))

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
  :mode ("\\.html\\'" "\\.erb\\'" "\\.dtl\\'")
  :hook (web-mode . vsp/web-mode-hook)
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-comment-style 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-script-padding 2)
  (web-mode-style-padding 2)
  :init
  (setq web-mode-engines-alist '(("liquid" . "\\.html\\'")
                                 ("elixir" . "\\.eex\\'")
                                 ("phoenix" . "\\.heex\\'")))
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

;; Hide commands that don't apply to the current mode.
(setq read-extended-command-predicate #'command-completion-default-include-p)

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

(use-package ibuffer-vc
  ;; Group buffers, in ibuffer, by git project root.
  :hook (ibuffer . vsp/setup-ibuffer-vc)
  :config
  (defun vsp/setup-ibuffer-vc ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic))))

(use-package esup
  ;; Benchmark Emacs Startup time without ever leaving your Emacs.
  :commands esup
  :custom
  ;; Work around a bug where esup tries to step into the byte-compiled version
  ;; of `cl-lib', and fails horribly.
  (esup-depth 0))

(use-package gcmh
  ;; Enforce a sneaky Garbage Collection strategy to minimize GC interference
  ;; with the activity. During normal use a high GC threshold is set. When
  ;; idling GC is triggered and a low threshold is set.
  :config (gcmh-mode 1))

;;; init.el ends here
