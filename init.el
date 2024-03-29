;;; init.el --- Vitor's .emacs file  -*- lexical-binding: t -*-

;; Set my name. Used by org export and mail packages.
(setq user-full-name "Vitor M. de Sousa Pereira")

;; Configure archives where packages will be downloaded from and initialize
;; package.el
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(setq package-archive-priorities '(("melpa" . 300)
                                   ("gnu" . 200)
                                   ("nongnu" . 100)))

(package-initialize)

;; Download all packages declared with use-package by default. Emacs' packages
;; must set `:ensure nil`.
(setq use-package-always-ensure t)

;; Native compile packages as part of their installation.
(setq package-native-compile t)

;; Turn off startup message.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message (user-login-name))

;; Make *scratch* buffer use fundamental instead of elisp mode.
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(setq-default tab-width 4
              indent-tabs-mode nil
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
(blink-cursor-mode 0)
(column-number-mode 1)
(electric-pair-mode 1)
(global-auto-revert-mode 1)
(global-subword-mode 1)
(save-place-mode 1)
(show-paren-mode 1)
(winner-mode 1)
(pixel-scroll-precision-mode 1)

;; Set registers, for quick file access.
(set-register ?i '(file . "~/.emacs.d/init.el"))
(set-register ?t '(file . "~/.emacs.d/templates"))

(use-package recentf
  ;; Store a list of recently opened files.
  :ensure nil
  :defer 1
  :bind ("C-x C-r" . v/recentf-find-file)
  :custom
  (recentf-max-menu-items 10)
  (recentf-max-saved-items 100)
  :init
  (recentf-mode 1)
  :config
  (defun v/recentf-find-file ()
    (interactive)
    (find-file (completing-read "Open recent: " recentf-list nil t))))

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

;; <C-i> and <TAB>, and <C-m> and <RET> are the same character. Binding one
;; always binds the other. Remaps them to other keys so we can distinguish
;; between them.
(keyboard-translate ?\C-i ?\H-i)
(keyboard-translate ?\C-m ?\H-m)

(bind-key* "C-x C-b" 'ibuffer)
(bind-key* "C-x TAB" 'imenu)
(bind-key* "C--" 'undo)
(bind-key* "M-z" 'undo)
(bind-key* "C-x k" 'kill-this-buffer)
(bind-key* "M-o" 'other-window)
(bind-key* "C-x o" 'ff-find-other-file)
(bind-key* "H-i" 'mark-defun)

;; Use CMD as meta on both Cocoa and Mitsuharu builds. Option is used to insert
;; all sorts of characters.
(setq ns-command-modifier 'meta
      ns-option-modifier 'none
      ns-pop-up-frames nil)

;; I kept accidentally triggering these bindings that increase and decrease font
;; size.
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))

(use-package exec-path-from-shell
  :if (and (memq window-system '(mac ns)) (not (boundp 'ns-system-appearance)))
  :defer 1
  :config
  (exec-path-from-shell-initialize))

(use-package display-fill-column-indicator
  :hook (prog-mode . display-fill-column-indicator-mode))

(use-package eshell
  :ensure nil
  :bind ("C-x m" . eshell)
  :config
  (defun v/prompt-pwd (path max-len)
    "Return a potentially trimmed-down version of the directory PATH, replacing
parent directories with their initial characters to try to get the character
length of PATH (sans directory slashes) down to MAX-LEN."
    (let* ((components (split-string (abbreviate-file-name path) "/"))
           (len (+ (1- (length components))
                   (cl-reduce '+ components :key 'length)))
           (str ""))
      (while (and (> len max-len) (cdr components))
        (setq str (concat str
                          (cond ((= 0 (length (car components))) "/")
                                ((= 1 (length (car components)))
                                 (concat (car components) "/"))
                                (t
                                 (if (string= "."
                                              (string (elt (car components) 0)))
                                     (concat (substring (car components) 0 2)
                                             "/")
                                   (string (elt (car components) 0) ?/)))))
              len (- len (1- (length (car components))))
              components (cdr components)))
      (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

  (defun v/with-face (str &rest face-plist)
    (propertize str 'face face-plist))

  (defun v/eshell-prompt-function ()
    (concat
     (v/with-face (v/prompt-pwd (eshell/pwd) 1) :foreground "#b8bb26")
     (v/with-face " ❱" :foreground "#fb4934")
     (v/with-face " " :foreground "#ebdbb2")))

  (setq eshell-prompt-function 'v/eshell-prompt-function
        eshell-highlight-prompt nil))

(use-package project
  ;; Emacs' built-in projectile-ish mode. Default prefix is C-x p.
  :ensure nil
  :bind* (("M-RET" . project-find-file)))

(use-package ibuffer-vc
  ;; Group buffers, in ibuffer, by git project root.
  :hook (ibuffer . v/setup-ibuffer-vc)
  :config
  (defun v/setup-ibuffer-vc ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic))))

(use-package ws-butler
  ;; Trim whitespace without touching the point.
  :hook (prog-mode . ws-butler-mode))

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t))

(use-package mood-line
  :hook (after-init . mood-line-mode))

(use-package mwim
  ;; Make C-a and C-e do what I actually want.
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end))
  :custom
  (mwim-beginning-of-line-function '((t . beginning-of-visual-line)
                                     (message-mode . message-beginning-of-line)
                                     (org-mode . org-beginning-of-line)))
  (mwim-end-of-line-function '((t . end-of-visual-line)
                               (org-mode . org-end-of-line))))

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

(defun v/open-in-finder ()
  "Reveal the currently shown file in Finder."
  (interactive)
  (shell-command (concat "open -R " (buffer-file-name))))

;;; Programming

(setq treesit-language-source-alist
      '((javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript"
             "master"
             "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                    "master"
                    "typescript/src")))

(defun v/install-tree-sitter-grammar (name)
  (unless (treesit-language-available-p name)
    (treesit-install-language-grammar name)))

(defun v/project-recompile ()
  "Run `recompile' in the project root."
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (recompile)))

(use-package dash-at-point
  :if (eq system-type 'darwin)
  :bind ("C-c d" . dash-at-point))

;; (use-package flymake
;;   :hook (python-mode . flymake-mode))

;; Don't delay showing the error at point in the minibuffer.
(setf help-at-pt-timer-delay 0.1
      help-at-pt-display-when-idle '(flymake-diagnostic))

(use-package flymake-collection
  ;; Collection of diagnostic functions (linters and so on) to use with Flymake.
  :hook (after-init . flymake-collection-hook-setup))

(use-package eglot
  :ensure nil
  :defer t
  :bind (:map eglot-mode-map
              ("C-c C-r" . eglot-rename)
              ("C-c C-f" . eglot-format))
  :hook ((eglot-mode . eldoc-mode)
         (eglot-mode . flymake-mode)))

(use-package make-mode
  :ensure nil
  :commands makefile-gmake-mode
  :mode (("Makefile"  . makefile-gmake-mode)
         ("\\.mk\\'" . makefile-gmake-mode)))

(use-package cmake-mode
  :load-path "/usr/local/share/emacs/site-lisp/cmake/"
  :mode "CMakeLists.txt")

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package caddyfile-mode
  :mode ("Caddyfile\\'" "caddy\\.conf\\'"))

(use-package sh-script
  :ensure nil
  :defer t
  :custom (sh-basic-offset 2)
  :hook (sh-mode . v/sh-mode-hook)
  :config
  (defun v/sh-mode-hook ()
    ;; Make scripts executable as soon they're saved.
    (add-hook 'after-save-hook
              'executable-make-buffer-file-executable-if-script-p
              0
              'local)))

(use-package fish-mode
  :mode "\\.fish\\'"
  :hook (fish-mode . v/fish-mode-hook)
  :config
  (defun v/fish-mode-hook ()
    (add-hook 'before-save-hook 'fish_indent-before-save 0 'local)))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . eglot-ensure)
  :custom
  (python-fill-docstring-style 'pep-257-nn))

(use-package ruby-mode
  :mode ("\\.rb\\'" . ruby-mode))

(use-package ruby-end
  :hook (ruby-mode . ruby-end-mode))

(use-package lua-mode
  :mode "\\.lua\\'"
  :custom (lua-indent-level 2))

(use-package cc-mode
  :ensure nil
  :mode ("\\.h\\'" . c++-mode)
  :bind (:map c++-mode-map
         ("C-c C-c" . v/project-recompile)))

(use-package objc-mode
  :ensure nil
  :mode "\\.mm\\'")

(use-package google-c-style
  :load-path "~/.emacs.d/vendor/"
  :hook (c-mode-common . google-set-c-style))

(use-package php-mode
  :mode "\\.php\\'")

;; Ignore GCC generate header dependency files.
(add-to-list 'completion-ignored-extensions ".d")

(use-package go-mode
  :mode (("\\.go\\'" . go-mode)
         ("go\\.mod\\'" . go-dot-mod-mode))
  :hook (go-mode . eglot-ensure))

(use-package cider
  :commands (cider-jack-in-clj cider-jack-in-cljs cider-jack-in-clj&cljs)
  :bind (:map clojure-mode-map
         ("C-c c" . v/cider-reset)
         :map cider-mode-map
         ("C-c C-f" . cider-format-buffer))
  :config
  (defun v/cider-reset ()
    (interactive)
    (cider-insert-in-repl "(reset)" t)))

(defconst v/lispy-modes '(cider-repl-mode
                          clojure-mode
                          emacs-lisp-mode
                          lisp-mode
                          lisp-data-mode))

(eval `(use-package paredit
         :hook (,v/lispy-modes . paredit-mode)))

(eval `(use-package rainbow-delimiters
         :hook (,v/lispy-modes . rainbow-delimiters-mode)))

(use-package js-ts-mode
  :ensure nil
  :mode ("\\.js\\'" "\\.json\\'")
  :custom
  (js-expr-indent-offset 2)
  (js-indent-level 2)
  :config
  (v/install-tree-sitter-grammar 'javascript))

(use-package typescript-ts-mode
  :ensure nil
  :mode "\\.tsx?\\'"
  :config
  (v/install-tree-sitter-grammar 'tsx)
  (v/install-tree-sitter-grammar 'typescript))

(use-package nvm
  :commands (nvm-use-for)
  :custom
  (nvm-dir "~/.local/share/nvm/")
  :init
  (defun v/nvm ()
    (interactive)
    (nvm-use-for)))

(use-package css-mode
  :ensure nil
  :defer t
  :custom (css-indent-offset 2))

(use-package web-mode
  :mode ("\\.html\\'" "\\.erb\\'")
  :hook (web-mode . v/web-mode-hook)
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-comment-style 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-script-padding 2)
  (web-mode-style-padding 2)
  :init
  (setq web-mode-engines-alist '(("django" . "templates/.*\\.html\\'")
                                 ("liquid" . "0x1.pt/.*\\.html\\'")))
  :config
  (defun v/web-mode-electric-pair-p (c)
    "Don't pair curly braces in web-mode as it already has its own
completions."
    (if (char-equal c ?{)
        t
      (electric-pair-default-inhibit c)))

  (defun v/web-mode-hook ()
    (setq-local
     electric-pair-inhibit-predicate 'v/web-mode-electric-pair-p)
    (display-fill-column-indicator-mode 0)))

(use-package emmet-mode
  :commands emmet-expand-line
  :hook (web-mode . v/emmet-setup-capf)
  :init
  (defun v/emmet-setup-capf ()
    (add-hook 'completion-at-point-functions
              (lambda () (emmet-expand-line nil))
              0
              'local)))

;;; Completion at point UI

(use-package corfu
  :hook ((prog-mode org-mode eshell-mode) . corfu-mode))

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
  :hook ((prog-mode text-mode) . v/tempel-setup-capf)
  :init
  ;; Setup completion at point.
  (defun v/tempel-setup-capf ()
    (add-hook 'completion-at-point-functions #'tempel-expand -1 'local)))

;;; Writing

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-c b" . org-cite-insert))
  :hook (org-mode . visual-line-mode)
  :custom
  (org-agenda-files '("~/Documents/Agenda.org"))
  (org-capture-templates
   '(("f" "Fav" entry (file "~/Documents/Fav.org")
      "\n* TODO %:annotation%?\n:PROPERTIES:\n:ADDED: %U\n:END:\n\n"
      :prepend t)
     ("s" "Scratch" entry (file "~/Documents/Scratch.org")
      "* %?\n\n")))
  (org-export-time-stamp-file nil)
  (org-html-doctype "html5")
  (org-html-validation-link nil)
  (org-startup-indented t)
  (org-startup-folded t))

(use-package org-ql
  :commands (org-ql-find org-ql-search))

(use-package markdown-mode
  :mode "\\.md\\'"
  :hook (markdown-mode . visual-line-mode))

(use-package visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode))

(use-package flyspell
  ;; Portuguese dictionaries can be downloaded at
  ;; https://natura.di.uminho.pt/wiki/doku.php?id=dicionarios:main
  :ensure nil
  :hook ((text-mode . flyspell-mode))
  :custom
  (ispell-really-aspell t)
  (flyspell-issue-message-flag nil))

(use-package flyspell-correct
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper)))

;;; Other

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
