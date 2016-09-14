;;; package --- Init file
;;; Commentary:
;;; Code:
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(require 'sublime-themes)
(require 'spaceline)
(require 'ergoemacs-mode)
(require 'flx-ido)
(require 'editorconfig)
(require 'project-explorer)
(require 'spaceline-config)
(require 'org)
(require 'ledger-mode)
(require 'rvm)
(require 'projectile)
(require 'magit)
(require 'flycheck)
(require 'flycheck-color-mode-line)
(require 'vimish-fold)
(require 'multiple-cursors)
(require 'highlight-indent-guides)
(require 'rainbow-delimiters)
(require 'autopair)
(require 'web-mode)
(require 'js2-mode)
(require 'coffee-mode)
(require 'scss-mode)
(require 'yaml-mode)
(require 'sws-mode)
(require 'php-mode)
(require 'json-mode)
(require 'markdown-mode)
(require 'haml-mode)
(require 'avy)
(require 'use-package)

;; Load theme
(load-theme 'spolsky t)

;; Disable backup and auto save
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Disable menubar and toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Set windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Set linum-mode - view line number
(add-hook 'prog-mode-hook 'linum-mode)

(setq indent-tabs-mode nil)


;; Packages:

(use-package ergoemacs-mode
  :init
  (progn
  (setq ergoemacs-theme-options (quote ((save-options-on-exit off))))
  (setq ergoemacs-theme nil)
  (setq ergoemacs-keyboard-layout "us")
  )
  :config
  (ergoemacs-mode 1)
  )

(use-package flx-ido
  :init
  (progn
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  (setq gc-cons-threshold 20000000))
  :config
  (progn
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1))
  )

(use-package editorconfig
  :config
  (editorconfig-mode 1)
  )

(use-package project-explorer
  :bind
  ("<F8>" . project-explorer-toggle)
  )

(use-package spaceline
  :config
  (spaceline-emacs-theme)
  )

(use-package avy
  :bind
  (("C-:" . avy-goto-char)
   ("C-'" . avy-goto-char-2)
   ("M-g f" . avy-goto-line)
   ("M-g w" . avy-goto-word-1))
  )

(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   )
  :init
  (progn
  (setq org-agenda-files (list "~/perkamen"))
   (setq org-log-done t)
   (setq org-capture-templates
	 '(("t" "Personal Todo" entry (file+headline "~/perkamen/todos.org" "Tasks")
	 "* TODO [#C] %?")
	("w" "Work Todo" entry (file+headline "~/perkamen/work.org" "Tasks")
	 "* TODO [#C] %?")
	))
   )
   )

(use-package ledger-mode
  :config
  (progn
  (add-to-list 'auto-mode-alist '("\\.dat$" . ledger-mode))
  (setq ledger-clear-whole-transaction 1))
  )

(use-package projectile
  :config
  (projectile-global-mode)
  )

(use-package rvm
  :config
  (rvm-use-default))

(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package flycheck
  :config
  (progn
    (add-hook 'after-init-hook 'global-flycheck-mode)
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
    (setq-default flycheck-temp-prefix ".flycheck"))
  )

(flycheck-add-mode 'javascript-eslint 'web-mode)

(use-package vimish-fold
  :config
  (vimish-fold-global-mode 1)
  :bind
  (("C-x v f" . vimish-fold)
   ("C-x v v" . vimish-fold-delete))
  )

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this))
  :config
  (delete-selection-mode 1)
  )

(use-package highlight-indent-guides
  :config
  (progn
    (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
    (setq highlight-indent-guides-method 'character))
  )

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

(use-package autopair
  :config
  (autopair-global-mode))

(use-package web-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (add-hook 'web-mode-hook 'linum-mode))
  )

(use-package js2-mode
  :config
  (progn
    (setq js-indent-level 2)
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-hook 'js2-mode-hook 'linum-mode))
  )

(use-package json-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
    (add-hook 'json-mode-hook 'linum-mode))
  )

(use-package coffee-mode
  :config
  (add-to-list 'coffee-mode-hook 'flycheck-mode)
  )

(use-package vue-mode)

(use-package scss-mode
  :config
  (progn
  (add-hook 'scss-mode-hook
	    (lambda ()
	    (setq css-indent-offset 2))
	    )
  (add-hook 'scss-mode-hook 'flycheck-mode)
  (add-hook 'scss-mode-hook 'linum-mode)
  )
  )

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  )

(use-package haml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
  )

(use-package sws-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jade$" . sws-mode))
  )

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
	ad-do-it)
    ad-do-it))

(provide 'init)

;;; init.el ends here
