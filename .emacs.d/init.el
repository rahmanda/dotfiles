;;; package --- Init file
;;; Commentary:
;;; Code:
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(require 'sublime-themes)
(require 'spaceline)
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
(require 'jade-mode)
(require 'avy)
(require 'yasnippet)
(require 'drag-stuff)
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

;; Set linum-mode - view line number
(global-linum-mode 1)

(setq indent-tabs-mode nil)


;; Packages:

(use-package evil
  :config
  (evil-mode 1))

(use-package evil-magit)

;; Dependencies for evil-mc
(use-package multiple-cursors
  :config
  (delete-selection-mode 1)
  )

(use-package evil-mc
  :config
  (evil-mc-mode 1))

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

(use-package project-explorer)

(use-package spaceline
  :config
  (spaceline-emacs-theme)
  )

(use-package avy)

(use-package org
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
  (progn
    (projectile-mode)
    (setq projectile-enable-caching t)
    (setq projectile-indexing-method 'alien)
  )
)

(use-package rvm
  :config
  (rvm-use-default))

(use-package magit)

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
  (add-hook 'coffee-mode-hook 'flycheck-mode)
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

(use-package jade-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.{jade|pug}$" . jade-mode))
    (add-hook 'jade-mode-hook 'linum-mode)
    )
  )

(use-package yasnippet
  :config
  (progn
    (yas-global-mode 1)
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    )
  )

(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1)
  :bind
  (:map evil-visual-state-map
	;; move visual lines up/down
	("K" . drag-stuff-up)
	("J" . drag-stuff-down)
  (:map evil-normal-state-map
	;; move current line up/down
	("K" . drag-stuff-up)
	("J" . drag-stuff-down)
	)
  ))

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
	ad-do-it)
    ad-do-it))

(use-package general
  :config
  (progn
    ;; reset dired mode space
    (define-key dired-mode-map (kbd "SPC") nil)

    (general-def :states '(normal motion emacs) "SPC" nil)
    (general-define-key
      :states '(normal motion emacs)
      "ESC" 'keyboard-quit
     )
    (general-create-definer my-leader-def
      :prefix "SPC")
    (my-leader-def
      :keymaps 'normal
      "f f" 'find-file
      "f d" 'dired
      "f j" 'dired-jump
      "f r" 'make-directory
      "p f" 'projectile-find-file
      "p p" 'projectile-switch-project
      "p d" 'projectile-dired
      "p t" 'project-explorer-toggle
      "p s" 'projectile-ag
      "c c" 'avy-goto-char
      "c C" 'avy-goto-char-2
      "c l" 'avy-goto-line
      "c w" 'avy-goto-word-1
      "c d" 'comment-dwim
      "o l" 'org-store-link
      "o a" 'org-agenda
      "o c" 'org-capture
      "g s" 'magit-status
      "g b" 'magit-blame-addition
      "v f" 'vimish-fold
      "v v" 'vimish-fold-delete
      "v m" 'vue-mode
      "m c" 'evil-mc-make-all-cursors
      "m u" 'evil-mc-undo-all-cursors
      "m n" 'evil-mc-make-and-goto-next-match
      "m N" 'evil-mc-skip-and-goto-next-match
      "w j" 'windmove-left
      "w l" 'windmove-right
      "w i" 'windmove-up
      "w k" 'windmove-down
      "x"   'smex
      "y l" '(lambda () (interactive) (load-theme 'mccarthy t))
      "y d" '(lambda () (interactive) (load-theme 'spolsky t))
      "[" 'text-scale-decrease
      "]" 'text-scale-increase
    )
  )
)

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (drag-stuff yasnippet yaml-mode web-mode vue-mode vimish-fold use-package sws-mode sublime-themes spaceline smex scss-mode rvm rainbow-delimiters projectile project-explorer popup php-mode persistent-soft pallet multiple-cursors markdown-mode magit ledger-mode json-mode js2-mode jade-mode highlight-indent-guides haml-mode flycheck-color-mode-line flx-ido evil editorconfig coffee-mode avy autopair))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
