;;; package --- Init file
;;; Commentary:
;;; Code:
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;; Load theme
(load-theme 'spolsky t)

;; Disable backup and auto save
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Disable menubar and toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Set ergoemacs-mode - keybinding
(require 'ergoemacs-mode)
(ergoemacs-mode 1)

;; Set editorconfig - indenting and trailing spaces control
(require 'editorconfig)
(editorconfig-mode 1)

;; Set helm - files discovery tool
(require 'helm)
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)

;; Set project-explorer - tree view directories
(require 'project-explorer)
(global-set-key [f8] 'project-explorer-toggle)

;; Set spaceline - status bar theme
(require 'spaceline-config)
(spaceline-emacs-theme)

;; Set windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Set linum-mode - view line number
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'scss-mode-hook 'linum-mode)
(add-hook 'web-mode-hook 'linum-mode)
(add-hook 'js2-mode-hook 'linum-mode)

;; Set org - todos and agenda organizer
(require 'org)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-agenda-files (list "~/your-org-agenda-directory"))
(setq org-log-done t)
(setq org-capture-templates
      '(("t" "Personal Todo" entry (file+headline "~/your-org-personal/todos.org" "Tasks")
	 "* TODO [#C] %?")
	("w" "Work Todo" entry (file+headline "~/your-org-work/work.org" "Tasks")
	 "* TODO [#C] %?")
	))

;; Set ledger-mode - financial organizer
(require 'ledger-mode)
(setq ledger-clear-whole-transaction 1)
(add-to-list 'auto-mode-alist '("\\.dat$" . ledger-mode))

;; Set rvm - Ruby Version Manager
(require 'rvm)
(rvm-use-default)

;; Set projectile mode - project's files discovery tool
(require 'projectile)
(projectile-global-mode)

;; Set helm-projectile - integrating helm with projectile
(require 'helm-projectile)
(helm-projectile-on)

;; Set magit - git made easy
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status) (global-set-key (kbd "C-x g") 'magit-status)

;; Set flycheck - linting
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(javascript-jshint)))
(flycheck-add-mode 'javascript-eslint 'web-mode)
(setq-default flycheck-temp-prefix ".flycheck")
(setq-default flycheck-disabled-checkers
	      (append flycheck-disabled-checkers
		      '(json-jsonlist)))

;; Set vimish fold - folding code
(require 'vimish-fold)
(vimish-fold-global-mode 1)
(global-set-key (kbd "C-x v f") #'vimish-fold)
(global-set-key (kbd "C-x v v") #'vimish-fold-delete)

;; Set multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(delete-selection-mode 1)

;; Set highlight indent guides mode
(require 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

;; Set rainbow delimiters mode - highlight braces and quotes depth level
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; enable auto pair - auto complete for braces and quotes
(require 'autopair)
(autopair-global-mode)

;; Set web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(setq js-indent-level 2)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(require 'scss-mode)
(add-hook 'scss-mode-hook
	  (lambda ()
	    (setq css-indent-offset 2))
	  )
(add-hook 'scss-mode-hook 'flycheck-mode)

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
	ad-do-it)
    ad-do-it))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(require 'sws-mode)
(add-to-list 'auto-mode-alist '("\\.jade$" . sws-mode))

(provide 'init)
;;; init.el ends here
