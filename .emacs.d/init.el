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

(require 'editorconfig)
(editorconfig-mode 1)

;; Load theme
(load-theme 'spolsky t)

;; Disable menubar and toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Set toggle for project-explorer
(global-set-key [f8] 'project-explorer-toggle)

;; Set powerline
(require 'spaceline-config)
(spaceline-emacs-theme)

;; Set smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Custom tabs for coffee
;; This gives you a tab of 2 spaces
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("79a3f477ac0cb4a106f78b6109614e991564a5c2467c36e6e854d4bc1102e178" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "e87a2bd5abc8448f8676365692e908b709b93f2d3869c42a4371223aab7d9cf8" "0fb6369323495c40b31820ec59167ac4c40773c3b952c264dd8651a3b704f6b5" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Set windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Set hook for linum-mode
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'scss-mode-hook 'linum-mode)
(add-hook 'web-mode-hook 'linum-mode)
(add-hook 'js2-mode-hook 'linum-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq auto-save-default nil)
(setq make-backup-files nil)

(require 'ergoemacs-mode)
(ergoemacs-mode 1)

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-agenda-files (list "~/org"))
(setq org-log-done t)

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(require 'rvm)
(rvm-use-default)

;; Set projectile mode
(projectile-global-mode)
(setq projectile-completion-system 'ido) ;; Use flx-ido search
(setq projectile-switch-project-action 'projectile-dired) ;; hook projectile

;; Set magit
(global-set-key (kbd "C-x g") 'magit-status) (global-set-key (kbd "C-x g") 'magit-status)

;; Set ido mode
(ido-mode t)

;; Set flycheck
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

;; Set vimish fold
(vimish-fold-global-mode 1)
(global-set-key (kbd "C-x v f") #'vimish-fold)
(global-set-key (kbd "C-x v v") #'vimish-fold-delete)

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(delete-selection-mode 1)

(indent-guide-global-mode)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; enable auto pair
(autopair-global-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(setq js-indent-level 2)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(custom-set-variables
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
)

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
