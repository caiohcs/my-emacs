(require 'package)
(setq package-enable-at-startup nil)

(setq package-check-signature nil)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defvar ispell-program-name "aspell")

(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  ;; Dashboard requirements.
  (use-package page-break-lines
    :ensure t)
  (use-package all-the-icons
    :ensure t)
  ;; Dashboard configuration.
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Welcome to Emacs")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (agenda    . 5)
                          (registers . 5)))
  (setq dashboard-set-init-info t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t))

;;; org
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-startup-indented t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '( (python . t)
      (emacs-lisp . t)
      (lisp . t)
      (C . t))))

(use-package org-bullets
  :after org
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

;; Package used to create presentations using reveal.js.
;; Requires the installation of reveaj.js.
(use-package ox-reveal
  :after org
  :ensure t
  :config
  (setq org-reveal-root "file:///home/spvk/notes/presentations/reveal.js"))

(use-package company
  :ensure t
  :defer 2.3
  :init
  (use-package company-quickhelp
    :ensure t)
  :config
  (company-quickhelp-mode))

;;; C/C++
(use-package cmake-ide
  :ensure t
  :defer 7.4
  :config (cmake-ide-setup))

(defun my-c-mode-common-hook ()
  (c-set-offset 'substatement-open 0)
  (setq  c-default-style "bsd"
	 c-basic-offset 4
	 c-indent-level 4
	 c-indent-tabs-mode t
	 c-tab-always-indent t
	 c++-tab-always-indent t
	 tab-width 4
	 backward-delete-function nil)
  (aggressive-indent-mode)
  (company-mode))

(add-hook 'c++-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;; Lisp
(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package slime-company
  :after company
  :ensure t
  :init
  (slime-setup '(slime-fancy slime-company)))

(use-package magit
  :ensure t
  :defer 9.2)

;; zerodark-theme kaolin-themes moe-theme dracula-theme are nice themes
(use-package kaolin-themes
  :ensure t)

(use-package treemacs
  :ensure t
  :defer 12.4)

;; Requires gocode https://github.com/nsf/gocode
;; After installing gocode, we also have to create a symbolic link
(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :config
  (use-package company-go
    :requires company
    :ensure t)
  (use-package go-errcheck
    :ensure t)
  (defun my-go-mode-hook ()
    (setq tab-width 4)
    (setq gofmt-command "goimports")
    (set (make-local-variable 'company-backends) '(company-go))
    (company-mode))
  (add-hook 'go-mode-hook 'my-go-mode-hook))

(use-package docker
  :disabled
  :ensure t
  :defer 30)

(use-package dockerfile-mode
  :disabled
  :ensure t
  :defer 9)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(use-package yaml-mode
  :disabled
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode))

;;; Global
;; Ivy is a generic completion tool
(use-package ivy
  :ensure t
  :config
  (ivy-mode)
  (use-package swiper
    :ensure t
    :bind (("\C-s" . swiper)))
  :config
  (use-package counsel
    :ensure t))

(use-package smartparens
  :ensure t
  :defer 5.1
  :config (smartparens-global-mode))

(use-package highlight-parentheses
  :ensure t
  :defer 12.1
  :config (global-highlight-parentheses-mode))

(use-package buffer-move
  :ensure t
  :bind
  (("C-c <C-up>"   . buf-move-up)
   ("C-c <C-down>"  . buf-move-down)
   ("C-c <C-left>"  . buf-move-left)
   ("C-c <C-right>" . buf-move-right)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-: C-m b" . mc/edit-lines)
	 ("C-: C-m a" . mc/mark-all-like-this)
	 ("C-: C-m >" . mc/mark-next-like-this)
	 ("C-: C-m <" . mc/mark-previous-like-this)))

(use-package avy
  :ensure t
  :bind (("M-s" . avy-goto-word-1)))

(use-package eyebrowse
  :ensure t)

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

;; Provides some syntax checking
(use-package flycheck
  :ensure t
  :defer 11.3
  :init (global-flycheck-mode))

(use-package color-identifiers-mode
  :disabled
  :ensure t
  :defer 9)

;;; Variables
(global-visual-line-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(defvar show-paren-delay 0)
(global-linum-mode)
(global-set-key (kbd "TAB") 'self-insert-command)
(global-set-key (kbd "\C-c h") 'highlight-symbol-at-point)
(global-set-key (kbd "\C-x g") 'magit-status)

;;; Change the backup/autosave folder.
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; latin accents
(defun my-latin-accents-function (start end)
  (interactive "r")
  (defun cmp-and-fixcase (reg cmp out)
    (let ((case-fold-search t))
      (if (string-match-p reg cmp)
       	  (let ((case-fold-search nil))
	    (if (string-match-p "\\`[a-z]*\\'" reg)
               	(progn (delete-region start end) (insert out))
              (progn (delete-region start end) (insert (upcase out))))) nil)))
  (if (use-region-p)
      (let ((regionp (buffer-substring start end)))
	(cond ((cmp-and-fixcase regionp "aa" "á"))
	      ((cmp-and-fixcase regionp "ga" "à"))
	      ((cmp-and-fixcase regionp "ta" "ã"))
	      ((cmp-and-fixcase regionp "ae" "é"))
	      ((cmp-and-fixcase regionp "ge" "è"))
	      ((cmp-and-fixcase regionp "te" "ẽ"))
	      ((cmp-and-fixcase regionp "ce" "ê"))
	      ((cmp-and-fixcase regionp "co" "ô"))
	      ((cmp-and-fixcase regionp "to" "õ"))
	      ((cmp-and-fixcase regionp "ai" "í")) 
	      ))))
(global-set-key (kbd "C-: C-a") 'my-latin-accents-function)

;; C/C++ Development Config(semantic-mode 1) ;; CEDET holdover(global-ede-mode 1) ;; CEDET holdover(setq c-default-style "bsd") ;; BSD/Allman brackets(setq c-basic-offset 4) ;; 4-space indent(add-hook 'c-mode-common-hook 'company-mode)(add-hook 'c-mode-common-hook 'irony-mode)(add-hook 'c-mode-common-hook 'display-line-numbers-mode);; (Conditional) C/C++ Keybinds(add-hook 'c-mode-common-hook 'flycheck-mode)(add-hook 'c-mode-common-hook (lambda () (local-set-key (kbd "<C-tab>") 'company-complete)))(add-hook 'c-mode-common-hook (lambda () (local-set-key (kbd "C-c j") 'find-tag))) ;; Python Development Configs(setenv "PYTHONPATH" (shell-command-to-string "$SHELL --login -c 'echo -n $PYTHONPATH'"))(setq py-python-command "python3")(defcustom python-shell-interpreter "python3" "Default Python interpreter for shell." :type 'string :group 'python)(add-hook 'python-mode-hook 'company-mode)(add-hook 'python-mode-hook 'flycheck-mode)(add-hook 'python-mode-hook 'toggle-truncate-lines)(add-hook 'python-mode-hook 'display-line-numbers-mode) ;; elisp development configs(add-hook 'emacs-lisp-mode-hook 'toggle-truncate-lines)(add-hook 'emacs-lisp-mode-hook 'display-line-numbers-mode) ;; Perl development configs(add-hook 'perl-mode-hook 'company-mode)(add-hook 'perl-mode-hook 'toggle-truncate-lines)(add-hook 'perl-mode-hook 'display-line-numbers-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (kaolin-valley-light)))
 '(custom-safe-themes
   (quote
    ("3ee39fe8a6b6e0f1cbdfa33db1384bc778e3eff4118daa54af7965e9ab8243b3" default)))
 '(package-selected-packages
   (quote
    (go-errcheck company-go eyebrowse expand-region smartparens highlight-parentheses go-mode flycheck treemacs buffer-move magit slime-company slime use-package ox-reveal org-bullets multiple-cursors kaolin-themes dashboard counsel company-quickhelp cmake-ide all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
