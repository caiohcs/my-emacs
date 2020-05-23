# My Emacs Configuration

![img](./imgs/my-emacs.png)


# Installation

-   Copy emacs to ~/.emacs
-   Copy settings.org to ~/.emacs.d/settings.org
-   Install all package requirements. For instance, C/C++ utilities requires clang.


# Table of Contents

1.  [My Emacs Configuration](#org3cbc506)
2.  [Installation](#org08d05d5)
3.  [Programming](#org7efa5c6)
    1.  [C and C++](#orgdc76d2b)
    2.  [Golang](#orgdf0c427)
    3.  [Color identifiers](#org7cb0a95)
    4.  [Lisp](#orgafa0f45)
4.  [Spell checking](#org7788971)
    1.  [Config](#org5d4a851)
5.  [Dashboard](#orge022965)
6.  [Org](#org8fc6f0f)
    1.  [Config](#orgb104538)
    2.  [Exporting](#org79ec3d3)
    3.  [Presentations](#org69749a0)
7.  [Company](#orgf238562)
8.  [Magit](#org41b196f)
9.  [Theme](#org1358518)
10. [Treemacs](#org7d91fd0)
11. [Docker](#orgb076562)
12. [Yaml](#org2d6ec0b)
13. [Global](#org5e87293)
    1.  [Emacs completion](#org6efa441)
    2.  [Parentheses](#org51aebd1)
    3.  [Moving buffers](#org5b0614a)
    4.  [Multiple cursors](#org07560be)
    5.  [Text navigation](#org7b74e13)
    6.  [Windows managing](#org70d644f)
    7.  [Smart region expanding](#org4d069f8)
    8.  [Syntax checking](#org3dc4d65)
    9.  [Tool bar, menu bar, line numbering etc](#org456b165)
    10. [Change backup/autosave folder](#org3a65304)
14. [Latin accents](#orgf6d1862)


# Programming


## C and C++


### Requirements

Most of the C and C++ utilities requires clang installed on the system.


### Config

```emacs-lisp
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
```


## Golang


### Requirements

Autocompletion requires gocode, available at <https://github.com/nsf/gocode>. We can install gocode with go get, then we need to create a symbolic link at /usr/bin.


### Config

```emacs-lisp
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
```


## Color identifiers

```emacs-lisp
(use-package color-identifiers-mode
  :disabled
  :ensure t
  :defer 9)
```


## Lisp

I use Steel Bank Common Lisp.


### Config

```emacs-lisp
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
```


# Spell checking

I use aspell for spell checking.


## Config

```emacs-lisp
(defvar ispell-program-name "aspell")
```


# Dashboard

```emacs-lisp
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
```


# Org


## Config

```emacs-lisp
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
```


## Exporting

```emacs-lisp
;; Export to html with syntax highlighting
(use-package htmlize
  :after org
  :ensure t)

;; Export to Markdown with syntax highlighting
(use-package ox-gfm
  :after org
  :ensure t)
```


## Presentations


### Requirements

Requires reveal.js to create html presentations.


### Config

```emacs-lisp
;; Package used to create presentations using reveal.js.
;; Requires the installation of reveaj.js.
(use-package ox-reveal
  :after org
  :ensure t
  :config
  (setq org-reveal-root "file:///home/spvk/notes/presentations/reveal.js"))
```


# Company

```emacs-lisp
(use-package company
  :ensure t
  :defer 2.3
  :init
  (use-package company-quickhelp
    :ensure t)
  :config
  (company-quickhelp-mode))
```


# Magit

```emacs-lisp
(use-package magit
  :ensure t
  :defer 9.2)

(global-set-key (kbd "\C-x g") 'magit-status)
```


# Theme

My favorite themes packages are zerodark-theme, kaolin-themes, moe-theme and dracula-theme.

```emacs-lisp
;; zerodark-theme kaolin-themes moe-theme dracula-theme are nice themes
(use-package kaolin-themes
  :ensure t)
```


# Treemacs

```emacs-lisp
(use-package treemacs
  :ensure t
  :defer 12.4)
```


# Docker

```emacs-lisp
(use-package docker
  :disabled
  :ensure t
  :defer 30)

(use-package dockerfile-mode
  :disabled
  :ensure t
  :defer 9)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
```


# Yaml

```emacs-lisp
(use-package yaml-mode
  :disabled
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode))
```


# Global


## Emacs completion

```emacs-lisp
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
```


## Parentheses

```emacs-lisp
(use-package smartparens
  :ensure t
  :defer 5.1
  :config (smartparens-global-mode))

(use-package highlight-parentheses
  :ensure t
  :defer 12.1
  :config (global-highlight-parentheses-mode))

(defvar show-paren-delay 0)

(show-paren-mode 1)
```


## Moving buffers

```emacs-lisp
(use-package buffer-move
  :ensure t
  :bind
  (("C-c <C-up>"   . buf-move-up)
   ("C-c <C-down>"  . buf-move-down)
   ("C-c <C-left>"  . buf-move-left)
   ("C-c <C-right>" . buf-move-right)))
```


## Multiple cursors

```emacs-lisp
(use-package multiple-cursors
  :ensure t
  :bind (("C-: C-m b" . mc/edit-lines)
	 ("C-: C-m a" . mc/mark-all-like-this)
	 ("C-: C-m >" . mc/mark-next-like-this)
	 ("C-: C-m <" . mc/mark-previous-like-this)))
```


## Text navigation

```emacs-lisp
(use-package avy
  :ensure t
  :bind (("M-s" . avy-goto-word-1)))
```


## Windows managing

```emacs-lisp
(use-package eyebrowse
  :ensure t)
```


## Smart region expanding

```emacs-lisp
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))
```


## Syntax checking

```emacs-lisp
;; Provides some syntax checking
(use-package flycheck
  :ensure t
  :defer 11.3
  :init (global-flycheck-mode))

```


## Tool bar, menu bar, line numbering etc

```emacs-lisp
;;; Variables
(global-visual-line-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode)
(global-set-key (kbd "TAB") 'self-insert-command)
(global-set-key (kbd "\C-c h") 'highlight-symbol-at-point)
```


## Change backup/autosave folder

```emacs-lisp
;;; Change the backup/autosave folder.
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
```


# Latin accents

I created this function to insert the latin accents that I use the most.

```emacs-lisp
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
```