# My Emacs configuration

![img](./imgs/my-emacs.png)

This is my Emacs configuration, I use it for:

-   Programming languages (C, C++, Go, Javascript, Python, Lisp, Shell script)
-   Markup languages (Latex, HTML, CSS, Org)
-   Other languages (Makefile, yaml, dockerfile)
-   RSS
-   Media player
-   Window manager
-   File manager
-   Password manager
-   Wiki
-   Blog

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [My Emacs configuration](#my-emacs-configuration)
- [Installation](#installation)
- [Straight](#straight)
- [Programming](#programming)
    - [Magit](#magit)
    - [Company](#company)
    - [Language Server Protocol](#language-server-protocol)
    - [Jump to definition](#jump-to-definition)
        - [Requirements](#requirements)
        - [Config](#config)
    - [C and C++](#c-and-c)
        - [Requirements](#requirements-1)
        - [Config](#config-1)
    - [Golang](#golang)
        - [Requirements](#requirements-2)
        - [Config](#config-2)
    - [Lisp](#lisp)
        - [Config](#config-3)
    - [Scheme](#scheme)
    - [Clojure](#clojure)
    - [Python](#python)
    - [Haskell](#haskell)
    - [Web](#web)
        - [HTML/CSS](#htmlcss)
        - [HTTP](#http)
        - [JS](#js)
    - [Yaml](#yaml)
    - [Docker](#docker)
    - [Yasnippet](#yasnippet)
        - [Config](#config-4)
        - [Tiny](#tiny)
- [Dashboard](#dashboard)
- [Org](#org)
    - [Config](#config-5)
    - [Roam](#roam)
    - [Exporting](#exporting)
    - [Presentations](#presentations)
        - [Requirements](#requirements-3)
        - [Config](#config-6)
- [Plots](#plots)
    - [gnuplot](#gnuplot)
    - [R](#r)
- [Markdown](#markdown)
- [LaTeX](#latex)
- [Theme](#theme)
- [Hydra](#hydra)
    - [Hydra Modal editing](#hydra-modal-editing)
    - [Hydra EMMS](#hydra-emms)
    - [Hydra to launch programs](#hydra-to-launch-programs)
    - [Hydra dictionary](#hydra-dictionary)
    - [Hydra ispell](#hydra-ispell)
    - [Hydra multiple cursors](#hydra-multiple-cursors)
    - [Hydra project](#hydra-project)
    - [Hydra AVY](#hydra-avy)
    - [Hydra macros](#hydra-macros)
    - [Hydra Youtube dl](#hydra-youtube-dl)
    - [Hydra dump jump](#hydra-dump-jump)
    - [Hydra IDE](#hydra-ide)
    - [Hydra lsp](#hydra-lsp)
    - [Hydra Org Roam](#hydra-org-roam)
    - [Hydra frames](#hydra-frames)
    - [Hydra tabs](#hydra-tabs)
- [Ledger](#ledger)
- [Bookmarks](#bookmarks)
- [Prescient](#prescient)
- [Ivy](#ivy)
- [Regular expressions](#regular-expressions)
- [Kill ring](#kill-ring)
- [Which-key](#which-key)
- [Modeline](#modeline)
- [Parentheses](#parentheses)
- [Buffer-move](#buffer-move)
- [Toggle window split](#toggle-window-split)
- [Ace-window](#ace-window)
- [Ace-link](#ace-link)
- [Multiple cursors](#multiple-cursors)
- [Avy](#avy)
- [Undo-tree](#undo-tree)
- [Dired](#dired)
- [Smart region expanding](#smart-region-expanding)
- [Tool bar, menu bar, line numbering etc](#tool-bar-menu-bar-line-numbering-etc)
- [Change backup/autosave folder](#change-backupautosave-folder)
- [Sticky buffers](#sticky-buffers)
- [Read process output](#read-process-output)
- [Spell checking](#spell-checking)
- [Diminish](#diminish)
- [Treemacs](#treemacs)
- [Display commands](#display-commands)
- [Password manager](#password-manager)
- [Debbugs](#debbugs)
- [PDF](#pdf)
    - [Requirements](#requirements-4)
    - [Config](#config-7)
- [EMMS](#emms)
- [RSS](#rss)
    - [Requirements](#requirements-5)
    - [Config](#config-8)
    - [Youtube-dl](#youtube-dl)
    - [EXWM](#exwm)
    - [Screenshots](#screenshots)
- [Bookmarks](#bookmarks-1)
- [Blog](#blog)
- [Webjump](#webjump)
- [Gnus](#gnus)
- [Theme](#theme-1)
- [Auto tangle this file on save](#auto-tangle-this-file-on-save)
- [Reduce gc-threshold](#reduce-gc-threshold)

<!-- markdown-toc end -->


# Installation

-   Copy files to ~/.config/emacs or ~/.emacs.d/

# Straight

```emacs-lisp
(when (< emacs-major-version 27)
  (setq gc-cons-threshold most-positive-fixnum)
  (setq gc-cons-percentage 0.6))

(setq straight-check-for-modifications '(check-on-save))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package diminish
  :straight t
  :defer t)
(require 'bind-key)

(use-package use-package-ensure-system-package
  :straight t)
```


# Programming


## Magit

```emacs-lisp
(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)))
```


## Company

Company is a text completion framework.

```emacs-lisp
(use-package company
  :straight t
  :commands company-mode
  :bind (:map company-active-map
	      ("C-n" . 'company-select-next)
	      ("C-p" . 'company-select-previous))
  :hook ((emacs-lisp-mode . company-mode)
	 (lisp-mode . company-mode)
	 (sly-mrepl-mode . company-mode))
  :config
  (setq company-idle-delay 0)

  ;; Using digits to select company-mode candidates
  ;; https://oremacs.com/2017/12/27/company-numbers/
  (setq company-show-numbers t)

  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora-company-number))
     (number-sequence 0 9))
    (define-key map " " (lambda ()
			  (interactive)
			  (company-abort)
			  (self-insert-command 1)))
    (define-key map (kbd "<return>") nil))

  (defun ora-company-number ()
    "Forward to `company-complete-number'.

Unless the number is potentially part of the candidate.
In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
	   (re (concat "^" company-prefix k)))
      (if (cl-find-if (lambda (s) (string-match re s))
		      company-candidates)
	  (self-insert-command 1)
	(company-complete-number (string-to-number k))))))

(use-package company-quickhelp
  :straight t
  :hook (company-mode . company-quickhelp-local-mode))

(use-package company-prescient
  :straight t
  :after company
  :config
  (company-prescient-mode))
```


## Language Server Protocol

LSP gives Emacs IDE features.

```emacs-lisp
(use-package lsp-mode
  :straight t
  :config (setq lsp-diagnostic-package :flymake)
  :hook
  ((c++-mode . lsp)
   (c-mode . lsp)
   (js-mode . lsp)
   (python-mode . lsp)))
```


## Jump to definition

It's good to have two frameworks for jumping to definition (lsp also provides its own functions).


### Requirements

Silver searcher: <https://github.com/ggreer/the_silver_searcher>


### Config

```emacs-lisp
(use-package dumb-jump
  :straight t
  :defer t
  :config
  (setq dumb-jump-selector 'ivy))
```


## C and C++


### Requirements

-   Requires clang installed on the system.
-   Requires ccls installed on the system. <https://github.com/MaskRay/ccls>

I'm assuming that the ccls binary is at /usr/bin/ccls.


### Config

```emacs-lisp
(use-package ccls
  :after lsp-mode
  :straight t
  :config (setq ccls-executable "/usr/bin/ccls"))

(defun my-c-mode-common-hook ()
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'access-label '/)
  (c-set-offset 'inclass '+)
  (setq c-default-style "bsd"
	c-basic-offset 4
	c-indent-level 4
	c-indent-tabs-mode t
	c-tab-always-indent t
	c++-tab-always-indent t
	tab-width 4
	backward-delete-function nil))

(add-hook 'c++-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
```


## Golang


### Requirements

Autocompletion requires gocode, available at <https://github.com/nsf/gocode>. We can install gocode with go get, then we need to create a symbolic link at /usr/bin.


### Config

```emacs-lisp
(use-package go-mode
  :straight t
  :mode ("\\.go\\'" . go-mode)
  :config
  (defun my-go-mode-hook ()
    (setq tab-width 4)
    (setq gofmt-command "goimports")
    (set (make-local-variable 'company-backends) '(company-go))
    (company-mode))
  (add-hook 'go-mode-hook 'my-go-mode-hook))

(use-package company-go
  :after (company go-mode)
  :straight t)

(use-package go-errcheck
  :after go-mode
  :straight t)
```


## Lisp


### Config

```emacs-lisp
(use-package sly
  :straight t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package suggest
  :straight t
  :defer t)

(use-package lispy
  :straight t
  :hook ((emacs-lisp-mode . lispy-mode)
	 (lisp-mode . lispy-mode)
	 (clojure-mode . lispy-mode)
	 (scheme-mode . lispy-mode)
	 (sly-mrepl-mode . lispy-mode)))
```


## Scheme

```emacs-lisp
(use-package geiser
  :straight t
  :defer run-geiser
  :config
  ;; I have to use the guile2.2 binary because of Fedora
  (setq geiser-guile-binary "guile2.2"))
```


## Clojure

```emacs-lisp
(use-package cider
  :straight t
  :defer t)
```


## Python

```emacs-lisp
(use-package elpy
  :straight t
  :hook (python-mode . elpy-enable)
  :config (setq elpy-rpc-backend "jedi"))

(use-package lpy
  :straight t
  :hook (python-mode . lpy-mode))
```


## Haskell

```emacs-lisp
(use-package haskell-mode
  :straight t
  :mode ("\\.hs\\'" . haskell-mode))
```


## Web


### HTML/CSS

```emacs-lisp
(use-package emmet-mode
  :straight t
  :hook ((web-mode . emmet-mode)
	 (css-mode . emmet-mode)))

(use-package web-mode
  :straight t
  :mode (("\\.html\\'" . web-mode)))

(use-package rainbow-mode
  :straight t
  :diminish rainbow-mode
  :hook ((org-mode . rainbow-mode)
	 (web-mode . rainbow-mode)))

(use-package impatient-mode
  :straight t
  :defer t)
```


### HTTP

```emacs-lisp
(use-package simple-httpd
  :straight t
  :defer t)

(use-package request
  :straight t
  :defer t)
```


### JS

```emacs-lisp
(use-package js2-mode
  :straight t
  :mode ("\\.js\\'" . js2-mode))

(use-package tide
  :straight t
  :hook (js-mode . tide-mode)
  :config
  (tide-setup)
  (setq company-tooltip-align-annotations t)
  (eldoc-mode)
  (tide-hl-identifier-mode)
  (company-mode))
```


## Yaml

```emacs-lisp
(use-package yaml-mode
  :straight t
  :mode ("\\.yml\\'" . yaml-mode))
```


## Docker

```emacs-lisp
(use-package docker
  :straight t
  :commands docker)

(use-package dockerfile-mode
  :straight t
  :mode ("Dockerfile\\'" . dockerfile-mode))
```


## Yasnippet


### Config

```emacs-lisp
(use-package yasnippet
  :straight t
  :hook ((lisp-interaction-mode . yas-minor-mode)
	 (emacs-lisp-mode . yas-minor-mode)
	 (lisp-mode . yas-minor-mode)
	 (org-mode . yas-minor-mode)
	 (c++-mode . yas-minor-mode)
	 (c-mode . yas-minor-mode)))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet
  :defer 3.2
  :config (yas-reload-all))
```


### Tiny

Tiny Is Not Yasnippet

```emacs-lisp
(use-package tiny
  :straight t
  :defer t)
```


# Dashboard

```emacs-lisp
;; Dashboard requirements.
(use-package all-the-icons
  :straight t)

(use-package page-break-lines
  :straight t)

(use-package dashboard
  :straight t
  :after (all-the-icons page-break-lines)
  :config
  ;; Dashboard configuration.
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Welcome to Emacs")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents . 5)
			  (bookmarks . 5)))
  (setq dashboard-set-init-info t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))
```


# Org


## Config

```emacs-lisp
(use-package org
  :straight t
  :mode ("\\.org\\'" . org-mode)
  :diminish org-indent-mode
  :config
  (setq org-agenda-files "~/notes/agenda.org")
  (setq org-directory "~/notes")
  (require 'ox-html)
  (require 'ox-latex)
  (require 'ox-md)
  (setq org-startup-indented t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (shell . t)
     (lisp . t)
     (gnuplot . t)
     (R . t)
     (C . t)))
  (setq org-src-window-setup 'current-window)
  (setq org-agenda-window-setup 'current-window))

;; GNU Emacs minor mode that provides vi-like bindings for org-mode
(use-package worf
  :straight t
  :hook (org-mode . worf-mode))

(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode))

(use-package ob-async
  :straight t
  :defer 7.3)

;; references
(use-package org-ref
  :straight t
  :defer t
  :config
  (setq reftex-default-bibliography '("~/notes/roam/math.bib")
	org-ref-default-bibliography '("~/notes/roam/math.bib")))
```


## Roam

```emacs-lisp
(use-package org-roam
  :straight t
  ;; :ensure-system-package
  ;; ((sqlite3)
  ;;  (graphviz))
  :defer t
  :bind
  (:map org-roam-mode-map
	(("C-c n l" . org-roam)
	 ("C-c n f" . org-roam-find-file)
	 ("C-c n g" . org-roam-graph-show))
	:map org-mode-map
	(("C-c n i" . org-roam-insert))
	(("C-c n I" . org-roam-insert-immediate)))
  :config
  (setq org-roam-directory "~/notes/roam/")
  (setq org-roam-index-file "Index.org")
  (setq org-roam-graph-node-extra-config '(("shape" . "ellipse")
					   ("style" . "rounded,filled")
					   ("fillcolor" . "#EFEFFF")
					   ("color" . "#DEDEFF")
					   ("fontcolor" . "#111111")))
  (setq org-roam-graph-viewer "chromium")
  (setq org-roam-capture-templates
	(list `("d" "default" plain #'org-roam--capture-get-point
		"%?"
		:file-name "%<%Y%m%d%H%M%S>-${slug}"
		:head ,(concat "#+title: ${title}\n"
			       "#+author: \"Caio Henrique\"\n"
			       "#+date: <%<%Y-%m-%d>>\n")
		:unnarrowed t)))
  (require 'org-roam-protocol))

(use-package org-roam-server
  :straight t
  :defer t
  :config
  (setq org-roam-server-host "0.0.0.0"
	org-roam-server-port 8082
	org-roam-server-export-inline-images t
	org-roam-server-authenticate nil
	org-roam-server-network-poll t
	org-roam-server-network-arrows nil
	org-roam-server-network-label-truncate t
	org-roam-server-network-label-truncate-length 60
	org-roam-server-network-label-wrap-length 20))
```


## Exporting

```emacs-lisp
;; Export to html with syntax highlighting
(use-package htmlize
  :straight t
  :defer t)

;; Export to Markdown with syntax highlighting
(use-package ox-gfm
  :straight t
  :defer t)
```


## Presentations


### Requirements

Requires reveal.js to create html presentations.


### Config

```emacs-lisp
;; Package used to create presentations using reveal.js.
;; Requires the installation of reveaj.js.
(use-package ox-reveal
  :straight t
  :defer t
  :config
  (setq org-reveal-root "file:///home/spvk/notes/presentations/reveal.js"))
```


# Plots


## gnuplot

```emacs-lisp
(use-package gnuplot
  :straight t
  :defer t)
```


## R

```emacs-lisp
(use-package ess
  :straight t
  :defer t)
```


# Markdown

```emacs-lisp
(use-package markdown-toc
  :straight t
  :defer t)
```


# LaTeX

```emacs-lisp
(use-package tex
  :straight auctex
  :defer t
  :hook (TeX-mode . (lambda ()
		      (company-mode)))
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

(use-package company-auctex
  :straight t
  :after (auctex company)
  :config
  (company-auctex-init))

(use-package ivy-bibtex
  :straight t
  :after auctex
  :config
  (setq bibtex-completion-bibliography
	'("~/projects/tex/test.bib")))
```


# Theme

My favorite themes packages are zerodark-theme, kaolin-themes, moe-theme and dracula-theme. There is a function bound to <f5> to switch between light mode and dark mode.

```emacs-lisp
(use-package kaolin-themes
  :straight t)

(use-package doom-themes
  :straight t)

(defun toggle-light-dark-theme--custom-choices (theme)
  "Used to create the choice widget options of the
toggle-light-dark-theme custom variables."
  `(const :tag ,(symbol-name theme) ,theme))

(defcustom toggle-light-dark-theme-light-theme 'doom-acario-light
  "The light theme that the function toggle-light-dark-theme will use."
  :type `(choice ,@(mapcar #'toggle-light-dark-theme--custom-choices
			   (custom-available-themes))))

(defcustom toggle-light-dark-theme-dark-theme 'kaolin-galaxy
  "The dark theme that the function toggle-light-dark-theme will use."
  :type `(choice ,@(mapcar #'toggle-light-dark-theme--custom-choices
			   (custom-available-themes))))

(defvar toggle-light-dark-theme--current-theme 'light)

(defun toggle-light-dark-theme ()
  "Disables all custom enabled themes and then toggles between a
light and a dark theme, which are the values of the variables
toggle-light-dark-theme-light-theme and toggle-light-dark-theme-dark-theme."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (powerline-reset)
  (cond ((eq toggle-light-dark-theme--current-theme 'light)
	 (load-theme toggle-light-dark-theme-dark-theme)
	 (setq toggle-light-dark-theme--current-theme 'dark))
	(t
	 (load-theme toggle-light-dark-theme-light-theme)
	 (setq toggle-light-dark-theme--current-theme 'light))))

(define-key-after
  global-map
  [menu-bar options customize customize-toggle-light-dark-theme]
  '("Toggle light and dark theme" . toggle-light-dark-theme)
  'customize-themes)

(global-set-key (kbd "<f5>") #'toggle-light-dark-theme)
```


# Hydra

```emacs-lisp
(use-package hydra
  :straight t
  :defer 2.5)
```


## Hydra Modal editing

![img](./imgs/hydra.png)

```emacs-lisp
(load-file (concat user-emacs-directory "other-settings/hydra-modal.el"))
```


## Hydra EMMS

```emacs-lisp
(defhydra hydra-emms (:color teal
			     :hint nil)
  "
    _p_:laylist  _b_:rowse  _r_:eset  _c_:onnect
    _k_:ill      _u_:pdate
  "
  ("q" nil "quit")
  ("p" emms)
  ("b" emms-smart-browse)
  ("r" emms-player-mpd-update-all-reset-cache)
  ("c" mpd/start-music-daemon)
  ("k" mpd/kill-music-daemon)
  ("u" mpd/update-database))

(global-set-key (kbd "s-m") 'hydra-emms/body)
(global-set-key (kbd "C-: m") 'hydra-emms/body)
```


## Hydra to launch programs

```emacs-lisp
(defun exwm-async-run (name)
  (start-process name nil name))

(defhydra hydra-programs (:color teal
			     :hint nil)
  "
  _B_:rowser _a_:genda    _e_:lfeed _p_:ass     _y_:tdl
  _g_:nus    _D_:ebbugs   _s_:hell  _w_:ebjump  _d_:ictionary
  _i_:spell  _b_:ookmarks _E_:ww    _r_:ecentf  _c_:alc
  "
  ("q" nil "quit")
  ("B" (exwm-async-run "chromium"))
  ("b" hydra-bookmarks/body)
  ("d" hydra-dictionary/body)
  ("a" org-agenda)
  ("i" hydra-ispell/body)
  ("e" elfeed)
  ("E" eww-search-words)
  ("p" pass)
  ("c" calc)
  ("r" counsel-recentf)
  ("g" gnus)
  ("D" debbugs-gnu)
  ("s" eshell)
  ("w" webjump)
  ("y" hydra-ytdl/body))

(global-set-key (kbd "C-c p") 'hydra-programs/body)
```


## Hydra dictionary

```emacs-lisp
(defhydra hydra-dictionary (:color teal
				   :hint nil)
    ("q" nil "quit")
    ("l" dictionary-lookup-definition "lookup")
    ("s" dictionary-search "search")
    ("n" dictionary-new-search "new search")
    ("p" dictionary-previous "previous")
    ("c" dictionary-close "close"))
```


## Hydra ispell

```emacs-lisp
(defhydra hydra-ispell (:color teal
			       :hint nil)
    "
    _r_:egion  _c_:hange-dictionary
    "
    ("q" nil "quit")
    ("r" ispell-region)
    ("c" ispell-change-dictionary))
```


## Hydra multiple cursors

```emacs-lisp
(defhydra hydra-multiple-cursors (:color pink
					 :hint nil
					 :post hydra-modal--call-body-conditionally)
  ("q" nil "quit")
  ("n" mc/mark-next-like-this "next" :column "Mark")
  ("p" mc/mark-previous-like-this "previous")
  ("N" mc/unmark-next-like-this "next" :column "Unmark")
  ("P" mc/unmark-previous-like-this "previous")
  ("r" mc/mark-all-like-this "like region" :column "All like this")
  ("R" mc/mark-all-in-region "in region")
  ("a" mc/edit-beginnings-of-lines "beginning" :column "Lines")
  ("e" mc/edit-ends-of-lines "end")
  ("i n" mc/insert-numbers "numbers" :column "Insert")
  ("i l" mc/insert-letters "letters")
  ("S s" mc/sort-regions "sort" :column "Sort")
  ("S r" mc/reverse-regions "reverse")
  ("s n" mc/skip-to-next-like-this "next" :column "Skip")
  ("s p" mc/skip-to-previous-like-this "previous"))

(global-set-key (kbd "C-c m") 'hydra-multiple-cursors/body)
```


## Hydra project

```emacs-lisp
(defhydra hydra-project (:color teal
				:hint nil)
  "
  _f_: find-file  _g_: regexp  _e_: eshell   _G_: interactive regexp
  _c_: compile    _d_: dired   _r_: replace  _&_: async shell
  _b_: buffers    _p_: projects            ^^_k_: kill buffer
  "
  ("q" nil "quit")
  ("f" project-find-file)
  ("g" project-find-regexp)
  ("b" project-switch-to-buffer)
  ("k" project-kill-buffers)
  ("G" project-search)
  ("r" project-query-replace-regexp)
  ("d" project-dired)
  ("e" project-eshell)
  ("c" project-compile)
  ("p" project-switch-project)
  ("&" project-async-shell-command))

(global-set-key (kbd "C-c P") 'hydra-project/body)
```


## Hydra AVY

```emacs-lisp
(defhydra hydra-avy (:color teal
			    :hint nil
			    :post hydra-modal--call-body-conditionally)
  ("q" nil "quit")
  (":" avy-goto-char-timer "timer" :column "Motion")
  (";" avy-goto-char-2 "char2")
  ("p" avy-goto-word-1-above "above")
  ("n" avy-goto-word-1-below "below")
  ("'" avy-goto-line "line")
  ("p" avy-pop-mark "pop")
  ("m '" avy-move-line "line" :column "Move")
  ("m r" avy-move-region "region")
  ("t" avy-transpose-lines-in-region "transpose")
  ("k r" avy-kill-region "region" :column "Kill")
  ("k '" avy-kill-whole-line "line"))

(global-set-key (kbd "C-:") 'hydra-avy/body)
```


## Hydra macros

```emacs-lisp
(defhydra hydra-macros (:color teal
			       :hint nil)
  "
  _r_: region  _e_: execute   _c_: counter  _f_: format  
  _n_: next    _p_: previous  _i_: insert   _q_: query
 _(_: start  _)_: stop
  "
  ("q" nil "quit")
  ("Q" kbd-macro-query)
  ("(" kmacro-start-macro-or-insert-counter)
  (")" kmacro-end-or-call-macro)
  ("r" apply-macro-to-region-lines)
  ("e" kmacro-end-and-call-macro)
  ("n" kmacro-cycle-ring-next)
  ("p" kmacro-cycle-ring-previous)
  ("i" kmacro-insert-counter)
  ("c" kmacro-set-counter)
  ("q" kbd-macro-query)
  ("f" kmacro-set-format))

(global-set-key (kbd "C-c M") 'hydra-macros/body)
```


## Hydra Youtube dl

```emacs-lisp
(defhydra hydra-ytdl (:color teal
			     :hint nil)
  "
  _d_:ownload   _l_:ist  _o_:pen  _p_:laylist
  "
  ("q" nil "quit")
  ("d" ytdl-download)
  ("o" ytdl-download-open)
  ("l" ytdl-show-list)
  ("p" ytdl-download-playlist))
```


## Hydra dump jump

<https://github.com/jacktasia/dumb-jump>

```emacs-lisp
(defhydra hydra-dumb-jump (:color teal :columns 3)
  "Dumb Jump"
  ("q" nil "quit")
  ("j" dumb-jump-go "Go")
  ("o" dumb-jump-go-other-window "Other window")
  ("e" dumb-jump-go-prefer-external "Go external")
  ("i" dumb-jump-go-prompt "Prompt")
  ("l" dumb-jump-quick-look "Quick look")
  ("b" dumb-jump-back "Back")
  ("x" dumb-jump-go-prefer-external-other-window "Go external other window"))
```


## Hydra IDE

```emacs-lisp
(defhydra hydra-ide (:color teal
			    :hint nil)
  ("q" nil "quit")
  ("l" hydra-lsp/body "lsp" :column "IDE features")
  ("d" hydra-dumb-jump/body "dumb-jump"))

(global-set-key (kbd "C-c i") 'hydra-ide/body)
```


## Hydra lsp

<https://github.com/abo-abo/hydra/wiki/lsp-mode>

```emacs-lisp
(defhydra hydra-lsp (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace))
```


## Hydra Org Roam

```emacs-lisp
(defhydra hydra-roam (:color teal
			     :hint nil)
  "
  _f_:ind file  _i_:nsert  _I_:ndex  _g_:raph
  _c_:apture  _s_:erver
  "
  ("q" nil "quit")
  ("f" org-roam-find-file)
  ("i" org-roam-insert)
  ("I" org-roam-jump-to-index)
  ("g" org-roam-graph)
  ("c" org-roam-capture)
  ("s" org-roam-server-mode))

(global-set-key (kbd "C-c r") 'hydra-roam/body)
```


## Hydra frames

```emacs-lisp
(defhydra hydra-frames-windows (:color teal
				       :hint nil)
  "
  Frame commands:
  _m_: make-frame   _d_: delete-frame          _Z_: suspend-frame
  _q_: quit         _b_: buffer-other-frame    _M_: toggle-maximize
  _o_: other-frame  _f_: find-file-other-frame
  Window commands:
  _0_: delete-window     _1_: delete-other-window  _2_: split below
  _3_: split right       _\\^_: enlarge vertical     _-_: shrink vertical
  _{_: shrink horizontal _}_: enlarge horizontal   _+_: balance-windows
  _a_: ace-window        _t_: toggle-window-split  _O_: other-window
  _k_/_j_/_h_/_l_: windmove-up/down/left/right
  _M-k_/_M-j_/_M-h_/_M-l_: buf-move-up/down/left/right
  Tab hydra: _T_"
  ;; Frame commands
  ("m" make-frame-command)
  ("b" switch-to-buffer-other-frame)
  ("d" delete-frame)
  ("o" other-frame)
  ("f" find-file-other-frame)
  ("Z" suspend-frame)
  ("M" toggle-frame-maximized)
  ;; Window commands
  ("0" delete-window)
  ("1" delete-other-windows)
  ("2" split-window-below)
  ("3" split-window-right)
  ("^" enlarge-window :color pink)
  ("-" shrink-window :color pink)
  ("}" enlarge-window-horizontally :color pink)
  ("{" shrink-window-horizontally :color pink)
  ("+" balance-windows)
  ("t" toggle-window-split)
  ("a" ace-window)
  ("O" other-window)
  ("k" windmove-up)
  ("j" windmove-down)
  ("h" windmove-left)
  ("l" windmove-right)
  ("M-k" buf-move-up)
  ("M-j" buf-move-down)
  ("M-h" buf-move-left)
  ("M-l" buf-move-right)
  ;; Tab hydra
  ("T" hydra-tab/body)
  ("q" nil))

(global-set-key (kbd "C-z") 'hydra-frames-windows/body)
```


## Hydra tabs

```emacs-lisp
(defhydra hydra-tab (:color teal
			    :hint nil)
  "
  tab-bar commands:
  _2_: tab-new  _<tab>_: tab-next      _C-<tab>_: tab-previous  _b_: switch-to-buffer-other-tab
  _u_: tab-undo  _d_: dired-other-tab  _r_: tab-rename         _<RET>_: tab-bar-select-tab-by-name
  "
  ("q" nil "quit")
  ("2" tab-new)
  ("r" tab-rename)
  ("b" switch-to-buffer-other-tab)
  ("f" find-file-other-tab)
  ("<RET>" tab-bar-select-tab-by-name)
  ("C-<tab>" tab-previous)
  ("<tab>" tab-next)
  ("u" tab-undo)
  ("d" dired-other-tab))
```


# Ledger

```emacs-lisp
(use-package ledger-mode
  :straight t
  :mode "\\.dat\\'"
  :config
  (setq ledger-reports
	'(("bal" "gpg2 --decrypt %(ledger-file) 2>/dev/null | %(binary) -f - bal")
	  ("reg" "gpg2 --decrypt %(ledger-file) 2>/dev/null | %(binary) -f - reg")
	  ("payee" "gpg2 --decrypt %(ledger-file) 2>/dev/null | %(binary) -f - reg @%(payee)")
	  ("account" "gpg2 --decrypt %(ledger-file) 2>/dev/null | %(binary) -f - reg %(account)"))))
```


# Bookmarks

```emacs-lisp
(defhydra hydra-bookmarks (:color teal
				  :hint nil)
  ("m" bookmark-set "set")
  ("b" bookmark-jump "jump")
  ("l" list-bookmarks "list")
  ("s" bookmark-save "save")
  ("q" nil "quit"))
```


# Prescient

```emacs-lisp
(use-package prescient
  :straight t
  :defer t
  :config
  (setq prescient-sort-length-enable nil)
  (setq prescient-save-file (concat user-emacs-directory "personal-settings/prescient-save.el"))
  (prescient-persist-mode))
```


# Ivy

```emacs-lisp
;;; Global
;; Ivy is a generic completion tool
(use-package ivy
  :straight t
  :diminish ivy-mode
  :defer 0.9
  :config
  (ivy-mode))

(use-package swiper
  :straight t
  :after ivy
  :bind (("C-s" . swiper)
	 ("C-M-s" . swiper-thing-at-point)))

(use-package counsel
  :straight t
  :after ivy
  :diminish counsel-mode
  :config
  (counsel-mode))

(use-package ivy-avy
  :straight t
  :after (ivy avy))

(use-package ivy-prescient
  :straight t
  :after counsel
  :config
  (ivy-prescient-mode)
  (setq ivy-initial-inputs-alist ivy-prescient--old-initial-inputs-alist))
```


# Regular expressions

```emacs-lisp
(use-package visual-regexp-steroids
  :straight t
  :defer t)
```


# Kill ring

```emacs-lisp
(use-package popup-kill-ring
  :straight t
  :bind (("M-y" . popup-kill-ring)))
```


# Which-key

```emacs-lisp
(use-package which-key
  :straight t
  :defer t)
```


# Modeline

```emacs-lisp
(display-time-mode t)

(use-package spaceline
  :straight t
  :defer 2.2
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator 'arrow)
  (setq spaceline-line-column-p nil)
  (setq spaceline-buffer-size nil)
  (setq spaceline-workspace-numbers-unicode t)
  (setq spaceline-buffer-encoding-abbrev-p nil)
  (spaceline-spacemacs-theme))
```


# Parentheses

```emacs-lisp
(use-package smartparens
  :straight t
  :defer 5.1
  :diminish smartparens-mode
  :config
  (smartparens-global-mode))

(use-package highlight-parentheses
  :straight t
  :defer 5.3
  :diminish highlight-parentheses-mode
  :config (global-highlight-parentheses-mode))

(defvar show-paren-delay 0)
(show-paren-mode t)
```


# Buffer-move

```emacs-lisp
(use-package buffer-move
  :straight t
  :defer t)
```


# Toggle window split

From <https://www.emacswiki.org/emacs/ToggleWindowSplit>

```emacs-lisp
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))
```


# Ace-window

```emacs-lisp
(use-package ace-window
  :straight t
  :defer t)
```


# Ace-link

```emacs-lisp
(use-package ace-link
  :straight t
  :defer 4.1
  :config (ace-link-setup-default))
```


# Multiple cursors

```emacs-lisp
(use-package multiple-cursors
  :straight t
  :defer t
  :init
  (setq mc/list-file (concat user-emacs-directory "other-settings/mc-lists.el")))
```


# Avy

```emacs-lisp
(use-package avy
  :straight t
  :defer t)
```


# Undo-tree

```emacs-lisp
(use-package undo-tree
  :straight t
  :bind (("C-x u" . undo-tree-visualize))
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))
```


# Dired

```emacs-lisp
(use-package dired
  :hook (dired-mode . dired-omit-mode)
  :bind (:map dired-mode-map
	      ("<return>" . dired-find-alternate-file)
	      ("C-<return>" . dired-find-file)
	      ("<dead-circumflex>" . dired-up-directory)
	      ("E" . image-dired)
	      ("J" . dired-omit-mode)))

(use-package dired-x
  :after dired
  :config
  (setq dired-omit-verbose nil)
  (setq dired-omit-files
	"^\\..+$"))

(use-package peep-dired
  :straight t
  :after dired
  :bind (:map dired-mode-map
	      ("P" . 'peep-dired)))

(use-package dired-rainbow
  :straight t
  :after dired
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html"
					"jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib"
				       "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt"
					    "pdb" "pdf" "ps" "rtf" "djvu" "epub"
					    "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown"
					    "md" "mkd" "nfo" "pod" "rst"
					    "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb"
					    "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg"
					 "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico"
					 "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql"
					       "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++"
					    "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp"
					    "go" "f" "for" "ftn" "f90" "f95" "f03" "f08"
					    "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz"
					      "xz" "z" "Z" "jar" "war" "ear" "rar"
					      "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak"
					    "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature"
					     "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast"
					     "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))
```


# Smart region expanding

```emacs-lisp
(use-package expand-region
  :straight t
  :bind (("C-=" . er/expand-region)))
```


# Tool bar, menu bar, line numbering etc

```emacs-lisp
;;; Variables
(global-visual-line-mode)
(global-set-key (kbd "TAB") 'self-insert-command)
(global-set-key (kbd "\C-c h") 'highlight-symbol-at-point)
(setq visible-bell 1)

;; For versions >= 27, this is done on early-init.el
(when (< emacs-major-version 27)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
```


# Change backup/autosave folder

```emacs-lisp
;;; Change the backup/autosave folder.
(defvar backup-dir (expand-file-name (concat user-emacs-directory "backup/")))
(defvar autosave-dir (expand-file-name (concat user-emacs-directory "autosave/")))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
```


# Sticky buffers

<https://lists.gnu.org/archive/html/help-gnu-emacs/2007-05/msg00975.html>

```emacs-lisp
(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))
```


# Read process output

Sets read-process-output-max to 1mb since the default is low. This should improve things that use servers like LSP.

```emacs-lisp
(setq read-process-output-max (* 1024 1024))
```


# Spell checking

```emacs-lisp
(defvar ispell-program-name "aspell")
```


# Diminish

```emacs-lisp
(diminish 'visual-line-mode)
(diminish 'auto-revert-mode)
(diminish 'eldoc-mode)
```


# Treemacs

```emacs-lisp
(use-package treemacs
  :straight t
  :defer t)
```


# Display commands

```emacs-lisp
(use-package command-log-mode
  :straight t
  :defer t)
```


# Password manager

Requires pass.

```emacs-lisp
(use-package pass
  :straight t
  :defer t)
```


# Debbugs

```emacs-lisp
(use-package debbugs
  :straight t
  :defer t)
```


# PDF


## Requirements

See <https://github.com/politza/pdf-tools>.


## Config

```emacs-lisp
(use-package pdf-tools
  :straight t
  :mode ("\\.pdf\\'" . pdf-view-mode))
```


# EMMS

I use mpd/mpc to play music. I used Uncle Dave's config as a reference for the following settings.

```emacs-lisp
(use-package emms
  :straight t
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (emms-all)
  (setq emms-seek-seconds 5)
  (setq emms-player-list '(emms-player-mpd emms-player-mpv))
  (setq emms-info-functions '(emms-info-mpd))
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6601")
  :commands hydra-emms/body
  :bind
  (("<XF86AudioPrev>" . emms-previous)
   ("<XF86AudioNext>" . emms-next)
   ("<XF86AudioPlay>" . emms-pause)
   ("<XF86AudioStop>" . emms-stop)))

(setq mpc-host "localhost:6601")

(defun mpd/start-music-daemon ()
  "Start MPD, connects to it and syncs the metadata cache."
  (interactive)
  (shell-command "mpd")
  (mpd/update-database)
  (emms-player-mpd-connect)
  (emms-cache-set-from-mpd-all)
  (message "MPD Started!"))

(defun mpd/kill-music-daemon ()
  "Stops playback and kill the music daemon."
  (interactive)
  (emms-stop)
  (call-process "killall" nil nil nil "mpd")
  (message "MPD Killed!"))


(defun mpd/update-database ()
  "Updates the MPD database synchronously."
  (interactive)
  (call-process "mpc" nil nil nil "update")
  (message "MPD Database Updated!"))
```


# RSS


## Requirements

cURL.


## Config

```emacs-lisp
(use-package elfeed
  :straight t
  :defer t
  :config (load-file (concat user-emacs-directory "personal-settings/feeds.el")))
```


## Youtube-dl

```emacs-lisp
(use-package ytdl
  :straight t
  :commands ytdl-download
  :config
  (setq ytdl-media-player 'mpv))
```


## EXWM

```emacs-lisp
(use-package exwm
  :straight t
  :disabled t
  :bind (("<XF86AudioRaiseVolume>" . (lambda ()
				       (interactive)
				       (call-process-shell-command "amixer set Master 5%+" nil 0)))
	 ("<XF86AudioLowerVolume>" . (lambda ()
				       (interactive)
				       (call-process-shell-command "amixer set Master 5%-" nil 0)))
	 ("<XF86AudioMute>" . (lambda ()
				(interactive)
				(call-process-shell-command "amixer set Master toggle" nil 0)))
	 ("<print>" . (lambda ()
			(interactive)
			(call-process-shell-command "flameshot gui" nil 0))))
  :config
  (add-hook 'exwm-init-hook (lambda ()
			      (exwm-input-set-simulation-keys
			       '(([?\C-w] . ?\C-x)
				 ([?\M-w] . ?\C-c)
				 ([?\C-y] . ?\C-v)
				 ([?\C-s] . ?\C-f)))))

  (setq exwm-replace nil)
  (require 'exwm-config)
  (setq exwm-workspace-number 4)
  (require 'exwm-randr)
  (exwm-enable)
  (setq exwm-randr-workspace-output-plist '(0 "eDP-1" 1 "HDMI-1"))
  (add-hook 'exwm-randr-screen-change-hook
	    (lambda ()
	      (start-process-shell-command
	       "xrandr" nil "xrandr --output eDP-1 --right-of HDMI-1 --auto")))
  (exwm-randr-enable))
```


## Screenshots

Requires Emacs built with cairo.

```emacs-lisp
(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".svg"))
	 (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))
```


# Bookmarks

```emacs-lisp
(setq bookmark-default-file (concat user-emacs-directory "personal-settings/bookmarks"))
```


# Blog

```emacs-lisp
(use-package org-static-blog
  :straight t
  :defer t
  :config (load-file (concat user-emacs-directory "personal-settings/blog.el")))
```


# Webjump

```emacs-lisp
(use-package webjump
  :defer t
  :config (load-file (concat user-emacs-directory "personal-settings/webjump-sites.el")))
```


# Gnus

```emacs-lisp
(use-package gnus
  :defer t
  :config (setq gnus-init-file (concat user-emacs-directory "personal-settings/gnus.el")))
```


# Theme

```emacs-lisp
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#F5F5F9" "#D70000" "#005F00" "#AF8700" "#1F55A0" "#AF005F" "#007687" "#0F1019"])
 '(custom-enabled-themes '(doom-acario-light use-package))
 '(custom-safe-themes
   '("7e5d400035eea68343be6830f3de7b8ce5e75f7ac7b8337b5df492d023ee8483" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" default))
 '(fci-rule-color "#4E4E4E")
 '(jdee-db-active-breakpoint-face-colors (cons "#D0D0E3" "#009B7C"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#D0D0E3" "#005F00"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#D0D0E3" "#4E4E4E"))
 '(objed-cursor-color "#D70000")
 '(pdf-view-midnight-colors (cons "#0F1019" "#F5F5F9"))
 '(rustic-ansi-faces
   ["#F5F5F9" "#D70000" "#005F00" "#AF8700" "#1F55A0" "#AF005F" "#007687" "#0F1019"])
 '(vc-annotate-background "#F5F5F9")
 '(vc-annotate-color-map
   (list
    (cons 20 "#005F00")
    (cons 40 "#3a6c00")
    (cons 60 "#747900")
    (cons 80 "#AF8700")
    (cons 100 "#bc7900")
    (cons 120 "#c96c00")
    (cons 140 "#D75F00")
    (cons 160 "#c93f1f")
    (cons 180 "#bc1f3f")
    (cons 200 "#AF005F")
    (cons 220 "#bc003f")
    (cons 240 "#c9001f")
    (cons 260 "#D70000")
    (cons 280 "#b41313")
    (cons 300 "#922727")
    (cons 320 "#703a3a")
    (cons 340 "#4E4E4E")
    (cons 360 "#4E4E4E")))
 '(vc-annotate-very-old-color nil))
```


# Auto tangle this file on save

```emacs-lisp
(add-to-list 'safe-local-variable-values
	     '(eval add-hook 'after-save-hook
		    (lambda () (org-babel-tangle))
		    nil t))
```


# Reduce gc-threshold

```emacs-lisp
(setq gc-cons-threshold 100000000)
(setq gc-cons-percentage 0.1)
```
