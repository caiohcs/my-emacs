# My Emacs configuration

![img](./imgs/my-emacs.png)

My configuration has 59 packages installed. It loads fast, in my machine Doom Emacs takes around 0.6s to start, my configuration takes around 0.8s. I don't care much about startup time since I never close my Emacs, so I haven't tried using advanced optimizations like Byte compilation.

I like Hydra-mode since it's easy to customize, so for fast navigation and quick insertions I use a Hydra. I tap F1 to call the Hydra body, then I can do stuff like:

| Key      | Action                 |
|-------- |---------------------- |
| f        | forward word           |
| b        | backward word          |
| v        | page down              |
| V        | page up                |
| i        | quick insertion        |
| s        | save position          |
| j        | jump to saved position |
| g        | jump to a line number  |
| G        | avy jump to word       |
| SPC      | set mark command       |
| m        | mark operator          |
| M        | multiple cursors block |
| w        | cut operator           |
| W        | kill region and save   |
| y        | popup kill ring        |
| &#x2026; | &#x2026;               |

The delete/mark/cut/copy operators are vim-style, meaning that you can type "mb4l" to mark backwards 4 lines, or "d3w" to delete 3 words forwards.

This nagivation Hydra has the hint menu hidden by default since it can slow down Emacs while holding some key like f to forward-word, so when this Hydra's body is called the cursor color changes to red to indicate that it's active. You can press H to open the hint menu.

There is also a Hydra for windows management (using Eyebrowse) on F2, another one for switching between windows on F3, and one for Avy shorcuts on M-s.


# Installation

-   Copy init.el and settings.org to ~/.emacs.d/
-   Install all package requirements. For instance, C/C++ utilities requires clang.


# Programming


## Company

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


## Language Server Protocol

I use LSP for C and C++.

```emacs-lisp
(use-package lsp-mode
  :ensure t
  :hook
  ((c++-mode . lsp)
  (c-mode . lsp))
  :commands lsp)
```


## Projectile

I think that LSP requires projectile, so it's going to be installed as a dependency anyway.

```emacs-lisp
(use-package projectile
  :ensure t
  :commands projectile-command-map) 
```


## Syntax checking

```emacs-lisp
;; Provides some syntax checking
(use-package flycheck
  :ensure t
  :defer 4.3)
```


## C and C++


### Requirements

-   Requires clang installed on the system.
-   Requires ccls installed on the system. <https://github.com/MaskRay/ccls>

I'm assuming that the ccls binary is at /usr/bin/ccls.


### Config

```emacs-lisp
;;; C/C++
(use-package cmake-ide
  :ensure t
  :defer 7.4
  :config (cmake-ide-setup))

(use-package ccls
  :requires lsp-mode
  :ensure t
  :config
  (setq ccls-executable "/usr/bin/ccls"))

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
  :commands slime
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package slime-company
  :after (slime company)
  :ensure t
  :init
  (slime-setup '(slime-fancy slime-company)))
```


## Yaml

I left the Yaml package disabled, so delete the :disabled line if you want this package.

```emacs-lisp
(use-package yaml-mode
  :disabled
  :ensure t
  :mode ("\\.yml\\'" . yaml-mode))
```


## Docker

I left the Docker packages disabled, so delete the :disabled line if you want these packages.

```emacs-lisp
(use-package docker
  :disabled
  :ensure t
  :commands docker)

(use-package dockerfile-mode
  :disabled
  :ensure t
  :defer 4)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
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
  (setq dashboard-set-file-icons t)


  (defun dashboard-insert-custom (list-size)
    (defun string-centralized (str)
      (let* ((indent
	      (concat "%"
		      (number-to-string
		       (/ (- (window-body-width) (string-width str)) 2))
		      "s"))
	     (str (concat indent str indent)))
	(format str " " " ")))

    (insert (string-centralized (format-time-string "%a %d %b %Y" (current-time))))
    (newline)
    (insert (string-centralized (format-time-string "%H:%M:%S" (current-time)))))

  (add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
  (add-to-list 'dashboard-items '(custom) t)

  ; (cancel-timer *my-timer*)
(setq *ntimescall* 0)
  (defun test-dashboard ()   (setq *ntimescall* (1+ *ntimescall* )) (setq *my-timer* (run-at-time "5 sec" 1 #'(lambda ()
								       (if (string=
									    (buffer-name (window-buffer))
									    "*dashboard*")
									   (dashboard-refresh-buffer)
									 (when (timerp *my-timer*)
									   (cancel-timer *my-timer*))))))))
 ; (add-hook 'dashboard-mode-hook #'test-dashboard)
```


# Org


## Config

```emacs-lisp
;;; org
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :diminish org-indent-mode
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
  :ensure t
  :commands org-export-dispatch)

;; Export to Markdown with syntax highlighting
(use-package ox-gfm
  :after org
  :ensure t
  :commands org-gfm-export-to-markdown)
```


### Presentations

1.  Requirements

    Requires reveal.js to create html presentations.

2.  Config

    ```emacs-lisp
    ;; Package used to create presentations using reveal.js.
    ;; Requires the installation of reveaj.js.
    (use-package ox-reveal
      :after org
      :ensure t
      :commands org-reveal-export-to-html
      :config
      (setq org-reveal-root "file:///home/spvk/notes/presentations/reveal.js"))
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


# Global


## Hydra

```emacs-lisp
(use-package hydra
  :ensure t
  :defer 2.5
  :config
  (defhydra hydra-wind-move (:color amaranth 
			     :hint nil
			     :post hydra-movement/cond-body-call)
    "
  _b_: left wind   _p_: up wind
  _f_: right wind  _n_: down wind
    "
    ("q" nil "quit")
    ("<f3>" nil "quit")
    ("b" windmove-left)
    ("f" windmove-right)
    ("p" windmove-up)
    ("n" windmove-down))
  (global-set-key (kbd "<f3>") 'hydra-wind-move/body)

  (defhydra hydra-eyebrowse (:color amaranth :hint nil)
    "
  %s(eyebrowse-mode-line-indicator)  
  _p_: prev wind   _c_: creat wind  _r_: renam wind
  _n_: next wind   _C_: close wind  _l_: last wind
  _0_: switch to 0      ^^...       _9_: switch to 9   
    "
    ("q" nil "quit")
    ("<f2>" nil "quit")
    ("p" eyebrowse-prev-window-config nil)
    ("n" eyebrowse-next-window-config nil)
    ("l" eyebrowse-last-window-config nil)
    ("r" eyebrowse-rename-window-config nil)
    ("c" eyebrowse-create-window-config nil)
    ("C" eyebrowse-close-window-config nil)
    ("0" eyebrowse-switch-to-window-config-0 nil)
    ("1" eyebrowse-switch-to-window-config-1 nil)
    ("2" eyebrowse-switch-to-window-config-2 nil)
    ("3" eyebrowse-switch-to-window-config-3 nil)
    ("4" eyebrowse-switch-to-window-config-4 nil)
    ("5" eyebrowse-switch-to-window-config-5 nil)
    ("6" eyebrowse-switch-to-window-config-6 nil)
    ("7" eyebrowse-switch-to-window-config-7 nil)
    ("8" eyebrowse-switch-to-window-config-8 nil)
    ("9" eyebrowse-switch-to-window-config-9 nil))
  (global-set-key (kbd "<f2>") 'hydra-eyebrowse/body)

  (defhydra hydra-avy (:color blue 
		       :hint nil
		       :post hydra-movement/cond-body-call)
    "
    _s_: word 1   _n_: word bellow   _p_: word above
    _l_: line     _c_: char timer    _g_: char timer
    "
    ("q" nil "quit")
    ("s" avy-goto-word-1) 
    ("p" avy-goto-word-1-above) 
    ("n" avy-goto-word-1-below) 
    ("l" avy-goto-line) 
    ("c" avy-goto-char-timer) 
    ("g" avy-goto-char-timer))
  (global-set-key (kbd "M-s") 'hydra-avy/body)


  ;;; hydra-movement to make moving around easier
  (defun hydra-movement/cond-body-call ()
    (if hydra-movement/inside-body
	(hydra-movement/call-body)))

  (setq hydra-movement/inside-body nil)

  (defun hydra-movement/call-body () 
    (interactive)
    (set-cursor-color "#ff0000") 
    (setq hydra-is-helpful nil)
    (setq hydra-movement/inside-body t)
    (hydra-movement/body))

(defun hydra-call/hydra-modal-operators (operator)
  (setq hydra-call-operators/operator operator)
  (setq hydra-call-operators/repeat nil)
  (setq hydra-call-operators/backwards nil)
  (hydra-modal-operators/body))

(defhydra hydra-modal-operators (:color blue
				 :hint nil
				 :post hydra-movement/cond-body-call)
  "
_b_:ackwards  _w_:ord  _l_:ine  _p_:aragraph  _r_:egion
  "
  ("b" (setq hydra-call-operators/backwards t) :color red)
  ("w" (funcall hydra-call-operators/operator 'word))
  ("l" (funcall hydra-call-operators/operator 'line))
  ("p" (funcall hydra-call-operators/operator 'paragraph))
  ("r" (funcall hydra-call-operators/operator 'region))

  ("0" (setq hydra-call-operators/repeat (concat hydra-call-operators/repeat "0")) :color red)
  ("1" (setq hydra-call-operators/repeat (concat hydra-call-operators/repeat "1")) :color red)
  ("2" (setq hydra-call-operators/repeat (concat hydra-call-operators/repeat "2")) :color red)
  ("3" (setq hydra-call-operators/repeat (concat hydra-call-operators/repeat "3")) :color red)
  ("4" (setq hydra-call-operators/repeat (concat hydra-call-operators/repeat "4")) :color red)
  ("5" (setq hydra-call-operators/repeat (concat hydra-call-operators/repeat "5")) :color red)
  ("6" (setq hydra-call-operators/repeat (concat hydra-call-operators/repeat "6")) :color red)
  ("7" (setq hydra-call-operators/repeat (concat hydra-call-operators/repeat "7")) :color red)
  ("8" (setq hydra-call-operators/repeat (concat hydra-call-operators/repeat "8")) :color red)
  ("9" (setq hydra-call-operators/repeat (concat hydra-call-operators/repeat "9")) :color red))

(defun current-line-blank-p ()
    (interactive)
    (string-match-p "\\`$" (thing-at-point 'line)))

(defun hydra-modal-operator/mark (operand) 
  (let ((times (if (not hydra-call-operators/repeat) 1 (string-to-number hydra-call-operators/repeat))))
    (cond
     ((eq 'line operand)
      (cond (hydra-call-operators/backwards
	     (end-of-visual-line)
	     (call-interactively 'set-mark-command)
	     (previous-line (1- times))
	     (beginning-of-visual-line))
	    (t (beginning-of-visual-line)
	       (call-interactively 'set-mark-command)
	       (next-line (1- times))
	       (end-of-visual-line))))

     ((eq 'word operand)
      (call-interactively 'set-mark-command)
      (if hydra-call-operators/backwards
	  (backward-word times)
	(forward-word times))))))

(defun hydra-modal-operator/delete (operand)
  (interactive)
  (cond
   ((eq 'line operand)
    (if (and (current-line-blank-p) (not hydra-call-operators/repeat) (string= hydra-call-operators/repeat "1"))
	(kill-line)
      (hydra-modal-operator/mark operand)
      (delete-region (region-beginning) (region-end))
      (kill-line)))

   (t (hydra-modal-operator/mark operand)
      (delete-forward-char 1))))

(defun hydra-modal-operator/cut (operand)
(interactive)
(cond
 ((eq 'line operand)
  (if (and (current-line-blank-p) (not hydra-call-operators/repeat) (string= hydra-call-operators/repeat "1"))
      (kill-line)
    (hydra-modal-operator/mark operand)
    (kill-region -1 -1 t)
    (kill-line)))

 (t (hydra-modal-operator/mark operand)
    (kill-region -1 -1 t))))

;;; Need to finish this for the lines
(defun hydra-modal-operator/copy (operand)
  (interactive)
  (cond
   ((eq 'line operand)
    (if (and (current-line-blank-p) (not hydra-call-operators/repeat) (string= hydra-call-operators/repeat "1"))
	nil
      (hydra-modal-operator/mark operand)
      (let ((str (buffer-substring (region-beginning) (region-end))))
	(remove-text-properties 0 (1- (string-width str)) '(read-only t) str)
	(kill-new str t))
      (deactivate-mark)))

   ((eq 'region operand)
    (let ((str (buffer-substring (region-beginning) (region-end))))
      (remove-text-properties 0 (1- (string-width str)) '(read-only t) str)
      (kill-new str t))
    (deactivate-mark))

   (t (hydra-modal-operator/mark operand)
      (let ((str (buffer-substring (region-beginning) (region-end))))
	(remove-text-properties 0 (1- (string-width str)) '(read-only t) str)
	(kill-new str t))
      (deactivate-mark))))

  (defhydra hydra-movement (:hint nil
			    :color amaranth 
			    :post (progn (set-cursor-color "#000000") 
				    (setq hydra-is-helpful t)))
    "
  _f_: next word   _b_: prev word  _n_: next line   _p_: prev line   _s_: save point
  _j_: jump point  _w_: cut        _W_: copy        _V_: page up     _u_: universal arg
  _y_: yank        _v_: page down  _e_: end line    _S_: swiper      _<SPC>_: mark
  _l_: recenter    _G_: goto line  _a_: beg line    _U_: undo        _i_: insert text
  _F_: next char   _B_: prev char  _g_: avy         _<_: beg buffer  _>_: end buffer
  _=_: exp region  _m_: mark all   _M_: edit lines  _h_: hide hints  _<return>_: newline
    "
    ("<f1>" (setq hydra-movement/inside-body nil) :exit t)
    ("q" (setq hydra-movement/inside-body nil) :exit t)
    ("h" (setq hydra-is-helpful (not hydra-is-helpful)))
    ("f" forward-word)
    ("b" backward-word)
    ("n" next-line)
    ("p" previous-line)
    ("s" (point-to-register 'g))
    ("j" (jump-to-register 'g))
    ("W" (hydra-call/hydra-modal-operators 'hydra-modal-operator/copy) :exit t)
    ("<SPC>" set-mark-command)
    ("y" popup-kill-ring)
    ("v" scroll-up)
    ("V" scroll-down)
    ("l" recenter-top-bottom)
    ("G" goto-line)
    ("a" beginning-of-line)
    ("e" end-of-line)
    ("F" forward-char)
    ("B" backward-char)
    ("g" hydra-avy/body :exit t)
    ("i" (lambda (txt)
	   (interactive "sQuick insertion:")
	   (insert txt)))
    ("=" er/expand-region)
    ("m" (hydra-call/hydra-modal-operators 'hydra-modal-operator/mark) :exit t)
    ("M-m" mc/mark-all-like-this)
    ("M" mc/edit-lines)
    ("u" universal-argument)
    ("S" swiper)
    ("<" beginning-of-buffer)
    (">" end-of-buffer)
    ("U" undo)
    ("<return>" newline)
    ("<DEL>" delete-backward-char)
    ("<deletechar>" delete-forward-char)
    ("M-w" hydra-wind-move/body :exit t)
    ("d" (hydra-call/hydra-modal-operators 'hydra-modal-operator/delete) :exit t)
    ("w" (hydra-call/hydra-modal-operators 'hydra-modal-operator/cut) :exit t))
  (global-set-key (kbd "<f1>") 'hydra-movement/call-body))
```


## Emacs completion

```emacs-lisp
;;; Global
;; Ivy is a generic completion tool
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :defer 1.1
  :config
  (ivy-mode)
  (use-package swiper
    :ensure t
    :bind (("\C-s" . swiper)))
  :config
  (use-package counsel
    :ensure t))
```


## Kill ring

```emacs-lisp
(use-package popup-kill-ring
  :ensure t
  :bind (("M-y" . popup-kill-ring))) 
```


## Modeline

```emacs-lisp
(display-time-mode t)

(use-package spaceline
  :ensure t
  :defer 2.2
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator (quote arrow))
  (setq spaceline-line-column-p nil)
  (setq spaceline-buffer-size nil)
  (setq spaceline-workspace-numbers-unicode t)
  (setq spaceline-buffer-encoding-abbrev-p nil)
  (spaceline-spacemacs-theme))
```


## Parentheses

```emacs-lisp
(use-package smartparens
  :ensure t
  :defer 5.1
  :diminish smartparens-mode
  :config 
  (smartparens-global-mode)
  (sp-local-pair 'org-mode "*" "*")
  (sp-local-pair 'org-mode "_" "_"))

(use-package highlight-parentheses
  :ensure t
  :defer 12.1
  :diminish highlight-parentheses-mode
  :config (global-highlight-parentheses-mode))

(defvar show-paren-delay 0)
(show-paren-mode t)
```


## Buffer moving

```emacs-lisp
(use-package buffer-move
  :ensure t
  :bind
  (("C-c <C-up>"   . buf-move-up)
   ("C-c <C-down>"  . buf-move-down)
   ("C-c <C-left>"  . buf-move-left)
   ("C-c <C-right>" . buf-move-right)))
```


## Windows moving

```emacs-lisp
(global-set-key (kbd "C-c <M-up>") 'windmove-up) 
(global-set-key (kbd "C-c <M-down>") 'windmove-down) 
(global-set-key (kbd "C-c <M-right>") 'windmove-right) 
(global-set-key (kbd "C-c <M-left>") 'windmove-left) 
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
  :commands hydra-avy/body)
```


## Windows management

```emacs-lisp
(use-package eyebrowse
  :ensure t
  :config (eyebrowse-mode t))
```


## Smart region expanding

```emacs-lisp
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))
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


## Read process output

Sets read-process-output-max to 1mb since the default is low. This should improve things that use servers like LSP.

```emacs-lisp
(setq read-process-output-max (* 1024 1024))
```


## Spell checking

I use aspell for spell checking.


### Config

```emacs-lisp
(defvar ispell-program-name "aspell")
```


## Change sexp keyword indentation

This changes the identation style from:

```emacs-lisp
(defhydra foo (:color blue
		      :help nil))
```

to:

```emacs-lisp
(defhydra foo (:color blue
		   :help nil))
```

Code from <https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned> posted by the user Aquaactress.

```emacs-lisp
(advice-add #'calculate-lisp-indent :override #'void~calculate-lisp-indent)

(defun void~calculate-lisp-indent (&optional parse-start)
  "Add better indentation for quoted and backquoted lists."
  ;; This line because `calculate-lisp-indent-last-sexp` was defined with `defvar` 
  ;; with it's value ommited, marking it special and only defining it locally. So  
  ;; if you don't have this, you'll get a void variable error.
  (defvar calculate-lisp-indent-last-sexp)
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  state
	  ;; setting this to a number inhibits calling hook
	  (desired-indent nil)
	  (retry t)
	  calculate-lisp-indent-last-sexp containing-sexp)
      (cond ((or (markerp parse-start) (integerp parse-start))
	     (goto-char parse-start))
	    ((null parse-start) (beginning-of-defun))
	    (t (setq state parse-start)))
      (unless state
	;; Find outermost containing sexp
	(while (< (point) indent-point)
	  (setq state (parse-partial-sexp (point) indent-point 0))))
      ;; Find innermost containing sexp
      (while (and retry
		  state
		  (> (elt state 0) 0))
	(setq retry nil)
	(setq calculate-lisp-indent-last-sexp (elt state 2))
	(setq containing-sexp (elt state 1))
	;; Position following last unclosed open.
	(goto-char (1+ containing-sexp))
	;; Is there a complete sexp since then?
	(if (and calculate-lisp-indent-last-sexp
		 (> calculate-lisp-indent-last-sexp (point)))
	    ;; Yes, but is there a containing sexp after that?
	       (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
					       indent-point 0)))
		 (if (setq retry (car (cdr peek))) (setq state peek)))))
      (if retry
	  nil
	;; Innermost containing sexp found
	(goto-char (1+ containing-sexp))
	(if (not calculate-lisp-indent-last-sexp)
	    ;; indent-point immediately follows open paren.
	       ;; Don't call hook.
	       (setq desired-indent (current-column))
	       ;; Find the start of first element of containing sexp.
	       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
	       (cond ((looking-at "\\s(")
		      ;; First element of containing sexp is a list.
		      ;; Indent under that list.
		      )
		     ((> (save-excursion (forward-line 1) (point))
			 calculate-lisp-indent-last-sexp)
		      ;; This is the first line to start within the containing sexp.
		      ;; It's almost certainly a function call.
		      (if (or
			   ;; Containing sexp has nothing before this line
			   ;; except the first element. Indent under that element.
			   (= (point) calculate-lisp-indent-last-sexp)

			   ;; First sexp after `containing-sexp' is a keyword. This
			   ;; condition is more debatable. It's so that I can have
			   ;; unquoted plists in macros. It assumes that you won't
			   ;; make a function whose name is a keyword.
			   (when-let (char-after (char-after (1+ containing-sexp)))
			     (char-equal char-after ?:))

			   ;; Check for quotes or backquotes around.
			   (let* ((positions (elt state 9))
				  (last (car (last positions)))
				  (rest (reverse (butlast positions)))
				  (any-quoted-p nil)
				  (point nil))
			     (or
			      (when-let (char (char-before last))
				(or (char-equal char ?')
				    (char-equal char ?`)))
			      (progn
				(while (and rest (not any-quoted-p))
				  (setq point (pop rest))
				  (setq any-quoted-p
					(or
					 (when-let (char (char-before point))
					   (or (char-equal char ?')
					       (char-equal char ?`)))
					 (save-excursion
					   (goto-char (1+ point))
					   (looking-at-p
					    "\\(?:back\\)?quote[\t\n\f\s]+(")))))
				any-quoted-p))))
			  ;; Containing sexp has nothing before this line
			     ;; except the first element.  Indent under that element.
			     nil
			     ;; Skip the first element, find start of second (the first
										  ;; argument of the function call) and indent under.
			     (progn (forward-sexp 1)
				    (parse-partial-sexp (point)
							calculate-lisp-indent-last-sexp
							0 t)))
		      (backward-prefix-chars))
		     (t
		      ;; Indent beneath first sexp on same line as
		      ;; `calculate-lisp-indent-last-sexp'.  Again, it's
		      ;; almost certainly a function call.
		      (goto-char calculate-lisp-indent-last-sexp)
		      (beginning-of-line)
		      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
					  0 t)
		      (backward-prefix-chars)))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by lisp-indent-offset
      ;; or if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
	(cond ((elt state 3)
	       ;; Inside a string, don't change indentation.
	       nil)
	      ((and (integerp lisp-indent-offset) containing-sexp)
	       ;; Indent by constant offset
	       (goto-char containing-sexp)
	       (+ (current-column) lisp-indent-offset))
	      ;; in this case calculate-lisp-indent-last-sexp is not nil
	      (calculate-lisp-indent-last-sexp
	       (or
		;; try to align the parameters of a known function
		(and lisp-indent-function
		     (not retry)
		     (funcall lisp-indent-function indent-point state))
		;; If the function has no special alignment
		;; or it does not apply to this argument,
		;; try to align a constant-symbol under the last
		;; preceding constant symbol, if there is such one of
		;; the last 2 preceding symbols, in the previous
		;; uncommented line.
		(and (save-excursion
		       (goto-char indent-point)
		       (skip-chars-forward " \t")
		       (looking-at ":"))
		     ;; The last sexp may not be at the indentation
		     ;; where it begins, so find that one, instead.
		     (save-excursion
		       (goto-char calculate-lisp-indent-last-sexp)
		       ;; Handle prefix characters and whitespace
		       ;; following an open paren.  (Bug#1012)
		       (backward-prefix-chars)
		       (while (not (or (looking-back "^[ \t]*\\|([ \t]+"
						     (line-beginning-position))
				       (and containing-sexp
					    (>= (1+ containing-sexp) (point)))))
			 (forward-sexp -1)
			 (backward-prefix-chars))
		       (setq calculate-lisp-indent-last-sexp (point)))
		     (> calculate-lisp-indent-last-sexp
			(save-excursion
			  (goto-char (1+ containing-sexp))
			  (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
			  (point)))
		     (let ((parse-sexp-ignore-comments t)
			   indent)
		       (goto-char calculate-lisp-indent-last-sexp)
		       (or (and (looking-at ":")
				(setq indent (current-column)))
			   (and (< (line-beginning-position)
				   (prog2 (backward-sexp) (point)))
				(looking-at ":")
				(setq indent (current-column))))
		       indent))
		;; another symbols or constants not preceded by a constant
		;; as defined above.
		normal-indent))
	      ;; in this case calculate-lisp-indent-last-sexp is nil
	      (desired-indent)
	      (t
	       normal-indent))))))
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
	      ((cmp-and-fixcase regionp "ai" "í"))))))
(global-set-key (kbd "C-: C-a") 'my-latin-accents-function)
```
