(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(setq package-check-signature nil)


;;; USE-PACKAGE
(package-install 'use-package)
(eval-when-compile (require 'use-package))

(setq inferior-lisp-program "/usr/bin/sbcl")
(use-package ivy
  :ensure t
  :defer 0.1
  :config
  (ivy-mode)
  (use-package swiper
    :ensure t
    :defer 9)
  :config
  (use-package counsel
    :ensure t
    :defer 13))

(use-package company-quickhelp
  :ensure t
  :defer 7)

(use-package company
  :ensure t
  :defer 5
  :config
  (company-quickhelp-mode))

(use-package magit
  :ensure t
  :defer 6)

(use-package buffer-move
  :ensure t
  :defer 11)

(use-package company-go
  :ensure t
  :defer 7)

(use-package dracula-theme		; moe-theme is nice too
  :ensure t
  :defer 0.2)

(use-package go-mode
  :ensure t
  :defer 15
  :config
  (use-package go-errcheck
    :ensure t)
  (defun my-go-mode-hook ()
    (setq tab-width 4)
    (setq gofmt-command "goimports")
    (set (make-local-variable 'company-backends) '(company-go))
    (company-mode))
  (add-hook 'go-mode-hook 'my-go-mode-hook))

(use-package highlight-parentheses
  :ensure t)

(use-package docker
  :ensure t
  :defer 30)

(use-package dockerfile-mode
  :ensure t
  :defer 9)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(use-package yaml-mode
  :ensure t
  :defer 15)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;;; GLOBAL
(global-highlight-parentheses-mode) ; highlights matching parentheses with different colors 
(global-visual-line-mode)	    ; enables visual line mode
(menu-bar-mode -1)		    ; disables menu bar
(tool-bar-mode -1)		    ; disables tool bar
(scroll-bar-mode -1)		    ; disables scroll bar
(show-paren-mode)		; highlights the matching parentheses
(global-linum-mode)		; enables (absolute) line numbers
(global-set-key "\C-s" 'swiper)	; uses swiper to search instead of isearch
(global-set-key (kbd "TAB") 'self-insert-command) ; enables TAB key to insert tabs
(global-set-key (kbd "\C-c h") 'highlight-symbol-at-point)
(global-set-key (kbd "\C-x g") 'magit-status)

;; Buffer move bindings
(global-set-key (kbd "C-c <C-up>")     'buf-move-up)
(global-set-key (kbd "C-c <C-down>")   'buf-move-down)
(global-set-key (kbd "C-c <C-left>")   'buf-move-left)
(global-set-key (kbd "C-c <C-right>")  'buf-move-right)

;; C/C++ Development Config(semantic-mode 1) ;; CEDET holdover(global-ede-mode 1) ;; CEDET holdover(setq c-default-style "bsd") ;; BSD/Allman brackets(setq c-basic-offset 4) ;; 4-space indent(add-hook 'c-mode-common-hook 'company-mode)(add-hook 'c-mode-common-hook 'irony-mode)(add-hook 'c-mode-common-hook 'display-line-numbers-mode);; (Conditional) C/C++ Keybinds(add-hook 'c-mode-common-hook 'flycheck-mode)(add-hook 'c-mode-common-hook (lambda () (local-set-key (kbd "<C-tab>") 'company-complete)))(add-hook 'c-mode-common-hook (lambda () (local-set-key (kbd "C-c j") 'find-tag))) ;; Python Development Configs(setenv "PYTHONPATH" (shell-command-to-string "$SHELL --login -c 'echo -n $PYTHONPATH'"))(setq py-python-command "python3")(defcustom python-shell-interpreter "python3" "Default Python interpreter for shell." :type 'string :group 'python)(add-hook 'python-mode-hook 'company-mode)(add-hook 'python-mode-hook 'flycheck-mode)(add-hook 'python-mode-hook 'toggle-truncate-lines)(add-hook 'python-mode-hook 'display-line-numbers-mode) ;; elisp development configs(add-hook 'emacs-lisp-mode-hook 'toggle-truncate-lines)(add-hook 'emacs-lisp-mode-hook 'display-line-numbers-mode) ;; Perl development configs(add-hook 'perl-mode-hook 'company-mode)(add-hook 'perl-mode-hook 'toggle-truncate-lines)(add-hook 'perl-mode-hook 'display-line-numbers-mode)

;;; THEME
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#303030" "#ff4b4b" "#d7ff5f" "#fce94f" "#5fafd7" "#d18aff" "#afd7ff" "#c6c6c6"])
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("332fcf3c7208aca9fab65d54203f78a242482e7fd65f5725a2482c20b1730732" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" default)))
 '(package-selected-packages
   (quote
    (docker-compose-mode dockerfile-mode buffer-move magit exec-path-from-shell counsel swiper go-errcheck dracula-theme zenburn-theme use-package slime moe-theme ivy go-mode docker company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
