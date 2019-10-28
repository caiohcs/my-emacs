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
(eval-after-load 'company
  '(define-key company-active-map (kbd "C-c d") #'company-quickhelp-manual-begin))

(use-package magit
  :ensure t
  :defer 6)

(use-package buffer-move
  :ensure t
  :defer 11)

(use-package color-identifiers-mode
  :ensure t
  :defer 9)

(use-package company-go
  :ensure t
  :defer 7)

(use-package kaolin-themes    	; moe-theme and dracula-theme are nice too
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
(global-color-identifiers-mode 1)

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
 '(custom-enabled-themes (quote (kaolin-galaxy)))
 '(custom-safe-themes
   (quote
    ("34dc2267328600f3065630e161a8ae59939700684c232073cdd5afbf78456670" "0f1733ad53138ddd381267b4033bcb07f5e75cd7f22089c7e650f1bb28fc67f4" "fa477d10f10aa808a2d8165a4f7e6cee1ab7f902b6853fbee911a9e27cf346bc" "7d4340a89c1f576d1b5dec57635ab93cdc006524bda486b66d01a6f70cffb08e" "e62b66040cb90a4171aa7368aced4ab9d8663956a62a5590252b0bc19adde6bd" "a9d67f7c030b3fa6e58e4580438759942185951e9438dd45f2c668c8d7ab2caf" "53760e1863395dedf3823564cbd2356e9345e6c74458dcc8ba171c039c7144ed" "ff829b1ac22bbb7cee5274391bc5c9b3ddb478e0ca0b94d97e23e8ae1a3f0c3e" "11e0bc5e71825b88527e973b80a84483a2cfa1568592230a32aedac2a32426c1" "51043b04c31d7a62ae10466da95a37725638310a38c471cc2e9772891146ee52" "030346c2470ddfdaca479610c56a9c2aa3e93d5de3a9696f335fd46417d8d3e4" "886fe9a7e4f5194f1c9b1438955a9776ff849f9e2f2bbb4fa7ed8879cdca0631" "332fcf3c7208aca9fab65d54203f78a242482e7fd65f5725a2482c20b1730732" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" default)))
 '(package-selected-packages
   (quote
    (color-identifiers-mode kaolin-themes docker-compose-mode dockerfile-mode buffer-move magit exec-path-from-shell counsel swiper go-errcheck dracula-theme zenburn-theme use-package slime moe-theme ivy go-mode docker company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
