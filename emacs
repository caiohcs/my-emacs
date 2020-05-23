(require 'package)
(setq package-enable-at-startup nil)

(setq package-check-signature nil)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (agenda . 5)
                          (registers . 5)))
  (setq dashboard-set-init-info t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t))

;; Ivy is a generic completion tool
(use-package ivy
  :ensure t
  :config
  (ivy-mode)
  (use-package swiper
    :ensure t)
  :config
  (use-package counsel
    :ensure t))

(use-package org
  :ensure t
  :defer 17
  :config
  (setq org-startup-indented t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '( (python . t)
      (emacs-lisp . t)
      (lisp . t)
      (C . t))))

(use-package org-bullets
  :requires org
  :ensure t
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :defer 12)

;; Package used to create presentations using reveal.js.
;; Requires the installation of reveaj.js.
(use-package ox-reveal
  :requires org
  :ensure t
  :defer 10.1
  :config
  (setq org-reveal-root "file:///home/spvk/notes/presentations/reveal.js"))

(use-package multiple-cursors
  :ensure t
  :bind (("C-: C-m b" . mc/edit-lines)
	 ("C-: C-m a" . mc/mark-all-like-this)
	 ("C-: C-m >" . mc/mark-next-like-this)
	 ("C-: C-m <" . mc/mark-previous-like-this)))

(use-package company
  :ensure t
  :defer 2.3
  :init
  (use-package company-quickhelp
    :ensure t)
  :config
  (company-quickhelp-mode))

(eval-after-load 'company
  '(define-key company-active-map (kbd "C-c d") #'company-quickhelp-manual-begin))

;;; C/C++
(use-package cmake-ide
  :ensure t
  :defer 7.4)

(add-hook 'c-mode-common-hook 'company-mode)
(cmake-ide-setup)
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
  (aggressive-indent-mode))

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

(use-package buffer-move
  :ensure t
  :defer 11
  :config
  (global-set-key (kbd "C-c <C-up>")     'buf-move-up)
  (global-set-key (kbd "C-c <C-down>")   'buf-move-down)
  (global-set-key (kbd "C-c <C-left>")   'buf-move-left)
  (global-set-key (kbd "C-c <C-right>")  'buf-move-right))

;; zerodark-theme kaolin-themes moe-theme dracula-theme are nice themes
(use-package kaolin-themes
  :ensure t)

(use-package treemacs
  :ensure t
  :defer 12.4)

;; Provides some syntax checking
(use-package flycheck
  :ensure t
  :defer 11.3
  :init (global-flycheck-mode))

;;(use-package lsp-mode
;;  :ensure t
;;  :defer 4)

;(use-package lisp-mode
;  :config
;  (defun my-lisp-mode-hook ()
;     (set (make-local-variable 'company-backends) '(slime-company))
;    (company-mode))
;  (add-hook 'lisp-mode-hook 'my-lisp-mode-hook))

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

(use-package highlight-parentheses
  :ensure t
  :defer 12.1
  :config (global-highlight-parentheses-mode))

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
  :defer 15)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(use-package smartparens
  :ensure t
  :defer 5.1
  :config (smartparens-global-mode))

;;; GLOBAL
(global-visual-line-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(defvar show-paren-delay 0)
(global-linum-mode)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "TAB") 'self-insert-command)
(global-set-key (kbd "\C-c h") 'highlight-symbol-at-point)
(global-set-key (kbd "\C-x g") 'magit-status)

;;; backup/autosave
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


;; (use-package color-identifiers-mode
;;   :ensure t
;;   :defer 9)


;; C/C++ Development Config(semantic-mode 1) ;; CEDET holdover(global-ede-mode 1) ;; CEDET holdover(setq c-default-style "bsd") ;; BSD/Allman brackets(setq c-basic-offset 4) ;; 4-space indent(add-hook 'c-mode-common-hook 'company-mode)(add-hook 'c-mode-common-hook 'irony-mode)(add-hook 'c-mode-common-hook 'display-line-numbers-mode);; (Conditional) C/C++ Keybinds(add-hook 'c-mode-common-hook 'flycheck-mode)(add-hook 'c-mode-common-hook (lambda () (local-set-key (kbd "<C-tab>") 'company-complete)))(add-hook 'c-mode-common-hook (lambda () (local-set-key (kbd "C-c j") 'find-tag))) ;; Python Development Configs(setenv "PYTHONPATH" (shell-command-to-string "$SHELL --login -c 'echo -n $PYTHONPATH'"))(setq py-python-command "python3")(defcustom python-shell-interpreter "python3" "Default Python interpreter for shell." :type 'string :group 'python)(add-hook 'python-mode-hook 'company-mode)(add-hook 'python-mode-hook 'flycheck-mode)(add-hook 'python-mode-hook 'toggle-truncate-lines)(add-hook 'python-mode-hook 'display-line-numbers-mode) ;; elisp development configs(add-hook 'emacs-lisp-mode-hook 'toggle-truncate-lines)(add-hook 'emacs-lisp-mode-hook 'display-line-numbers-mode) ;; Perl development configs(add-hook 'perl-mode-hook 'company-mode)(add-hook 'perl-mode-hook 'toggle-truncate-lines)(add-hook 'perl-mode-hook 'display-line-numbers-mode)

;;; THEME
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#303030" "#ff4b4b" "#d7ff5f" "#fce94f" "#5fafd7" "#d18aff" "#afd7ff" "#c6c6c6"])
 '(custom-enabled-themes (quote (kaolin-valley-light)))
 '(custom-safe-themes
   (quote
    ("58c2c8cc4473c5973e77f4b78a68c0978e68f1ddeb7a1eb34456fce8450be497" "0eb3c0868ff890b0c4ee138069ce2a8936a8a69ba150efa6bfb9fb7c05af5ec3" "d04406d092bf8c6b0859a2b83f72db6c6444519a7f1f24199d1ca495393f477b" "a70b47c87e9b0940f6fece46656200acbfbc55e129f03178de8f50934ac89f58" "b69323309e5839676409607f91c69da2bf913914321c995f63960c3887224848" "f7b0f2d0f37846ef75157f5c8c159e6d610c3efcc507cbddec789c02e165c121" "3ee39fe8a6b6e0f1cbdfa33db1384bc778e3eff4118daa54af7965e9ab8243b3" "1068ae7acf99967cc322831589497fee6fb430490147ca12ca7dd3e38d9b552a" "2f945b8cbfdd750aeb82c8afb3753ebf76a1c30c2b368d9d1f13ca3cc674c7bc" "054e929c1df4293dd68f99effc595f5f7eb64ff3c064c4cfaad186cd450796db" "d0fe9efeaf9bbb6f42ce08cd55be3f63d4dfcb87601a55e36c3421f2b5dc70f3" "7675ffd2f5cb01a7aab53bcdd702fa019b56c764900f2eea0f74ccfc8e854386" "e61752b5a3af12be08e99d076aedadd76052137560b7e684a8be2f8d2958edc3" "ae65ccecdcc9eb29ec29172e1bfb6cadbe68108e1c0334f3ae52414097c501d2" "3788e589eb432e6a515d557cbeb8dc4eaca9e00ae54f932b4bd43ed78605532e" "53993d7dc1db7619da530eb121aaae11c57eaf2a2d6476df4652e6f0bd1df740" "ed573618e4c25fa441f12cbbb786fb56d918f216ae4a895ca1c74f34a19cfe67" "34dc2267328600f3065630e161a8ae59939700684c232073cdd5afbf78456670" "0f1733ad53138ddd381267b4033bcb07f5e75cd7f22089c7e650f1bb28fc67f4" "fa477d10f10aa808a2d8165a4f7e6cee1ab7f902b6853fbee911a9e27cf346bc" "7d4340a89c1f576d1b5dec57635ab93cdc006524bda486b66d01a6f70cffb08e" "e62b66040cb90a4171aa7368aced4ab9d8663956a62a5590252b0bc19adde6bd" "a9d67f7c030b3fa6e58e4580438759942185951e9438dd45f2c668c8d7ab2caf" "53760e1863395dedf3823564cbd2356e9345e6c74458dcc8ba171c039c7144ed" "ff829b1ac22bbb7cee5274391bc5c9b3ddb478e0ca0b94d97e23e8ae1a3f0c3e" "11e0bc5e71825b88527e973b80a84483a2cfa1568592230a32aedac2a32426c1" "51043b04c31d7a62ae10466da95a37725638310a38c471cc2e9772891146ee52" "030346c2470ddfdaca479610c56a9c2aa3e93d5de3a9696f335fd46417d8d3e4" "886fe9a7e4f5194f1c9b1438955a9776ff849f9e2f2bbb4fa7ed8879cdca0631" "332fcf3c7208aca9fab65d54203f78a242482e7fd65f5725a2482c20b1730732" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" default)))
 '(org-agenda-files (quote ("~/projects/lisp/pcl.org" "~/notes/journal.org")))
 '(package-selected-packages
   (quote
    (smartparens smartparens-config org-reveal org-revel free-keys htmlize x-path-walker dashboard zerodark-theme cmake-ide slime-company lsp-mode flycheck treemacs multiple-cursors color-identifiers-mode kaolin-themes docker-compose-mode dockerfile-mode buffer-move magit exec-path-from-shell counsel swiper go-errcheck dracula-theme zenburn-theme use-package slime moe-theme ivy go-mode docker company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
