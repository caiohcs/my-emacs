(require 'package)
(setq package-enable-at-startup nil)

(setq package-check-signature nil)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))


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
    (ox-gfm go-errcheck company-go eyebrowse expand-region smartparens highlight-parentheses go-mode flycheck treemacs buffer-move magit slime-company slime use-package ox-reveal org-bullets multiple-cursors kaolin-themes dashboard counsel company-quickhelp cmake-ide all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
