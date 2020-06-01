(setq gc-cons-threshold 100000000)

(require 'package)
(setq package-enable-at-startup nil)

(setq package-check-signature nil)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(use-package diminish
  :ensure t
  :defer t)
(require 'bind-key)

(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))


;; C/C++ Development Config(semantic-mode 1) ;; CEDET holdover(global-ede-mode 1) ;; CEDET holdover(setq c-default-style "bsd") ;; BSD/Allman brackets(setq c-basic-offset 4) ;; 4-space indent(add-hook 'c-mode-common-hook 'company-mode)(add-hook 'c-mode-common-hook 'irony-mode)(add-hook 'c-mode-common-hook 'display-line-numbers-mode);; (Conditional) C/C++ Keybinds(add-hook 'c-mode-common-hook 'flycheck-mode)(add-hook 'c-mode-common-hook (lambda () (local-set-key (kbd "<C-tab>") 'company-complete)))(add-hook 'c-mode-common-hook (lambda () (local-set-key (kbd "C-c j") 'find-tag))) ;; Python Development Configs(setenv "PYTHONPATH" (shell-command-to-string "$SHELL --login -c 'echo -n $PYTHONPATH'"))(setq py-python-command "python3")(defcustom python-shell-interpreter "python3" "Default Python interpreter for shell." :type 'string :group 'python)(add-hook 'python-mode-hook 'company-mode)(add-hook 'python-mode-hook 'flycheck-mode)(add-hook 'python-mode-hook 'toggle-truncate-lines)(add-hook 'python-mode-hook 'display-line-numbers-mode) ;; elisp development configs(add-hook 'emacs-lisp-mode-hook 'toggle-truncate-lines)(add-hook 'emacs-lisp-mode-hook 'display-line-numbers-mode) ;; Perl development configs(add-hook 'perl-mode-hook 'company-mode)(add-hook 'perl-mode-hook 'toggle-truncate-lines)(add-hook 'perl-mode-hook 'display-line-numbers-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (kaolin-breeze)))
 '(custom-safe-themes
   (quote
    ("3788e589eb432e6a515d557cbeb8dc4eaca9e00ae54f932b4bd43ed78605532e" "ed573618e4c25fa441f12cbbb786fb56d918f216ae4a895ca1c74f34a19cfe67" "53993d7dc1db7619da530eb121aaae11c57eaf2a2d6476df4652e6f0bd1df740" "a7928e99b48819aac3203355cbffac9b825df50d2b3347ceeec1e7f6b592c647" "58c2c8cc4473c5973e77f4b78a68c0978e68f1ddeb7a1eb34456fce8450be497" "f7b0f2d0f37846ef75157f5c8c159e6d610c3efcc507cbddec789c02e165c121" "b69323309e5839676409607f91c69da2bf913914321c995f63960c3887224848" "a70b47c87e9b0940f6fece46656200acbfbc55e129f03178de8f50934ac89f58" "054e929c1df4293dd68f99effc595f5f7eb64ff3c064c4cfaad186cd450796db" "2f945b8cbfdd750aeb82c8afb3753ebf76a1c30c2b368d9d1f13ca3cc674c7bc" "d04406d092bf8c6b0859a2b83f72db6c6444519a7f1f24199d1ca495393f477b" "0eb3c0868ff890b0c4ee138069ce2a8936a8a69ba150efa6bfb9fb7c05af5ec3" "3ee39fe8a6b6e0f1cbdfa33db1384bc778e3eff4118daa54af7965e9ab8243b3" default)))
 '(org-agenda-files (quote ("~/notes/fit.org" "~/notes/journal.org")))
 '(package-selected-packages
   (quote
    (esup diminish spaceline popup-kill-ring docker ccls dash-functional lsp-mode ox-gfm go-errcheck company-go eyebrowse expand-region smartparens highlight-parentheses go-mode flycheck treemacs buffer-move magit slime-company slime use-package ox-reveal org-bullets multiple-cursors kaolin-themes dashboard counsel company-quickhelp all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
