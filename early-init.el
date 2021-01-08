(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)
(setq package-enable-at-startup nil)

;; From doom-emacs
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)
