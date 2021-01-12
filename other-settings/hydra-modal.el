;;; A hydra for modal text editing.

(defun hydra-modal--call-body-conditionally ()
  "Call the modal hydra if it hasn't exited properly using the
function `hydra-modal--call-body'.  Otherwise, do nothing."
  (unless hydra-modal--has-exited-p
      (hydra-modal--call-body)))

(defvar hydra-modal--has-exited-p t
  "Indicate whether the modal hydra has exited properly.")

(defvar hydra-modal-cursor-color "#ff0000"
  "The color used by the cursor when hydra modal is active.")

(defvar hydra-modal--original-cursor-color nil
  "The cursor color before calling the modal hydra.")

(defvar hydra-modal-hide-docstring-p nil
  "Indicate whether the modal hydra hides the docstring or not.")

(defun hydra-modal--call-body ()
  "Change the cursor color and set auxiliary variables, then call `hydra-modal/body'."
  (interactive)
  (setq hydra-modal--original-cursor-color (face-attribute 'cursor :background))
  (set-cursor-color hydra-modal-cursor-color)
  (if hydra-modal-hide-docstring-p (setq hydra-is-helpful nil))
  (setq hydra-modal--has-exited-p nil)
  (hydra-modal/body))

(defun hydra-modal--verb-name-to-function (verb)
  (cl-case verb
    ('case #'hydra-modal--verb-case)
    ('mark #'hydra-modal--verb-mark)
    ('delete #'hydra-modal--verb-delete)
    ('copy #'hydra-modal--verb-copy)
    ('cut #'hydra-modal--verb-cut)
    (t (error "Invalid verb `%s'" verb))))

(defun hydra-modal--call-verb (verb)
  "Call the hydra-modal--verbs/body and set auxiliary variables."
  (setq hydra-modal--active-verb-name verb)
  (setq hydra-modal--active-verb (hydra-modal--verb-name-to-function verb))
  (setq hydra-modal--modifier-repeat nil)
  (setq hydra-modal--modifier-backwards nil)
  (setq hydra-modal--modifier-inside nil)
  (setq hydra-modal--modifier-until nil)
  (hydra-modal--verbs/body))

(defhydra hydra-modal--verbs (:color teal
				     :hint nil
				     :post hydra-modal--call-body-conditionally)
  "
%`hydra-modal--active-verb-name
Modifiers:
  _b_:ackwards _0_-_9_:repeat _u_:ntil  _i_:nside
Objects:
  _w_:ord  _l_:ine  _p_:aragraph  _r_:egion
    "
  ("q" nil "quit")
  ("b" (setq hydra-modal--modifier-backwards t) :color red)
  ("i" (progn
	 (call-interactively (lambda (arg) (interactive "c") (setq hydra-modal--modifier-inside arg)))
	 (funcall hydra-modal--active-verb 'another)))
  ("u" (progn
	 (call-interactively (lambda (arg) (interactive "c") (setq hydra-modal--modifier-until arg)))
	 (funcall hydra-modal--active-verb 'another)))
  ("w" (funcall hydra-modal--active-verb 'word))
  ("l" (funcall hydra-modal--active-verb 'line))
  ("p" (funcall hydra-modal--active-verb 'paragraph))
  ("r" (funcall hydra-modal--active-verb 'region))

  ("0" (setq hydra-modal--modifier-repeat (concat hydra-modal--modifier-repeat "0")) :color red)
  ("1" (setq hydra-modal--modifier-repeat (concat hydra-modal--modifier-repeat "1")) :color red)
  ("2" (setq hydra-modal--modifier-repeat (concat hydra-modal--modifier-repeat "2")) :color red)
  ("3" (setq hydra-modal--modifier-repeat (concat hydra-modal--modifier-repeat "3")) :color red)
  ("4" (setq hydra-modal--modifier-repeat (concat hydra-modal--modifier-repeat "4")) :color red)
  ("5" (setq hydra-modal--modifier-repeat (concat hydra-modal--modifier-repeat "5")) :color red)
  ("6" (setq hydra-modal--modifier-repeat (concat hydra-modal--modifier-repeat "6")) :color red)
  ("7" (setq hydra-modal--modifier-repeat (concat hydra-modal--modifier-repeat "7")) :color red)
  ("8" (setq hydra-modal--modifier-repeat (concat hydra-modal--modifier-repeat "8")) :color red)
  ("9" (setq hydra-modal--modifier-repeat (concat hydra-modal--modifier-repeat "9")) :color red))

;;; TODO: move this to somewhere else
(defhydra hydra-indentation (:color blue
                                    :hint nil
                                    :post hydra-modal--call-body-conditionally)
  "
  Indentation:
  _c_:C  _l_:Lisp
    "
  ("q" nil "quit")
  ("l" indent-sexp)
  ("c" c-indent-defun))

(defun hydra-modal--current-line-empty-p ()
  "Returns non-nil if the current line is empty."
  (string-match-p "\\`$" (thing-at-point 'line)))

(defun hydra-modal--navigate-to-specific-char (char &optional increment)
  (or increment (setq increment 1))
  (let ((tmp-pos (point)))
    (while (not (= char (char-after tmp-pos))) (setq tmp-pos (+ increment tmp-pos)))
    (goto-char tmp-pos)))

(defun hydra-modal--verb-mark (object)
  "Mark the object."
  (let ((times (if (not hydra-modal--modifier-repeat)
		   1
		 (string-to-number hydra-modal--modifier-repeat))))
    (cond
     ((eq 'another object)
      (cond
       (hydra-modal--modifier-until
	(call-interactively 'set-mark-command)
	(if hydra-modal--modifier-backwards
	    (hydra-modal--navigate-to-specific-char hydra-modal--modifier-until -1)
	  (hydra-modal--navigate-to-specific-char hydra-modal--modifier-until 1)))

       (hydra-modal--modifier-inside
	(cond
	 ((= hydra-modal--modifier-inside ?w)
	  (backward-word)
	  (call-interactively 'set-mark-command)
	  (mark-word))

	 ((member hydra-modal--modifier-inside '(?\" ?' ?` ?\ ?* ?\\ ?/))
	  (hydra-modal--navigate-to-specific-char hydra-modal--modifier-inside -1)
	  (forward-char 1)
	  (call-interactively 'set-mark-command)
	  (hydra-modal--navigate-to-specific-char hydra-modal--modifier-inside 1))

	 (t (funcall #'(lambda (arg)
			 (let ((delimiters `((,?\( ,?\)) (,?\< ,?\>) (,?\{ ,?\}) (,?\[ ,?\]))))
			   (while delimiters
			     (let ((del-pair (pop delimiters)))
			       (when (member arg del-pair)
				 (hydra-modal--navigate-to-specific-char (car del-pair) -1)
				 (forward-char 1)
				 (call-interactively 'set-mark-command)
				 (hydra-modal--navigate-to-specific-char (cadr del-pair) 1))))))
		     hydra-modal--modifier-inside))))))

     ((eq 'line object)
      (cond (hydra-modal--modifier-backwards
	     (end-of-visual-line)
	     (call-interactively 'set-mark-command)
	     (previous-line (1- times))
	     (beginning-of-visual-line))
	    (t (beginning-of-visual-line)
	       (call-interactively 'set-mark-command)
	       (next-line (1- times))
	       (end-of-visual-line))))

     ((eq 'word object)
      (call-interactively 'set-mark-command)
      (if hydra-modal--modifier-backwards
	  (backward-word times)
	(forward-word times))))))

(defun hydra-modal--verb-delete (object)
  "Change the case of object."
  (interactive)
  (cond
   ((eq 'line object)
    (if (and (hydra-modal--current-line-empty-p) (not hydra-modal--modifier-repeat) (string= hydra-modal--modifier-repeat "1"))
        (kill-line)
      (hydra-modal--verb-mark object)
      (delete-region (region-beginning) (region-end))
      (kill-line)))

   ((or hydra-modal--modifier-until hydra-modal--modifier-inside)
    (hydra-modal--verb-mark 'another)
    (delete-forward-char 1))

   (t (hydra-modal--verb-mark object)
      (delete-forward-char 1))))

(defun hydra-modal--verb-cut (object)
  "Cut the object."
  (interactive)
  (cond
   ((eq 'line object)
    (if (and (hydra-modal--current-line-empty-p) (not hydra-modal--modifier-repeat) (string= hydra-modal--modifier-repeat "1"))
	(kill-line)
      (hydra-modal--verb-mark object)
      (kill-region -1 -1 t)
      (kill-line)))

   ((or hydra-modal--modifier-until hydra-modal--modifier-inside)
    (hydra-modal--verb-mark 'another)
    (kill-region -1 -1 t))

   (t (hydra-modal--verb-mark object)
      (kill-region -1 -1 t))))

(defun hydra-modal--verb-copy (object)
  "Copy the object."
  (interactive)
  (cond
   ((eq 'line object)
    (unless (and (hydra-modal--current-line-empty-p) (not hydra-modal--modifier-repeat) (string= hydra-modal--modifier-repeat "1"))
      (hydra-modal--verb-mark object)
      (let ((str (buffer-substring (region-beginning) (region-end))))
	(remove-text-properties 0 (length str) '(read-only t) str)
	(kill-new str t))
      (deactivate-mark)))

   ((eq 'region object)
    (let ((str (buffer-substring (region-beginning) (region-end))))
      (remove-text-properties 0 (length str) '(read-only t) str)
      (kill-new str t))
    (deactivate-mark))

   ((or hydra-modal--modifier-until hydra-modal--modifier-inside)
    (hydra-modal--verb-mark 'another)
    (let ((str (buffer-substring (region-beginning) (region-end))))
      (remove-text-properties 0 (length str) '(read-only t) str)
      (kill-new str t))
    (deactivate-mark))

   (t (hydra-modal--verb-mark object)
      (let ((str (buffer-substring (region-beginning) (region-end))))
	(remove-text-properties 0 (length str) '(read-only t) str)
	(kill-new str t))
      (deactivate-mark))))

(defun hydra-modal--verb-case (object)
  "Verb `case'.  Change the case of object."
  (interactive)
  (hydra-modal--verb-mark object)
  (hydra-modal-verb--case/body))

(defhydra hydra-modal-verb--case (:color teal
					 :post hydra-modal--call-body-conditionally
					 :hint nil)
  "
  _u_:pcase  _d_:owncase  _c_:apitalize
"
  ("q" nil "quit")
  ("u" (upcase-region (region-beginning) (region-end)))
  ("d" (downcase-region (region-beginning) (region-end)))
  ("c" (capitalize-region (region-beginning) (region-end))))

;; TODO: fix the docstring
(defhydra hydra-modal (:hint nil
			     :color amaranth
			     :post (progn (set-cursor-color hydra-modal--original-cursor-color)
					  (setq hydra-is-helpful t)))
  "
  Navigation:
  _f_: forward     _b_: backward   _F_: fast forward  _B_: fast backward
  _n_: next line   _p_: prev line  _v_: page down     _V_: page up
  _a_: beg line    _e_: end line   _<_: beg buffer    _>_: end buffer
  _s_: save pos    _j_: jump pos   _g_: avy hydra     _S_: swiper
 _C-n_: in sexp   _C-p_: out sexp _C-f_: forw sexp   _C-b_: back sexp
  Edition:
  _y_: popup yank  _Y_: yank       _i_: insert      _u_: undo  _C-s_: save buffer
  _=_: exp region  _M_: MC hydra   _r_: copy regis  _I_: insert regis
 _<DEL>_: del   _<SPC>_: set mark _<tab>_: ind hydra  _<return>_: newline
  Verbs:
  _m_:ark  _d_:elete  _w_:cut  _W_:copy  _c_:ase
    "
  ("q" (setq hydra-modal--has-exited-p t) :exit t)
  ("h" (setq hydra-is-helpful (not hydra-is-helpful)))
  ("o" (progn (end-of-line) (newline)))
  ("F" forward-word)
  ("B" backward-word)
  ("C-f" forward-sexp)
  ("C-b" backward-sexp)
  ("C-n" down-list)
  ("C-p" backward-up-list)
  ("M-f" counsel-find-file)
  ("P" (move-to-window-line 0))
  ("n" next-line)
  ("N" (move-to-window-line -1))
  ("p" previous-line)
  ("+" (enlarge-window 1))
  ("-" (enlarge-window -1))
  ("t" (tiny-expand))
  ("T" (insert "m3\\n10|%d^2 = %(* x x)"))
  ("s" (point-to-register ?g))
  ("j" (jump-to-register ?g))
  ("G" (lambda (arg) (interactive "cInsert char:") (hydra-modal--navigate-to-specific-char arg)))
  ("<SPC>" set-mark-command)
  ("y" popup-kill-ring)
  ("Y" yank)
  ("<tab>" hydra-indentation/body :exit t)
  ("v" scroll-up)
  ("V" scroll-down)
  ("l" recenter-top-bottom)
  ("L" (move-to-window-line (/ (window-height) 2)))
  ("a" beginning-of-line)
  ("r" (lambda (arg) (interactive "cChoose a register:") (copy-to-register arg 1 1 nil t)))
  ("e" end-of-line)
  ("C-e" eval-last-sexp)
  ("f" (when (= (skip-syntax-forward "-") 0) (forward-char 1)))
  ("b" (when (= (skip-syntax-backward "-") 0) (backward-char 1)))
  ("g" hydra-avy--body :exit t)
  ("I" (lambda (arg) (interactive "cChoose a register:") (insert-register arg)))
  ("i" (lambda (txt)
	 (interactive "sQuick insertion:")
	 (insert txt)))
  ("=" er--expand-region)
  ("M" hydra-multiple-cursors--body :exit t)
  ("U" universal-argument)
  ("S" swiper)
  ("C-s" save-buffer)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("u" undo)
  ("<return>" newline)
  ("<DEL>" delete-backward-char)
  ("<deletechar>" delete-forward-char)
  (":" avy-goto-char)
  ("c" (hydra-modal--call-verb 'case) :exit t)
  ("m" (hydra-modal--call-verb 'mark) :exit t)
  ("d" (hydra-modal--call-verb 'delete) :exit t)
  ("W" (hydra-modal--call-verb 'copy) :exit t)
  ("w" (hydra-modal--call-verb 'cut) :exit t))

(global-set-key (kbd "C-c c") 'hydra-modal--call-body)
