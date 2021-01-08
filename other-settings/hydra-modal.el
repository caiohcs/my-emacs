;;; A hydra for modal text editing.

(defun hydra-modal--cond-body-call ()
  ""
  (if hydra-modal--inside-body-p
      (hydra-modal--call-body)))

(setq hydra-modal--inside-body-p nil)

(defun hydra-modal--call-body ()
  "Change the cursor color and set auxiliary variables, then call `hydra-modal/body'."
  (interactive)
  (setq *original-cursor-color* (face-attribute 'cursor :background))
  (set-cursor-color "#ff0000")
  (setq hydra-is-helpful nil)
  (setq hydra-modal--inside-body-p t)
  (hydra-modal/body))

(defun hydra-modal--call-operators (operator)
  (setq hydra-modal--call-operators-operator operator)
  (setq hydra-modal--call-operators-repeat nil)
  (setq hydra-modal--call-operators-backwards nil)
  (setq hydra-modal--call-operators-inside nil)
  (setq hydra-modal--call-operators-until nil)
  (hydra-modal--operators/body))

(defhydra hydra-modal--operators (:color blue
                                        :hint nil
                                        :post hydra-modal--cond-body-call)
  "
  _b_:ackwards  _w_:ord  _l_:ine  _p_:aragraph  _r_:egion  _u_:ntil  _i_:nside
    "
  ("b" (setq hydra-modal--call-operators-backwards t) :color red)
  ("i" (progn
         (call-interactively (lambda (arg) (interactive "c") (setq hydra-modal--call-operators-inside arg)))
         (funcall hydra-modal--call-operators-operator 'another)))
  ("u" (progn
         (call-interactively (lambda (arg) (interactive "c") (setq hydra-modal--call-operators-until arg)))
         (funcall hydra-modal--call-operators-operator 'another)))
  ("w" (funcall hydra-modal--call-operators-operator 'word))
  ("l" (funcall hydra-modal--call-operators-operator 'line))
  ("p" (funcall hydra-modal--call-operators-operator 'paragraph))
  ("r" (funcall hydra-modal--call-operators-operator 'region))

  ("0" (setq hydra-modal--call-operators-repeat (concat hydra-modal--call-operators-repeat "0")) :color red)
  ("1" (setq hydra-modal--call-operators-repeat (concat hydra-modal--call-operators-repeat "1")) :color red)
  ("2" (setq hydra-modal--call-operators-repeat (concat hydra-modal--call-operators-repeat "2")) :color red)
  ("3" (setq hydra-modal--call-operators-repeat (concat hydra-modal--call-operators-repeat "3")) :color red)
  ("4" (setq hydra-modal--call-operators-repeat (concat hydra-modal--call-operators-repeat "4")) :color red)
  ("5" (setq hydra-modal--call-operators-repeat (concat hydra-modal--call-operators-repeat "5")) :color red)
  ("6" (setq hydra-modal--call-operators-repeat (concat hydra-modal--call-operators-repeat "6")) :color red)
  ("7" (setq hydra-modal--call-operators-repeat (concat hydra-modal--call-operators-repeat "7")) :color red)
  ("8" (setq hydra-modal--call-operators-repeat (concat hydra-modal--call-operators-repeat "8")) :color red)
  ("9" (setq hydra-modal--call-operators-repeat (concat hydra-modal--call-operators-repeat "9")) :color red))

(defhydra hydra-indentation (:color blue
                                    :hint nil
                                    :post hydra-modal--cond-body-call)
  "
  Hydra for indentation
  _c_:C  _l_:Lisp
    "
  ("q" nil "quit")
  ("l" indent-sexp)
  ("c" c-indent-defun))

(defun current-line-empty-p ()
  "Returns non-nil if the current line is empty."
  (string-match-p "\\`$" (thing-at-point 'line)))

(defun navigate-to-specific-char (char &optional increment)
  (or increment (setq increment 1))
  (let ((tmp-pos (point)))
    (while (not (= char (char-after tmp-pos))) (setq tmp-pos (+ increment tmp-pos)))
    (goto-char tmp-pos)))

(defun hydra-modal--operator-mark (operand)
  (let ((times (if (not hydra-modal--call-operators-repeat)
		   1
		 (string-to-number hydra-modal--call-operators-repeat))))
    (cond
     ((eq 'another operand)
      (cond
       (hydra-modal--call-operators-until
	(call-interactively 'set-mark-command)
	(if hydra-modal--call-operators-backwards
	    (navigate-to-specific-char hydra-modal--call-operators-until -1)
	  (navigate-to-specific-char hydra-modal--call-operators-until 1)))

       (hydra-modal--call-operators-inside
	(cond
	 ((= hydra-modal--call-operators-inside ?w)
	  (backward-word)
	  (call-interactively 'set-mark-command)
	  (mark-word))

	 ((member hydra-modal--call-operators-inside '(?\" ?' ?` ?\ ?* ?\\ ?/))
	  (navigate-to-specific-char hydra-modal--call-operators-inside -1)
	  (forward-char 1)
	  (call-interactively 'set-mark-command)
	  (navigate-to-specific-char hydra-modal--call-operators-inside 1))

	 (t (funcall #'(lambda (arg)
			 (let ((delimiters `((,?\( ,?\)) (,?\< ,?\>) (,?\{ ,?\}) (,?\[ ,?\]))))
			   (while delimiters
			     (let ((del-pair (pop delimiters)))
			       (when (member arg del-pair)
				 (navigate-to-specific-char (car del-pair) -1)
				 (forward-char 1)
				 (call-interactively 'set-mark-command)
				 (navigate-to-specific-char (cadr del-pair) 1))))))
		     hydra-modal--call-operators-inside))))))

     ((eq 'line operand)
      (cond (hydra-modal--call-operators-backwards
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
      (if hydra-modal--call-operators-backwards
	  (backward-word times)
	(forward-word times))))))

(defun hydra-modal--operator-delete (operand)
  (interactive)
  (cond
   ((eq 'line operand)
    (if (and (current-line-empty-p) (not hydra-modal--call-operators-repeat) (string= hydra-modal--call-operators-repeat "1"))
        (kill-line)
      (hydra-modal--operator-mark operand)
      (delete-region (region-beginning) (region-end))
      (kill-line)))

   ((or hydra-modal--call-operators-until hydra-modal--call-operators-inside)
    (hydra-modal--operator-mark 'another)
    (delete-forward-char 1))

   (t (hydra-modal--operator-mark operand)
      (delete-forward-char 1))))

(defun hydra-modal--operator-cut (operand)
  (interactive)
  (cond
   ((eq 'line operand)
    (if (and (current-line-empty-p) (not hydra-modal--call-operators-repeat) (string= hydra-modal--call-operators-repeat "1"))
        (kill-line)
      (hydra-modal--operator-mark operand)
      (kill-region -1 -1 t)
      (kill-line)))

   ((or hydra-modal--call-operators-until hydra-modal--call-operators-inside)
    (hydra-modal--operator-mark 'another)
    (kill-region -1 -1 t))

   (t (hydra-modal--operator-mark operand)
      (kill-region -1 -1 t))))

(defun hydra-modal--operator-copy (operand)
  (interactive)
  (cond
   ((eq 'line operand)
    (unless (and (current-line-empty-p) (not hydra-modal--call-operators-repeat) (string= hydra-modal--call-operators-repeat "1"))
      (hydra-modal--operator-mark operand)
      (let ((str (buffer-substring (region-beginning) (region-end))))
        (remove-text-properties 0 (length str) '(read-only t) str)
        (kill-new str t))
      (deactivate-mark)))

   ((eq 'region operand)
    (let ((str (buffer-substring (region-beginning) (region-end))))
      (remove-text-properties 0 (length str) '(read-only t) str)
      (kill-new str t))
    (deactivate-mark))

   ((or hydra-modal--call-operators-until hydra-modal--call-operators-inside)
    (hydra-modal--operator-mark 'another)
    (let ((str (buffer-substring (region-beginning) (region-end))))
      (remove-text-properties 0 (length str) '(read-only t) str)
      (kill-new str t))
    (deactivate-mark))

   (t (hydra-modal--operator-mark operand)
      (let ((str (buffer-substring (region-beginning) (region-end))))
        (remove-text-properties 0 (length str) '(read-only t) str)
        (kill-new str t))
      (deactivate-mark))))

(defun hydra-modal--operator-case (operand)
  (interactive)
  (hydra-modal--operator-mark operand)
  (let ((hydra-case-arg nil))
    (call-interactively #'(lambda (arg)
                            (interactive "c") (setq hydra-case-arg arg)))
    (cond
     ((= hydra-case-arg ?u)
      (upcase-region (region-beginning) (region-end)))
     ((= hydra-case-arg ?d)
      (downcase-region (region-beginning) (region-end)))
     ((= hydra-case-arg ?c)
      (capitalize-region (region-beginning) (region-end))))))

(defhydra hydra-modal (:hint nil
                                :color amaranth
                                :post (progn (set-cursor-color *original-cursor-color*)
                                             (setq hydra-is-helpful t)))
  "
  Navigation:
  _f_: forward     _b_: backward   _F_: forward word  _B_: backward word
  _n_: next line   _p_: prev line  _v_: page down     _V_: page up
  _a_: beg line    _e_: end line   _<_: beg buffer    _>_: end buffer
  _s_: save pos    _j_: jump pos   _g_: avy hydra     _S_: swiper
 _C-n_: in sexp   _C-p_: out sexp _C-f_: forw sexp   _C-b_: back sexp
  Edition:
  _y_: popup yank  _Y_: yank       _i_: insert      _u_: undo  _C-s_: save buffer
  _=_: exp region  _M_: MC hydra   _r_: copy regis  _I_: insert regis
 _<DEL>_: del   _<SPC>_: set mark _<tab>_: ind hydra  _<return>_: newline
  Operators:
  _m_: mark  _d_: delete  _w_: cut  _W_: copy  _c_: case
    "
  ("<f1>" (setq hydra-modal--inside-body-p nil) :exit t)
  ("q" (setq hydra-modal--inside-body-p nil) :exit t)
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
  ("G" (lambda (arg) (interactive "cInsert char:") (navigate-to-specific-char arg)))
  ("W" (hydra-modal--call-operators 'hydra-modal--operator-copy) :exit t)
  ("<SPC>" set-mark-command)
  ("y" popup-kill-ring)
  ("Y" yank)
  ("<tab>" hydra-indentation--body :exit t)
  ("v" scroll-up)
  ("c" (hydra-modal--call-operators 'hydra-modal--operator-case) :exit t)
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
  ("m" (hydra-modal--call-operators 'hydra-modal--operator-mark) :exit t)
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
  ("M-w" ace-window)
  ("d" (hydra-modal--call-operators 'hydra-modal--operator-delete) :exit t)
  ("w" (hydra-modal--call-operators 'hydra-modal--operator-cut) :exit t))

(global-set-key (kbd "<f1>") 'hydra-modal--call-body)
