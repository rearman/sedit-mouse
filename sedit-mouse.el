;;; sedit-mouse.el --- structural editing with the mouse -*- lexical-binding: t; -*-

;; Description:

;; The Interlisp-D editor SEDIT, from the Medley InterLisp system developed at
;; Xerox-Parc, had mouse-bound structural editing commands.  This minor mode is
;; an attempt to recreate those within emacs, as much as is practical.  See the
;; 'Bindings' section.  See "https://interlisp.org" for more info on SEDIT and
;; the medley system in general.

;; Terms:

;; This software is released to the public domain.

;; Bindings:

;; For a complete guide of the original SEDIT, see
;; "https://drive.google.com/file/d/12LW5zCZauJvC63NRMJhjNv5qJkuuCflb/view".
;; Page 3 (Printed B-3) is where the mouse bindings start.  The bindings below
;; were taken from the 'SEDITDEMO' available on the medley system at
;; "https://interlisp.org"

;; Click
;; mouse-1: Normal selection
;; mouse-2: Select/expand Structure (via 'expand-region')
;; mouse-3: Expand the region towards the click

;; Mod/Drag & Click
;; S-mouse-2: Copy structure and yank in place
;; C-mouse-2: Kill structure
;; C-S-mouse-2: Move structure via 'mouse-drag-and-drop-region'

;; TODO

;; 1. Make C-S-mouse-2 work as click-and-drag.  Currently works with two clicks
;;    instead of click, hold, release.

;; 2. Make mouse-3 single click closer to SEDIT function - currently covering
;;    the character selection case, but selecting by structures after a
;;    middle click will take some more thought.

;; 3. TESTING - Make *-mouse-2 do something wrt whitespace/newline.  SEDIT has
;;    the interlisp pretty-printer running interactively full time, so copying
;;    in sub-sexps automatically formats the entire sexp as you go.  Using a
;;    function to select the current defun and send it to 'cl-prettyprint'.

;;; Code:

(require 'expand-region)
(require 'cl-extra) ; `cl-prettyprint' has no autoload

(defgroup sedit-mouse nil
  "Structural editing with the mouse, after Medley Interlisp's SEDIT."
  :group 'mouse)

(defun sedit-down-mouse-2 (click)
  (interactive "e")
  (let ((click-pos (cadr (event-start click))))
    (unless (sedit--mouse-inside-region-p click-pos)
      (mouse-set-point click)
      (mouse-drag-region click))))

;; Unused - originally wanted to have drag-and-drop copy on shift, but setting
;; this var to shift interferes with the intended move function of C-S.  Could
;; be used if you make S-<mouse-2> move, and C-S-<mouse-2> copy, but I'm going
;; for a straight recreation.
;; (setq mouse-drag-and-drop-region 'shift)

(defun sedit-mouse-select-structure (_click)
  (interactive "e")
  (er/expand-region 1))

(defun sedit-mouse-copy (click)
  (interactive "e")
  (sedit-mouse-select-structure click)
  (kill-ring-save nil nil t)
  (yank)
  (deactivate-mark)
  (insert-char 32) ; add a space after the yank so prettyprinting works
  (sedit-auto-prettify-sexp))

(defun sedit-mouse-kill (click)
  (interactive "e")
  (sedit-mouse-select-structure click)
  (kill-region nil nil t)
  (sedit-auto-prettify-sexp))

(defun sedit-mouse-move (click)
  (interactive "e")
  (sedit-mouse-select-structure click)
  (mouse-drag-and-drop-region click)
  (deactivate-mark)
  (insert-char 32) ; add a space after the structure so prettyprinting works
  (backward-sexp)
  (insert-char 32) ; add a space before the structure so prettyprinting works
  (sedit-auto-prettify-sexp))

(defun sedit-extend-selection (click)
  (interactive "e")
  (if (not (use-region-p))
      (mouse-set-mark click)
    (let ((click-pos (cadr (event-start click))))
      (cond ((sedit--set-point-p click-pos)
	     (mouse-set-point click))
	    ((sedit--set-mark-p click-pos)
	     (mouse-set-mark click))))))

(defun sedit--mouse-inside-region-p (pos)
  (and (use-region-p) (<= pos (region-end)) (>= pos (region-beginning))))

;; Inverse of previous (may be useful at some point)
(defun sedit--mouse-outside-region-p (pos)
  (and (use-region-p) (or (> pos (region-end)) (< pos (region-beginning)))))

(defun sedit--set-point-p (pos)
  (or (and (> pos (region-end)) (> (point) (mark)))
      (and (< pos (region-beginning)) (< (point) (mark)))))

(defun sedit--set-mark-p (pos)
  (or (and (> pos (region-end)) (> (mark) (point)))
      (and (< pos (region-beginning)) (< (mark) (point)))))

(defun sedit--toplevel-bounds ()
  "Return (START . END) of the top-level form around point, or nil."
  (let* ((ppss (syntax-ppss))
	 (start (or (car (nth 9 ppss))
		    (and (not (nth 8 ppss)) (looking-at "(") (point)))))
    (when start
      (cons start (save-excursion (goto-char start) (forward-sexp) (point))))))

(defun sedit--placeholder-prefix (text)
  "Return a symbol-name prefix that occurs nowhere in TEXT."
  (let ((n 0))
    (while (string-search (format "sedit--ph%d-" n) text)
      (setq n (1+ n)))
    (format "sedit--ph%d-" n)))

(defun sedit--comment-unsafe-p (pos)
  "Non-nil if a placeholder atom at POS would change the form.
That is so after a reader prefix or the dot of a dotted pair."
  (save-excursion (goto-char pos)
		  (forward-comment (- (buffer-size)))
		  (looking-back "\\(?:['`,@]\\|\\(?:^\\|\\s-\\|\\s>\\)\\.\\)"
				(max (point-min) (- (point) 2)))))

(defun sedit--prettify-string (text)
  "Return TEXT, one Lisp form, reflowed by `cl-prettyprint'.
Empty lists stay `()' and comments are kept.  Return nil when a
comment sits where a placeholder would change the form.  Signal an
error when TEXT does not read as exactly one form.  The current
buffer's major mode, `indent-tabs-mode' and `tab-width' set the
layout."
  (let ((mode major-mode)
	(tabs indent-tabs-mode)
	(width tab-width)
	(prefix (sedit--placeholder-prefix text))
	(comments nil)
	(unsafe nil)
	(index 0)
	(start nil))
    (with-temp-buffer (delay-mode-hooks (funcall mode))
		      (setq-local indent-tabs-mode tabs)
		      (setq-local tab-width width)
		      (insert text)
		      ;; Find every comment, last first, so earlier positions stay valid.
		      (goto-char (point-min))
		      (let ((found nil))
			(while (setq start (comment-search-forward (point-max)
								   t))
			  (push start found)
			  (end-of-line))
			(dolist (pos found)
			  (when (sedit--comment-unsafe-p pos)
			    (setq unsafe t))
			  (goto-char pos)
			  (let* ((end (line-end-position))
				 (gap-start (save-excursion (skip-syntax-backward " ")
							    (point)))
				 ;; A trailing comment keeps the exact gap between code and `;'.
				 (gap (save-excursion (goto-char gap-start)
						      (unless (bolp)
							(buffer-substring-no-properties gap-start
											pos)))))
			    (push (list index
					(string-trim-right (buffer-substring-no-properties pos
											   end))
					gap)
				  comments)
			    ;; Swap the comment for a placeholder atom read sees as one element.
			    (delete-region pos end)
			    (insert " "
				    prefix
				    "c"
				    (number-to-string index)
				    " ")
			    (setq index (1+ index)))))
		      (unless unsafe
			;; Swap each empty list outside a string for a placeholder atom.
			(goto-char (point-min))
			(while (re-search-forward "(\\(?:\\s-\\|\\s>\\)*)"
						  nil
						  t)
			  (unless (nth 8
				       (save-excursion (syntax-ppss (match-beginning 0))))
			    (replace-match (concat prefix "nil") t t)))
			(let* ((source (buffer-string))
			       (result (read-from-string source)))
			  (unless (string-empty-p (string-trim (substring source
									  (cdr result))))
			    (error "More than one form"))
			  (erase-buffer)
			  (let ((print-escape-newlines nil)
				(print-length nil)
				(print-level nil)
				(print-quoted t))
			    (cl-prettyprint (car result))))
			(goto-char (point-min))
			(while (search-forward (concat prefix "nil") nil t)
			  (replace-match "()" t t))
			;; Put each comment back, joined to its line or on a line of its own.
			(goto-char (point-min))
			(let ((gaps nil))
			  (while (re-search-forward (concat (regexp-quote prefix)
							    "c\\([0-9]+\\)\\_>")
						    nil
						    t)
			    (let ((entry (assq (string-to-number (match-string 1))
					       comments)))
			      (replace-match "" t t)
			      (if (nth 2 entry)
				  (let ((pos (point)))
				    (skip-syntax-backward " >")
				    (delete-region (point) pos))
				(delete-horizontal-space)
				(unless (bolp) (insert-char 10)))
			      (insert (nth 1 entry))
			      (when (nth 2 entry)
				;; The marker sits at `;' & advances past whitespace inserted there.
				(push (cons (copy-marker (- (point)
							    (length (nth 1
									 entry)))
							 t)
					    (nth 2 entry))
				      gaps))
			      (delete-horizontal-space)
			      (unless (eolp) (insert-char 10))))
			  ;; `indent-sexp' prints no progress message, unlike `indent-region'.
			  (goto-char (point-min))
			  (skip-syntax-forward " >")
			  (indent-sexp)
			  ;; `indent-sexp' moves trailing comments to `comment-column'; undo that.
			  (dolist (gap gaps)
			    (goto-char (car gap))
			    (let ((pos (point)))
			      (skip-syntax-backward " ")
			      (delete-region (point) pos)
			      (insert (cdr gap)))
			    (set-marker (car gap) nil)))
			(string-trim (buffer-string))))))

(defun sedit-auto-prettify-sexp ()
  "Reflow the top-level form around point with `cl-prettyprint'.
Empty lists stay `()' and comments are kept.  A form with a comment
after a reader prefix or a dotted-pair dot is only re-indented.  A
form that does not read is left as it was.  Point ends just after
the form."
  (interactive)
  (condition-case err
      (let ((bounds (sedit--toplevel-bounds)))
	(when bounds
	  (let* ((start (car bounds))
		 (end (copy-marker (cdr bounds)))
		 (old (buffer-substring-no-properties start end))
		 (new (sedit--prettify-string old)))
	    (cond ((null new)
		   (save-excursion (goto-char start) (indent-sexp)))
		  ((not (string= new old))
		   (atomic-change-group (delete-region start end)
					(goto-char start)
					(insert new))))
	    (goto-char end)
	    (set-marker end nil))))
    (error (message "sedit-auto-prettify-sexp: %s; form left as it was"
		    (error-message-string err)))))

(define-minor-mode sedit-mouse-mode
  "Toggles the SEDIT mouse mode.
Intended to be an as-close-as-possible reconstruction of the
mouse bindings from Medley Interlisp's SEDIT editor.

Makes the following bindings:
 Click
mouse-1: Normal selection
mouse-2: Select/expand Structure (via `expand-region')
mouse-3: Set mark and turn on delete-selection-mode

 Mod/Drag & Click
S-mouse-2: Copy structure and yank in place
C-mouse-2: Kill structure
C-S-mouse-2: move structure

You can enable this mode locally in desired buffers, or use
`global-sedit-mouse-mode' to enable it globally."
  :init-value
  ()
  :lighter
  " SMse"
  :keymap
  (list (cons (kbd "<down-mouse-2>") #'sedit-down-mouse-2)
	(cons (kbd "S-<down-mouse-2>") #'sedit-down-mouse-2)
	(cons (kbd "C-<down-mouse-2>") #'sedit-down-mouse-2)
	(cons (kbd "C-S-<down-mouse-2>") #'sedit-down-mouse-2)
	(cons (kbd "<mouse-2>") #'sedit-mouse-select-structure)
	(cons (kbd "<S-mouse-2>") #'sedit-mouse-copy)
	(cons (kbd "<C-mouse-2>") #'sedit-mouse-kill)
	(cons (kbd "<C-S-mouse-2>") #'sedit-mouse-move)
	(cons (kbd "<mouse-3>") #'sedit-extend-selection)))

(defun turn-on-sedit-mouse-mode ()
  (when (not sedit-mouse-mode) (sedit-mouse-mode 1)))

(define-globalized-minor-mode global-sedit-mouse-mode
  sedit-mouse-mode
  turn-on-sedit-mouse-mode
  :group 'sedit-mouse)

(provide 'sedit-mouse)
;;; sedit-mouse.el ends here
