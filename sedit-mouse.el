;;; sedit-mouse.el --- structural editing with the mouse

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

;; 3. Make S-mouse-2 do something wrt whitespace/newline when yanking.  SEDIT
;;    has the interlisp pretty-printer running interactively full time, so
;;    copying in sub-sexps automatically formats the entire sexp as you go.

;;; Code:

(require 'expand-region)

(defun sedit/down-mouse-2 (click)
  (interactive "e")
  (let ((click-pos (cadr (event-start click))))
    (unless (and (use-region-p)
		 (<= click-pos (region-end))
		 (>= click-pos (region-beginning)))
      (mouse-set-point click)
      (mouse-drag-region click))))

;; Unused - originally wanted to have drag-and-drop copy on shift, but setting
;; this var to shift interferes with the intended move function of C-S.  Could
;; be used if you make S-<mouse-2> move, and C-S-<mouse-2> copy, but I'm going
;; for a straight recreation.
;; (setq mouse-drag-and-drop-region 'shift)

(defun sedit/mouse-2 (click &optional operation)
  (interactive "e")
  (er/expand-region 1)
  (cl-case operation
    (copy (kill-ring-save nil nil t)
	   (yank))
    (kill (kill-region nil nil t))
    (move (mouse-drag-and-drop-region click))))

(defun sedit/mouse-3 (click)
  (interactive "e")
  (sedit/extend-selection click))

(defun sedit/mouse-copy (click)
  (interactive "e")
  (sedit/mouse-2 click 'copy))

(defun sedit/mouse-kill (click)
  (interactive "e")
  (sedit/mouse-2 click 'kill))

(defun sedit/mouse-move (click)
  (interactive "e")
  (sedit/mouse-2 click 'move))

;; Could be used in `sedit/down-mouse-2', but doesn't work for some reason...
(defun sedit/mouse-inside-region-p (pos)
  (or (<= pos (region-end))
      (>= pos (region-beginning))))

;; Inverse of previous (may be useful at some point)
(defun sedit/mouse-outside-region-p (pos)
  (or (> pos (region-end))
      (< pos (region-beginning))))

(defun sedit/set-point-p (pos)
  (or (and (> pos (region-end))
	   (> (point) (mark)))
      (and (< pos (region-beginning))
	   (< (point) (mark)))))

(defun sedit/set-mark-p (pos)
  (or (and (> pos (region-end))
	   (> (mark) (point)))
      (and (< pos (region-beginning))
	   (< (mark) (point)))))

(defun sedit/extend-selection (click)
  (interactive "e")
  (if (not (use-region-p))
      (mouse-set-mark click)
    (let ((click-pos (cadr (event-start click))))
      (if (sedit/set-point-p click-pos)
	  (mouse-set-point click)
	(when (sedit/set-mark-p click-pos)
	  (mouse-set-mark click))))))

(define-minor-mode sedit-mouse-mode
  "Toggles the SEDIT mouse mode.
Intended to be an as-close-as-possible reconstruction of the
mouse bindings from Medley Interlisp's SEDIT editor.

Makes the following bindings:
 Click
mouse-1: Normal selection
mouse-2: Select/expand Structure (via 'expand-region')
mouse-3: Set mark and turn on delete-selection-mode

 Mod/Drag & Click
S-mouse-2: Copy structure and yank in place
C-mouse-2: Kill structure
C-S-mouse-2: move structure

You can enable this mode locally in desired buffers, or use
`global-sedit-mouse-mode' to enable it for all modes that
support it."
  :init-value nil
  :lighter " SMse"
  :keymap
  (list (cons (kbd "<down-mouse-2>") #'sedit/down-mouse-2)
	(cons (kbd "S-<down-mouse-2>") #'sedit/down-mouse-2)
	(cons (kbd "C-<down-mouse-2>") #'sedit/down-mouse-2)
	(cons (kbd "C-S-<down-mouse-2>") #'sedit/down-mouse-2)
	(cons (kbd "<mouse-2>") #'sedit/mouse-2)
	(cons (kbd "<S-mouse-2>") #'sedit/mouse-copy)
	(cons (kbd "<C-mouse-2>") #'sedit/mouse-kill)
	(cons (kbd "<C-S-mouse-2>") #'sedit/mouse-move)
	(cons (kbd "<mouse-3>") #'sedit/mouse-3)))

(defun turn-on-sedit-mouse-mode ()
  (when (not sedit-mouse-mode)
    (sedit-mouse-mode 1)))

(define-globalized-minor-mode global-sedit-mouse-mode
  sedit-mouse-mode turn-on-sedit-mouse-mode)

;; Leaving in case something breaks for `define-minor-mode'
;; (keymap-global-set "<down-mouse-2>" 'sedit/down-mouse-2)
;; (keymap-global-set "S-<down-mouse-2>" 'sedit/down-mouse-2)
;; (keymap-global-set "C-<down-mouse-2>" 'sedit/down-mouse-2)
;; (keymap-global-set "C-S-<down-mouse-2>" 'sedit/down-mouse-2)
;; (keymap-global-set "<mouse-2>" 'sedit/mouse-2)
;; (keymap-global-set "S-<mouse-2>" 'sedit/mouse-copy)
;; (keymap-global-set "C-<mouse-2>" 'sedit/mouse-kill)
;; (keymap-global-set "C-S-<mouse-2>" 'sedit/mouse-move)
;; (keymap-global-set "<mouse-3>" 'sedit/mouse-3)

(provide 'sedit-mouse)
;;; sedit-mouse.el ends here
