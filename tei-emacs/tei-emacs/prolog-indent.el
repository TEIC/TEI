;;; prolog-indent.el --- Indentation of Prolog code.
;;;****************************************************************************
;;; $Id$
;;;****************************************************************************
;;;
;;; Description:
;;;
;;;	This file implements additional routines to the prolog mode
;;;	supplied with Emacs. The indentation is improved and some
;;;	electric keys are activated.
;;;
;;; Installation:
;;;	Put the following lines in your .emacs file:
;;;
;;;	    (require 'prolog-indent)
;;;
;;;	This assumes that the file prolog-indent.el can be found on
;;;	the load-path:
;;;
;;;	    (require 'cl)
;;;	    (setq load-path (adjoin "PROLOG-INDENT-DIRECTORY" load-path))
;;;
;;; Bugs and Problems:
;;;	Comments inside Prolog goals are not supported.
;;;	Things might get mixed up for infix operators.
;;;
;;; To do:
;;;
;;; Changes:
;;;
;;; Author:	
;;;	Gerd Neugebauer
;;;	Mainzer Str. 16
;;;	56321 Rhens (Germany)
;;;	Net: gerd@uni-koblenz.de
;;;
;;;****************************************************************************
;;; LCD Archive Entry:
;;; Not yet.
;;;****************************************************************************
;;;
;;; Copyright (C) 1995,1996 Gerd Neugebauer
;;;
;;; prolog-indent.el is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU General Public
;;; License for full details.
;;;
;;; Everyone is granted permission to copy, modify and redistribute
;;; prolog-indent.el, but only under the conditions described in the
;;; GNU General Public License.	 A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities.  It should be in a
;;; file named COPYING.	Among other things, the copyright notice
;;; and this notice must be preserved on all copies.
;;;

;;;----------------------------------------------------------------------------
;;; Load prolog.el if not already loaded.
(eval-when-compile (load-library "prolog"))
(if (null prolog-mode-map) (load-library "prolog"))


(defvar Prolog-prefix-op-regex "\\\\\\+"
  "Regular expression of a Prolog prefix operator of length 2.")
(defvar Prolog-infix1-op-regex "[^:/=]=\\|.[*+]\\|[^*]/\\|[^:]-\\|.^"
  "Regular expression of a Prolog infix operator")
(defvar Prolog-infix2-op-regex "\\(==\\|=<\\|>=\\|<<\\|>>\\|is\\)"
  "Regular expression of a Prolog infix operator of length 2.")
(defvar Prolog-infix3-op-regex "=[:\\\\]=\\|=\\.\\.\\|\\\\=="
  "Regular expression of a Prolog infix operator of length 3.")
(defvar Prolog-ascii-escape-regex "0'."
  "Regular expression of a Prolog ASCII characters (length 3).")

;;;----------------------------------------------------------------------------
(defun backward-Prolog-goal (&optional fwd)
  "Move point backward to the beginning of a Prolog goal.
This includes compound terms constructed with infix operators of low priority.
Some of those operators are built into this routine:

+ - * /
\\+
== =< >= << >> is
=.. =:= =\\\\== \\\\===

Comments inside a Prolog goal are not considered!
"
  (if fwd (forward-char fwd))
  (Prolog-backward-to-noncomment (point-min))
  (forward-char -1)
  (if (looking-at "!")	; the cut is a goal even if it is no sexp
      nil
    (forward-char 1)
    (backward-sexp 1))
  (let ((beg (point)))
    (Prolog-backward-to-noncomment (point-min))
    (or (bobp) (forward-char -1))
    (or (bobp) (forward-char -1))
    (cond
     ((looking-at Prolog-prefix-op-regex) )
     ((looking-at Prolog-infix2-op-regex)
       (backward-Prolog-goal))
     ((looking-at Prolog-infix1-op-regex)
      (backward-Prolog-goal 1))
     ((looking-at Prolog-ascii-escape-regex)
       (backward-Prolog-goal))
     ( (progn (or (bobp) (forward-char -1))
	      (looking-at Prolog-infix3-op-regex))
       (backward-Prolog-goal))
     ( t (goto-char beg))))
  (or (bobp)
      (progn
	(forward-char -1)
	(cond ((looking-at "[a-z_A-Z0-9'](")
	       (backward-Prolog-goal 1))
	      ( t (forward-char 1))))))
  
;;;----------------------------------------------------------------------------
(defun Prolog-indent (&optional lines)
  "Indent current line as Prolog code.
With argument, indent any additional lines along with this one."
  (interactive "p")
  (if (numberp lines)
      (let ((fwd (if (> lines 0) 1 -1)))
	(if (< lines 0) (setq lines (- 0 lines)))
	(while (> lines 1)
	  (setq lines (- lines 1))
	  (Prolog-indent-line)
	  (forward-line fwd))))
  (Prolog-indent-line)
)

;;;----------------------------------------------------------------------------
(defun Prolog-indent-buffer ()
  "Indent the whole buffer as Prolog code."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (end-of-line)
    (while  (not (eobp))
      (Prolog-indent-line)
      (forward-line 1)
      (end-of-line)
      )))

;;;----------------------------------------------------------------------------
(defun Prolog-in-comment-p ()
  "Check whether point is inside a C-style comment."
  (let ((beg (if (bobp)
		 (point)
	       (save-excursion 
		 (beginning-of-line)
		 (search-backward "/*" (point-min) 'move)
		 (point))))
	(pos (if (bobp)
		 (point-max)
	       (save-excursion 
		 (beginning-of-line)
		 (search-backward "*/" (point-min) 'move)
		 (point)))))
    (< pos beg)))

;;;----------------------------------------------------------------------------
(defun Prolog-indent-line ()
  "Indent current line as Prolog code."
  (let (indent pos beg)
    (if (Prolog-in-comment-p)
	nil
      (setq indent (Prolog-indentation-level)
	    pos (- (point-max) (point)))
      (beginning-of-line)
      (setq beg (point))
      (skip-chars-forward " \t")
      (if (zerop (- indent (current-column)))
	  nil
	(delete-region beg (point))
	(indent-to indent))
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos)))
      )))

(defvar Prolog-colon-minus-indent tab-width
  "Indentation of :- if at the beginning of a line.")

(defvar Prolog-body-indent tab-width
  "Indentation of the body after :- or --> at the end of a line.")

(defvar Prolog-then-indent 2
  "Indentation of literals after ->")

;;;----------------------------------------------------------------------------
(defun Prolog-indentation-level ()
  "Compute the Prolog indentation level."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (cond
     ((looking-at "%[^%]") comment-column)	;Small comment starts
     ((looking-at "%%%")   0)			;Large comment starts
     ((looking-at "/\\*")  0)
     ((looking-at "?-")    0)
     ((looking-at "\\([)}]\\|]\\)")	        ;Closing parenthesis
      (forward-char 1)				; is aligned at opening
      (backward-sexp)
      (current-column))
     ((looking-at ";")
      (Prolog-backward-to-noncomment (point-min))
      (backward-Prolog-goal)
      (max (- (current-column) prolog-indent-width) 0)
      )
     ((looking-at ":-\\|-->")
      (Prolog-backward-to-noncomment (point-min))
      (cond
       ((or (bobp)
	    (= (preceding-char) ?.))
	0 )
       ( t Prolog-colon-minus-indent)))
     ((looking-at "->")
      (Prolog-backward-to-noncomment (point-min))
      (backward-Prolog-goal)
      (+ (current-column) prolog-indent-width))
     ((bobp)               0)
     (t
      (Prolog-backward-to-noncomment (point-min))
      (if (save-excursion 
	    (beginning-of-line 1)
	    (skip-chars-forward " \t")
	    (looking-at "%%[^%]"))
	  (current-column)
	(let ((pos (point)))
	  (skip-chars-backward " \t")
	  (or (bobp) (forward-char -1))
	  (or (bobp) (forward-char -1))			 ;Backward twice
	  (cond
	   ((looking-at ".;")
	    (goto-char pos)
	    (+ -1 (current-column) prolog-indent-width))
	   ((looking-at ".,")
	    (backward-Prolog-goal 1)
	    (current-column))
	   ((looking-at "->")
	    (cond 
	     ((= (preceding-char) ?-)
	      Prolog-body-indent
	      )
	     ( t
	       (backward-Prolog-goal)
	       (+ (current-column) Prolog-then-indent))))
	   ((looking-at "[a-z_A-Z0-9'](")
	    (forward-char 2)
	    (current-column))
	   ((looking-at ".[({]")
	    (forward-char 1)
	    (+ (current-column) prolog-indent-width))
	   ((looking-at ":-") Prolog-body-indent)
	   ((looking-at ".[^.]")
	    (goto-char pos)
	    (max (- (current-column) prolog-indent-width) 0))
	   (t 0 ))
	  ))))))

;;;----------------------------------------------------------------------------
;;; Taken from the prolog mode by Ken'ichi HANDA (handa@etl.go.jp)
(defun Prolog-backward-to-noncomment (lim)
  (let (opoint stop)
    (while (not stop)
      (skip-chars-backward " \t\n\f" lim)
      (setq opoint (point))
      (if (and (>= (point) (+ 2 lim))
	       (= (preceding-char) ?/) (= (char-after (- (point) 2)) ?*))
	  (search-backward "/*" lim 'mv)
	(let ((p (max lim (save-excursion (beginning-of-line) (point)))))
	  (if (nth 4 (parse-partial-sexp p (point)))
	      (search-backward "%" p 'mv)
	    (goto-char opoint)
	    (setq stop t)))))))


(defvar Prolog-boc-regexp "^['a-z]")

;;;----------------------------------------------------------------------------
(defun Prolog-beginning-of-clause ()
  "Return the position of the beginning of the clause or nil."
  (save-excursion
    (catch 'boc
      (while t
	(cond
	 ((null (search-backward-regexp Prolog-boc-regexp (point-min) t))
	  (throw 'boc nil))
	 ((not (Prolog-in-comment-p))
	  (throw 'boc (point))))))))

(defvar Prolog-eoc-regexp "\\.[ \t\f]*$")

;;;----------------------------------------------------------------------------
(defun Prolog-end-of-clause ()
  "Return the position of the beginning of the clause or nil."
  (save-excursion
    (catch 'boc
      (while t
	(cond
	 ((null (search-forward-regexp Prolog-eoc-regexp (point-max) t))
	  (throw 'boc nil))
	 ((not (Prolog-in-comment-p))
	  (skip-chars-backward " \t\f\n")
	  (throw 'boc (point))))))))

;;;----------------------------------------------------------------------------
(defun Prolog-indent-region (beg end) 
  (interactive "d\nm")
  (let (tmp)
    (if (null end)
	(message "Mark not set")
      (if (< end beg) (setq tmp end end beg beg tmp))
      (save-excursion 
	(setq end (- (point-max) end))
	(goto-char beg)
	(Prolog-indent-line)
	(forward-line 1)
	(beginning-of-line)
	(while (> (- (point-max) (point)) end)
	  (Prolog-indent-line)
	  (forward-line 1)
	  (beginning-of-line))))))

;;;----------------------------------------------------------------------------
(defun Prolog-indent-clause ()
  (interactive)
  (let ((beg (Prolog-beginning-of-clause))
	(end (Prolog-end-of-clause)))
    (if (and beg end) (Prolog-indent-region beg end))))

;;;----------------------------------------------------------------------------
(defun Prolog-fill-paragraph (&optional arg) (interactive)
  (if (Prolog-in-comment-p) (fill-paragraph arg) (Prolog-indent-clause)))

(defvar Prolog-electric t
  "Variable indicating if the Prolog electric characters should be active.")

;;;----------------------------------------------------------------------------
;;; Taken from the prolog mode by Ken'ichi HANDA (handa@etl.go.jp)
;;; Modified
(defun Prolog-electic-char (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if (or arg
	  (not Prolog-electric))
      (self-insert-command (prefix-numeric-value arg))
      (progn
	(self-insert-command (prefix-numeric-value arg))
	(Prolog-indent-line))))

;;;----------------------------------------------------------------------------
;;; Activate some additional keys
(define-key prolog-mode-map "("    'Prolog-electic-char   )
(define-key prolog-mode-map ")"    'Prolog-electic-char   )
(define-key prolog-mode-map "{"    'Prolog-electic-char   )
(define-key prolog-mode-map "}"    'Prolog-electic-char   )
(define-key prolog-mode-map ";"    'Prolog-electic-char   )
(define-key prolog-mode-map "-"    'Prolog-electic-char   )
(define-key prolog-mode-map ">"    'Prolog-electic-char   )
(define-key prolog-mode-map "\t"   'Prolog-indent         )
(define-key prolog-mode-map "\M-q" 'Prolog-fill-paragraph )

;;;----------------------------------------------------------------------------
(defun prolog-indent-line (&optional arg) (interactive "P")
  (Prolog-indent (prefix-numeric-value arg)))

(provide 'prolog-indent)
