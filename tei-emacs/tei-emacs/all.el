;; From: blackie@ifad.dk (Jesper K. Pedersen)
;; Newsgroups: comp.emacs
;; Subject: Re: hiding lines; XEDIT-"all"-like command
;; Date: 07 Dec 1998 11:28:59 +0100

;; This is an implementation of the xedit `all' command for GNU Emacs.
;;
;; It works mostly like `occur' except that changes to the `*All*' buffer
;; are propagated back to the original buffer.
;;
;; The package is inspired by a discussion in `gnu.emacs.help'.  It uses
;; `before-change-functions' and `after-change-functions' to propagate
;; the changes from a separate buffer, instead of `invisible' and
;; 'intangible' text properties in the original buffer as has been
;; proposed on the newsgroup.  I feel using text properties is
;; conceptually cleaner, but this version works.
;;
;; Insert
;;	(autoload 'all "all" nil t)
;; in your `.emacs' file to enable this package.
;;
;; Type `M-x all RET' to try it out.
;;
;; You need GNU Emacs 19.23 or later to use it.
;;
;; Some code have been stolen from `replace.el' in the Emacs 19.25.93
;; distribution.

;;; all.el --- Edit all lines matching a given regexp.

;; Copyright (C) 1985, 1986, 1987, 1992, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1994 Per Abrahamsen

;; Author: Per Abrahamsen <abraham@iesd.auc.dk>
;; Version: $Id$
;; Keywords: matching

;; LCD Archive Entry:
;; all|Per Abrahamsen|abraham@iesd.auc.dk|
;; Edit all lines matching a given regexp|
;; 09-Aug-1994|0.0|~/misc/all.el.Z|

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Comments:

;; Just like occur, except that changes in the *All* buffer is
;; propagated to the original buffer.

;; I also added highlighting of the matches.

;; You can no longer use mouse-2 to find a match in the original file,
;; since the default definition of mouse to is useful.
;; However, `C-c C-c' still works.

;; Line numbers are not listed in the *All* buffer.

;; Ok, it is _not_ just like occur.

;; Some limitations:

;; - Undo in the *All* buffer is an ordinary change in the original.
;; - Changes to the original buffer is not reflected in the *All* buffer.
;; - A single change in the *All* buffer must be limited to a single match.

;; Requires GNU Emacs 19.23 or later.

;;; Code:

(defvar all-mode-map ())

(if all-mode-map
    ()
  (setq all-mode-map (make-sparse-keymap))
  (define-key all-mode-map "\C-c\C-c" 'all-mode-goto))

(defvar all-buffer nil)

(defun all-mode ()
  "Major mode for output from \\[all].

All changes made in this buffer will be propagated to the buffer where
you ran \\[all].

Press \\[all-mode-find] to return to the original buffer."
  (kill-all-local-variables)
  (use-local-map all-mode-map)
  (setq major-mode 'all-mode)
  (setq mode-name "All")
  (make-local-variable 'all-buffer)
  (run-hooks 'all-mode-hook))

(defun all-mode-find (pos)
  ;; Find position in original buffer corresponding to POS.
  (let ((overlay (all-mode-find-overlay pos)))
    (+ (marker-position (overlay-get overlay 'marker))
       (- pos (overlay-start overlay)))))

(defun all-mode-find-overlay (pos)
  ;; Find the overlay containing POS.
  (let ((overlays (overlays-at pos)))
    (while (and overlays (null (overlay-get (car overlays) 'marker)))
      (setq overlays (cdr overlays)))
    (car-safe overlays)))

(defun all-mode-goto ()
  "Move point to the corresponding position in the original buffer."
  (interactive)
  (let ((pos (all-mode-find (point))))
    (if pos
	(pop-to-buffer all-buffer)
      (error "This text is not from the original buffer"))
    (goto-char pos)))

(defvar all-initialization-p nil)

(defun all-before-change-function (from to)
  ;; Check that change is legal
  (and all-buffer
       (not all-initialization-p)
       (let ((start (all-mode-find-overlay from))
	     (end (all-mode-find-overlay to)))
	 (not (and start (eq start end))))
       (error "Changes should be limited to a single text piece")))

(add-hook 'before-change-functions 'all-before-change-function)

(defun all-after-change-function (from to length)
  ;; Propagate changes from *All* buffer.
  (and all-buffer
       (null all-initialization-p)
       (let ((buffer (current-buffer))
	     (pos (all-mode-find from)))
	 (if pos
	     (progn
	       (set-buffer all-buffer)
	       (delete-region pos (+ pos length))
	       (save-excursion
		 (goto-char pos)
		 (insert-buffer-substring buffer from to))
	       (set-buffer buffer))))))

(add-hook 'after-change-functions 'all-after-change-function)

;;;###autoload
(defun all (regexp &optional nlines)
  "Show all lines in the current buffer containing a match for REGEXP.

If a match spreads across multiple lines, all those lines are shown.

Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative.
NLINES defaults to `list-matching-lines-default-context-lines'.
Interactively it is the prefix arg.

The lines are shown in a buffer named `*All*'.
Any changes made in that buffer will be propagated to this buffer."
  (interactive (list (let* ((default (car regexp-history))
			    (input
			     (read-from-minibuffer
			      (if default
				  (format
   "Edit lines matching regexp (default `%s'): " default)
				"Edit lines matching regexp: ")
			      nil nil nil
			      'regexp-history)))
		       (if (> (length input) 0) input
			 (setcar regexp-history default)))
		     current-prefix-arg))
  (setq nlines (if nlines (prefix-numeric-value nlines)
		 list-matching-lines-default-context-lines))
  (setq all-initialization-p t)
  (let ((first t)
	(buffer (current-buffer))
	(prevend nil)
	(prevstart nil)
	(prevpos (point-min)))
    (with-output-to-temp-buffer "*All*"
      (save-excursion
	(set-buffer standard-output)
	(all-mode)
	(setq all-buffer buffer)
	(insert "Lines matching ")
	(prin1 regexp)
	(insert " in buffer " (buffer-name buffer) ?. ?\n)
	(insert "--------\n"))
      (if (eq buffer standard-output)
	  (goto-char (point-max)))
      (save-excursion
	(beginning-of-buffer)
	;; Find next match, but give up if prev match was at end of buffer.
	(while (and (not (= prevpos (point-max)))
		    (re-search-forward regexp nil t))
	  (goto-char (match-beginning 0))
	  (beginning-of-line)
	  (setq prevpos (point))
	  (goto-char (match-end 0))
	  (let* ((start (save-excursion
			  (goto-char (match-beginning 0))
			  (forward-line (if (< nlines 0) nlines (- nlines)))
			  (point)))
		 (end (save-excursion
			(goto-char (match-end 0))
			(if (> nlines 0)
			    (forward-line (1+ nlines))
			    (forward-line 1))
			(point)))
		 marker)
	    (cond ((null prevend)
		   (setq prevstart start
			prevend end))
		  ((> start prevend)
		   (all-insert)
		   (setq prevstart start
			 prevend end))
		  (t
		   (setq prevend end)))))
	(if prevend
	    (all-insert)))))
  (setq all-initialization-p nil))

(defun all-insert ()
  ;; Insert match.
  (save-excursion
    (setq marker (make-marker))
    (set-marker marker prevstart)
    (set-buffer standard-output)
    (let ((from (point))
	  to)
      (insert-buffer-substring buffer prevstart prevend)
      (setq to (point))
      (overlay-put (make-overlay from to) 'marker marker)
      (goto-char from)
      (while (re-search-forward regexp to t)
	(overlay-put (make-overlay (match-beginning 0) (match-end 0))
		     'face 'highlight))
      (goto-char to)
      (if (> nlines 0)
	  (insert "--------\n")))))

(provide 'all)

;;; all.el ends here
