;;; multi-mode.el --- Allowing multiple major modes in a buffer.
;;;****************************************************************************
;;; $Id$
;;;****************************************************************************
;;
;;
;; Description:
;;
;;	Sometimes it is  desirable to  have different  major modes  in  one
;;	buffer.  One   such   case occurs   when   editing  comments  in  a
;;	programming language buffer.  Here it  might be desirable to use  a
;;	totally  different mode than the  one used  for editing the program
;;	code itself.
;;
;;	I have adoped this practice for editing Prolog.  The code itself is
;;	edited   in prolog-mode,   whereas  the   comments  are edited   in
;;	LaTeX-mode.
;;
;;	It  is  desirable to use different   modes instead of enhancing one
;;	mode  because much  efford has  already  been put in various  modes
;;	which needs not to be duplicated.
;;
;;	The multi-mode minor  mode provides  a means  to accomplish  such a
;;	feature.
;;
;;	The modes  are described  by  initializing strings.   I assume that
;;	certain tokens (i.e. transition strings)  indicate the places where
;;	a new mode should be entered. To  determine the mode  at a point it
;;	is only neccessary to find the last transition string before point.
;;
;;	The desired   modes are described  in  a list of   pairs or triples
;;	consisting  of   a  transition   string  and a   mode   (a symbol).
;;	Optionally a function symbol can be specified which is evaluated to
;;	activate  the desired mode.  Additionally   the mode in absence  of
;;	preceding transition strings has to be specified.
;;
;;
;;
;;
;; Installation:
;;	1. Ensure that multi-mode.el is on the load-path.
;;	2. For efficiency it might be desirable to byte-compile 
;;	   multi-mode.el.
;;	3. Put the following in your .emacs file or a similar place
;;	   where it is loaded when needed.
;;
;;	   (autoload 'multi-mode
;;		     "multi-mode"
;;		     "Allowing multiple major modes in a buffer."
;;		     t)
;;
;;	4. Define  your own  incarnation of multi  mode  to serve as major
;;	   mode.  This can be done in your .emacs file.  E.g.
;;
;;	   (defun multi-c-fundamental-mode () (interactive)
;;	     (multi-mode 1
;;			 'c-mode
;;			 '(\"/*\" fundamental-mode my-fundamental-setup)
;;			 '(\"*/\" c-mode)))
;;
;;	   This major mode can now be used to turn on the multi-mode in the
;;	   minibuffer, or in the auto-mode-alist, or in the local variables
;;	   of a file. E.g.
;;
;;	   (setq auto-mode-alist
;;		 (cons '("\\.[ch]$" . multi-c-fundamental-mode)
;;		 auto-mode-alist)
;;
;;
;; Bugs and Problems:
;;	- It is rather easy to hang my whole workstation when
;;	  multi-mode-transitions has not a proper value.
;;	  For sake of efficiency I omit finer type checking to avoid this
;;	  problem.
;;	  CURE: Never try to change the variable yourself. Use the
;;		function multi-mode instead.
;;
;; To do:
;;	- The generalization of transition strings to regular expressions
;;	  seems to be straight forward. For efficience reasons I will do it
;;	  only upon request.
;;
;;	- No provisions have been made to make the key bindings accessible
;;	  in all modes. This might be desirable.
;;
;;
;; Changes:
;; $Log$
;; Revision 1.1  2004/09/23 13:23:11  rahtz
;; Initial revision
;;
; Revision 1.2  1994/06/11  22:08:22  gerd
; *** empty log message ***
;
; Revision 1.1  1994/05/24  12:41:30  gerd
; A little hack has been enhanced to a minor mode.
;;
;;
;; Author:	
;;	Gerd Neugebauer
;;	Ödenburger Str. 16
;;	64295 Darmstadt (Germany)
;;
;;	Net: gerd@intellektik.informatik.th-darmstadt.de
;;	     gerd@imn.th-leipzig.de
;;
;;*****************************************************************************
;; LCD Archive Entry:
;; multi-mode|Gerd Neugebauer|gerd@intellektik.informatik.th-darmstadt.de|
;; Minor mode allowing multiple major modes in a single buffer.|
;; $Date$|$Revision$||
;;*****************************************************************************
;;
;; Copyright (C) 1994 Gerd Neugebauer
;;
;; multi-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU General Public
;; License for full details.
;;
;; Everyone is granted permission to copy, modify and redistribute
;; multi-mode.el, but only under the conditions described in the
;; GNU General Public License.	 A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.	Among other things, the copyright notice
;; and this notice must be preserved on all copies.
;;

;;;----------------------------------------------------------------------------
;;; Variable definitions and initializations

(make-variable-buffer-local 'multi-mode-transitions)
(defvar multi-mode-transitions nil
  "Transition definition for the multi-mode minor mode.
")

(defvar multi-mode-beware '( (not isearch-mode) )
  "List of forms to check if multi-mode is desirable. If all forms evaluate to
t then a mode switch is allowed.")

(make-variable-buffer-local 'multi-mode)
(defvar multi-mode nil
  "This variable indicates if the multi-mode minor mode is active.")


;;;----------------------------------------------------------------------------
;;; Definition of the minor mode

(defun multi-mode (&optional arg &optional initial-mode &rest transitions) 
  "Allowing multiple major modes in a buffer.

Toggle multi mode. With numeric argument turn on multi mode if the argument
is positive.

Sometimes it is desirable to have different major modes in one buffer.  One
such case occurs  when editing comments  in a programming  language buffer.
Here it  might be desirable  to use a totally  different mode  than the one
used for editing the program code itself.

It is desirable   to use different  modes  instead  of enhancing one   mode
because much effort has  already been put  in various modes which needs not
to be duplicated.

The multi-mode minor mode provides a means to accomplish such a feature.

The  modes are described  by initializing  strings.  I assume that  certain
tokens (i.e.  transition  strings)  indicate  the places  where a  new mode
should be entered.  To determine  the mode at a point  it is only necessary
to find the last transition string before point.

The desired modes are described in a list of pairs or triples consisting of
a transition string and  a mode (a  symbol).  Optionally a function  symbol
can   be specified   which  is evaluated  to   activate  the desired  mode.
Additionally the mode in absence of preceding transition  strings has to be
specified.

When called not interactively there are additional arguments to specify the
transitions.

INITIAL-MODE  is a symbol denoting   a major mode   to  be used before  any
transition strings are present before point.

The remaining optional arguments are pairs
	(TOKEN MODE)
or triples
	(TOKEN MODE FUNCTION)

The first element TOKEN is  a string which indicates  the token to activate
the mode MODE specified in the second argument. Optionally a third argument
can  be given. The  symbol FUNCTION is  used as a function  to be called to
turn on the mode given as second argument. It defaults to MODE.

Consider the following example:

(multi-mode 1
	    c-mode
	    (\"/*\" fundamental-mode my-fundamental-setup)
	    (\"*/\" c-mode))

The first argument forces multi-mode to be  turned on.  The second argument
declares the default mode to be c-mode. Thus at the beginning of the buffer
c-mode is turned on. If a '/*'  is found before point then fundamental-mode
may be turned on. This is done using the function my-fundamental-setup.

If a '*/' is found before point then c-mode may be activated. Which mode is
used depends on the last activating  pattern before point.  In this example
everything between /* and */ can be edited  in fundamental mode. Everything
else is in c-mode.

See also the documentation of multi-mode-beware."
  (interactive "P")
  (setq multi-mode				; 
	(if (null arg) (not multi-mode)		; Toggle multi-mode if no arg
	  (> (prefix-numeric-value arg) 0)))	; otherwise turn it on or off
  (let ((ok   t)				; result of type check
	(tt   transitions)			; transition table
	trans)					; transition
    (while tt					; Check the transition table
      (setq trans (car tt)			; Get a single transition
            tt    (cdr tt))			; Advance the table pointer
      (if (or (not (listp   trans))		; Check the transition
	      (not (stringp (first trans)))	;
	      (not (symbolp (second trans))))	;
	  (setq ok nil				; set error mark
		tt nil))			; stop checking
    )
    (if (and ok					; if transition table is ok
	     initial-mode			; check the initial-mode
	     (symbolp initial-mode))		;
	(setq multi-mode-transitions		; set the local variable
	      (cons initial-mode transitions)))	;
  )

  (force-mode-line-update)			; show the new state
)

(or (assq 'multi-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(multi-mode " MULTI") minor-mode-alist)))


(defun multi-mode-update-mode ()
  "Perform a mode switch if multi-mode is active."
  ;; This function only starts to work if multi mode is on
  ;; and a transition table is defined. Additionally the forms in
  ;; multi-mode-beware are evaluated to see if a mode switch is desirable.
  (if (and multi-mode
	   multi-mode-transitions
	   (eval (cons 'and multi-mode-beware))
	   )
      (let* ((transition    (cdr multi-mode-transitions)) ; get transitions
	     (mode          (car multi-mode-transitions)) ; get initial mode
	     (mode-function mode)       ; set initial mode switching function
	     (pos           -1)		; set the initial position of a transit
	     hit			; new position of a transition string
	     tt)			; transition table
	(while transition
	  (setq tt         (car transition)	; extract a single transition
		transition (cdr transition))	; advance the transitions
	  (setq hit (save-excursion		; we don't want to change point
		      (if (search-backward	; unfailing backward search
			   (first tt)		;  for the transition pattern
			   nil
			   t)
			  (point)		; upon sucess use the position
			-2)))			; otherwise try to ignore it
	  (if (> hit pos)			; pattern is newer than prev.
	      (setq pos		  hit		;  then save position
		    mode	  (second tt)	;  the desired mode and an
		    mode-function (or (third tt) mode)); optional function
	  )
	)
	(if (not (equal major-mode mode))	; if mode needs switching
	    (let ((transitions multi-mode-transitions)) ; save transition table
	      (if (fboundp mode-function)	; if mode function is defined
		  (funcall mode-function))	;  then call it
	      (setq multi-mode-transitions transitions  ; restore transition
		    multi-mode t)))		; and minor mode
      )
  )
)

;;;----------------------------------------------------------------------------
;;; The function is hooked into the post-command-hook to be called after
;;; each function.
;;;
(add-hook 'post-command-hook 'multi-mode-update-mode)

(provide 'multi-mode)
