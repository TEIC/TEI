;;; xmlchars.el --- Insert Unicode characters.

;; $Id$

;; Most of these are taken from XML named characters from the ISO 8859 entity sets.
;; Hence the name.

;; Copyright (C) 2003 Norman Walsh

;; Author: Norman Walsh <ndw@nwalsh.com>
;; Maintainer: Norman Walsh <ndw@nwalsh.com>
;; Created: 2003-09-29
;; Version: 1.0
;; CVS ID: $Id$
;; Keywords: utf-8 unicode xml characters

;; Inspired by http://www.tbray.org/ongoing/When/200x/2003/09/27/UniEmacs

;; This file is NOT part of GNU emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary

;; This is all quite possibly a bit crude. But it works. I have a few more
;; ideas that I may implement if time allows. Suggestions welcome.

;;; Usage

;; 1. Load this file.
;; 2. Bind 'smart-double-quote, 'smart-single-quote, 'insert-xml-char,
;;    and 'shortcut-xml-char as you see fit.
;;
;;    I bind the quotes to " and ' in nxml-mode:
;;
;;      (define-key nxml-mode-map "\"" 'smart-double-quote)
;;      (define-key nxml-mode-map "\'" 'smart-single-quote)))
;;
;;    I bind the inserts to keys in my personal C-t map:
;;
;;      (define-key ctl-t-map "c" 'insert-special-char)
;;      (define-key ctl-t-map "e" 'insert-accented-char)))

;;; Changes

;; v1.1
;;   Added straight double and single quotes to the rotation
;; v1.0
;;   First release

;;; Code:

(defconst emacs-ldquo #x5397c)
(defconst emacs-rdquo #x5397d)
(defconst emacs-lsquo #x53978)
(defconst emacs-rsquo #x53979)
(defconst emacs-quot  #x0022)
(defconst emacs-apos  #x0027)

(defvar xml-chars-menu-alist
  '(("[nop]"     . 0)
    ("angst"     . #x212B)
    ("cent"      . #x00A2)
    ("copy"      . #x00A9)
    ("Dagger"    . #x2021)
    ("dagger"    . #x2020)
    ("deg"       . #x00B0)
    ("emsp"      . #x2003)
    ("ensp"      . #x2002)
    ("ETH"       . #x00D0)
    ("eth"       . #x00F0)
    ("euro"      . #x20AC)
    ("half"      . #x00BD)
    ("laquo"     . #x00AB)
    ("ldquo"     . #x201c)
    ("lsquo"     . #x2018)
    ("mdash"     . #x2014)
    ("micro"     . #x00B5)
    ("middot"    . #x00B7)
    ("ndash"     . #x2013)
    ("not"       . #x00AC)
    ("numsp"     . #x2007)
    ("para"      . #x00B6)
    ("permil"    . #x2030)
    ("puncsp"    . #x2008)
    ("raquo"     . #x00BB)
    ("rdquo"     . #x201d)
    ("rsquo"     . #x2019)
    ("reg"       . #x00AE)
    ("sect"      . #x00A7)
    ("THORN"     . #x00DE)
    ("thorn"     . #x00FE)
    ("trade"     . #x2122)
    ))

(defvar xml-chars-name-alist
  '(("AElig"     . #x00C6)
    ("Aacute"    . #x00C1)
    ("Acirc"     . #x00C2)
    ("Agrave"    . #x00C0)
    ("Aring"     . #x00C5)
    ("Atilde"    . #x00C3)
    ("Auml"      . #x00C4)
    ("Ccedil"    . #x00C7)
    ("Eacute"    . #x00C9)
    ("Ecirc"     . #x00CA)
    ("Egrave"    . #x00C8)
    ("Euml"      . #x00CB)
    ("Iacute"    . #x00CD)
    ("Icirc"     . #x00CE)
    ("Igrave"    . #x00CC)
    ("Iuml"      . #x00CF)
    ("Ntilde"    . #x00D1)
    ("Oacute"    . #x00D3)
    ("Ocirc"     . #x00D4)
    ("Ograve"    . #x00D2)
    ("Oslash"    . #x00D8)
    ("Otilde"    . #x00D5)
    ("Ouml"      . #x00D6)
    ("Uacute"    . #x00DA)
    ("Ucirc"     . #x00DB)
    ("Ugrave"    . #x00D9)
    ("Uuml"      . #x00DC)
    ("Yacute"    . #x00DD)
    ("aacute"    . #x00E1)
    ("acirc"     . #x00E2)
    ("aelig"     . #x00E6)
    ("agrave"    . #x00E0)
    ("aring"     . #x00E5)
    ("atilde"    . #x00E3)
    ("auml"      . #x00E4)
    ("ccedil"    . #x00E7)
    ("eacute"    . #x00E9)
    ("ecirc"     . #x00EA)
    ("egrave"    . #x00E8)
    ("euml"      . #x00EB)
    ("iacute"    . #x00ED)
    ("icirc"     . #x00EE)
    ("igrave"    . #x00EC)
    ("iuml"      . #x00EF)
    ("ntilde"    . #x00F1)
    ("oacute"    . #x00F3)
    ("ocirc"     . #x00F4)
    ("ograve"    . #x00F2)
    ("omacr"     . #x14d)
    ("oslash"    . #x00F8)
    ("otilde"    . #x00F5)
    ("ouml"      . #x00F6)
    ("szlig"     . #x00DF)
    ("uacute"    . #x00FA)
    ("ucirc"     . #x00FB)
    ("ugrave"    . #x00F9)
    ("uuml"      . #x00FC)
    ("yacute"    . #x00FD)
    ("yuml"      . #x00FF)
    ("frac12"    . #x00BD)
    ("frac13"    . #x2153)
    ("frac14"    . #x00BC)
    ("frac34"    . #x00BE)
    ("hellip"    . #x2026)
    ("iexcl"     . #x00A1)
    ("iquest"    . #x00BF)
    ("nbsp"      . #x00A0)
    ("plusmn"    . #x00B1)
    ("pound"     . #x00A3)
    ("yen"       . #x00A5)
    ))

(defvar xml-chars-shortcut-alist
  '(("AE"  . "AElig")
    ("A'"  . "Aacute")
    ("A^"  . "Acirc")
    ("A`"  . "Agrave")
    ("Ao"  . "Aring")
    ("A~"  . "Atilde")
    ("A\"" . "Auml")
    ("C,"  . "Ccedil")
    ("E'"  . "Eacute")
    ("E^"  . "Ecirc")
    ("E`"  . "Egrave")
    ("E\"" . "Euml")
    ("I'"  . "Iacute")
    ("I^"  . "Icirc")
    ("I`"  . "Igrave")
    ("I\"" . "Iuml")
    ("N~"  . "Ntilde")
    ("O'"  . "Oacute")
    ("O^" .  "Ocirc")
    ("O`"  . "Ograve")
    ("O/"  . "Oslash")
    ("O~"  . "Otilde")
    ("O\"" . "Ouml")
    ("U'"  . "Uacute")
    ("U^"  . "Ucirc")
    ("U`"  . "Ugrave")
    ("U\"" . "Uuml")
    ("Y'"  . "Yacute")
    ("a'"  . "aacute")
    ("a^"  . "acirc")
    ("ae"  . "aelig")
    ("a`"  .  "agrave")
    ("ao"  . "aring")
    ("a~"  . "atilde")
    ("a\"" . "auml")
    ("c,"  . "ccedil")
    ("e'"  . "eacute")
    ("e^"  . "ecirc")
    ("e`"  . "egrave")
    ("e\"" . "euml")
    ("i'"  . "iacute")
    ("i^"  . "icirc")
    ("i`"  . "igrave")
    ("i\"" . "iuml")
    ("n~"  . "ntilde")
    ("o'"  . "oacute")
    ("o^"  . "ocirc")
    ("o`"  . "ograve")
    ("o-"  . "omacr")
    ("o/"  . "oslash")
    ("o~"  . "otilde")
    ("o\"" . "ouml")
    ("sz"  . "szlig")
    ("u'"  . "uacute")
    ("u^"  . "ucirc")
    ("u`"  . "ugrave")
    ("u\"" . "uuml")
    ("y'"  . "yacute")
    ("y\"" . "yuml")
    ("12" . "frac12")
    ("13" . "frac13")
    ("14" . "frac14")
    ("15" . "frac15")
    ("16" . "frac16")
    ("18" . "frac18")
    ("23" . "frac23")
    ("25" . "frac25")
    ("34" . "frac34")
    ("35" . "frac35")
    ("38" . "frac38")
    ("45" . "frac45")
    ("56" . "frac56")
    ("58" . "frac58")
    ("78" . "frac78")
    ("<<" . "laquo")
    (".." . "hellip")
    ("!i" . "iexcl")
    ("?i" . "iquest")
    ("  " . "nbsp")
    ("+-" . "plusmn")
    ("$c" . "cent")
    ("$e" . "euro")
    ("$p" . "pound")
    ("$y" . "yen")
    ))

(defun  insert-xml-char (&optional charname)
  "Insert a special character by name; use a popup-menu if no name is given."
  (interactive)
  (let* ((xml-chars-menu
	  (list "Special chars" (append (list "") xml-chars-menu-alist)))
	 (value
	  (cond
	   ((and charname (assoc charname xml-chars-menu-alist))
	    (cdr (assoc charname xml-chars-menu-alist)))
	   ((and charname (assoc charname xml-chars-name-alist))
	    (cdr (assoc charname xml-chars-name-alist)))
	   (t
	    (x-popup-menu t
	     xml-chars-menu)))))
    (cond
     ((integerp value)
      (if (= 0 value)
	  (message "Cancelled")
	(ucs-insert value)))
     ((stringp  value) (insert value))
     (t (insert charname)))))

(defun shortcut-xml-char ()
  "Read a (two-character) keyboard shortcut and insert the corresponding character."
  (interactive)
  (let* ((c1 (read-char))
	 (c2 (read-char))
	 (str (concat (char-to-string c1) (char-to-string c2))))
    (cond
     ((assoc str xml-chars-shortcut-alist)
      (insert-xml-char (cdr (assoc str xml-chars-shortcut-alist))))
     (t (insert str)))))

(defun in-start-tag ()
  "Crude test to see if point is in side an open start tag."
  (interactive)
  (let (slim here pgt plt)
    (setq here (point))
    (setq slim
	  (if (> here 1024)
	      (- here 1024)
	    0))
    (setq pgt (search-backward ">" slim t))
    (goto-char here)
    (setq plt (search-backward "<" slim t))
    (goto-char here)
    (if (and pgt plt)
	(> plt pgt)
      plt)))

(defun smart-double-quote ()
  "Insert a left or right double quote as appropriate. Left quotes are inserted after a space, newline, or >. Right quotes are inserted after any other character, except if the preceding character is a quote, cycles through the three quote styles."
  (interactive)
  (let ((ch (char-before)))
    (cond
     ((in-start-tag)
      (insert "\""))
     ((or (not ch)
	  (char-equal ch ?>)
	  (char-equal ch 32)
	  (char-equal ch 10))
      (insert-xml-char "ldquo"))
     ((char-equal ch emacs-ldquo)
      (progn
	(delete-backward-char 1)
	(insert "\"")))
     ((char-equal ch emacs-quot)
      (progn
	(delete-backward-char 1)
	(insert-xml-char "rdquo")))
     ((char-equal ch emacs-rdquo)
      (progn
	(delete-backward-char 1)
	(insert-xml-char "ldquo")))
     ((char-equal ch emacs-ldquo)
      (progn
	(delete-backward-char 1)
	(insert-xml-char "rdquo")))
     ((char-equal ch emacs-lsquo)
      (insert-xml-char "ldquo"))
     (t (insert-xml-char "rdquo")))))

(defun smart-single-quote ()
  "Insert a left or right single quote as appropriate. Left quotes are inserted after a space, newline, or >. Right quotes are inserted after any other character, except if the preceding character is a quote, cycles through the three quote styles."
  (interactive)
  (let ((ch (char-before)))
    (cond
     ((in-start-tag)
      (insert "'"))
     ((or (not ch)
	  (char-equal ch ?>)
	  (char-equal ch 32)
	  (char-equal ch 10))
      (insert-xml-char "lsquo"))
     ((char-equal ch emacs-lsquo)
      (progn
	(delete-backward-char 1)
	(insert "'")))
     ((char-equal ch emacs-apos)
      (progn
	(delete-backward-char 1)
	(insert-xml-char "rsquo")))
     ((char-equal ch emacs-rsquo)
      (progn
	(delete-backward-char 1)
	(insert-xml-char "lsquo")))
     ((char-equal ch emacs-ldquo)
      (insert-xml-char "lsquo"))
     (t (insert-xml-char "rsquo")))))

;;; EOF
