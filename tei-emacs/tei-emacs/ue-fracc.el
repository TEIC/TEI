;;; $Id$
;;; Copyright (C) 1994 by C.Queinnec (Polytechnique & INRIA)

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; This package is based on C.Queinnec fracc.el package.
;;; It has been modified to provide the same conventions to type
;;; accented characters as the ftex mode in micro-emacs written by Eric
;;; Picheral.

;;; This package is based on Marc Shapiro's ftex-mode.el package.
;;; It has been extended over time to provide various kinds of accents
;;; for TeX, LaTeX, BibTeX (they're not the same), HTML, ISO latin 1 or
;;; Mac.

;;; The basic purpose of this package is to provide an uniform way to
;;; type accents (for instance e followed by ' to mean e acute) and to
;;; convert the intended character in conformance with the local
;;; conventions of the buffer. HTML codes accented letters in a way
;;; (although some servers accept ISO 889 letters), BibTeX (at least
;;; mine, does not support accents at all and expects them to be
;;; wrapped inside { }), etc. 

;;; This package also takes into account ligatures such as oei and
;;; cedillas. It is entirely driven by tables specifying contexts
;;; that, if followed by an accent, have to be modified.
;;; Are currently supported the following configurations:
;;;   e'   [aeou][`^]   [eiu]#   c~[au]   oe[iu]    ~[:;?!]
;;; for (La)TeX, BibTeX, HTML, ISO 8859, Mac.

;;; Installation:

;;; To use this package, add to your .emacs (taking care of load-path)
;;;      (require 'fracc)
;;; or   (autoload 'fracc-mode "fracc")
;;; When installed, you can activate it with \M-x fracc-mode.
;;; Alternatively, you may want to have fracc available all the time so
;;; in addition you can tell that:
;;;   (add-hook 'find-file-hooks
;;;     '(lambda () (fracc-mode (fracc-select-default-encoding))) )
;;; Still alternatively, you can install a call to fracc-mode on various 
;;; mode hooks. For instance,
;;;   (add-hook 'tex-mode-hook
;;;     '(lambda () (fracc-mode fracc-tex-encoding)) )

;;; Repository:

;;; Newer versions will be sent to the LCD Archive but may appear earlier
;;; on ftp.inria.fr:INRIA/Projects/icsla/Miscellaneous/fracc.el
;;; Other Emacs packages can be found with World Wide Web with URL:
;;;     ftp://ftp.inria.fr/INRIA/Projects/icsla/WWW/elisp.html

;;; Code:

(defvar fracc-encoding nil
  "This buffer-specific variable is true whenever the fracc mode is on.
Its value designates the precise encoding to be used for French accents
(hence the name of the mode)." )
(make-variable-buffer-local 'fracc-encoding)

;;; Default encoding to be used. Will be appropriately set below.

(defvar fracc-default-encoding 'see-below
  "This variable defines the default encoding to be used by fracc when
no specific encoding is specified on the property-list of the major-mode." )

;;; These keymaps intercept accents ' ` ^ #[to code trema] 
;;; hard vowels: a o u
;;; another vowel: i
;;; punctuations: ? ! ; :

(defvar fracc-keymap
  (make-sparse-keymap)
  "This is the keymap used by the fracc (French accents) minor mode." )

(define-key fracc-keymap "'" 'fracc-acute)
(define-key fracc-keymap "`" 'fracc-grave)
(define-key fracc-keymap "^" 'fracc-circumflex)
(define-key fracc-keymap "#" 'fracc-trema)
(define-key fracc-keymap "\"" 'fracc-trema)
(define-key fracc-keymap "a" 'fracc-ao-vowel)
(define-key fracc-keymap "o" 'fracc-ao-vowel)
(define-key fracc-keymap "u" 'fracc-u-vowel)
(define-key fracc-keymap "i" 'fracc-i-vowel)
(define-key fracc-keymap "A" 'fracc-ao-vowel)
(define-key fracc-keymap "O" 'fracc-ao-vowel)
(define-key fracc-keymap "U" 'fracc-u-vowel)
(define-key fracc-keymap "I" 'fracc-i-vowel)
(define-key fracc-keymap "?" 'fracc-punctuation)
(define-key fracc-keymap "!" 'fracc-punctuation)
(define-key fracc-keymap ";" 'fracc-punctuation)
(define-key fracc-keymap ":" 'fracc-punctuation)

(defun fracc-acute () (interactive) (fracc-handle-accent ?'))
(defun fracc-grave () (interactive) (fracc-handle-accent ?`))
(defun fracc-circumflex () (interactive) (fracc-handle-accent ?^))
(defun fracc-trema () (interactive) (fracc-handle-accent ?#))
(defun fracc-ao-vowel () (interactive) (fracc-handle-accent ?a))
(defun fracc-u-vowel () (interactive) (fracc-handle-accent ?u))
(defun fracc-i-vowel () (interactive) (fracc-handle-accent ?i))
(defun fracc-punctuation () (interactive) (fracc-handle-accent ?!))

;;; Install a minor mode keymap. It will be active if the
;;; fracc-encoding variable is not nil. Limit the visibility of
;;; this minor mode to the local buffer only.

(defun fracc-mode (encoding)
  "Handle French accent and convert them to some appropriate encoding depending
on the major mode. An argument can be given to force the encoding to use.
Possible encodings are:
	fracc-no-encoding
	fracc-ISO-8859-encoding
	fracc-tex-encoding
	fracc-tex8-encoding
	fracc-bibtex-encoding
	fracc-html-encoding
	fracc-Mac-encoding
	...
"
  (interactive (list (fracc-select-default-encoding)))
  (make-variable-buffer-local 'minor-mode-map-alist)
  (or (assq 'fracc-encoding minor-mode-map-alist)
      (setq minor-mode-map-alist
            (cons (cons 'fracc-encoding fracc-keymap)
                  minor-mode-map-alist ) ) )
  (setq fracc-encoding encoding)
  ;; support for isearch
  (or (assq 'fracc-isearch-buffer minor-mode-map-alist)
      (setq minor-mode-map-alist
            (cons (cons 'fracc-isearch-buffer fracc-isearch-keymap)
                  minor-mode-map-alist ) ) )
  t )

;;; This function tries to guess the appropriate encoding of accents.
;;; It currently uses the major-mode but can use anything reachable.

(defun fracc-select-default-encoding ()
  "Select the encoding of French accents based on the current major mode.
This encoding is stored in the property list of the major-mode under
key: fracc-encoding. When you load the fracc package, default fracc modes
are defined for (plain-)(la)(sli)(bib)tex-mode and html-(helper-)mode. If no
specific encoding is given then use the encoding specified by the
fracc-default-encoding variable."
  (let ((encoding (get major-mode 'fracc-encoding)))
    (or encoding
        fracc-default-encoding ) ) )

;;; The basic function to handle accents (and punctuation signs and
;;; hard vowels and others). If nothing can be tried then just insert
;;; the accent.

(defun fracc-handle-accent (accent)
  "This function takes an accent, extracts from the current encoding the
possible contexts where an action is possible, checks if one of this
context is met then remove that context and perform the action.
  The encoding is formatted as an alist:
     (case-fold-search-value (accent-char . clauses) ... )
Each clause is ( context-string  . actions )
  means if preceding characters are matched by context-string then remove
  them and process the actions. NOTE: context-string is not a regexp.
Actions may be ( . \"string\" )
  which means insert string.
            or ()
  which means insert the accent
            or ( \"string\" . actions)
  which means insert string then continue to process actions.
            or ( expression . actions)
  which means evaluate expression then continue to process actions.
  An interesting particular case is:
            or ( \"\\&\" . actions)
  which means insert the context-string and continue to process actions.
  This works since all strings are inserted with replace-match,
  therefore they may use \\& to match the context-string.
"
  (let ((specs (assq accent (cdr fracc-encoding))))
    (if (consp specs)
        (let* ((clauses (cdr specs))
               (case-fold-search (car fracc-encoding))
               (fixedcase (not case-fold-search))
               size *context-string* action 
               (dot (point))
               (performedp nil) )
          (while (consp clauses)
            (goto-char dot)
            (setq *context-string* (car (car clauses)))
            (setq size (length *context-string*))
            (setq action (cdr (car clauses)))
            (setq clauses (cdr clauses))
            (condition-case ()
                (progn
                  (backward-char size)
                  (if (search-forward *context-string* dot t)
                      (let ((md (match-data))
                            (cs *context-string*) )
                        (setq clauses nil)
                        (setq performedp t)
                        (while (consp action)
                          (if (stringp (car action))
                              (fracc-replace-match 
                               (car action) md cs fixedcase )
                            (eval (car action)) )
                          (setq action (cdr action)) )
                        (if (stringp action)
                            (fracc-replace-match action md cs fixedcase)
                          (self-insert-command 1) ) )
                    (goto-char dot) ) )
              (error (goto-char dot)) ) )
          (if performedp 'nothing
            (self-insert-command 1) ) )
      (self-insert-command 1) ) ) )

;;; NOTE: case-fold-search is t that means that search ignores case.
;;; if nil, then case of letters is respected when searching.
;;; For fixedcase the second argument of replace-match:
;;; if t then the case of the replacement text is respected
;;; if nil then the case of the replacement text is adapted.
;;; This is the opposite of case-fold-search.

;;; This function uses *context-string* and the two markers *beg* and
;;; *end*.  It is similar to replace-match except that it refills the
;;; initial content of the zone to be replaced. Some experiments
;;; showed that it is better for replace-match that the zone to
;;; replace is not empty (it seems then to capture the next char).

(defun fracc-replace-match (regexp md cs fixedcase)
  "This function acts as replace-match to replace the *context-string* with
REGEXP in the buffer. It may be used more than once due to multiple actions."
  (let ((*beg* (car md))
        (*end* (car (cdr md)))
        (*context-string* cs) )
    (if (equal *beg* *end*)
        ;; this is not the first replace-match
        (progn
          (insert *context-string*)
          (move-marker *end* (point)) ) )
    (store-match-data md)
    (replace-match regexp fixedcase)
    (move-marker *beg* (point)) ) )

;;;==================================================================
;;; Special support for incremental search. This is still experimental
;;; because there is a lot of problems interacting with isearch.  So
;;; if you want this to be active, modify the boolean
;;; fracc-isearch-wanted, otherwise don't care.

(defvar fracc-isearch-wanted nil
  "Boolean telling if fracc is active while isearching strings." )

(defvar fracc-isearch-keymap nil
  "This is the keymap used by isearch with French accent support." )

(defun fracc-build-isearch-keymap ()
  "Build the keymap used by isearch with French accent support."
  (or fracc-isearch-keymap
      (let ((map (copy-keymap isearch-mode-map)))
        (define-key map "'" 'fracc-isearch-handle-char)
        (define-key map "'" 'fracc-isearch-handle-char)
        (define-key map "`" 'fracc-isearch-handle-char)
        (define-key map "^" 'fracc-isearch-handle-char)
        (define-key map "#" 'fracc-isearch-handle-char)
        (define-key map "\"" 'fracc-isearch-handle-char)
        (define-key map "a" 'fracc-isearch-handle-char)
        (define-key map "o" 'fracc-isearch-handle-char)
        (define-key map "u" 'fracc-isearch-handle-char)
        (define-key map "i" 'fracc-isearch-handle-char)
        (define-key map "A" 'fracc-isearch-handle-char)
        (define-key map "O" 'fracc-isearch-handle-char)
        (define-key map "U" 'fracc-isearch-handle-char)
        (define-key map "I" 'fracc-isearch-handle-char)
        (define-key map "?" 'fracc-isearch-handle-char)
        (define-key map "!" 'fracc-isearch-handle-char)
        (define-key map ";" 'fracc-isearch-handle-char)
        (define-key map ":" 'fracc-isearch-handle-char)
        (setq fracc-isearch-keymap map) ) ) )

;;; The trick is to associate a buffer to the isearch-string, to
;;; perform accent management in it and to copy its new content
;;; back to isearch-string.

(make-variable-buffer-local 'fracc-isearch-buffer)
(setq-default fracc-isearch-buffer nil)

(defun fracc-isearch-handle-char ()
  "This function handles French accents under isearch."
  (interactive)
  (let ((accent (isearch-last-command-char)))
    ;; Searching for e' is in fact searching for \'e which may appear
    ;; before e. Should return to the beginning but also preserve
    ;; backspacing on the pattern to search.  FUTURE
    (setq isearch-string
          (save-excursion
            (set-buffer fracc-isearch-buffer)
            (erase-buffer)
            (insert isearch-string)
            (fracc-handle-accent accent)
            (buffer-string) ) )
    (setq isearch-message isearch-string)
    (isearch-search-and-update) ) )

(defun fracc-isearch-hook ()
  "This function installs the necessary support for French accents with the
incremental search package (isearch.el)."
  (setq overriding-local-map (fracc-build-isearch-keymap))
  (let ((current-fracc-encoding fracc-encoding))
    (setq fracc-isearch-buffer
          (get-buffer-create " *Fracc-Isearch* ") )
    (save-excursion
      (set-buffer fracc-isearch-buffer)
      (erase-buffer) 
      (fracc-mode current-fracc-encoding) )
    t ) )

(defun fracc-isearch-end-hook ()
  "This function removes the necessary support for French accents with the
incremental search package (isearch.el)."
  (setq fracc-isearch-buffer nil) )

;;; Run the previous function anytime isearch is used but only if
;;; you want to run this experimental stuff.

(if fracc-isearch-wanted
    (progn
      (add-hook 'isearch-mode-hook 'fracc-isearch-hook)
      (add-hook 'isearch-mode-end-hook 'fracc-isearch-end-hook) ) )

;;;==================================================================
;;; Various encodings.

(defvar fracc-no-encoding
  '(nil ; respect case of letters.
    )
  "This is the null encoding where nothing is done." )

;;; Some entries are commented since they do not correspond to French
;;; accents.  They are left as examples for other languages.

(defvar fracc-ISO-8859-encoding
  '(nil ; respect case of letters.
    (?'
     ;;("A" . "\301")
     ("E" . "\311")
     ;;("I" . "\315")
     ;;("O" . "\323")
     ;;("U" . "\332")
     ;;("Y" . "\335")
     ;;("a" . "\341")
     ("e" . "\351")
     ;;("i" . "\355")
     ;;("o" . "\363")
     ;;("u" . "\372")
     ;;("y" . "\375")
     ("'" . "\264") )
    (?`
     ("A" . "\300")
     ("E" . "\310")
     ;;("I" . "\314")
     ;;("O" . "\322")
     ("U" . "\331")
     ("a" . "\340")
     ("e" . "\350")
     ;;("i" . "\354")
     ;;("o" . "\362")
     ("u" . "\371") )
    (?^
     ("A" . "\302")
     ("E" . "\312")
     ("I" . "\316")
     ("O" . "\324")
     ("U" . "\333")
     ("a" . "\342")
     ("e" . "\352")
     ("i" . "\356")
     ("o" . "\364")
     ("u" . "\373") )
    (?#
     ;;("A" . "\304")
     ("E" . "\313")
     ("I" . "\317")
     ;;("O" . "\326")
     ("U" . "\334")
     ;;("a" . "\344")
     ("e" . "\353")
     ("i" . "\357")
     ;;("o" . "\366")
     ;;("s" . "\337")
     ("u" . "\374") )
    (?a                ; means any hard vowel ie [aou]
     ("c~" "\347")
     ("C~" "\307") )
    (?u                ; means only u
     ("c~" "\347")
     ("C~" "\307") )
    ;; nothing for OE (as noted by besancon@excalibur.ens.fr (BESANCON Thierry))
    ;; nothing for punctuations.
 )
  "This is the French encoding towards ISO 8859 accented letters.
This list was adapted from Cedric Beust's 8bits-mode package." )

(defvar fracc-Mac-encoding
  '(nil ;  respect case of letters.
    (?'
     ("E" . "\203")
     ("e" . "\216")
     ("'" . "\323") )
    (?`
     ("A" . "\313")
     ;;("E" . "\???")
     ;;("U" . "\???")
     ("a" . "\210")
     ("e" . "\217")
     ("u" . "\235")
     ("`" . "\322") )
    (?^
     ;;("A" . "\???")
     ;;("E" . "\???")
     ;;("I" . "\???")
     ;;("O" . "\???")
     ;;("U" . "\???")
     ("a" . "\211")
     ("e" . "\220")
     ("i" . "\224")
     ("o" . "\231")
     ("u" . "\236") )
    (?#
     ;;("E" . "\???")
     ("I" . "\225")
     ("U" . "\206")
     ("e" . "\221")
     ("i" . "\225")
     ("u" . "\237") )
    (?a                ; means any hard vowel ie [aou]
     ("c~" "\215")
     ("C~" "\202") )
    (?u                ; means only u
     ("c~" "\215")
     ("C~" "\202")
     ("OE" "\316")
     ("oe" "\317") )
    (?i
     ("OE" "\316")
     ("oe" "\317") )
 )
  "This is the French encoding towards Mac accented letters." )

(defvar fracc-tex8-encoding
  '(nil ; respect case of letters.
    (?'
     ;;("A" . "\301")
     ("E" . "\311")
     ;;("I" . "\315")
     ;;("O" . "\323")
     ;;("U" . "\332")
     ;;("Y" . "\335")
     ;;("a" . "\341")
     ("e" . "\351")
     ;;("i" . "\355")
     ;;("o" . "\363")
     ;;("u" . "\372")
     ;;("y" . "\375")
     ("'" . "\264") )
    (?`
     ("A" . "\300")
     ("E" . "\310")
     ;;("I" . "\314")
     ;;("O" . "\322")
     ("U" . "\331")
     ("a" . "\340")
     ("e" . "\350")
     ;;("i" . "\354")
     ;;("o" . "\362")
     ("u" . "\371") )
    (?^
     ("A" . "\302")
     ("E" . "\312")
     ("I" . "\316")
     ("O" . "\324")
     ("U" . "\333")
     ("a" . "\342")
     ("e" . "\352")
     ("i" . "\356")
     ("o" . "\364")
     ("u" . "\373") )
    (?#
     ;;("A" . "\304")
     ("E" . "\313")
     ("I" . "\317")
     ;;("O" . "\326")
     ("U" . "\334")
     ;;("a" . "\344")
     ("e" . "\353")
     ("i" . "\357")
     ;;("o" . "\366")
     ;;("s" . "\337")
     ("u" . "\374") )
    (?a                ; means any hard vowel ie [aou]
     ("c~" "\347")
     ("C~" "\307") )
    (?u                ; means only u
     ("c~" "\347")
     ("C~" "\307")
     ("oe" "\\\\\\&{}")
     ("OE" "\\\\\\&{}") )
    (?i
     ("oe" "\\\\\\&{}")
     ("OE" "\\\\\\&{}") )
    (?!
     ("~" " ")
     ("\t" " ")
     )
    (?;
     ("~" " ")
     ("\t" " ")
     )
    (?:
     ("~" " ")
     ("\t" " ")
     )
    (??
     ("~" " ")
     ("\t" " ")
     )
 )
  "This is the encoding used for French accent in TeX mode (iso-latin1)." ) 

(defvar fracc-tex-encoding
  '(t ; ignore case of letters.
    (?'
     ("e''" . "\\\\'\\&")
     ("\\'e" "e'")       ; could also be written as ("\\'e" . "e''")
     ("e"   . "\\\\'\\&") )
    (?`
     ("\\`a" "\\&")
     ("\\`e" "\\&")
     ("\\`o" "\\&")
     ("\\`u" "\\&")
     ("a" . "\\\\`\\&")
     ("e" . "\\\\`\\&")
     ("o" . "\\\\`\\&")
     ("u" . "\\\\`\\&") )
    (?^
     ("\\^a" "\\&")
     ("\\^e" "\\&")
     ("\\^i" "\\&")
     ("\\^o" "\\&")
     ("\\^u" "\\&")
     ("a" . "\\\\^\\&")
     ("e" . "\\\\^\\&")
     ("i" . "\\\\^\\\\\\&{}")
     ("o" . "\\\\^\\&")
     ("u" . "\\\\^\\&") )
    (?#
     ("\\\"e" "\\&")
     ("\\\"i" "\\&")
     ("\\\"u" "\\&")
     ("e" . "\\\\\"\\&")
     ("i" . "\\\\\"\\\\\\&{}")
     ("u" . "\\\\\"\\&") )
    (?a
     ("c~" "\\\\c{c}") )
    (?u
     ("c~" "\\\\c{c}")
     ("oe" "\\\\\\&{}") )
    (?i
     ("oe" "\\\\\\&{}") )
    (?!
     ("~" "~")
     (" " "~")
     (""  "~") )
    )
  "This is the encoding used for French accent in TeX mode (ascii)." ) 
    
(defvar fracc-bibtex-encoding
  '(t ; ignore case of letters.
    (?'
     ("e''" . "{\\\\'e}''")
     ("{\\'e}" "e'")
     ("e"   . "{\\\\'e}") )
    (?`
     ("a" . "{\\\\`\\&}")
     ("e" . "{\\\\`\\&}")
     ("o" . "{\\\\`\\&}")
     ("u" . "{\\\\`\\&}") )
    (?^
     ("a" . "{\\\\^\\&}")
     ("e" . "{\\\\^\\&}")
     ("i" . "{\\\\^\\\\\\&}")
     ("o" . "{\\\\^\\&}")
     ("u" . "{\\\\^\\&}") )
    (?#
     ("e" . "{\\\\\"\\&}")
     ("i" . "{\\\\\"\\\\\\&}")
     ("u" . "{\\\\\"\\&}") )
    (?a
     ("c~" "{\\\\c{c}}") )
    (?u
     ("c~" "{\\\\c{c}}")
     ("oe" "{\\\\\\&}") )
    (?i
     ("oe" "{\\\\\\&}") )
    )
  "This is the encoding used for French accent in BibTeX mode.
The difference with TeX mode is that accented letters are wrapped inside
{ and }." )

;;; NOTE: Problem on C~a giving \C{C}a.  Fortunately I never wrote a
;;; sentence beginning with that sequence of letters.

(defvar fracc-html-encoding
  '(nil ; respect case of letters.
    (?'
     ("e''" . "&eacute;''")
     ("&eacute;" "e'")
     ("e"   . "&eacute;")
     ("E''" . "&Eacute;''")
     ("&Eacute;" "E'")
     ("E"   . "&Eacute;") )
    (?`
     ("a" . "&agrave;")
     ("e" . "&egrave;")
     ("u" . "&ugrave;")
     ("A" . "&Agrave;")
     ("E" . "&Egrave;")
     ("U" . "&Ugrave;") )
    (?^
     ("a" . "&acirc;")
     ("e" . "&ecirc;")
     ("i" . "&icirc;")
     ("o" . "&ocirc;")
     ("u" . "&ucirc;")
     ("A" . "&Acirc;")
     ("E" . "&Ecirc;")
     ("I" . "&Icirc;")
     ("O" . "&Ocirc;")
     ("U" . "&Ucirc;") )
    (?#
     ("e" . "&euml;")
     ("i" . "&iuml;")
     ("u" . "&uuml;")
     ("E" . "&Euml;")
     ("I" . "&Iuml;")
     ("U" . "&Uuml;") )
    (?a
     ("c~" "&ccedil;")
     ("c~" "&Ccedil;") )
    (?u
     ("c~" "&ccedil;")
     ("c~" "&Ccedil;") )
    ;; oe does not exist.
    )
  "This is the encoding used for French accent in HTML document." )
;;; NOTE: the encoding respects case since Netscape seems to dislike
;;; things like &AGRAVE;. Bug signalled by Paul Provost <paulp@buzzimage.com>.
 
;;; Specify default encoding for various modes.

(put 'tex-mode		'fracc-encoding fracc-tex8-encoding)
(put 'plain-tex-mode	'fracc-encoding fracc-tex8-encoding)
(put 'latex-mode	'fracc-encoding fracc-tex8-encoding)
(put 'slitex-mode	'fracc-encoding fracc-tex8-encoding)
(put 'bibtex-mode	'fracc-encoding fracc-bibtex-encoding)
(put 'html-mode		'fracc-encoding fracc-html-encoding)
(put 'html-helper-mode	'fracc-encoding fracc-html-encoding)

;;; Specify the default encoding to be used:

(setq fracc-default-encoding fracc-ISO-8859-encoding)

;;; so this package can be required

(provide 'fracc)

;;; FUTURE, implement a help mode explaining active substitutions.

;;; end of fracc.el
