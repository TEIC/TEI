;;; unidata.el --- UNIDATA parser and viewer.

;; Copyright (C) 2000-2001 MIYASHITA Hisashi

;; Keywords: character set, ISO10646, Unicode

;; This file is part of Mule-UCS

;; Mule-UCS is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Mule-UCS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Comment: This program parses and shows UNIDATA distributed by
;;          Unicode Consortium.

(require 'trans-util)
(require 'txt-tbl)
(or (featurep 'mule-ucs-unicode)
    (load-library "unicode"))
;(require 'unicode)

(defvar unidata-default-file-name
  "UnicodeData.txt"
  "*The filename of UNIDATA distributed by Unicode consortium.")

(defvar unidata-temporary-buffer-name
  "*unidata tmp*")

(defvar unidata-temporary-buffer nil)

(defvar unidata-field-properties
  '((codepoint hex)
    (name string)
    (category
     (;; Normative categories
      ("Lu" . (letter uppercase))
      ("Ll" . (letter lowercase))
      ("Lt" . (letter titlecase))
      ("Mn" . (mark non-spacing))
      ("Mc" . (mark spacing-combining))
      ("Me" . (mark enclosing))
      ("Nd" . (number decimal-digit))
      ("Nl" . (number letter))
      ("No" . (number other))
      ("Zs" . (separator space))
      ("Zl" . (separator line))
      ("Zp" . (separator paragraph))
      ("Cc" . (other control))
      ("Cf" . (other format))
      ("Cs" . (other surrogate))
      ("Co" . (other private))
      ("Cn" . (other not-assigned))
      ;; Informative categories
      ("Lm" . (letter modifier))
      ("Lo" . (letter other))
      ("Pc" . (punctuation connector))
      ("Pd" . (punctuation dash))
      ("Ps" . (punctuation open))
      ("Pe" . (punctuation close))
      ("Pi" . (punctuation initial-quotation))
      ("Pf" . (punctuation final-quotation))
      ("Po" . (punctuation other))
      ("Sm" . (symbol math))
      ("Sc" . (symbol currency))
      ("Sk" . (symbol modifier))
      ("So" . (symbol other))))
    (combining-class
     number
     (;;(0 . "Spacing, split, enclosing, reordrant, and Tibetan subjoined")
      ;; too long explanation, I shorten this.
      (0 . "Spacing")
      (1 . "Overlays and interior")
      (7 . "Nuktas") 
      (8 . "Hiragana/Katakana voicing marks")
      (9 . "Viramas")
      (10 . "Start of fixed position classes")
      (199 . "End of fixed position classes")
      (200 . "Below left attached")
      (202 . "Below attached")
      (204 . "Below right attached")
      (208 . "Left attached (reordrant around single base character)")
      (210 . "Right attached")
      (212 . "Above left attached")
      (214 . "Above attached")
      (216 . "Above right attached")
      (218 . "Below left")
      (220 . "Below")
      (222 . "Below right")
      (224 . "Left (reordrant around single base character)")
      (226 . "Right")
      (228 . "Above left")
      (230 . "Above")
      (232 . "Above right")
      (233 . "Double below")
      (234 . "Double above")
      (240 . "Below (iota subscript)")))
    (bidirectional-category
     string
     (("L" . "Left-to-Right")
      ("LRE" . "Left-to-Right Embedding")
      ("LRO" . "Left-to-Right Override")
      ("R" . "Right-to-Left")
      ("AL" . "Right-to-Left Arabic")
      ("RLE" . "Right-to-Left Embedding")
      ("RLO" . "Right-to-Left Override")
      ("PDF" . "Pop Directional Format")
      ("EN" . "European Number")
      ("ES" . "European Number Separator")
      ("ET" . "European Number Terminator")
      ("AN" . "Arabic Number")
      ("CS" . "Common Number Separator")
      ("NSM" . "Non-Spacing Mark")
      ("BN" . "Boundary Neutral")
      ("B" . "Paragraph Separator")
      ("S" . "Segment Separator")
      ("WS" . "Whitespace")
      ("ON" . "Other Neutrals")))
    (decomposition-mapping string)
    (decimal-digit-value number)
    (digit-value number)
    (numeric-value number)
    (mirrored (("Y" . mirrored)
	       ("N" . not-mirrored)))
    (oldname string)
    (iso106460-comment string)
    (uppercase-mapping number)
    (lowercase-mapping number)
    (titlecase-mapping number)))

(defvar unidata-values
  (make-vector (cn "0x10000") nil))

(defun unidata-set-entry (codepoint val)
  (aset unidata-values codepoint val))

(defun unidata-setup (&optional filename revert)
  (setq filename (or filename
		     unidata-default-file-name))
  (if (not (file-readable-p filename))
      (signal 'file-error
	      (list
	       "Cannot UNIDATA file:" filename)))
  (if (or revert
	  (not (and (bufferp unidata-temporary-buffer)
		    (buffer-live-p unidata-temporary-buffer))))
      (progn
	(setq unidata-temporary-buffer
	      (get-buffer-create unidata-temporary-buffer-name))
	(save-excursion
	  (set-buffer unidata-temporary-buffer)
	  (erase-buffer)
	  (insert-file-contents filename)
	  (goto-char (point-min)))))
  unidata-temporary-buffer)

(defun unidata-read-until (codepoint)
  (let (cp
	begin end line
	fields curfield
	slots curslot
	alist val prop spec)
    (save-excursion
      (set-buffer (unidata-setup))
      (while (progn
	       (setq begin (point))
	       (if (>= begin (point-max))
		   nil
		 (beginning-of-line 2)
		 (setq end (point))
		 (setq line (buffer-substring begin end))
		 (if (not (string-match ";" line))
		     t
		   (setq slots (split-string line ";")
			 fields unidata-field-properties
			 alist nil)
		   (while (and (setq curfield (car fields))
			       (setq curslot (car slots)))
		     (setq fields (cdr fields)
			   slots (cdr slots))
		     (if (string= curslot "")
			 nil
		       (setq prop (car curfield)
			     spec (nth 1 curfield))

		       (cond ((eq spec 'string)
			      (setq val curslot))
			     ((eq spec 'number)
			      (setq val (c-notated-string-to-number curslot)))
			     ((eq spec 'hex)
			      (setq val (hex-string-to-number curslot)))
			     ((listp spec)
			      (setq val (cdr (assoc curslot spec)))
			      (if (null val)
				  (error "Invalid value:%s" curslot)))
			     (t
			      (error "Invalid spec:%S" spec)))
		       (setq alist (cons (cons prop val) alist))))
		   (setq cp (cdr (assq 'codepoint alist)))
		   (if (null cp)
		       (error "Cannot identify the codepoint." line))
		   (message "Set entry:%X" cp)
		   (unidata-set-entry cp (nreverse alist))
		   (if (and codepoint
			    (= cp codepoint))
		       nil t))))))))

(defun unidata-get-entry (codepoint)
  (or (aref unidata-values codepoint)
      (progn
	(unidata-read-until codepoint)
	(aref unidata-values codepoint))))

(defun unidata-get-property (codepoint prop)
  (cdr (assq prop (unidata-get-entry codepoint))))

(defun unidata-update (&optional filename)
  (unidata-setup filename t)
  (unidata-read-until nil))

(defun unidata-display-entry (char-or-codepoint charp)
  (let* ((codepoint (if charp (char-to-ucs char-or-codepoint)
		      char-or-codepoint))
	 (char (if charp char-or-codepoint
		 (ucs-to-char codepoint)))
	 (alist (if codepoint (unidata-get-entry codepoint)))
	 elem prop spec type print-method tmp str)
    (insert
     (if charp
	 (format
	  "Character `%c' UNIDATA information.\n"
	  char)
       (format
	"U+%04X UNIDATA information.\n"
	codepoint)))
    (insert  "---------------------------------\n")
    (cond ((and charp codepoint)
	   (insert
	    (format
	     " This is converted to U+%04X\n"
	     codepoint)))
	  ((and (not charp) char)
	   (insert 
	    (format
	     " This is converted to a character:%c%S\n"
	     char (split-char char))))
	  (t
	   (insert " This cannot be converted \n")))
    (insert "under the current environment.\n\n")
    (if (null alist)
	(insert "===NO UNIDATA ENTRY!!!===\n")
      (while (setq elem (car alist))
	(setq prop (car elem)
	      alist (cdr alist))
	(if (not (eq prop 'codepoint))
	    (progn
	      (indent-to 4)
	      (insert (symbol-name prop))
	      (indent-to 32)
	      (setq spec (assq prop unidata-field-properties)
		    type (nth 1 spec)
		    print-method (nth 2 spec)
		    str
		    (cond ((eq type 'string)
			   (cdr elem))
			  ((eq type 'number)
			   (number-to-string (cdr elem)))
			  ((eq type 'hex)
			   (format "0x%X" (cdr elem)))
			  ((listp type)
			   (format "%S" (cdr elem)))
			  (t
			   (error "Invalid property slot:%S" spec))))

	      (if (and print-method
		       (setq tmp (assoc (cdr elem)
					print-method)))
		  (setq str (concat str " => " (cdr tmp))))
		       
	      (insert str ?\n)))))))

(defun unidata-show-entry-from-UCS (codepoint)
  "View UNIDATA entry corresponding to the given UCS codepoint."
  (interactive "nUCS codepoint:")
  (with-output-to-temp-buffer "*Unidata Entry*"
    (save-excursion
     (set-buffer standard-output)
     (unidata-display-entry codepoint nil)
     (help-mode))))

(defun unidata-show-entry-from-char (char)
  "View UNIDATA entry corresponding to the char
located at the current point."
  (interactive (list (following-char)))
  (with-output-to-temp-buffer "*Unidata Entry*"
    (save-excursion
     (set-buffer standard-output)
     (unidata-display-entry char t)
     (help-mode))))

(provide 'unidata)
