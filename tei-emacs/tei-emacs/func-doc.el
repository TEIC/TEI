;; func-doc.el - Get Info on command under point; completion for commands.
;;
;; Copyright (C) 1996, 1997, 1998 Peter S. Galbraith
 
;; Author:    Peter S. Galbraith <GalbraithP@dfo-mpo.gc.ca>
;; Created:   15 Jan 1996.
;; Version:   1.09 (5 January 1998)
;; Keywords:  info, completion, commands, latex, perl, awk, texinfo

;; RCS $Id$
;; Note: RCS version number does not correspond to release number.

;; Everyone is granted permission to copy, modify and redistribute this
;; file provided:
;;   1. All copies contain this copyright notice.
;;   2. All modified copies shall carry a prominant notice stating who
;;      made modifications and the date of such modifications.
;;   3. The name of the modified file be changed.
;;   4. No charge is made for this software or works derived from it.
;;      This clause shall not be construed as constraining other software
;;      distributed on the same medium as this software, nor is a
;;      distribution fee considered a charge.

;; LCD Archive Entry:
;; func-doc|Peter Galbraith|GalbraithP@dfo-mpo.gc.ca|
;; Get Info on command under point; completion for commands.|
;; 07-August-1996|1.05|~/misc/func-doc.el
;; ----------------------------------------------------------------------------
;;; Commentary:

;; New versions of this package (if they exist) may be found at:
;;  ftp://ftp.phys.ocean.dal.ca/users/rhogee/elisp/func-doc.el

;; Description:
;;
;;  This package provides the `func-doc' command to interface to Info for
;;  functions and commands in various modes, as well as command completion.
;;
;;  The package currently supports Awk, Perl, texinfo and LaTeX but is
;;  extensible.  If you've got an Info file with a node that list functions,
;;  than this package can probably interface to it.
;;  Also makes Meta-[TAB] (aka M-\t) do typing completion using the keywords
;;  extracted from the function list.
;;
;;  In addition, it works in Info files for supported topics.  e.g. in the
;;  Info file "gawk", you may use "C-c d" (func-doc) to look-up the page
;;  about a given command.  This is faster than using the Info index node.

;;  Installation instructions:
;;
;;  All you need to do is add this line to your .emacs file:
;;   (require 'func-doc)
;;  making the commands `func-doc' and `func-doc-complete' available.
;;
;;  To make func-doc display the Info buffer in another window, set the
;;  following variable to `t' in your ~/.emacs file: 
;;   (setq func-doc-split-window t)
;;
;;  It is suggested that you make `C-c d' locally bound to func-doc in
;;  supported modes and M-[tab] bound to `func-doc-complete' (Note that
;;  auc-tex already provides completion for latex, and you probably don't want
;;  to override it).  The code to do this is left uncommented below, it's up
;;  to you to leave it, comment it out, or paste it in your ~/.emacs file.
;;
;;  You may also use "C-h d" by adding this to yor ~/.emacs file:
;;    (define-key help-map "d" 'func-doc)
;;
;;  For XEmacs, the code below must be loaded *before* you edit a file which
;;  will use func-doc (except for info which should be loaded before to
;;  define "C-c d", unless you use "C-h d" in the help-map).

(cond 
 ((fboundp 'eval-after-load)
  ;; Better method of installation

  (eval-after-load                        ;For awk-mode
   "awk-mode" 
   '(progn 
      (require 'cc-mode)                ;c-mode-map defined here for Emacs-19
      (require 'c-mode)                 ;c-mode-map defined here for Emacs-20
      (define-key c-mode-map "\M-\t" 'func-doc-complete)
      (define-key c-mode-map "\C-cd" 'func-doc)))
  
  (eval-after-load                        ;For perl-mode
   "perl-mode" 
   '(progn 
      (define-key perl-mode-map "\M-\t" 'func-doc-complete)
      (define-key perl-mode-map "\C-cd" 'func-doc)))
  
  (eval-after-load                        ;For emacs' latex-mode
   "latex-mode" 
   '(progn 
      (define-key tex-mode-map "\M-\t" 'func-doc-complete)
      (define-key tex-mode-map "\C-cd" 'func-doc)))
  
  (eval-after-load                        ;For auc-tex's LaTeX-mode:
   "latex" 
   ;; Note: not defining M-[tab] because auc-tex has in-text completion, but
   ;; func-doc-complete `may' know some completions unknown to auc-tex.
   '(define-key LaTeX-mode-map "\C-cd" 'func-doc))
  
  (eval-after-load                        ;For texinfo
   "texinfo" 
   '(progn 
      (define-key texinfo-mode-map "\M-\t" 'func-doc-complete)
      (define-key texinfo-mode-map "\C-cd" 'func-doc)))
  
  (eval-after-load                        ;For auc-tex's TeXinfo mode:
   "tex-info" 
   ;; Note: overriding auctex's M-[tab] definition!
   '(progn 
      (define-key TeXinfo-mode-map "\M-\t" 'func-doc-complete)
      (define-key TeXinfo-mode-map "\C-cd" 'func-doc)))

  (eval-after-load                        ;For auc-tex's TeXinfo mode:
   "info" 
   '(progn 
      (define-key Info-mode-map "\C-cd" 'func-doc))))
 (t
  ;; XEmacs doesn't have eval-after-load
  (defun func-doc-awk-mode-hook ()
    (require 'cc-mode)
    (define-key c-mode-map "\M-\t" 'func-doc-complete)
    (define-key c-mode-map "\C-cd" 'func-doc)
    (remove-hook 'awk-mode-hook 'func-doc-awk-mode-hook))
  (defun func-doc-perl-mode-hook () 
    (define-key perl-mode-map "\M-\t" 'func-doc-complete)
    (define-key perl-mode-map "\C-cd" 'func-doc)
    (remove-hook 'perl-mode-hook 'func-doc-perl-mode-hook))
  (defun func-doc-latex-mode-hook () 
    (define-key tex-mode-map "\M-\t" 'func-doc-complete)
    (define-key tex-mode-map "\C-cd" 'func-doc)
    (remove-hook 'tex-mode-hook 'func-doc-latex-mode-hook))
  (defun func-doc-LaTeX-mode-hook () 
   ;; Note: not defining M-[tab] because auc-tex has in-text completion, but
   ;; func-doc-complete `may' know some completions unknown to auc-tex.
   (define-key LaTeX-mode-map "\C-cd" 'func-doc)
   (remove-hook 'LaTeX-mode-hook 'func-doc-LaTeX-mode-hook))
  (defun func-doc-texinfo-mode-hook () 
    (define-key texinfo-mode-map "\M-\t" 'func-doc-complete)
    (define-key texinfo-mode-map "\C-cd" 'func-doc)
    (remove-hook 'texinfo-mode-hook 'func-doc-texinfo-mode-hook))
  (defun func-doc-tex-info-mode-hook () 
   ;; Note: overriding auctex's M-[tab] definition!
    (define-key TeXinfo-mode-map "\M-\t" 'func-doc-complete)
    (define-key TeXinfo-mode-map "\C-cd" 'func-doc)
    (remove-hook 'TeXinfo-mode-hook 'func-doc-TeXinfo-mode-hook))
  (add-hook 'awk-mode-hook 'func-doc-awk-mode-hook)
  (add-hook 'perl-mode-hook 'func-doc-perl-mode-hook)
  (add-hook 'latex-mode-hook 'func-doc-latex-mode-hook)
  (add-hook 'LaTeX-mode-hook 'func-doc-LaTeX-mode-hook)
  (add-hook 'texinfo-mode-hook 'func-doc-texinfo-mode-hook)
  (add-hook 'TeXinfo-mode-hook 'func-doc-TeXinfo-mode-hook)
  (if (boundp 'Info-mode-map)
      (define-key Info-mode-map "\C-cd" 'func-doc))))

;; Configuration
;;
;;  In the variable func-doc-config (defined below), the third element of each
;;  list entry is the name of the info file to use.  By default, these are set
;;  to "latex" for LaTeX-mode, and "perl" for perl-mode.  If you choose to use
;;  other Info files, like "latex2e" or "perl5", you should edit that field
;;  accordingly.  
;;
;;  func-doc-config has multiple entries for latex commented out.  If one
;;  doesn't work with your info file, please try the other by changing the
;;  comments.  If in a latex-mode, func-doc gives you an error like: 
;;      No such node: Command Index 
;;  then you have different latex info files and should look at what
;;  version you have (C-h i, select the latex page and see what version it
;;  says) and change the configuration in func-doc-config below.
;;
;;  func-doc-config has two entries for texinfo.  The one commented out seeks
;;  the info nodes where the commands are more generally discussed. It's really
;;  up to you which one you prefer to use.
;;
;;  ** If you modify func-doc-config, you have to restart emacs to use it **

;; Info files:  
;;
;;  This package interfaces to Info files about LaTeX, texinfo, Perl and gawk.
;;  Here's where you can find some of these:
;;
;;  LaTeX
;;   in a CTAN site (ftp.shsu.edu) or mirror (ftp.duke.edu) in 
;;    /tex-archive/info/latex-help-texinfo/latex.texi
;;    /tex-archive/info/latex-help-texinfo/latex2.texi
;;    /tex-archive/info/latex2e-help-texinfo/latex2e.texi
;;    (Default configuration is for latex2e.texi)
;;
;;  Perl (Version 4 or 5; they both work)
;;    ftp://ftp.funet.fi/pub/languages/perl/CPAN/doc/manual/texinfo/
;;    ftp://ftp.pasteur.fr/pub/computing/unix/perl/CPAN/doc/manual/texinfo/
;;    ftp://mango.softwords.bc.ca/pub/perl/CPAN/doc/manual/texinfo/
;;    ftp://uiarchive.cso.uiuc.edu/pub/lang/perl/CPAN/doc/manual/texinfo/
;;    
;;    you can get the official mirror list from
;;    ftp://ftp.funet.fi/pub/languages/perl/CPAN/CPAN.html


;; Known problems with Info files
;;  texinfo - In node "Command and Variable Index"
;;    -> The function name are not preceded by the "@" character.
;;    -> Sometimes does not reference a deep enough node
;;       e.g. `iftex' is referenced to node Conditionals intead of deeper
;;       Conditional Commands where iftex is actually mentionned.

;; ----------------------------------------------------------------------------
;;; Change log:
;; V1.09 05jan98 PSG (RCS 1.12) 
;;  - c-mode-map defined in c-mode.el in Emacs-20
;;  - bypass func-doc-complete for c-mode and cc-mode.
;; V1.08 15may97 PSG (RCS 1.11) 
;;  Use -no-properties to extract font-lock'ed keyword.
;; V1.07 09aug96 PSG (RCS 1.10) - finetune prefers previous empty line for perl
;; V1.06 08aug96 PSG (RCS 1.8) - Don't use (when).
;; V1.05 07aug96 PSG (RCS 1.7) - Functionality while in supported Info files. 
;; V1.04 05aug96 PSG (RCS 1.4)
;;  - XEmacs compatible using add-hook.
;;  - Added pattern to func-doc-find-and-finetune. 
;; V1.03 01aug96 PSG - Up to date with latex2e.texi version V1.6 (26 April 96)
;; V1.02 22mar96 PSG - Fixed bug when word under cursor started with \ in latex
;; V1.01 15Feb96 PSG - Fixed bug when word under cursor not a valid command.
;; V1.00 15Jan96 Peter S Galbraith - Created.
;; ----------------------------------------------------------------------------
;;; To do:
;;    (define-key help-map "d" 'func-doc) instead of all hooks to [C-c i] ?
;;    In-text completion (rather than minibuffer).
;;    Should texinfo mode also provide help about TeX commands?
;;    Configure `func-doc-config' with user variables for user-friendliness?
;;    Create pull-down menu entries? Perl-mode has no pull-down menu.
;;; Code:

;; Beginning of user configuration:

(defvar func-doc-split-window nil
  "*When non-nil, `func-doc' will split the window to display the Info buffer
rather than just switch to it.")

;; End of user configuration

(require 'info)
(defvar func-doc-LaTeX-alist nil)
(defvar func-doc-awk-alist nil)
(defvar func-doc-perl-alist nil)
(defvar func-doc-texinfo-alist nil)

(defvar func-doc-awk-extra-alist 
      '(("int" . "Numeric Functions") ("sqrt" . "Numeric Functions") 
        ("exp" . "Numeric Functions") ("log" . "Numeric Functions") 
        ("sin" . "Numeric Functions") ("cos" . "Numeric Functions") 
        ("atan2" . "Numeric Functions") ("rand" . "Numeric Functions") 
        ("srand" . "Numeric Functions"))
      "Extra Info items for awk-mode")

;; Add c++-mode to list (see info-word.el)
(defvar func-doc-config
  '(
   ;;; LaTeX-modes

    ;; With latex.texi V1.1:
    ;;
    ;;  LaTeX commands are described in Info node "List of Commands" 
    ;;  which looks like:
    ;;   * _exp (subscript)::
    ;;                     ^^ Note the two colons.
;;; (latex-mode func-doc-LaTeX-alist "latex" "List of Commands" 
;;; "^* \\([^ :]+\\)" 1 nil t)

    ;; With latex2e.texi V1.2:
    ;;
    ;;  LaTeX commands are described in Info node "List of Commands" 
    ;;  which looks like:
    ;;   * _exp (subscript)::
    ;;                     ^^ Note the two colons.
;;; (latex-mode func-doc-LaTeX-alist "latex2e" "List of Commands" 
;;; "^* \\([^ :]+\\)" 1 nil t)

    ;; With latex2e.texi V1.4
    ;;
    ;;  LaTeX commands are described in Info node "List of Commands" 
    ;;  which looks like:
    ;;   * $:                                           Math Formulae.
    ;;      ^ Note the single colons, and the node name ^^^^^^^^^^^^^
;;; (latex-mode func-doc-LaTeX-alist "latex" "List of commands" 
;;; "^* \\([^ :]+\\): +\\([^.]+\\)" 2 nil t) 

    ;; With latex2e.texi V1.6
    ;;
    ;;  LaTeX commands are described in Info node "Command Index"
    ;;  which looks like:
    ;;   * $:                                           Math Formulae.
    ;;      ^ Note the single colons, and the node name ^^^^^^^^^^^^^
    ;; use this next one if latex "Command Index" Info node looks like:
    ;;* $:                                           Math Formulae.
    ;;   ^ Note the single colons, and the node name ^^^^^^^^^^^^^
    (latex-mode func-doc-LaTeX-alist "latex" "Command Index" 
    "^* \\([^ :]+\\): +\\([^.]+\\)" 2 nil t) 

    ;; Use either of these.  They both work, but they get you to different
    ;; spots in the Info file.
;;; (texinfo-mode func-doc-texinfo-alist "texinfo" "Command and Variable Index"
;;; "^* \\([^ :]+\\).*: +\\([^.]+\\)" 2 nil t) 
    (texinfo-mode func-doc-texinfo-alist "texinfo" "Command List"
    "^`\\(@.[^ '{]*\\)" "Command List" nil t) 

    (awk-mode   func-doc-awk-alist "gawk" "Index" 
     "^* \\([^ ]+\\)\\( special pattern\\| statement.*\\)?: +\\([^.]+\\)" 3 
     func-doc-awk-extra-alist t)

    (perl-mode  func-doc-perl-alist "perl5" "Function Index" 
;; You may want to edit this field   ^^^^^ to "perl" if that's the info file
;; you have.
     "^* \\([a-z]+\\).*: +\\([^.]+\\)" 2 nil t))
  "*func-doc configuration list
In each list element:
 0th element: major-mode.
 1st element: command keywords alist.
 2nd element: info file name.
 3rh element: info node to search for command keywords.
 4th element: regexp to use to search for keywords in said node. 
              1st group of match must hold command keyword.
 5th element: group number of the match to use as info node for keyword
              or a string, representing a constant Info node to look up.
 6th element: nil or alist of extra Info entries for the mode.
 7th element: t or nil to try to fine-tune final location in info node.")

(defvar func-doc-local-alist-name nil
  "Locally holds the command alist name for current buffer")
(make-variable-buffer-local 'func-doc-local-alist-name)

(defvar func-doc-local-topic-name nil
  "Locally holds the topic name for current buffer")
(make-variable-buffer-local 'func-doc-local-topic-name)

(defvar func-doc-local-finetune nil
  "Locally holds the falg to finetune func-doc position in the Info buffer.")
(make-variable-buffer-local 'func-doc-local-finetune)

(defvar func-doc-hist nil "Local history for func-doc help")

;;; Compatibility:   (from custom.el)
(or (fboundp 'buffer-substring-no-properties)
    ;; Introduced in Emacs 19.29.
    (defun buffer-substring-no-properties (beg end)
      "Return the text from BEG to END, without text properties, as a string."
      (let ((string (buffer-substring beg end)))
	(set-text-properties 0 (length string) nil string)
	string)))

(defun func-doc (split)
  "Try to find info about a command keyword relevant to this major mode
The initial guess is taken from the text around point."
  (interactive "P")                     ;Not processing `cmd' here...
  (func-doc-get-cmd-alist-name)
  (let* 
      ((cmd-alist (eval func-doc-local-alist-name))
       (wrd (func-doc-word t))          ;Get word under cursor
       (vwrd (assoc wrd cmd-alist))     ;Verified word as command ?
       (prompt (if vwrd
                   (format "%s command (default %s): " 
                           func-doc-local-topic-name wrd) 
                 (format "%s command: " func-doc-local-topic-name)))
       (crd (if (or (string-match "^18\\." emacs-version)
                    (boundp 'epoch::version))
                (completing-read prompt cmd-alist nil t nil)
              (completing-read prompt cmd-alist nil t nil 'func-doc-hist)))
       (cmd (cond ((and (equal crd "")
                        vwrd)
                   wrd)
                  ((not (equal crd ""))
                   crd)
                  (t 
                   nil)))
       (node (cdr (assoc cmd cmd-alist))))
    (if (not cmd) (message "No command to search for in Info")
      (if (not (or split func-doc-split-window))
          (func-doc-find-and-finetune cmd node func-doc-local-finetune)
          (let ((pop-up-windows t)
                (buffer (current-buffer)))
            (pop-to-buffer nil t) 
            (func-doc-find-and-finetune cmd node func-doc-local-finetune)
            (if (fboundp 'show-temp-buffer) ;From latex-hlp... what is it?
                (show-temp-buffer (current-buffer) t)
              (pop-to-buffer buffer)))))))

(defun func-doc-find-and-finetune (cmd node finetune)
;; Search for CMD in NODE; finetune if configured to do so.
  (Info-find-node func-doc-local-topic-name node)
  ;; Search for specific command?       
  (if (and 
       finetune
       (or 
        (re-search-forward 
         (concat "\n\n`@?\\(" (regexp-quote cmd) "\\)'") nil t)
        (re-search-forward 
         (concat "\n\n`@?\\(" (regexp-quote cmd) "\\)")  nil t)
        (re-search-forward 
         (concat "\n\n@?\\("  (regexp-quote cmd) " \\)") nil t)
        (re-search-forward 
         (concat "\n\n@?\\("  (regexp-quote cmd) "\\)")  nil t)
        (re-search-forward (concat "^`@?"    (regexp-quote cmd) "'") nil t)
        (re-search-forward (concat "^`@?"    (regexp-quote cmd))     nil t)
        (re-search-forward (concat "^@?"     (regexp-quote cmd) " ") nil t)
        (re-search-forward (concat "^@?"     (regexp-quote cmd))     nil t)
        (re-search-forward (concat "`@?"     (regexp-quote cmd) "'") nil t)
        (re-search-forward (concat "`@?"     (regexp-quote cmd))     nil t)
        (re-search-forward                   (regexp-quote cmd)    nil t)))
      (goto-char (or (match-beginning 1)
                     (match-beginning 0)))))

(defun func-doc-complete (arg)
  "Complete command at point"
;; Eventually, do full in-text completion
  (interactive "P")
  (cond
   ((or (string-equal major-mode "c-mode")
        (string-equal major-mode "cc-mode"))
    (call-interactively 'complete-symbol))
   (t
    (func-doc-get-cmd-alist-name)
    (let* ((str-list (func-doc-word nil))
           (start (car str-list))(end (car (cdr str-list)))
           (str (buffer-substring start end))
           (cmd-alist (eval func-doc-local-alist-name))
           (common (try-completion str cmd-alist))
           (common (or (and (equal t common) str)
                       common)))
      (if (not common) (message "Sorry, no completion for %s" str)
        (setq 
         common 
         (if (or (string-match "^18\\." emacs-version)
                 (boundp 'epoch::version))
             (completing-read "Command: " cmd-alist nil t common)
           (completing-read "Command: " cmd-alist nil t common 
                            'func-doc-hist)))
        (if (and common (not (string-equal "" common)))
            (progn (delete-region start end)
                   (insert common))))))))

(defun func-doc-in-info-buffer ()
  "Check if in Info buffer in an infofile relevant to func-doc.
Return appropriate func-doc-config-element of func-doc-config if so."
  (if (string-equal "*info*" (buffer-name))
      (let* ((infofile (file-name-nondirectory Info-current-file))
             ;;-> "gawk" or "gawk.info"
             (infofile-name 
              (substring infofile 0 (string-match "\.info$" infofile)))
             ;;-> "gawk"
             ;;(mapcar '(lambda (val) (nth 2 val)) 
             ;;      func-doc-config)
             ;; ->("latex" "texinfo" "gawk" "perl5")
             (infofile-list (car 
                             (cdr (assq t (mapcar 
                                           '(lambda (val) 
                                              (if (string-equal infofile-name
                                                                (nth 2 val))
                                                  (list t val)))
                                           func-doc-config))))))
        infofile-list)))

(defun func-doc-get-cmd-alist-name ()
;; Sets the value of alist named func-doc-local-alist-name.
;;
;; Get the name of the appropriate major-mode alist.
;; If that alist isn't built yet, do so, scooping up the commands from 
;;  info file for given major-mode.
;; To later extract the alist, use (eval func-doc-local-alist-name)
;; this way we keep a single alist per major-mode, instead of one per buffer,
;; thus saving memory.
  (let ((func-doc-config-element (assoc major-mode func-doc-config)))
    (if (not func-doc-config-element)
        ;; Not in a supported major mode
        (if (not (string-equal "*info*" (buffer-name)))
            ;; Not in info buffer either
            (error "Sorry, %s is not supported (yet) by func-doc." mode-name)
          ;; In Info buffer - check if supported file.
          (setq func-doc-config-element (func-doc-in-info-buffer))
          (if (not func-doc-config-element)
              (error "Sorry, Info file %s is not supported (yet) by func-doc." 
                     (file-name-nondirectory Info-current-file)))))
    (setq func-doc-local-alist-name (nth 1 func-doc-config-element))
    (setq func-doc-local-topic-name (nth 2 func-doc-config-element)) 
    (setq func-doc-local-finetune   (nth 7 func-doc-config-element)) 
    ;; Now see if that named alist is actually defined...
    (if (eval func-doc-local-alist-name)
        nil                           ;Yes, we are done!
      ;; major mode's alist is not built.
      (let* ((info-file (nth 2 func-doc-config-element))
             (info-node (nth 3 func-doc-config-element))
             (the-regexp (nth 4 func-doc-config-element))
             (grp (nth 5 func-doc-config-element))
             (grp-stringp (stringp grp)) ;Is it a string already, or number?
             (the-alist (eval (nth 6 func-doc-config-element)))
             (key)(inf) 
             (case-fold-search nil)) ;case sensitive search
        (save-window-excursion
          (let ((Info-history nil))     ;Don't save this move!
            (Info-find-node info-file info-node))
          (goto-char (point-min))
          (search-forward "* Menu:" nil t)
          (while (re-search-forward the-regexp nil t)
            (setq key (buffer-substring-no-properties 
                       (match-beginning 1) (match-end 1)))
            ;; I'd have to load cl.el to use (when)
            (if (not (assoc key the-alist))
                (progn
                  (setq inf (if grp-stringp 
                                grp 
                              (buffer-substring-no-properties 
                               (match-beginning grp) (match-end grp))))
                  (setq the-alist (cons (cons key inf) the-alist))))))
        (eval `(setq (,@ func-doc-local-alist-name) the-alist))))))

(defun func-doc-word (return-string)
  ;; Return current-word, possibly with a trailing \ or @ character.
  ;; RETURN-STRING: t   -> returns actual string, 
  ;;                nil -> return list of character positions
  (let* ((start (save-excursion 
                  (skip-syntax-backward "w_") 
                  ;; Gotta be a syntax-table way to do this!
                  (and (> (current-column) 0)
                       (progn (forward-char -1) t)
                       (not (looking-at "[@\\]"))
                       (forward-char 1))    
                  (point)))
         (end (save-excursion (skip-syntax-forward "w_") (point))))
    (if return-string
        (if (fboundp 'buffer-substring-no-properties)
            (buffer-substring-no-properties start end)
          (buffer-substring start end))
      (list start end))))

(provide 'func-doc)
;;; func-doc.el ends here
