;; XML/SGML loading for tei-emacs project
;; C Wittern/S Rahtz April/May/June 2001
;; Revised September-November 2003 for nxml-mode
;; Revised September 2007 for emacs22
;; drop P4, March 2013

;; set this to some value if you want XML files to
;; start off in anything other than utf-8
(setq default-xml-coding nil)

;; replacement function for PSGML function which inserts
;; doctype. calls  sgml-insert-doctype-extra-commands when it is done
(defun sgml-doctype-insert (doctype vars)
  "Insert string DOCTYPE (ignored if nil) and set variables in &rest VARS.
VARS should be a list of variables and values.
For backward compatibility a singel string instead of a variable is 
assigned to sgml-default-dtd-file.
All variables are made buffer local and are also added to the
buffers local variables list."
  (when doctype
    (unless (bolp)
      (insert "\n"))
    (unless (eolp)
      (insert "\n")
      (forward-char -1))
    (sgml-insert-markup doctype)
    (sgml-insert-doctype-extra-commands)
   )
  (while vars
    (cond ((stringp (car vars))
	   (sgml-set-local-variable 'sgml-default-dtd-file (car vars))
	   (setq vars (cdr vars)))
	  ((car vars)			; Avoid nil
	   (sgml-set-local-variable (car vars) (cadr vars))
	   (setq vars (cddr vars)))
          (t
  	   (setq vars (cddr vars)))))
  (setq sgml-top-tree nil))

;; after inserting a doctype, and if it was XML
;;  - possibly add an encoding attribute
;;  - set input method 
;;  - set coding system
;; regardless, go to the end of what we just inserted
(defun sgml-insert-doctype-extra-commands ()
 "what to do after inserting a DTD"
(when  (search-forward  "<?xml" nil t) 
    (when  default-xml-coding
      (re-search-forward "?>" nil t)
      (backward-char 2)
      (insert (concat " encoding=\"" default-xml-coding "\""))
     )
    (if default-xml-coding
      (set-buffer-file-coding-system (intern default-xml-coding))
      (if (eq system-type 'windows-nt)
          (set-buffer-file-coding-system 'utf-8-dos)
          (set-buffer-file-coding-system 'utf-8)
      ))
)
      (goto-char 1)
      (search-forward "DOCTYPE" nil t)
      (search-forward "]" nil t)
      (search-forward ">" nil t)
      (insert "\n")
)


;;(require 'uni-input)

;;; XML Characters from Norm Walsh

(defvar unicode-charref-format "&#x%x;")

(setq unicode-character-list-file (concat teiemacslispdir "/unichars.el"))

(load-library "xmlunicode")

(defun bind-nxml-mode-keys ()
  (if 
     (> emacs-minor-version 2)
     (set-language-environment "utf-8")
   )
  (define-key nxml-mode-map [menu-bar unichar]
    (cons "UniChar" unicode-character-menu-map))
  (set-input-method 'xml))

;(define-key ctl-t-map "c" 'unicode-character-menu-insert)
;(define-key ctl-t-map "e" 'unicode-character-shortcut-insert)
;(define-key ctl-t-map "u" 'unicode-character-insert)
;(define-key ctl-t-map "i" 'iso8879-character-insert)

;;; End of XML Characters

;; replace standard Mule encoding recognition with one which
;; recognizes xml encoding declaration
(setq set-auto-coding-function 'xml-set-auto-coding)

(defun xml-set-auto-coding (filename size)
  "Return coding system for a file FILENAME of which SIZE bytes follow point.
This is a replacement for standard set-auto-coding; it does the same thing
as the normal one, but also looks for <?xml and its optional 
encoding attribute
"
  (save-match-data (xml-set-auto-coding-1 filename size)))

(defun xml-set-auto-coding-1 (filename size)
  (let ((coding-system (auto-coding-alist-lookup filename)))

    (or coding-system
	(let* ((case-fold-search t)
	       (head-start (point))
	       (head-end (+ head-start (min size 1024)))
	       (tail-start (+ head-start (max (- size 3072) 0)))
	       (tail-end (+ head-start size))
	       coding-system head-found tail-found pos)
	  ;; Try a short cut by searching for the string "coding:"
	  ;; and for "unibyte:" at the head and tail of SIZE bytes.
	  (setq head-found (or (search-forward "coding:" head-end t)
			       (search-forward "unibyte:" head-end t)
			       (search-forward "<?xml " head-end t)
			       ))
	  (if (and head-found (> head-found tail-start))
	      ;; Head and tail are overlapped.
	      (setq tail-found head-found)
	    (goto-char tail-start)
	    (setq tail-found (or (search-forward "coding:" tail-end t)
				 (search-forward "unibyte:" tail-end t))))

	  ;; At first check the head.
	  (when head-found
	    (goto-char head-start)
	    (setq pos (re-search-forward "[\n\r]" head-end t))
	    (if (and pos
		     (= (char-after head-start) ?#)
		     (= (char-after (1+ head-start)) ?!))
		;; If the file begins with "#!" (exec interpreter magic),
		;; look for coding frobs in the first two lines.  You cannot
		;; necessarily put them in the first line of such a file
		;; without screwing up the interpreter invocation.
		(setq pos (search-forward "\n" head-end t)))
	    (if pos (setq head-end pos))
	    (when (< head-found head-end)
	      (goto-char head-start)
	      (when (and set-auto-coding-for-load
			 (re-search-forward
			  "-\\*-\\(.*;\\)?[ \t]*unibyte:[ \t]*\\([^ ;]+\\)"
			  head-end t))
		(setq coding-system 'raw-text))
	      (when (and (not coding-system)
			 (re-search-forward
			  "-\\*-\\(.*;\\)?[ \t]*coding:[ \t]*\\([^ ;]+\\)"
			  head-end t))
		(setq coding-system (intern (match-string 2)))
		(or (coding-system-p coding-system)
		    (setq coding-system nil)))
;; if we meet an xml declaration, its utf-8 unless
;; there is an encoding attribute
	      (when (and (not coding-system)
			 (re-search-forward "<?xml" head-end t))
                (if (re-search-forward ".*encoding=.\\([A-z0-9\\-]*\\)" 
                                                 head-end t)
   		   (setq s-coding-system (downcase (match-string 1)))
                   (if default-xml-coding
     		     (setq s-coding-system default-xml-coding)          
                     (setq s-coding-system "utf-8")))
                 (if (and (string= s-coding-system "utf-8")(eq system-type 'windows-nt))
                      (setq s-coding-system "utf-8-dos")
		    )
		(message (concat "XML declaration in " filename ", using encoding " s-coding-system))
		   (setq coding-system (intern s-coding-system))
		(or (coding-system-p coding-system)
		    (setq coding-system nil)))))

	  ;; If no coding: tag in the head, check the tail.
	  (when (and tail-found (not coding-system))
	    (goto-char tail-start)
	    (search-forward "\n\^L" nil t)
	    (if (re-search-forward
		 "^\\(.*\\)[ \t]*Local Variables:[ \t]*\\(.*\\)$" tail-end t)
		;; The prefix is what comes before "local variables:" in its
		;; line.  The suffix is what comes after "local variables:"
		;; in its line.
		(let* ((prefix (regexp-quote (match-string 1)))
		       (suffix (regexp-quote (match-string 2)))
		       (re-coding
			(concat
			 "^" prefix
			 "[ \t]*coding[ \t]*:[ \t]*\\([^ \t]+\\)[ \t]*"
			 suffix "$"))
		       (re-unibyte
			(concat
			 "^" prefix
			 "[ \t]*unibyte[ \t]*:[ \t]*\\([^ \t]+\\)[ \t]*"
			 suffix "$"))
		       (re-end
			(concat "^" prefix "[ \t]*end *:[ \t]*" suffix "$"))
		       (pos (point)))
		  (re-search-forward re-end tail-end 'move)
		  (setq tail-end (point))
		  (goto-char pos)
		  (when (and set-auto-coding-for-load
			     (re-search-forward re-unibyte tail-end t))
		    (setq coding-system 'raw-text))
		  (when (and (not coding-system)
			     (re-search-forward re-coding tail-end t))
		    (setq coding-system (intern (match-string 1)))
		    (or (coding-system-p coding-system)
			(setq coding-system nil))))))
	  coding-system))))


(setenv "SP_CHARSET_FIXED" "YES")
(setenv "SP_ENCODING" "XML")

;; XSL mode
(autoload 'xsl-mode "xslide" "Major mode for XSL stylesheets." t)

;; Turn on font lock when in XSL mode
(add-hook 'xsl-mode-hook
	  'turn-on-font-lock)

(defface xsl-fo-main-face
  '((((background light))
     (:foreground "SlateBlue3"))
    (((background dark))
     (:foreground "SlateBlue3")))
  "Used for local name portion of formatting object elements and attributes"
  :group 'xsl-faces)


; redefine psgml menu setup
(define-derived-mode xml-mode sgml-mode "XML"

 (easy-menu-add-item nil '("XML/SGML") "--")

 (easy-menu-add-item
  nil '("XML/SGML")
   ["Run XSLT or XML validator on file"	xmlrun t])
 (easy-menu-add-item
  nil '("XML/SGML")
   ["Open this XML file in web browser"	(browse-url (concat "file:///" (buffer-file-name))) t])
 (setq sgml-xml-p t)

;; XML-friendly settings
  (setq sgml-omittag nil)
  (setq sgml-shorttag nil)
  (setq sgml-namecase-general nil)
  (setq sgml-minimize-attributes 'max)
  (setq sgml-always-quote-attributes t)  
  (setq sgml-validate-command sgml-xml-validate-command)
  (make-local-variable 'sgml-declaration)
  (unless sgml-declaration
    (make-local-variable 'sgml-declaration)
    (setq sgml-declaration sgml-xml-declaration)))

;; Name of the file used to translate entities <--> display characters
;;
(setq sgml-display-char-list-filename (concat teiemacsdir "/sgml/ent/iso88591.map"))

;; a function from Tony Graham to make "</" complete the current element,
;; as xslide does
(defun my-xml-mode-slash ()
  "Do-the-right-thing-in-XML-mode slash"
  (interactive)
  (if (= (char-before) 60)
      (progn
	(delete-backward-char 1)
	(sgml-insert-end-tag))
      (insert "/")))

(add-hook 'xml-mode-hook
	  (lambda () (define-key xml-mode-map "/" `my-xml-mode-slash)))

;;autoload xpointer
(autoload 'sgml-xpointer "psgml-xpointer" nil t)

;; DocBook IDE mode
(autoload 'docbook-mode "docbookide" "Major mode for DocBook documents." t)

;; Turn on font lock when in DocBook mode
(add-hook 'docbook-mode-hook
	  'turn-on-font-lock)

;; font lock for psgml-mode 
(add-hook 'sgml-mode-hook 
               (lambda () 
                 (setq sgml-indent-data t) 
                 (setq sgml-markup-faces '((start-tag . font-lock-keyword-face) 
                                           (end-tag . font-lock-keyword-face) 
                                           (comment . font-lock-comment-face) 
                                           (pi . bold) 
                                           (sgml . bold) (doctype . bold) 
                                           (entity . bold-italic) 
                                           (shortref . bold))) 
                 (font-lock-mode))
)
      ;; in sgml documents, parse dtd immediately to allow immediate
      ;; syntax coloring
      (setq sgml-auto-activate-dtd t)

      ;; here we set the syntax color information for psgml
      (setq-default sgml-set-face t)
      ;;
      ;; Faces.
      ;;
      (make-face 'sgml-comment-face)
      (make-face 'sgml-doctype-face)
      (make-face 'sgml-end-tag-face)
      (make-face 'sgml-entity-face)
      (make-face 'sgml-ignored-face)
      (make-face 'sgml-ms-end-face)
      (make-face 'sgml-ms-start-face)
      (make-face 'sgml-pi-face)
      (make-face 'sgml-sgml-face)
      (make-face 'sgml-short-ref-face)
      (make-face 'sgml-start-tag-face)

      (set-face-foreground 'sgml-comment-face "dark turquoise")
      (set-face-foreground 'sgml-doctype-face "red")
      (set-face-foreground 'sgml-end-tag-face "blue")
      (set-face-foreground 'sgml-entity-face "magenta")
      (set-face-foreground 'sgml-ignored-face "gray40")
      (set-face-background 'sgml-ignored-face "gray60")
      (set-face-foreground 'sgml-ms-end-face "green")
      (set-face-foreground 'sgml-ms-start-face "yellow")
      (set-face-foreground 'sgml-pi-face "lime green")
      (set-face-foreground 'sgml-sgml-face "brown")
      (set-face-foreground 'sgml-short-ref-face "deep sky blue")
      (set-face-foreground 'sgml-start-tag-face "dark green")

      (setq-default sgml-markup-faces
      '((comment . sgml-comment-face)
      (doctype . sgml-doctype-face)
      (end-tag . sgml-end-tag-face)
      (entity . sgml-entity-face)
      (ignored . sgml-ignored-face)
      (ms-end . sgml-ms-end-face)
      (ms-start . sgml-ms-start-face)
      (pi . sgml-pi-face)
      (sgml . sgml-sgml-face)
      (short-ref . sgml-short-ref-face)
      (start-tag . sgml-start-tag-face)))


;; XML, SGML, DSSSL, DTD
(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)


;; load xml-mode 
(setq auto-mode-alist (append (list (cons "\\.gml\\'" 'sgml-mode))  auto-mode-alist))
(setq auto-mode-alist (append (list (cons "\\.sgm\\'" 'sgml-mode))  auto-mode-alist))
(setq auto-mode-alist (append (list (cons "\\.sgml\\'" 'sgml-mode))  auto-mode-alist))

;; nxml-mode setup
;; ------------------------------------------------------------------------------
(setq auto-mode-alist (append (list (cons "\\.xsp\\'" 'nxml-mode))  auto-mode-alist))
(setq auto-mode-alist (append (list (cons "\\.rng\\'" 'nxml-mode))  auto-mode-alist))
(setq auto-mode-alist (append (list (cons "\\.xsl\\'" 'nxml-mode))  auto-mode-alist))
(setq auto-mode-alist (append (list (cons "\\.tei\\'" 'nxml-mode))  auto-mode-alist))
(setq auto-mode-alist (append (list (cons "\\.odd\\'" 'nxml-mode))  auto-mode-alist))
(setq auto-mode-alist (append (list (cons "\\.xml\\'" 'nxml-mode))  auto-mode-alist))
(setq auto-mode-alist (append (list (cons "\\.xhtml\\'" 'nxml-mode))  auto-mode-alist))
(setq nxml-slash-auto-complete-flag t)

;; to stop elisp running out of space
(setq max-lisp-eval-depth 500)

(setq rng-schema-locating-files-default
  (list "schemas.xml" 
	"../schemas.xml"
	"../../schemas.xml"
	"../../../schemas.xml"
	"../../../../schemas.xml"
	"../../../../../schemas.xml"
	(abbreviate-file-name
        (expand-file-name "schemas.xml" (concat teiemacslispdir "") ))))

;; overload nxml-mode a little
(add-hook 'nxml-mode-hook (lambda () 
(easy-menu-define 
  tei-menu 
  nxml-mode-map
  "TEI menu"
  tei-menu-definition)
))

(defvar tei-menu-definition
  (list "TEI"
	(list "Insert skeleton TEI files"
["TEI all modules" (tei-file-skeleton "/custom/templates/All.xml" "/custom/schema/relaxng/tei_all.rnc") t ]
["TEI Lite" (tei-file-skeleton "/custom/templates/Lite.xml" "/custom/schema/relaxng/teilite.rnc") t ]
["TEI minimal" (tei-file-skeleton "/custom/templates/Simple.xml" "/custom/schema/relaxng/tei_minimal.rnc") t ]
["TEI manuscript description" (tei-file-skeleton "/custom/templates/Manuscript description.xml" "/custom/schema/relaxng/tei_ms.rnc") t ]
["TEI ODD" (tei-file-skeleton "/custom/templates/ODD customization.xml" "/custom/schema/relaxng/p5odds.rnc") t ]
)
   ["Run XSLT or XML validator on file"	xmlrun t]
   ["Open this XML file in web browser"	(browse-url (concat "file:///" (buffer-file-name))) t]
)
  "NXML menu definition.")

(add-hook 'nxml-mode-hook 'turn-on-auto-fill)
(add-hook 'nxml-mode-hook 'bind-nxml-mode-keys)

(defun tei-file-skeleton (skel schema)
 "insert skeleton file for TEI"
(interactive)
(goto-char (point-max))
(message "Starting a new TEI document")
(insert-file (concat teixmldir skel))
(goto-char (point-max))
(search-backward  "</body>")
(insert "\n")
(backward-char 1)
(rng-set-schema-file-1 (concat teixmldir  schema))
(rng-save-schema-location-1 t)
(rng-validate-mode)
(rng-validate-mode)
)

;; DTD mode

(setq auto-mode-alist
      (append
       (list
	'("\\.dcl$" . dtd-mode)
	'("\\.dec$" . dtd-mode)
	'("\\.dtd$" . dtd-mode)
	'("\\.ele$" . dtd-mode)
	'("\\.ent$" . dtd-mode)
	'("\\.mod$" . dtd-mode))
       auto-mode-alist))

(setq sgml-set-face t)

;; directory and file locations relative to tei-emacs home

(defun sgml-html-mode ()
"This version of html mode is just a wrapper around sgml mode."
(interactive)
(sgml-mode)
(make-local-variable 'sgml-declaration)
(make-local-variable 'sgml-default-doctype-name)
(setq
sgml-default-doctype-name    "html"
sgml-always-quote-attributes t
sgml-indent-step             2
sgml-indent-data             t
sgml-minimize-attributes     'max
sgml-omittag                 t
sgml-shorttag                t
)
(setq sgml-declaration (concat teixmldir "p4/schema/dtd/html4.dcl"))
)

(setq-default sgml-indent-data t)
(setq
sgml-always-quote-attributes   t
sgml-auto-insert-required-elements t
sgml-auto-activate-dtd         t
sgml-indent-data               t
sgml-indent-step               1
sgml-minimize-attributes       'max
sgml-omittag                   nil
sgml-shorttag                  nil
)

;; PSGML menus for creating new documents
(setq sgml-custom-dtd
'(



( "(XML) TEI Lite" 
 (concat 
"<?xml version=\"1.0\"?> 
<?xml-stylesheet type=\"text/css\" href=\"teixlite.css\"?>
<!DOCTYPE TEI.2 PUBLIC \"-//TEI//DTD TEI Lite XML ver. 1//EN\"
\"" teixmldir "p4/schema/dtd/teixlite.dtd\" []>"))

))


;; XSLT process package

(if (boundp 'w32-quote-process-args)
  (setq w32-quote-process-args ?\")) ;; Include only for MS Windows.

(require 'xmlrun)    
;(add-hook 'nxml-mode-hook 'xslt-process-mode)
;(autoload 'xslt-process-mode "xslt-process" "Emacs XSLT processing" t)
;(add-hook 'xml-mode-hook 'xslt-process-mode)
;(add-hook 'xsl-mode-hook 'xslt-process-mode)
 
;; end of xmlsgml-setup
