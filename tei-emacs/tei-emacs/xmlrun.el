;;;; xmlrun.el -- run various programs on an XML file
;; Sebastian Rahtz
;; copied from xslide-process.el by Tony Graham
;; Revised: $Date$
(defcustom xmlrun-offer-save t
  "*If non-nil, ask about saving modified buffers before \\[xmlrun-run] is run."
  :type '(choice (const :tag "Yes" t) (const :tag "No" nil))
  :group 'xmlrun-run)

(defcustom xmlrun-command
  (list
   ;; xsltproc
   "xsltproc -o %o %s %i"
   ;; simplest case of embedded stylesheet PI
   "saxon -a %i"
   ;; Instant Saxon
   "saxon -o %o %i %s"
   ;; Instant Saxon using xml-stylesheet PI and output
   "saxon -o %o -a %i"
   ;; Saxon
   "java com.icl.saxon.StyleSheet -o %o %i %s"
   ;; Saxon using xml-stylesheet PI
   "java com.icl.saxon.StyleSheet -a -o %o %i"
   ;; validate using RXP
   "rxp -V -s %i"
   ;; validate using xmllint
   "xmllint --noout --valid %i"
)
  "*The shell command to process an XSL document.

This is a `format' control string that by default should contain three
`%s' conversion specifications: the first will be replaced by the
value of xmlrun-input-file \(or the empty string, if nil\); the
second will be replaced by xmlrun-stylesheet-file \(or the empty
string, if nil\); the third will be replaced by
xmlrun-output-file \(or the empty string, if nil\).

If `xmlrun-files' is non-nil, the format string should contain
one `%s' conversion specification for each element of its result.

If xmlrun-command is a list, then every element should be a
string.  The strings will be tried in order and %-sequences in the
string will be replaced according to the list below, if the string contains
%-sequences with no replacement value the next string will be tried.

%b means the visited file of the current buffer
%i means the value of xmlrun-input-file
%s means the value of xmlrun-stylesheet-file
%o means the value of xmlrun-output-file
"
  :type '(repeat :tag "Commands" string)
  :group 'xmlrun)

(defvar xmlrun-files nil
  "If non-nil, a function of no arguments that returns a list of file names.
These file names will serve as the arguments to the `xmlrun-command'
format control string instead of the defaults.")

(defcustom xmlrun-error-regexps
  '(("file:\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):" 1 2 3)
    ("file:/\\(\\([A-Za-z]:\\)?[^:]+\\):\\([0-9]+\\):\\(\\([0-9]+\\):\\)?" 1 3 5)
    ("on line \\([0-9]+\\) of file:\\([^:]+\\):\\([0-9]+\\)" 3 1 2)
    ("on line \\([0-9]+\\) of file:\\(\\(/[a-z]:\\)?[^:]+\\):$" 2 1))
  "Alist of regexps to recognize error messages from `xmlrun'.
See `compilation-error-regexp-alist'."
  :type '(repeat :tag "Regexps"
		 (list (regexp :tag "Regexp")
		       (integer :tag "File index")
		       (integer :tag "Line index")
		       (choice :tag "Column index"
			       (integer)
			       (const :tag "None" nil))))
  :group 'xmlrun)

(defvar xmlrun-xml-source nil
  "*If non-nil, this is the name of the XML source file.")
(put 'xmlrun-xml-source 'xmlrun-type 'string)

(defvar xmlrun-result nil
  "*If non-nil, this is the name of the XSL result file.")
(put 'xmlrun-result 'xmlrun-type 'string)

(defvar xmlrun-command-history nil
  "The minibuffer history list for `xmlrun''s COMMAND argument.")
;;(make-variable-buffer-local 'xmlrun-command-history)

(eval-and-compile
  (autoload 'compile-internal "compile" ""))

(defun xmlrun-subst-expand-char (c parts)
  (cdr-safe (assq (downcase c) parts)))

(defun xmlrun-subst-expand (s parts)
  (loop for i from 0 to (1- (length s))
	as c = (aref s i)
	concat (if (eq c ?%)
		   (or (xmlrun-subst-expand-char (aref s (incf i)) parts)
		       (return nil)) 
		 (char-to-string (aref s i)))))

(defun xmlrun-populate-process-command-history ()
  (cond
   ((consp xmlrun-command)
    (let ((process-subst
	   (list
	    (cons ?b (and (buffer-file-name)
			  (file-name-nondirectory (buffer-file-name))))
	    (cons ?i xmlrun-input-file)
	    (cons ?s xmlrun-stylesheet-file)
	    (cons ?o xmlrun-output-file))))
      (setq xmlrun-command-history
	    (append
	     (mapcar (lambda (x)
		       (xmlrun-subst-expand x process-subst))
		     xmlrun-command)
	     xmlrun-command-history))))
   (t
    (apply 'format xmlrun-command
	   (if xmlrun-files
	       (funcall xmlrun-files)
	     (list (or xmlrun-xml-source "")
		   (let ((name (buffer-file-name)))
		     (if name
			 (file-name-nondirectory name)
		       ""))
		   (or xmlrun-result "")))))))

(defvar xmlrun-input-file nil
  "Filename of input file for `xmlrun' command")

(defvar xmlrun-input-history nil
  "The minibuffer history list for `xmlrun-get-process-parameters''s INPUT argument.")

(defvar xmlrun-stylesheet-file nil
  "Filename of stylesheet file for `xmlrun' command")

(defvar xmlrun-stylesheet-history nil
  "The minibuffer history list for `xmlrun-get-process-parameters''s STYLESHEET argument.")

(defvar xmlrun-output-file nil
  "Filename of output file for `xmlrun' command")

(defvar xmlrun-output-history nil
  "The minibuffer history list for `xmlrun-get-process-parameters''s OUTPUT argument.")

(defun xmlrun-get-process-parameters ()
  "Get and set the parameters for the `xmlrun' command"
  (interactive)
  (setq xmlrun-input-file
	(xmlrun-read-from-minibuffer "Input file: "
			      (file-name-nondirectory
			       (buffer-file-name))
			      'xmlrun-input-history))
  (setq xmlrun-stylesheet-file
	(xmlrun-read-from-minibuffer "Stylesheet file: "
			      (concat (file-name-sans-extension
				       (file-name-nondirectory
					(buffer-file-name)))
				      ".xsl")
			      'xmlrun-stylesheet-history))
  (setq xmlrun-output-file
	(xmlrun-read-from-minibuffer "Output file: "
			      (concat (file-name-sans-extension
				       (file-name-nondirectory
					xmlrun-input-file))
				      ".html")
			      'xmlrun-output-history)))

(defun xmlrun (command)
  "Process an XML file 

Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer *XML Process*.  

The first time that the program is run and whenever you provide a
prefix argument, e.g. \\[universal-argument] \\[xmlrun], prompts
for input filename, stylesheet file, and output filename.  Those
values are used with the templates in `xmlrun-command' to
populate this command's command history with the command lines to run
several XSLT processors using those values.  Use M-p and M-n to step
through the predefined commands, edit a command if necessary, or enter
a new command line.  The next time that this command is run, the
previously executed command is used as the default."
  (interactive
   (list (progn
	   (if (or
		current-prefix-arg
		(null xmlrun-command-history))
	       (progn
		 (xmlrun-get-process-parameters)
		 (xmlrun-populate-process-command-history)))
	   (read-from-minibuffer "Process command: "
				     (car xmlrun-command-history)
				     nil nil
				     'xmlrun-command-history))))
  (if xmlrun-offer-save
      (save-some-buffers nil nil))
  (compile-internal command "No more errors" "XML Process"
		    nil
		    xmlrun-error-regexps))

(defun xmlrun-read-from-minibuffer (prompt default history)
  "Read from minibuffer with default and command history."
(let ((value nil))
  (if (string-equal
       ""
       (setq value
	     (read-from-minibuffer (if default
				       (format
					"%s(default `%s') "
					prompt default)
				     (format "%s" prompt))
				   nil nil nil
				   history)))
	     default
	     value)))


(provide 'xmlrun)
