;;; session.el --- use vars, registers and file/buffer places across sessions

;; Copyright 1996-1999 Free Software Foundation, Inc.
;;
;; Author: Christoph Wedler <wedler@fmi.uni-passau.de>
;; Version: $Id$
;; Keywords: desktop, session, data, frames, tools
;; Requirements: XEmacs-20.2+ (XEmacs-19.13+ & custom cmds), Emacs-20.2+
;; X-URL: http://www.fmi.uni-passau.de/~wedler/session/

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

;;; Commentary:
;;
;; This package saves a file with various variables (defaults to all history
;; variables and rings) and registers at the end of each Emacs session, the
;; file is loaded at the beginning of the next session.  A special variable
;; contains the last changed files with their places (point, mark, ...); each
;; time you visit such a file, its places are restored.  Under XEmacs, you also
;; get two File submenus (for the last recently opened and changed files) and
;; an Edit submenus for the last recently killed strings (also on C-button3).

;; As you have probably already noticed, this package is a kind of extended
;; desktop "on demand".  Thus, you probably want to use just one, either
;; session or desktop (see == Alternatives, Related Packages == below).

;; Bug fixes, bug reports, improvements, and suggestions for the NEWEST VERSION
;; are strongly appreciated.  Please check the newest version first:
;;   http://www.fmi.uni-passau.de/~wedler/session/changes.html

;;; What is saved across sessions? ===========================================

;; This packages induces Emacs to save a session file (`session-save-file' is
;; "~/.session") before exiting, the file is loaded at the beginning of the
;; next Emacs session.  It does not save this file if you exit with C-0 C-x C-c
;; instead of simply C-x C-c.

;; The session file sets some global variables (`session-globals-regexp'
;; matches all history variables and rings).  Typically, these are variables
;; which are changed during some editing operations; you should not use it to
;; customize your Emacs (package custom is the appropriate choice for this).  A
;; special global variable, which is included in `session-globals-include',
;; stores some file/buffer places, see below.

;; Only variables which are non-empty lists are saved.  The list can be
;; truncated (`session-globals-max-size' is 50).  If there are equal elements
;; in the list, only the first is saved, long string elements can be dropped
;; (`session-globals-max-string' is 1024), non-printable/readable elements
;; (e.g. in `command-history') are simply dropped (no error!).

;; All registers in `session-registers' are also saved, dropping registers
;; containing long texts (`session-registers-max-string' is 1024).  To make
;; sense of this feature across sessions, the following feature is provided: if
;; a buffer is killed and some register contains a marker pointing in this
;; buffer, the register contents is changed to a file reference (this feature
;; is part of newer Emacs/XEmacs'en: `register-swap-out').

;;; Which places are stored for which buffers? ===============================

;; If you kill a buffer (this includes exiting Emacs) and the buffer is
;; visiting a file, some places are stored.  Theses places are restored if the
;; corresponding file is visited the next time.

;; Places (see `session-file-alist') are point, mark, the position of the last
;; change, the boundaries of the narrowed part (if it exists), and a permanent
;; flag (see below).  Some local variables (`session-locals-include' contains
;; `overwrite-mode') are also stored.  You should not add variables to this
;; list which are more appropriate for local variables in files, i.e.,
;; variables which are related to the contents of the file, e.g. `major-mode'.

;; By default, the places for a buffer are only stored if the buffer is
;; visiting a readable file and if the buffer is marked as permanent or if it
;; passes some tests: the mode/name test (e.g., for not storing places of gnus
;; buffers, see `session-auto-store') and the undo test (for not storing places
;; of buffers which have not been changed, see `session-undo-check').

;; The default is not used if the buffer is killed with an prefix argument
;; which is not 1 (`session-kill-buffer-commands' contains `kill-this-buffer',
;; but not `kill-buffer').  The prefix argument to this command has a special
;; meaning: ARG=3 or higher marks the buffer as permanent, a negative argument
;; resets the permanent mark.  Additionally, the buffer places are always
;; stored if the prefix argument is 2 or higher, it is never stored if it is 0
;; or lower.  See `session-store-buffer-places'.

;;; Key bindings, Menus (XEmacs only): =======================================

;; With the default installation (see `session-initialize'), this package adds
;; the following menus to the menubar of XEmacs:
;;  * "File >> Open...recently changed" to visit files with stored places,
;;  * "File >> Open...recently opened" to visit files in `file-name-history',
;;  * "Edit >> Yank...recently killed" to insert strings from `kill-ring'.

;; The default installation also adds the following bindings under XEmacs:
;;  * "C-x C-/" or "C-x undo" calls `session-jump-to-last-change' (jumps to
;;    positions of ARG'th last change, including that from the last session),
;;  * "C-button3" pops up the menu "Edit >> Yank...recently killed".

;;; Alternatives, Related Packages: ==========================================

;; Packages which maintain an alist (FILENAME . PLACES), set by kill-buffer,
;; used by find-file [method which is used by this package]:
;;  * XEMACS/packages/recent-files: maintain menu of recently opened files. [No
;;    places, menu has permanent/non-permanent entries.]
;;  * EMACS+XEMACS/packages/saveplace: automatically save place in
;;    files. [Just saves point.]
;;  * LCD/misc/context: Save some context from previous editing sessions.
;;    [Little extension to saveplace: function to load the last N visited
;;    files.]

;; Packages which load all files on startup which have been visited when ending
;; the last emacs session [too slow for me]:
;;  * EMACS/desktop: save partial status of Emacs when killed. [Also saves some
;;    global and local variables, point, mark...probably the best package in
;;    this category.]  Extension "aging" by desktop-phase.
;;  * revive: similar to desktop.  [Is included in windows, see below.]
;;  * XEMACS/packages/saveconf: Save Emacs buffer and window configuration
;;    between editing sessions. [By default, just load the packages which were
;;    visible before ending the last session.]
;;  * LCD/as-is/misc/new-saveconf: Enhanced saveconf. [Add-on & Patch]
;;  * LCD/packages/em-config: Save and restore Emacs configurations between
;;    sessions. [Additional treatment of window configurations] see below
;;  * LCD/misc/tinydesk: Saves/restores files of emacs session [``only
;;    filenames are read/saved'', fancy messages]

;; Packages which are quite restricted in its usage:
;;  * XEMACS/utils/savehist: Saves/restores just some history variables.
;;  * LCD/misc/tinyhist: Saves/restores just the command history.

;; Packages which deal with window configuration [not included--TODO]:
;;  * XEMACS/hyperbole/wconfig: Saves and yanks from save ring of window
;;    configurations.
;;  * windows: Save/restore frame & window configurations.  [Comes with revive,
;;    see above.]
;;  * LCD/as-is/win-config: Save/restore window configurations.
;;  * LCD/epoch/epoch-config: Save and restore Epoch screen/window
;;    configuration.
;;  * LCD/misc/wicos: Save and restore multiple window configurations (wicos)
;;    within emacs.
;;  * LCD/misc/escreen: emacs window session manager
;;  * LCD/packages/em-config: Save and restore Emacs configurations between
;;    sessions.

;;; Todo: ====================================================================

;; The features of this package and package desktop are probably not different
;; enough to justify two different packages.  Thus, a merge would be useful.
;; Since package desktop was created earlier, it should be done by integrating
;; the features of this package into desktop (idea from Richard Stallman) and
;; have an option whether old files get visited when Emacs starts.  I don't
;; have time for the integration anymore, but if you are interested, feel free
;; do so.

;; Todo/Ideas for the "new" desktop:
;;  * Using contexts for buffer positions (idea from bookmark and vc).
;;  * Define common code with bookmark to restore buffers from a
;;    file-representation (for files, dired, info buffers).
;;  * Saving window-configurations?

;;; Installation: ============================================================

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'session)
;;   (add-hook 'after-init-hook 'session-initialize)

;; To customize, use `M-x customize-group RET session RET' or the custom menus
;; (Emacs->Data->Session).

;; If you want to use both desktop and session, use:
;;   (setq desktop-globals-to-save '(desktop-missing-file-warning))

;;; Code:

(provide 'session)
(require 'cl)
(require 'custom)

(eval-and-compile
  (if (string-match "XEmacs\\|Lucid" emacs-version)
      ;; the XEmacs version should have an optional NOT-HACK-HOMEDIR
      (defun session-abbreviate-file-name (filename)
	(abbreviate-file-name filename t))
    (defalias 'session-abbreviate-file-name 'abbreviate-file-name))
  (defalias 'session-subst-char-in-string
    (if (fboundp 'subst-char-in-string) 'subst-char-in-string
      'session-subst-char-in-string-0)))



;;;;##########################################################################
;;;;  User options, configuration variables
;;;;##########################################################################


(defconst session-version "1.5"
  "Current version of package session.
Check <http://www.fmi.uni-passau.de/~wedler/session/changes.html> for
the newest.")


;;;===========================================================================
;;;  Customization and initialization
;;;===========================================================================

(defgroup session nil
  "Use vars, registers and file/buffer places across sessions."
  :group 'data
  :link '(emacs-commentary-link "session.el")
  :link '(url-link "http://www.fmi.uni-passau.de/~wedler/session/")
  :prefix "session-")

(defgroup session-globals nil
  "What to save across sessions."
  :group 'session
  :prefix "session-")

(defgroup session-places nil
  "Which places are stored for which buffers."
  :group 'session
  :prefix "session-")

(defgroup session-miscellaneous nil
  "Miscellaneous configurations of package session."
  :group 'session
  :prefix "session-")

;; I could imagine that a future version of package custom could make this
;; `PACKAGE-initialize' stuff easier
(defcustom session-use-package nil
  "Pseudo variable.  Used to initialize session in custom buffer.
Put `(session-initialize)' into your ~/.emacs to initialize package
session in future sessions.  See variable `session-initialize'."
  :group 'session
  :type '(boolean :format "%{%t%}: %[(session-initialize)%], %v\n"
		  :on "in use" :off "not yet initialized"
		  :help-echo "Initialize package Session."
		  :action session-initialize))

(defcustom session-initialize t
  "Whether/what to initialize with `session-initialize'.
If t, do full initialization.  Otherwise, the value should be a list
with element.  To enable, include

 * `de-saveplace' to de-install package saveplace (is redundant),
 * `session' to load and save the session file,
 * `places' to store and use places for files/buffers,
 * `keys' to setup the default key and mouse bindings (XEmacs only),
 * `menus' to setup the menus (XEmacs only, no effect with Emacs)."
  :group 'session-miscellaneous
  :type '(choice (const :tag "All" t)
		 (set :value (de-saveplace session places keys menus)
		      (const :tag "De-install saveplace" de-saveplace)
		      (const :tag "Load/Save Session" session)
		      (const :tag "Store/Use Places" places)
		      (const :tag "Setup Key/Mouse Bindings" keys)
		      (const :tag "Setup Menus (XEmacs)" menus))))


;;;===========================================================================
;;;  User Options and Configuration: Menu (XEmacs)
;;;===========================================================================

(defcustom session-menu-max-size 30
  "*Max number of entries which may appear in the session menus."
  :group 'session-miscellaneous
  :type 'integer)

(defcustom session-file-menu-max-string -80
  "*Max length of strings in submenus of the File menu.
If MAX is negative, max length is (- 0 MAX (length (buffer-name))).  See
`put-buffer-names-in-file-menu'."
  :group 'session-miscellaneous
  :type 'integer)

(defcustom session-edit-menu-max-string 50
  "*Max length of strings in submenus of the Edit menu."
  :group 'session-miscellaneous
  :type 'integer)

(defcustom session-menu-permanent-string " *"
  "*Marker for permanent files in menu \"File >> Open...recently changed\".
A file can set as permanent with prefix argument 3 for a command in
`session-kill-buffer-commands'.  It can be set as non-permanent with
prefix argument -1."
  :group 'session-miscellaneous
  :type 'string)

(defcustom session-set-file-name-exclude-regexp
  "/\\.overview\\|.session\\|News/"
  "*Regexp matching file names not to be stored in `file-name-history'.
This is used by `session-set-file-name-history'.  Value nil means, do
not exclude any file."
  :group 'session-miscellaneous
  :type '(choice (const nil) regexp))

;; calling `abbrev-file-name' on remote files opens the connection!
(defvar session-abbrev-inhibit-function
  (cond ((fboundp 'efs-ftp-path) 'efs-ftp-path)
	((fboundp 'ange-ftp-ftp-path) 'ange-ftp-ftp-path))
  "Function used to determine whether to abbreviate file name.
A file name is not abbreviated if this function returns non-nil when
called with the file name.")

(defvar session-directory-sep-char    ; directory-sep-char is not set
  (if (memq system-type '(ms-dos windows-nt)) ?\\ ?\/)
  "Directory separator character for session menus.")


;;;===========================================================================
;;;  User Options and Configuration: save global variables between sessions
;;;===========================================================================

(defcustom session-globals-max-size 50
  "*Maximal number of elements in the global variables.
Global variables are only saved if they are non-empty lists.  This value
can be shadowed by some element in `session-globals-include'.  If an
element appears more than once in the list, only the first appearance
will be stored."
  :group 'session-globals
  :type 'integer)

(defcustom session-globals-max-string 1024
  "*Maximal length of string elements in global variables."
  :group 'session-globals
  :type 'integer)

(defcustom session-registers-max-string 1024
  "*Maximal length of string elements in registers."
  :group 'session-globals
  :type 'integer)

(defcustom session-save-file (expand-file-name "~/.session")
  "File to save global variables and registers into.
It is saved at the end of an Emacs session and loaded at the beginning.
Used for variables which are typically changed by editing operations,
e.g., history and ring variables."
  :group 'session-globals
  :type 'file)

(defvar session-before-save-hook nil	; only customize for predefined fns
  "Hook to be run before `session-save-file' is saved.
The functions are called after the global variables are written,
directly before the file is actually saved.")

(defcustom session-globals-regexp "-\\(ring\\|history\\)\\'"
  "Regexp matching global variables to be saved between sessions.
Variables in `session-globals-exclude' are not saved, but variables in
`session-globals-include' are always saved."
  :group 'session-globals
  :type 'regexp)

(defcustom session-globals-exclude
  '(load-history register-alist vc-comment-ring flyspell-auto-correct-ring)
  "Global variables not to be saved between sessions.
It affects `session-globals-regexp' but not `session-globals-include'."
  :group 'session-globals
  :type '(repeat variable))

(defcustom session-globals-include '((kill-ring 10)
				     (session-file-alist 100 t)
				     (file-name-history 100))
  "Global variables to be saved between sessions.
Each element has one of the following forms:
  NAME,
  (NAME MAX-SIZE), or
  (NAME MAX-SIZE ASSOC-P).
where NAME is the symbol name of the variable, whose value must be a
non-empty list and string elements in this list must be smaller than
`session-globals-max-string'.  MAX-SIZE (default is
`session-globals-max-size') is the maximal number of elements to be
saved for this symbol where only the first of equal elements are saved,
and ASSOC-P (default is nil) non-nil means that the variable is an alist
where the equality of elements is checked on the `car'.

If MAX-SIZE or ASSOC-P is non-nil, it can be useful to include a
variable in this list even if it matches `session-globals-regexp'.
`session-globals-exclude' has no effect on these variables."
  :group 'session-globals
  :type '(repeat (group :value '(nil 50)
			variable
			(integer :tag "Max size")
			(option :value t (boolean :tag "Alist")))))


;;;===========================================================================
;;;  Configuration: registers and local variables
;;;===========================================================================

(defcustom session-registers '((?0 . ?9) ?- ?= ?\\ ?` region (?a . ?z))
  "*Registers to be saved in `session-save-file'.
Valid elements in this list are:
  CHAR or (FROM . TO) or `file' or `region' or t.
CHAR is a register to save, (FROM . TO) represents a list of registers
from FROM to TO.  `file' means, only save the following registers in
this list if they contain file or file-query references.  `region'
means, only save registers if they contain a region which has less then
`session-registers-max-string' characters.  t means, allow both content
types.  Processing of this list starts with type `file'."
  :group 'session-globals
  :type '(repeat (choice (const :tag "File registers:" file)
			 (const :tag "String registers:" region)
			 (const :tag "Any register type:" t)
			 (character :tag "Register")
			 (cons :tag "Registers"
			       (character :tag "From")
			       (character :tag "To")))))

(defcustom session-locals-include '(overwrite-mode)
  "Local variables to be stored for specific buffers.
See also `session-locals-predicate'."
  :group 'session-places
  :type '(repeat variable))

(defcustom session-locals-predicate 'local-variable-p
  "Function which must return non-nil for a local variable to be stored.
This function is called on all variables in `session-locals-include'
with the variable as the first and the current buffer as the second
argument.  Good values are nil (do not store any variable),
`local-variable-p' for local variables, `local-variable-if-set-p' for
variables which become local when set, and t (store all variables in
`session-locals-include')."
  :group 'session-places
  :type '(choice (const :tag "none" nil)
		 (const :tag "All" t)
		 (function-item local-variable-p)
		 (function-item local-variable-if-set-p)
		 (function :tag "Other function")))

(defvar session-register-swap-out (if (fboundp 'register-swap-out)
				      'register-swap-out
				    'session-register-swap-out)
  "Function processing markers in registers when a buffer is killed.
If non-nil, this function is added to `kill-buffer-hook'.")


;;;===========================================================================
;;;  User Options and Configuration: buffer check--undo, mode+name
;;;===========================================================================

(defcustom session-jump-undo-threshold 240
  "*Number of character positions the undo position must be different.
Used by `session-jump-to-last-change' with positive prefix argument."
  :group 'session-places
  :type 'integer)

;; Problem if homedir is a symlink (/bar/home -> /net/bar.home) & tmp-mounted
;;   (file-truename "~/foo") => "/tmp_mnt/net/bar.home/foo"
;;   (abbreviate-file-name "/tmp_mnt/net/bar.home/foo") => "/net/bar.home/foo"
;; I.e., there is a bug in `abbreviate-file-name' on both Emacs and XEmacs
;; (with 2nd arg t).  Workaround: use the following in your ~/.emacs:

;;(unless (string= (abbreviate-file-name (file-truename "~") t) "~") ; XEmacs
;;  (setq abbreviated-home-dir
;;	(let ((abbreviated-home-dir "$foo"))
;;	  (concat "\\`\\(?:"
;;		  (regexp-quote (abbreviate-file-name (expand-file-name "~")))
;;		  "\\|"
;;		  (regexp-quote (abbreviate-file-name (file-truename "~")))
;;		  "\\)\\(/\\|\\'\\)"))))

(defcustom session-use-truenames
  (and (string= (session-abbreviate-file-name (file-truename "~")) "~")
       (if (and (string-match "XEmacs\\|Lucid" emacs-version)
		(fboundp 'console-type)
		(eq (console-type) 'mswindows))
	   'session-xemacs-buffer-local-mswindows-file-p
	 t))
  "*Whether to use the canonical file names when saving/restoring places.
If a function, it is called with no argument and returns whether to use
the canonical names of files.  If non-nil, store and check file names
returned by `file-truename'."
  :group 'session-places
  :type '(choice (const :tag "No" nil)
		 (const :tag "Yes" t)
		 (function-item :tag "If not starting with \\\\"
				session-xemacs-buffer-local-mswindows-file-p)
		 (function :tag "Other function")))

(defcustom session-auto-store t
  "*Determines whether a buffer to be killed passes the mode/name check.
This boolean is used by `session-default-buffer-check-p', see
`session-buffer-check-function'.

A buffer passes the mode/name check, if it passes the mode check, see
below, and its file name is not matched by
`session-name-disable-regexp', or if fails the mode check and its file
name is matched by `session-name-enable-regexp'.

A buffer passes the mode check, if this variable is non-nil and its
major mode is not a member of `session-mode-disable-list', or if this
variable is nil and its major mode is a member of
`session-mode-enable-list'."
  :group 'session-places
  :type 'boolean)

(defcustom session-undo-check 1
  "*Determines how a buffer to be killed passes the undo check.
Its value is MIN or (MIN . LAST) where MIN is a number.  Used by
`session-default-buffer-check-p', see `session-buffer-check-function'.

To pass the undo check
 * the length of `buffer-undo-list', assumed to be -1 if no undo
   information is recorded, must be higher or equal to MIN,
 * the first form is used or LAST is nil: no further requirement
 * LAST is `and': additionally, `session-last-change' must be non-nil,
   i.e., the buffer has been changed previously,
 * LAST is `or': alternatively, `session-last-change' is non-nil."
  :group 'session-places
  :type '(choice (integer :tag "Min no of Changes")
		 (cons (integer :tag "Min no of Changes")
		       (choice :tag "Previous and New Changes"
			       (const :tag "Only consider New Changes" nil)
			       (const :tag "AND previously changed" and)
			       (const :tag "OR previously changed" or)))))

(defcustom session-kill-buffer-commands '(kill-this-buffer)
  "*Commands which kill a buffer.
If a prefix argument was provided to any of these commands, it will
influence the decision whether to store places for the buffer, see
`session-store-buffer-places'.  Using commands which use the minibuffer
for input, is useless."
  :group 'session-places
  :type '(repeat (function :tag "Command")))

(defcustom session-buffer-check-function 'session-default-buffer-check-p
  "Function which return non-nil if buffer places should be stored.
Used by `session-store-buffer-places'.  This function is called with the
buffer to check as argument.  You can also assume that the current
buffer is the buffer to check.

The default value `session-default-buffer-check-p' returns non-nil, if
the buffer
 * visits an existing readable file,
 * passes the mode/name check, see `session-auto-store', and
 * passes the undo check, see `session-undo-check', its default value 1
   means: the buffer must have been changed during the session."
  :group 'session-globals
  :type '(choice (function-item :tag "Default check"
				session-default-buffer-check-p)
		 (function :tag "Other function")))

(defcustom session-mode-disable-list
  '(vm-mode gnus-score-mode message-mode tar-mode)
  "*Major modes of buffers for which no places are stored.
See `session-buffer-check-function'."
  :group 'session-globals
  :type '(repeat (function :tag "Major mode")))

(defcustom session-mode-enable-list nil
  "*Major modes of buffers for which places are stored.
See `session-buffer-check-function'."
  :group 'session-globals
  :type '(repeat (function :tag "Major mode")))

(defcustom session-name-disable-regexp
  (concat "\\`" (regexp-quote
		 (if (fboundp 'temp-directory) (temp-directory) "/tmp")))
  "*File names of buffers for which no places are stored.
See `session-buffer-check-function'."
  :group 'session-places
  :type '(choice (const nil) regexp))

(defcustom session-name-enable-regexp nil
  "*File names of buffers for which places are stored.
See `session-buffer-check-function'."
  :group 'session-places
  :type '(choice (const nil) regexp))




;;;;##########################################################################
;;;;  Store buffer places and local variables, change register contents
;;;;##########################################################################


(defvar session-last-change nil
  "Position of last change in current buffer.
This variable is set by `session-find-file-hook' if the buffer was
changed in a previous session.  It can also be set by providing an
prefix argument to `session-jump-to-last-change'.")
(make-variable-buffer-local 'session-last-change)

(defvar session-file-alist nil
  "Alist for places and local variables for some files.
It has the form
  (NAME POINT MARK POINT-MIN POINT-MAX PERMANENT LAST-CHANGE
   (SYMBOL . VAR) ...)

NAME is the file name, POINT is the point position, MARK is the mark
position, POINT-MIN and POINT-MAX determine the narrow part if non-nil,
PERMANENT is the permanent marker (see `session-buffer-check-function'),
LAST-CHANGE is the position of the last change in the previous session
or was explicitly set with prefix argument 0 for command
\\[session-jump-to-last-change].  Optional pairs (SYMBOL . VAR) are
local variables with their values.")


;;;===========================================================================
;;;  Position of last change
;;;===========================================================================

(defun session-undo-position (pos undo-list)
  "Return position of first real element in UNDO-LIST.
POS should be nil."
  (while (and (null pos) undo-list)
    (setq pos (pop undo-list))
    (if (consp pos)
	(setq pos (if (stringp (car pos)) (cdr pos) (car pos))))
    (setq pos (and (integerp pos) (abs pos))))
  (cons pos undo-list))

(defun session-jump-to-last-change (&optional arg)
  "Jump to position of abs(ARG)'th last change.
With positive argument, two changes are considered different if their
positions differ by at least `session-jump-undo-threshold' character
positions.  With negative argument, two changes are considered different
if there is an undo boundary in the `buffer-undo-list' between them.

In both cases, use position in `session-last-change' as oldest position.
With prefix argument ARG=0, set `point' as oldest position."
  (interactive "p")
  (if (zerop arg)
      (progn
	(setq session-last-change (point))
	(message "Store %d as special last-change position (%s %d %s)"
		 session-last-change
		 (substitute-command-keys "\\[universal-argument]")
		 (let ((list (and (consp buffer-undo-list) buffer-undo-list)))
		   (while list
		     (or (car list) (setq arg (1- arg)))
		     (setq list (cdr list)))
		   (1- arg))
		 (substitute-command-keys "\\[session-jump-to-last-change]")))
    (let* ((undo-list (session-undo-position nil (and (consp buffer-undo-list)
						      buffer-undo-list)))
	   (pos (car undo-list)))
      (if (< arg 0)
	  (unless (zerop (setq arg (1+ arg)))
	    (setq undo-list (cdr undo-list))
	    (while (and (< arg 0) undo-list)
	      (or (pop undo-list)
		  (setq arg (1+ arg))))
	    (setq pos (car (session-undo-position nil undo-list))))
	(setq arg (1- arg))
	(while (and (> arg 0)
		    (car (setq undo-list
			       (session-undo-position nil (cdr undo-list)))))
	  (if (>= (abs (- (car undo-list) pos)) session-jump-undo-threshold)
	      (setq pos (car undo-list)
		    arg (1- arg))))
	(or (zerop arg) (setq pos nil)))
      (or pos (setq pos session-last-change))
      (if pos
	  (goto-char pos)
	(message "Do not know position of last change")))))


;;;===========================================================================
;;;  Yank menu (XEmacs only)
;;;===========================================================================

(defun session-no-selection-hook ()
  "XEmacs menubar bug workaround in `menu-no-selection-hook'."
  (if (eq this-command 'run-hooks)	; without breaks menubar
      (setq this-command last-command)))

(defun session-popup-yank-menu (event)
  ;; checkdoc-params: (event)
  "Pop up a menu for inserting items in `kill-ring'."
  (interactive "e")
  (when kill-ring
    (setq this-command last-command)
    (popup-menu '("Yank...recently killed"
		  :filter session-yank-menu-filter))))

(defun session-yank-menu-filter (menu-items)
  ;; checkdoc-params: (menu-items)
  "Return a menu for inserting items in `kill-ring'."
  (let ((menu nil)
	(ring nil)
	(max session-menu-max-size)
	  (len (length kill-ring))
	  (half-str-len (/ (- session-edit-menu-max-string 4) 2))
	  (i 1)				; first element already with `yank'
	  (active (not buffer-read-only))
	  elem)
    ;; Traversing (append kill-ring-yank-pointer kill-ring) instead indexing
    ;; (current-kill INDEX) would be probably more efficient, but would be a
    ;; very low-level hack
    (while (and (< i len) (> max 0))
      (setq elem (current-kill i t)
	    i (1+ i))
      (unless (assoc elem ring)
	(push (cons elem i) ring)
	(setq max (1- max))))
    (while ring
      (setq elem (car ring)
	    ring (cdr ring))
      (push (session-yank-string (car elem) half-str-len
				 (list 'yank (cdr elem))
				 active)
	    menu))
    (append menu-items
	    (list (session-yank-string (current-kill 0 t) half-str-len
				       'yank active)
		  (session-yank-string (current-kill 1 t) half-str-len
				       'yank-pop
				       (and active (eq this-command 'yank)))
		  ;; menubar requires `this-command' instead of `last-command'
		  "---")
	    menu)))

(defun session-yank-string (string half-len-str callback active)
  "Return menu item STRING with callback CALLBACK.
If ACTIVE is non-nil, the item is active.  HALF-LEN-STR is the length of
the two parts of a abbreviated menu item name."
  (vector (if (> (length string) session-edit-menu-max-string)
	      (concat (session-subst-char-in-string
		       ?\t ?\ (substring string 0 half-len-str) t)
		      " ... "
		      (session-subst-char-in-string
		       ?\t ?\ (substring string (- half-len-str)) t))
	    (session-subst-char-in-string ?\t ?\ string))
	  callback
	  active))

;; from EMACS-20.4/lisp/subr.el:
(defun session-subst-char-in-string-0 (fromchar tochar string
						&optional inplace)
  "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
  (let ((i (length string))
	(newstr (if inplace string (copy-sequence string))))
    (while (> i 0)
      (setq i (1- i))
      (if (eq (aref newstr i) fromchar)
	  (aset newstr i tochar)))
    newstr))


;;;===========================================================================
;;;  Menu filters (XEmacs only)
;;;===========================================================================

(defun session-file-opened-menu-filter (menu-items)
  ;; checkdoc-params: (menu-items)
  "This is the menu filter for \"File >> Open...recently opened\".
See `session-file-changed-menu-filter'."
  (session-file-changed-menu-filter menu-items file-name-history))

(defun session-file-changed-menu-filter (menu-items &optional files find-fn)
  ;; checkdoc-params: (menu-items)
  "This is the menu filter for \"File >> Open...recently changed\".
It dynamically creates a list of files to use as the contents of the
menu.  The files are taken from FILES or `session-file-alist'.  It
doesn't show the same name twice and shows `session-menu-max-size' names
at most.  FIND-FN or \\[find-file] is the function to use when selecting
a file in the menu."
  (let ((excl nil)
	(menu nil)
	(i session-menu-max-size)
	(max-string (if (natnump session-file-menu-max-string)
			session-file-menu-max-string
		      (- 0 session-file-menu-max-string
			 (length (buffer-name)))))
	elem desc name)
    (or files (setq files session-file-alist))
    (or find-fn (setq find-fn 'find-file))
    (while (and files (> i 0))
      (setq elem (car files)
	    desc (and (consp elem) elem)
	    files (cdr files))
      (if (consp elem) (setq elem (car elem)))
      (setq elem (session-abbrev-file-name (directory-file-name elem)))
      (or (member elem excl)
	  (progn
	    (setq i (1- i))
	    (push elem excl)
	    (setq name elem)
	    (and (> (length elem) max-string)
		 (fboundp 'split-path)
		 (let* ((path-separator (char-to-string
					 session-directory-sep-char))
			(components (split-path elem)))
		   (or (cdr components)
		       (eq session-directory-sep-char ?\/) ; the right one
		       (setq path-separator "/"
			     components (split-path elem)))
		   (let* ((prefix (if (< (length (car components)) 2)
				      (concat (pop components) path-separator
					      (pop components))
				    (pop components)))
			  (len (+ (length prefix) 5))
			  postfix)
		     (setq components (nreverse components))
		     (while (and (cdr components)
				 (< (incf len (length (car components)))
				    max-string))
		       (push (pop components) postfix))
		     (if postfix
			 (setq name (concat prefix path-separator
					    " ... " path-separator
					    (mapconcat 'identity postfix
						       path-separator)))))))
	    (push (vector name (list find-fn elem)
			  :keys (and (sixth desc)
				     session-menu-permanent-string))
		  ;;(if (get-file-buffer elem) " (buffer)" " (file)") is bad (efs)
		  menu))))
    (append menu-items (nreverse menu))))

(defun session-abbrev-file-name (name)
  "Return a version of NAME shortened using `directory-abbrev-alist'.
This function does not consider remote file names (see
`session-abbrev-inhibit-function') and substitutes \"~\" for the user's
home directory."
  (if (and session-abbrev-inhibit-function
	   (or (not (fboundp session-abbrev-inhibit-function))
	       (funcall session-abbrev-inhibit-function name)))
      name
    (session-abbreviate-file-name name)))


;;;===========================================================================
;;;  Functions in hooks
;;;===========================================================================

(defun session-set-file-name-history ()
  "Add file-name of current buffer to `file-name-history'.
Don't add the file name if it does not visit an existing readable file,
if it matches `session-set-file-name-exclude-regexp', or if it is
already at the front of `file-name-history'.  This function is useful in
`find-file-hooks'."
  (and buffer-file-name
       (file-exists-p buffer-file-name) (file-readable-p buffer-file-name)
       (let ((name (session-abbrev-file-name buffer-file-name)))
	 (or (string= (car file-name-history) name)
	     (string= (car file-name-history) buffer-file-name)
	     (and session-set-file-name-exclude-regexp
		  (string-match session-set-file-name-exclude-regexp name))
	     (push name file-name-history)))))

(defun session-find-file-hook ()
  "Function in `find-file-hooks'.  See `session-file-alist'."
  (let* ((ass (assoc (session-buffer-file-name) session-file-alist))
	 (point (second ass))
	 (mark (third ass))
	 (min (fourth ass))
	 (max (fifth ass))
	 (alist (nthcdr 7 ass)))
    (condition-case nil
	(while alist
	  (if (local-variable-if-set-p (caar alist) (current-buffer))
	      (set (caar alist) (cdar alist)))
	  (setq alist (cdr alist)))
      (error nil))
    (setq session-last-change (seventh ass))
    (and mark
	 (<= (point-min) mark) (<= mark (point-max))
	 (set-mark mark))
    (and min max
	 (<= (point-min) min) (<= max (point-max))
	 (narrow-to-region min max))
    (and point
	 (<= (point-min) point) (< point (point-max))
	 (goto-char point))))

(defun session-kill-buffer-hook ()
  "Function in `kill-buffer-hook'.
See `session-file-alist' and `session-registers'."
  (if buffer-file-name
      (condition-case nil
	  (session-store-buffer-places
	   (if (memq this-command session-kill-buffer-commands)
	       (prefix-numeric-value current-prefix-arg)
	     1))
	(error nil))))


;;;===========================================================================
;;;  Change register contents from marker to file
;;;===========================================================================

(defun session-register-swap-out ()
  "Turn markers in registers into file references when a buffer is killed."
  (and buffer-file-name
       (let ((tail register-alist))
	 (while tail
	   (and (markerp (cdr (car tail)))
		(eq (marker-buffer (cdr (car tail))) (current-buffer))
		(setcdr (car tail)
			(cons 'file buffer-file-name)))
	   (setq tail (cdr tail))))))

(if session-register-swap-out
    (add-hook 'kill-buffer-hook session-register-swap-out))



;;;;##########################################################################
;;;;  Save global variables, add functions to hooks
;;;;##########################################################################


(defvar session-successful-p nil
  "Whether the file `session-save-file' has been loaded successfully.")


;;;===========================================================================
;;;  The buffer file name
;;;===========================================================================

(defun session-xemacs-buffer-local-mswindows-file-p ()
  "Return t if the current buffer visits a local file on MS-Windows.
Also returns t if the current buffer does not visit a file.  Return nil
of the current buffer visits a file starting with \"\\\\\".  Workaround
for XEmacs bug in `file-truename' for file names starting with
\"\\\\\"."
  (or (< (length buffer-file-name) 2)
      (not (string= (substring buffer-file-name 0 2) "\\\\"))))

(defun session-buffer-file-name ()
  "Return the buffer file name according to `session-use-truenames'."
  (if (if (functionp session-use-truenames)
	  (funcall session-use-truenames)
	session-use-truenames)
      buffer-file-truename
    buffer-file-name))


;;;===========================================================================
;;;  Store places and local variables for buffer to be killed
;;;===========================================================================

(defun session-store-buffer-places (arg)
  "Store places and local variables in current buffer.
An entry for the current buffer and its places is added to the front of
`session-file-alist' if the buffer is visiting a file and if it is
mentioned in the list below.  ARG is the prefix argument to a command in
`session-kill-buffer-commands' or 1 for any other command.

ARG=-1: delete PERMANENT flag for buffer,
ARG=0: do nothing,
ARG=1: store buffer places, if the PERMANENT flag is set or the buffer
  passes the function in `session-buffer-check-function',
ARG=2: always store buffer places,
ARG=3: set PERMANENT flag and store buffer places.

See also `session-last-change' and `session-locals-includes'.

Note that not storing buffer places does not mean deleting an old entry
for the same file.  It means that there is the danger of the entry
becoming too old to be saved across session.  By default, only the first
100 entries of `session-file-alist' are saved, see
`session-globals-include'."
  (let ((file-name (session-buffer-file-name)))
    (when file-name
      (let ((option (nthcdr 5 (assoc file-name session-file-alist))))
	(and (< arg 0) (car option) (setcar option nil))
	(setq option (or (car option) (> arg 2)))
	(if (or (and option (> arg 0))
		(> arg 1)
		(and (= arg 1)
		     (funcall session-buffer-check-function (current-buffer))))
	    (let ((locals session-locals-include)
		  (store nil))
	      (while locals
		(if (if (functionp session-locals-include)
			(funcall session-locals-predicate
				 (car locals) (current-buffer))
		      session-locals-predicate)
		    (push (cons (car locals)
				(symbol-value (car locals)))
			  store))
		(setq locals (cdr locals)))
	      (push (nconc (list file-name
				 (point) (mark t)
				 (point-min)
				 (and (<= (point-max) (buffer-size))
				      (point-max))
				 option
				 (or (car (session-undo-position
					   nil (and (consp buffer-undo-list)
						    buffer-undo-list)))
				     session-last-change))
			   store)
		    session-file-alist)))))))

(defun session-find-file-not-found-hook ()
  "Query the user to delete the permanent flag for a non-existent file.
Always return nil."
  (let ((file-name (session-buffer-file-name)))
    (when file-name
      (let ((option (nthcdr 5 (assoc file-name session-file-alist))))
	(and (car option)
	     (y-or-n-p "Delete permanent flag for non-existent file? ")
	     (setcar option nil))))))


;;;===========================================================================
;;;  Default standard check for buffers to be killed
;;;===========================================================================

(defun session-default-buffer-check-p (buffer)
  "Default function for `session-buffer-check-function'.
Argument BUFFER should be the current buffer."
  (and
   ;; undo check -------------------------------------------------------------
   (or (and (eq (cdr-safe session-undo-check) 'or)
	    session-last-change)
       (and (or (not (eq (cdr-safe session-undo-check) 'and))
		session-last-change)
	    (>= (if (listp buffer-undo-list) (length buffer-undo-list) -1)
		(if (consp session-undo-check)
		    (car session-undo-check)
		  session-undo-check))))
   ;; mode and name check ----------------------------------------------------
   (let ((file (buffer-file-name buffer)))
     (and (file-exists-p file) (file-readable-p file)
	  (if (if session-auto-store
		  (not (memq major-mode session-mode-disable-list))
		(memq major-mode session-mode-enable-list))
	      (not (and session-name-disable-regexp
			(string-match session-name-disable-regexp file)))
	    (and session-name-enable-regexp
		 (string-match session-name-enable-regexp file)))))))


;;;===========================================================================
;;;  Save session file
;;;===========================================================================

(defun session-save-session ()
  "Save session: file places, *-ring, *-history, registers.
Save some global variables (see `session-globals-regexp' and
`session-globals-include'), which include an alist of files with their
places and local variables (see `session-auto-store' and
`session-locals-include'), and registers, see `session-save-registers'."
  (interactive)
  (and session-save-file
       (not (and (eq this-command 'save-buffers-kill-emacs)
		 (equal current-prefix-arg 0)))
       (or session-successful-p
	   (not (file-exists-p session-save-file))
	   (y-or-n-p "Overwrite old session file (not loaded)? "))
       (save-excursion
	 ;; `kill-emacs' doesn't kill the buffers ----------------------------
	 (let ((buffers (nreverse (buffer-list))))
	   (while buffers
	     (set-buffer (car buffers))
	     (when buffer-file-name
	       (session-store-buffer-places 1)
	       (if session-register-swap-out
		   (funcall session-register-swap-out)))
	     (setq buffers (cdr buffers))))
	 ;; create header of session file ------------------------------------
	 (set-buffer (get-buffer-create " session "))
	 (erase-buffer)
	 (set-buffer-file-coding-system (quote raw-text) nil)
	 (insert ";;;-*- coding: raw-text"
;;     (format "%s" buffer-file-coding-system)
     " -*-\n;;; Automatically generated on "
     (current-time-string)
		 "\n;;; Invoked by "
		 (user-login-name)
		 "@"
		 (system-name)
		 " using "
		 emacs-version
		 "\n")
	 (let ((s-excl session-globals-exclude)
	       (slist (append session-globals-include
			      (apropos-internal session-globals-regexp
						'boundp)))
	       ;;(print-readably t) ; no way!
	       symbol val vlist len ass-p)
	   ;; save global variables ------------------------------------------
	   (while slist
	     (setq symbol (car slist)
		   slist  (cdr slist)
		   len session-globals-max-size
		   ass-p nil)
	     (if (consp symbol)
		 (setq ass-p (third symbol)
		       len (or (second symbol) session-globals-max-size)
		       symbol  (first symbol)))
	     (and (default-boundp symbol)
		  (setq val (default-value symbol))
		  (consp val)
		  (not (memq symbol s-excl))
		  (condition-case nil
		      (progn
			(push symbol s-excl)
			;; only takes first of same elements, cut length
			(setq vlist nil)
			(while val
			  (or (and (stringp (car val))
				   (> (length (car val))
				      session-globals-max-string))
			      (if ass-p
				  (assoc (caar val) vlist)
				(member (car val) vlist))
			      (progn
				(push (car val) vlist)
				(>= (setq len (1- len)) 0))
			      (setq val nil))
			  (setq val (cdr val)))
			;; print (the tricky part (read/load isn't clever)):
			;; check each elem
			(while vlist
			  (condition-case nil
			      (push (read (prin1-to-string (car vlist))) val)
			    (error nil))
			  (setq vlist (cdr vlist)))
			(insert (format "(setq-default %S '%S)\n" symbol val)))
		    (error nil))))
	   (session-save-registers)
	   (run-hooks 'session-before-save-hook)
	   (if (file-exists-p session-save-file)
	       (delete-file session-save-file))
	   (condition-case var
	       (progn
		 (make-directory (file-name-directory session-save-file) t)
		 (write-region (point-min) (point-max) session-save-file))
	     (file-error
	      (or (y-or-n-p "Could not write session file.  Exit anyway? ")
		  (while t		; XEmacs: `signal-error'
		    (signal (car var) (cdr var))))))
	   (kill-buffer (current-buffer))))))

(defun session-save-registers ()
  "Save registers in `session-registers'."
  (let ((chars session-registers)
	(type 'file)
	register from to)
    (while chars
      (if (symbolp (car chars))
	  (setq type  (car chars)
		chars (cdr chars))
	(setq from (car chars)
	      chars (cdr chars))
	(if (consp from)
	    (setq to   (cdr from)
		  from (car from))
	  (setq to from))
	(while (<= from to)
	  (and (fboundp 'int-to-char) (numberp from)
	       (setq from (int-to-char from)))
	  (setq register (get-register from))
	  (cond ((null register))
		((and (memq type '(file t))
		      (consp register)
		      (memq (car register) '(file file-query)))
		 (insert (if (eq (car register) 'file)
			     (format "(set-register %S '(file . %S))\n"
				     from (cdr register))
			   (format "(set-register %S '(file-query %S %d))\n"
				   from (cadr register) (caddr register)))))
		((and (memq type '(region t))
		      (stringp register)
		      (< (length register) session-registers-max-string))
		 (insert (format "(set-register %S %S)\n" from register))))
	  (setq from (1+ from)))))))


;;;===========================================================================
;;;  Set hooks, load session file
;;;===========================================================================

;;;###autoload
(defun session-initialize (&rest dummies)
  ;; checkdoc-params: (dummies)
  "Initialize package session and read previous session file.
Setup hooks and load `session-save-file', see `session-initialize'.  At
best, this function is called at the end of the Emacs startup, i.e., add
this function to `after-init-hook'."
  (interactive)
  (setq session-use-package t)
  (when (or (eq session-initialize t)
	    (memq 'de-saveplace session-initialize))
    ;; Features of package saveplace, which has an auto-init, are covered by
    ;; this package.
    (when (functionp 'eval-after-load)
      (eval-after-load "saveplace"
	'(progn
	   (remove-hook 'find-file-hooks 'save-place-find-file-hook)
	   (remove-hook 'kill-emacs-hook 'save-place-kill-emacs-hook)
	   (remove-hook 'kill-buffer-hook 'save-place-to-alist)))))
  (when (or (eq session-initialize t)
	    (memq 'places session-initialize))
    ;; `session-find-file-hook' should be *very* late in `find-file-hooks',
    ;; esp. if some package, e.g. crypt or iso-cvt, change the buffer contents:
    (add-hook 'find-file-hooks 'session-find-file-hook t)
    (add-hook 'find-file-not-found-hooks 'session-find-file-not-found-hook t)
    (add-hook 'kill-buffer-hook 'session-kill-buffer-hook))
  (when (string-match "XEmacs\\|Lucid" emacs-version)
    (add-hook 'menu-no-selection-hook 'session-no-selection-hook)
    ;;(remove-hook 'menu-no-selection-hook 'session-no-selection-hook)
    (when (or (eq session-initialize t)
	      (memq 'keys session-initialize))
      (define-key ctl-x-map [(undo)] 'session-jump-to-last-change)
      (define-key ctl-x-map [(control ?\/)] 'session-jump-to-last-change)
      (define-key global-map [(control button3)] 'session-popup-yank-menu))
    (when (or (eq session-initialize t)
	      (memq 'menus session-initialize))
      (add-submenu '("File")
		   '("Open...recently changed" :included session-file-alist
		     :filter session-file-changed-menu-filter)
		   "Open...")
      (add-submenu '("File")
		   '("Open...recently opened" :included file-name-history
		     :filter session-file-opened-menu-filter)
		   "Open...")
      (add-hook 'find-file-hooks 'session-set-file-name-history)
      (add-submenu '("Edit")
		   '("Yank...recently killed"
		     :included kill-ring
		     :filter session-yank-menu-filter)
		   "Undo")))
  (when (or (eq session-initialize t)
	    (memq 'session session-initialize))
    (add-hook 'kill-emacs-hook 'session-save-session)
    (or session-successful-p
	(setq session-successful-p
	      (and session-save-file (load session-save-file t nil t))))))

;;; session.el ends here

; LocalWords:  Stallman saveplace saveconf wicos ng ARG'th excl desc slist fns
; LocalWords:  vlist esp cvt checkdoc params XEmacs'en flyspell UNIXish sep
; LocalWords:  LogFiles fromchar tochar newstr mswindows
