;;; cua.el --- emulate CUA key bindings

;; Copyright (C) 1997-2001 Free Software Foundation, Inc.

;; Author: Kim F. Storm <storm@cua.dk>
;; Maintainer: Kim F. Storm <storm@cua.dk>
;; Location: http://www.cua.dk/
;; Keywords: keyboard CUA
;; Version: 2.10

;; This file is not [yet] part of GNU Emacs, but is distributed under
;; the same terms.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;; Note:
;;   This file used to be named cua-mode.el; if you or your users
;;   are using a previous release under that name, you can continue
;;   naming the file cua-mode.el; provided you also continue using
;;   (require 'cua-mode) before activation [see below].


;;; Activation:

;; To activate CUA-mode, copy the cua.el file into a directory
;; on the load-path, byte-compile it (using M-x byte-compile-file),
;; and place the following lines into your .emacs:
;;
;;	(require 'cua)
;;	(CUA-mode t)
;;
;; If you don't want to modify the bindings of C-z C-x C-c C-v, but still
;; want the CUA enhanced functionality for the standard emacs bindings of
;; undo, kill, copy, and yank, use the following lines instead:
;; 
;;	(require 'cua)
;;	(CUA-mode 'emacs)
;;
;; If you bind any keys to 'undo or 'advertised-undo in your .emacs file,
;; CUA-mode should be activated *after* those bindings!  Otherwise,
;; bind the key to CUA-undo

;; You may also use Customize to turn on CUA-mode and control its
;; features and fine tune its behaviour. You'll find CUA-mode under
;; the Editing Basics group.

;; Optionally, place this line in your .emacs to turn the numeric
;; keypad keys into numeric command prefix keys (useful with C-x, C-c
;; and C-v too to save into and load from registers 1-9):
;;
;;	(CUA-keypad-mode 'prefix t)


;;; Commentary:

;; This is the CUA-mode package which provides a complete emulation of
;; the standard CUA key bindings (Motif/Windows/Mac GUI) for selecting
;; and manipulating the region where S-<movement> is used to
;; highlight & extend the region.

;; This package allow the C-z, C-x, C-c, and C-v keys to be
;; bound appropriately according to the Motif/Windows GUI, i.e.
;;	C-z	-> undo
;;	C-x	-> cut
;;	C-c	-> copy
;;	C-v	-> paste
;;
;; The tricky part is the handling of the C-x and C-c keys which
;; are normally used as prefix keys for most of emacs' built-in
;; commands.  With CUA-mode they still do!!!
;;
;; Only when the region is currently active (and highlighted since
;; transient-mark-mode is used), the C-x and C-c keys will work as CUA
;; keys
;; 	C-x -> cut
;; 	C-c -> copy
;; When the region is not active, C-x and C-c works as prefix keys!

;; This probably sounds strange and difficult to get used to - but
;; based on my own experience and the feedback from many users of
;; CUA-mode, it actually works very well and users adapt to it
;; instantly - or at least very quickly.  So give it a try!  
;; ... and in the few cases where you make a mistake and accidentally
;; delete the region - you just undo the mistake (with C-z).

;; If you really need to perform a command which starts with one of
;; the prefix keys even when the region is active, you have three options:
;; - press the prefix key twice very quickly (within 0.2 seconds),
;; - press the prefix key and the following key within 0.2 seconds), or
;; - use the SHIFT key with the prefix key, i.e. C-X or C-C
;;
;; This behaviour is controlled via the CUA-mode-inhibit-method and
;; CUA-mode-inhibit-delay variables. 

;; In addition to using the shifted movement keys, you can also use
;; [C-space] to start the region and use unshifted movement keys to extend
;; it. To cancel the region, use [C-space] or [C-g].

;; If you want to take advantage of CUA-mode's superior rectangle
;; support and uniform bindings, but prefer to use the standard
;; emacs cut, copy, paste, and undo bindings, set the variable
;; CUA-mode-emacs-bindings to t before the call to CUA-mode.


;;; New features

;; With release 2 of the CUA-mode package, the CUA commands have been
;; extended to handle both rectangles and registers in a homogeneous
;; manner, as well as adding the concept of a "global mark" as
;; described below.

;;; CUA rectangle support

;; Emacs' normal rectangle support is based on interpreting the region
;; between the mark and point as a "virtual rectangle", and using a
;; completely separate set of "rectangle commands" [C-x r ...] on the
;; region to copy, kill, fill a.s.o. the virtual rectangle.
;; 
;; CUA-mode's superior rectangle support is based on using a true visual
;; representation of the selected rectangle. To start a rectangle, use
;; [S-return] and extend it using the normal movement keys (up, down,
;; left, right, home, end, C-home, C-end). Once the rectangle has the
;; desired size, you can cut or copy it using C-x and C-c, and you can
;; subsequently insert it - as a rectangle - using C-v.  So the only new
;; command you need to know to work with CUA-mode rectangles is S-return!
;; 
;; Furthermore, CUA-mode's rectangles are not limited to the actual
;; contents of the buffer, so if the cursor is currently at the end of a
;; short line, you can still extend the rectangle to include more columns
;; of longer lines in the same rectangle.  Sounds strange? Try it!
;; 
;; You can enable padding for just this rectangle by pressing [M-p];
;; this works like entering 'picture mode' where the tabs and spaces
;; are automatically converted/inserted to make the rectangle truly
;; rectangular. Or you can do it for all rectangles by setting the
;; CUA-mode-auto-expand-rectangles variable to 'yes.

;; And there's more: If you want to extend or reduce the size of the
;; rectangle in one of the other corners of the rectangle, just use
;; [return] to move the cursor to the "next" corner.  Or you can use
;; the [M-up], [M-down], [M-left], and [M-right] keys to move the
;; entire rectangle overlay (but not the contents) in the given
;; direction.
;;
;; [S-return] cancels the rectangle
;; [C-space] activate region bounded by rectangle

;; If you type a normal (self-inserting) character when the rectangle is
;; active, the character is inserted on the "current side" of every line
;; of the rectangle.  The "current side" is the side on which the cursor
;; is currently located. If the rectangle is only 1 column wide,
;; insertion will be performed to the left when the cursor is at the
;; bottom of the rectangle.  So, for example, to comment out an entire
;; paragraph like this one, just place the cursor on the first character
;; of the first line, and enter the following:
;;     [S-return] [down].... [;; ] [S-return]
 
;; CUA-mode's rectangle support also includes all the normal rectangle
;; functions with easy access:
;;
;; [M-a] aligns all words at the left edge of the rectangle
;; [M-b] fills the rectangle with blanks (tabs and spaces)
;; [M-c] closes the rectangle by removing all blanks at the left edge
;;       of the rectangle
;; [M-f] fills the rectangle with a single character (prompt)
;; [M-F] performs text filling on the rectangle
;; [M-i] increases the first number found on each line of the rectangle
;;       by the amount given by the numeric prefix argument (default 1)
;;       It recognizes 0x... as hexadecimal numbers
;; [M-k] kills the rectangle as a normal multi-line text (for paste)
;; [M-l] downcases the rectangle
;; [M-m] kills the rectangle as a normal multi-line text (for paste)
;; [M-n] fills each line of the rectangle with increasing numbers using
;;       a supplied format string (prompt)
;; [M-o] opens the rectangle by moving the highlighted text to the 
;;       right of the rectangle and filling the rectangle with blanks.
;; [M-p] toggles rectangle padding, i.e. insert tabs and spaces to
;;       make rectangles truly rectangular
;; [M-r] replaces REGEXP (prompt) by STRING (prompt) in rectangle
;; [M-R] reverse the lines in the rectangle
;; [M-s] fills each line of the rectangle with the same STRING (prompt)
;; [M-t] performs text fill of the rectangle with TEXT (prompt)
;; [M-u] upcases the rectangle
;; [M-|] runs shell command on rectangle
;; [M-'] restricts rectangle to lines with CHAR (prompt) at left column
;; [M-/] restricts rectangle to lines matching REGEXP (prompt)
;; [C-?] Shows a brief list of the above commands.

;; [M-C-up] and [M-C-down] scrolls the lines INSIDE the rectangle up
;; and down; lines scrolled outside the top or bottom of the rectangle
;; are lost, but can be recovered using [C-z].


;;; CUA register support

;; Emacs' standard register support is also based on a separate set of
;; "register commands".
;; 
;; CUA-mode's register support is activated by providing a numeric
;; prefix argument to the C-x, C-c, and C-v commands. For example,
;; to copy the selected region to register 2, enter [M-2 C-c].
;; Or if you have activated the keypad prefix mode, enter [kp-2 C-c].
;; 
;; And CUA-mode will copy and paste normal region as well as rectangles
;; into the registers, i.e. you use exactly the same command for both.
;; 
;; In addition, the last highlighted text that is deleted (not copied),
;; e.g. by typing text over a highlighted region, is automatically saved
;; in register 0, so you can insert it using [kp-0 C-v].

;;; CUA Global Mark
 
;; The final feature provided by CUA-mode is the "global mark", which
;; makes it very easy to copy bits and pieces from the same and other
;; files into the current text.  To enable and cancel the global mark,
;; use [S-C-space].  The cursor will blink when the global mark
;; is active.  The following commands behave differently when the global
;; mark is set:
;; <ch>  All characters (including returns) you type are inserted 
;;       at the global mark!
;; [C-x] If you cut a region or rectangle, it is automatically inserted
;;       at the global mark, and the global mark is advanced.
;; [C-c] If you copy a region or rectangle, it is immediately inserted
;;       at the global mark, and the global mark is advanced.
;; [C-v] Copies a single character to the global mark.
;; [C-d] Moves (i.e. deletes and inserts) a single character to the
;;       global mark.
;; [backspace] deletes the character before the global mark, while
;; [delete] deltes the character after the global mark.

;; [S-C-space] Jumps to and cancels the global mark.
;; [C-u S-C-space] Cancels the global mark (stays in current buffer).

;;; CUA mode indications

;; As mentioned above, the cursor will blink when the global mark is
;; active.  In addition, you can choose to let CUA-mode use different
;; cursor colors to indicate overwrite mode and read-only buffers.
;; For example, the following setting will use a RED cursor in normal
;; (insertion) mode in read-write buffers, a YELLOW cursor in
;; overwrite mode in read-write buffers, and a GREEN cursor read-only
;; buffers:
;;
;;  (setq CUA-mode-normal-cursor-color "red")
;;  (setq CUA-mode-overwrite-cursor-color "yellow")
;;  (setq CUA-mode-read-only-cursor-color "green")
;;

;;; A few more details:

;; * When the region is highlighted, TAB and S-TAB will indent the entire
;;   region by the normal tab-width (or the given prefix argument).
;; 
;; * C-x C-x (exchange point and mark) no longer activates the mark (i.e. 
;;   highlights the region).  I found that to be confusing since the
;;   sequence C-x C-x (exchange once) followed by C-x C-x (change back)
;;   would then cut the region!  To activate the region in this way,
;;   use C-u C-x C-x.
;; 
;; * [delete] will delete (not copy) the highlighted region.
;; 
;; * The highlighted region is automatically deleted if other text is
;;   typed or inserted.  The previously highlighted text is saved in 
;;   register 0 (if CUA-mode-delete-to-register-0 is non-nil).
;; 
;; * You may choose to use M-r as a prefix for the register commands
;;   instead of C-x r by setting CUA-mode-register-commands-prefix.
;;   The original binding of M-r (move-to-window-line) is then moved
;;   to M-r M-r.
;;
;; * Normally, when you paste a rectangle using C-v, each line of the
;;   rectangle is inserted into the existing lines in the buffer.
;;   However, if overwrite-mode is turned on, the lines of the rectangle
;;   are inserted as a single block of lines at point.

;;; Todo:
;;  
;;  - fix loading of normal cursor color from frame parameters.
;;  - sticky region variable -> unshifted arrow keys deselects region/rectangle.
;;  - more documentation per function / variable
;;  - extended documentation via info would be fine
;;  - rectangle "box" mode
;;  - rectangle "move" mode (eg move all lines up/down)
;;  - insert/delete rectangle shall not modify tabs or eols if padding off.

;;; Implementation details

;; CUA-mode uses a versatile pre-command-hook and post-command-hook
;; to avoid rebinding any of the cursor movement or scrolling keys.
;;
;; The interpretation of C-x and C-c as either emacs prefix keys
;; or CUA cut/copy keys is handled via GNU emacs' key-translation-map.

;;; Acknowledgements

;; CUA-mode includes code and ideas from several related packages:
;;  pc-selection-mode by Michael Staats <michael@thp.Uni-Duisburg.DE>
;;  s-region by Morten Welinder <terra@diku.dk>
;;  delete-selection-mode by Matthieu Devin <devin@lucid.com>
;;
;; The rectangle handling and display code borrows from the standard
;; GNU emacs rect.el package and the the rect-mark.el package by Rick
;; Sladkey <jrs@world.std.com>.


;;;; Compatibility

;;; Support functions for pre 20.1/20.4 GNU emacsen
(eval-and-compile
  (if (fboundp 'unless) nil
    (defmacro unless (cond &rest body)
      "(unless COND BODY...): if COND yields nil, do BODY, else return nil."
      `(if ,cond nil ,@body))
    (put 'unless 'lisp-indent-function 1)
    (put 'unless 'edebug-form-spec '(&rest form)))

  (unless (fboundp 'when)
    (defmacro when (cond &rest body)
      "(when COND BODY...): if COND yields non-nil, do BODY, else return nil."
      `(if ,cond (progn ,@body)))
    (put 'when 'lisp-indent-function 1)
    (put 'when 'edebug-form-spec '(&rest form)))
  
  (unless (fboundp 'with-current-buffer)
    (defmacro with-current-buffer (buffer &rest body)
      "Execute the forms in BODY with BUFFER as the current buffer."
      `(save-excursion (set-buffer ,buffer) ,@body))))

(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (unless (fboundp 'defgroup)
    (defmacro defgroup (&rest rest) ()))
  (unless (fboundp 'defcustom)
    (defmacro defcustom (sym val str &rest rest)
      `(defvar ,sym ,val ,str)))
  (unless (fboundp 'defface)
    (defmacro defface (sym val str &rest rest)
      `(defvar ,sym (make-face ',sym) ,str))))

(unless (fboundp 'line-beginning-position)
  (defun line-beginning-position (&optional N)
    (save-excursion
      (beginning-of-line N)
      (point))))

(unless (fboundp 'line-end-position)
  (defun line-end-position (&optional N)
    (save-excursion
      (end-of-line N)
      (point))))


;;;; Customization:

(defgroup CUA-mode nil
  "Emulate CUA key bindings including C-x and C-c."
  :prefix "CUA-mode"
  :group 'editing-basics
  :group 'convenience
  :group 'emulations
  :link '(emacs-commentary-link :tag "Commentary" "cua.el")
  :link '(emacs-library-link :tag "Lisp File" "cua.el"))

;;;###autoload
(defcustom CUA-mode nil
  "Non-nil means that CUA emulation mode is enabled.
In CUA mode, shifted movement keys highlight and extend the region.
When a region is highlighted, the binding of the C-x and C-c keys are
temporarily changed to work as Motif, MAC or MS-Windows cut and paste.
Also, insertion commands first delete the region and then insert.
This mode enables Transient Mark mode and it provides a superset of the
PC Selection Mode and Delete Selection Modes.

Setting this variable directly does not take effect;
use either \\[customize] or the function `CUA-mode'."
  :set (lambda (symbol value)
	 (CUA-mode (or value 0)))
  :initialize 'custom-initialize-default
  :require 'cua
  :link '(emacs-commentary-link "cua.el")
  :version "20.5"
  :type 'boolean
  :group 'CUA-mode)

(defcustom CUA-mode-emacs-bindings nil
  "Non-nil means CUA functionality is enabled without binding CUA keys.
This causes the normal bindings of kill-region, copy-region-as-kill, yank,
and undo to behave in the CUA style, but the C-z, C-c, C-x, and
C-v keys are not rebound.")

(defcustom CUA-mode-remap-cx-shift-only nil
  "*If non-nil, only remap C-c and C-x if region was marked with S-<move>.
I.e. if the mark was set using \\[CUA-set-mark], those keys have their
normal prefix bindings."
  :type 'boolean
  :group 'CUA-mode)

(defcustom CUA-mode-highlight-shift-only nil
  "*If non-nil, only highlight region if marked with S-<move>.
When this is non-nil, CUA toggles `transient-mark-mode' on when the region
is marked using shifted movement keys, and off when the mark is cleared.
But when the mark was set using \\[CUA-set-mark], transient-mark-mode
is not turned on."
  :type 'boolean
  :group 'CUA-mode)


(defcustom CUA-mode-inhibit-method 
  (if (memq system-type '(windows-nt ms-dos)) 'shift 'twice)
  "How to inhibit the CUA interpretation of the next prefix key.

When the region is active, the following methods are available to 
use the normal functionality of the prefix keys:
delay	 The interpretation of a prefix keys as a CUA key is delayed
	 for `CUA-mode-inhibit-delay' milliseconds after the prefix
	 key has been typed.
	 If a second key is typed before the delay elapses, the prefix
	 key works as a normal prefix together with the second key.
twice	 As delay, but if the second key typed is the same prefix key,
	 the first prefix key is ignored, while the second prefix key
	 will works as a normal prefix key for the following keys.
shift	 The Shift key must be used together with the prefix key.
	 (Actually this method always works unless the shifted prefix
	 keys are explicitly bound to a command).

Notice that the delay and twice options only works well on systems
with a sit-for function supporting fractions of a second delays."
  :type '(choice (const delay) 
		 (const twice)
		 (const shift))
  :group 'CUA-mode)

(defcustom CUA-mode-inhibit-delay 250
  "Period during which typing another key inhibits CUA prefix keys.
Measured in milliseconds.
Used when `CUA-mode-inhibit-method' is delay or twice."
  :type 'integer
  :group 'CUA-mode)

(defcustom CUA-mode-register-commands-prefix nil
  "Remap register commands [C-x r ...] onto this prefix.
E.g. to use M-r as register command prefix, use the value [?\\M-r].
If set to nil, register commands are not remapped.
Other C-x ? commands can be remapped using CUA-remap-ctl-x-commands"
  :set (lambda (symbol value)
	 (if value
	     (CUA-remap-ctl-x-commands "r" value)))
  :type 'sexp
  :group 'CUA-mode)

(defcustom CUA-mode-keep-region-after-copy nil
  "If non-nil, don't deselect the region after copying."
  :type 'boolean
  :group 'CUA-mode)

(defcustom CUA-mode-auto-expand-rectangles nil
  "If non-nil, rectangles are padded with spaces to make straight edges.
This implies modifying buffer contents by expanding tabs and inserting spaces.
Consequently, this is inhibited in read-only buffers.
Can be toggled by [M-p] while the rectangle is active,"
  :type 'boolean
  :group 'CUA-mode)

(defcustom CUA-mode-global-mark-visible t
  "If non-nil, always keep global mark visible in other window."
  :type 'boolean
  :group 'CUA-mode)

(defcustom CUA-mode-feature-global-mark t
  "If non-nil, target for kill and copy region is global mark when active.
When nil, or global mark is not active, target is kill-ring."
  :type 'boolean
  :group 'CUA-mode)

(defcustom CUA-mode-feature-registers t
  "If non-nil, target for kill and copy region is a register if prefix arg.
When nil, or no prefix arg, target is kill-ring (or global mark)."
  :type 'boolean
  :group 'CUA-mode)

(defcustom CUA-mode-delete-to-register-0 t
  "*If non-nil, save last deleted region or rectangle to register 0."
  :type 'boolean
  :group 'CUA-mode)

(defcustom CUA-mode-auto-help t
  "*If non-nil, automatically show help for region, rectangle and global mark."
  :type 'boolean
  :group 'CUA-mode)

(defcustom CUA-mode-use-modeline nil
  "*If non-nil, use minor-mode hook to show status in mode line."
  :type 'boolean
  :group 'CUA-mode)

(defcustom CUA-mode-use-hyper-key nil
  "*If non-nil, bind rectangle commands to H-? instead of M-?.
If set to 'also, toggle region command is also on S-return.
Must be set prior to enabling CUA-mode."
  :type '(choice (const nil)
		 (const only)
		 (const also))
  :group 'CUA-mode)

(defcustom CUA-debug nil
  "*Enable CUA mode debugging."
  :type 'boolean
  :group 'CUA-mode)

(defface CUA-rectangle-face 'nil
  "*Font used by CUA for highlighting the rectangle."
  :group 'CUA-mode)

(defface CUA-rectangle-noselect-face 'nil
  "*Font used by CUA for highlighting the non-selected rectangle lines."
  :group 'CUA-mode)

(defface CUA-global-mark-face '((((class color)) 
                                  (:foreground "black")
				  (:background "yellow"))
                                 (t (:bold t)))
  "*Font used by CUA for highlighting the global mark."
  :group 'CUA-mode)

(defcustom CUA-mode-use-cursor-colors t
  "*If non-nil, use different cursor colors for indications."
  :type 'boolean
  :group 'CUA-mode)

(defcustom CUA-mode-normal-cursor-color nil
  "Normal (non-overwrite) cursor color.
Also used to indicate that rectangle padding is not in effect.
Automatically loaded from frame parameters, if nil."
  :initialize (lambda (symbol value)
		(set symbol (or value
				(and (boundp 'initial-cursor-color) initial-cursor-color)
				(and (boundp 'initial-frame-alist)
				     (assoc 'cursor-color initial-frame-alist)
				     (cdr (assoc 'cursor-color initial-frame-alist)))
				(and (boundp 'default-frame-alist)
				     (assoc 'cursor-color default-frame-alist)
				     (cdr (assoc 'cursor-color default-frame-alist)))
				(frame-parameter nil 'cursor-color))))
  :type 'color
  :group 'CUA-mode)

(defcustom CUA-mode-read-only-cursor-color "darkgreen"
  "*Cursor color used in read-only buffers, if non-nil."
  :type 'color
  :group 'CUA-mode)

(defcustom CUA-mode-overwrite-cursor-color "yellow"
  "*Cursor color used when overwrite mode is set, if non-nil.
Also used to indicate that rectangle padding is in effect."
  :type 'color
  :group 'CUA-mode)

(defcustom CUA-mode-global-mark-cursor-color "cyan"
  "*Indication for active global mark.
Will change cursor color to specified color if string."
  :type 'color
  :group 'CUA-mode)

(defcustom CUA-mode-global-mark-cursor-blink t
  "*If non-nil, use blinking cursor as indication for active global mark."
  :type 'boolean
  :group 'CUA-mode)

;;; Code:

;; Basic configuration options

(defvar CUA-prefix-key-mappings
  '((?\C-x . S-delete)
    (?\C-c . C-insert)
;;  (?\C-h . "C-?")
    )
  "List of prefix keys which are remapped via key-translation-map.
Each element in the list is a cons of the prefix key and the 
key it is translated into if the region is active.")

(defvar CUA-ctl-x-8-prefix-key nil
  "*Key which CUA uses in key-translation-map instead of C-x 8 prefix.
Must be set to a single character.  If no set, the first unused key
among the following list is chosen automatically:
H-8 (hyper 8), s-8 (super 8), C-H-8, C-H-s-8")

;; Misc variables

(defvar CUA-explicit-region-start nil
  "Current region was started using set-mark-command.")

(defvar CUA-last-action nil
  "Action taken by last command.")

(defvar CUA-cur-register nil
  "Current register selected by prefix arg.")

(defvar CUA-mode-status nil
  "Modeline status indication.")

(defvar CUA-orig-command nil
  "The original command before remapping.")

;;; Register support

(defun CUA-register ()
  (and (not CUA-cur-register)
       CUA-mode-feature-registers
       (integerp current-prefix-arg)
       (>= current-prefix-arg 0) (< current-prefix-arg 10)
       (setq CUA-cur-register (+ current-prefix-arg ?0)
	     current-prefix-arg nil
	     overriding-terminal-local-map nil))
  CUA-cur-register)

;;; Rectangle support

(require 'rect)

(defvar CUA-rectangle nil
  "If non-nil, restrict current region to this rectangle.
Value is a vector [top bot left right corner ins pad select].
CORNER specifies currently active corner 0=t/l 1=t/r 2=b/l 3=b/r.
INS specifies whether to insert on left(nil) or right(t) side.
If PAD is non-nil, tabs are converted to spaces when necessary.
If SELECT is a regexp, only lines starting with that regexp are affected.")

(defvar CUA-last-rectangle nil
  "Most recent rectangle geometry.
Note: car is buffer.")

;; rectangle restored by undo
(defvar CUA-next-rectangle nil)

;; buffer + point prior to current command when rectangle is active
;; checked in post-command hook to see if point was moved
(defvar CUA-start-point nil)

(defvar CUA-rect-last-killed nil
  "Last rectangle copied/killed; nil if last kill was not a rectangle.")

(defvar CUA-rect-overlays nil
  "List of overlays used to display current rectangle.")

(defun CUA-rect-top (&optional val)
  "Top of CUA rectangle (buffer position on first line)."
  (if (not val)
      (aref CUA-rectangle 0)
    (setq val (line-beginning-position))
    (if (<= val (aref CUA-rectangle 1))
	(aset CUA-rectangle 0 val)
      (aset CUA-rectangle 1 val)
      (CUA-rect-corner 2))))

(defun CUA-rect-bot (&optional val)
  "Bot of CUA rectangle (buffer position on last line)."
  (if (not val)
      (aref CUA-rectangle 1)
    (setq val (line-end-position))
    (if (>= val (aref CUA-rectangle 0))
	(aset CUA-rectangle 1 val)
      (aset CUA-rectangle 0 val)
      (CUA-rect-corner 2))))

(defun CUA-rect-left (&optional val)
  "Left column of CUA rectangle."
  (if (integerp val)
      (if (<= val (aref CUA-rectangle 3))
	  (aset CUA-rectangle 2 val)
	(aset CUA-rectangle 3 val)
	(CUA-rect-corner (if (CUA-rect-right-side) -1 1)))
    (aref CUA-rectangle 2)))

(defun CUA-rect-right (&optional val)
  "Right column of CUA rectangle."
  (if (integerp val)
      (if (>= val (aref CUA-rectangle 2))
	  (aset CUA-rectangle 3 val)
	(aset CUA-rectangle 2 val)
	(CUA-rect-corner (if (CUA-rect-right-side) -1 1)))
    (aref CUA-rectangle 3)))

(defun CUA-rect-corner (&optional advance)
  "Currently active corner of rectangle."
  (let ((c (aref CUA-rectangle 4)))
    (if (not (integerp advance))
	c
      (aset CUA-rectangle 4 
	    (if (= advance 0)
		(- 3 c) ; opposite corner
	      (mod (+ c 4 advance) 4)))
      (aset CUA-rectangle 5 0))))

(defun CUA-rect-right-side (&optional topbot)
  ;; t if point is on right side of rectangle.
  (if (and topbot (= (CUA-rect-left) (CUA-rect-right)))
      (< (CUA-rect-corner) 2)
    (= (mod (CUA-rect-corner) 2) 1)))

(defun CUA-rect-column ()
  (if (CUA-rect-right-side)
      (CUA-rect-right)
    (CUA-rect-left)))

(defun CUA-rect-insert-col (&optional col)
  "Currently active corner of rectangle."
  (if (integerp col)
      (aset CUA-rectangle 5 col)
    (if (CUA-rect-right-side t)
	(if (= (aref CUA-rectangle 5) 0)
	    (1+ (CUA-rect-right))
	  (aref CUA-rectangle 5))
      (CUA-rect-left))))

(defun CUA-rect-padding (&optional set val)
  (if set
      (aset CUA-rectangle 6 val))
  (and (not buffer-read-only)
       (aref CUA-rectangle 6)))

(defun CUA-rect-restriction (&optional val bounded negated)
  (if val
      (aset CUA-rectangle 7
	    (and (stringp val)
	     (> (length val) 0)
	     (list val bounded negated)))
    (aref CUA-rectangle 7)))

(defun CUA-rect-assert ()
  (message "%S (%d)" CUA-rectangle (point))
  (if (< (CUA-rect-right) (CUA-rect-left))
      (message "rectangle right < left"))
  (if (< (CUA-rect-bot) (CUA-rect-top))
      (message "rectangle bot < top")))

(defun CUA-rect-get-corners (&optional pad)
  ;; Calculate the rectangular region represented by point and mark,
  ;; putting start in the upper left corner and end in the
  ;; bottom right corner.
  (let ((top (point)) (bot (mark)) r l corner)
    (save-excursion
      (goto-char top)
      (setq l (current-column))
      (goto-char bot)
      (setq r (current-column))
      (if (<= top bot)
	  (setq corner (if (<= l r) 0 1))
	(setq top (prog1 bot (setq bot top)))
	(setq corner (if (<= l r) 2 3)))
      (if (<= l r)
	  (if (< l r)
	      (setq r (1- r)))
	(setq l (prog1 r (setq r l)))
	(goto-char top)
	(move-to-column l pad)
	(setq top (point))
	(goto-char bot)
	(move-to-column r pad)
	(setq bot (point))))
    (vector top bot l r corner 0 pad nil)))

(defun CUA-rect-set-corners ()
  ;; Set mark and point in opposite corners of current rectangle.
  (let (pp pc mp mc (c (CUA-rect-corner)))
    (cond
     ((= c 0)  ; top/left -> bot/right
      (setq pp (CUA-rect-top) pc (CUA-rect-left)
	    mp (CUA-rect-bot) mc (CUA-rect-right)))
     ((= c 1)  ; top/right -> bot/left
      (setq pp (CUA-rect-top) pc (CUA-rect-right)
	    mp (CUA-rect-bot) mc (CUA-rect-left)))
     ((= c 2)  ; bot/left -> top/right
      (setq pp (CUA-rect-bot) pc (CUA-rect-left)
	    mp (CUA-rect-top) mc (CUA-rect-right)))
     ((= c 3)  ; bot/right -> top/left
      (setq pp (CUA-rect-bot) pc (CUA-rect-right)
	    mp (CUA-rect-top) mc (CUA-rect-left))))
    (goto-char mp)
    (move-to-column mc (CUA-rect-padding))
    (set-mark (point))
    (goto-char pp)
    (move-to-column pc (CUA-rect-padding))))

(defun CUA-forward-line (n pad)
  (if (or (not pad) (< n 0))
      (= (forward-line n) 0)
    (next-line 1)
    t))

(defun CUA-rect-resize (command)
  ;; Adjust rectangle size based on movement command
  (let ((cmd (or (get command 'CUA-rect) command))
	(pad (CUA-rect-padding))
	(resized t))
    (cond
     ((eq cmd 'forward-char)
      (cond
       ((and (CUA-rect-right-side) (or pad (eolp)))
	(CUA-rect-right (1+ (CUA-rect-right)))
	(move-to-column (CUA-rect-right) pad))
       ((CUA-rect-right-side)
	(forward-char 1)
	(CUA-rect-right (current-column)))
       ((or pad (eolp))
	(CUA-rect-left (1+ (CUA-rect-left)))
	(move-to-column (CUA-rect-right) pad))
       (t
	(forward-char 1)
	(CUA-rect-left (current-column)))))
     ((eq cmd 'backward-char)
      (cond
       ((= (CUA-rect-right) 0)
	nil)
       ((and (not (CUA-rect-right-side)) (= (CUA-rect-left) 0))
	nil)
       ((and (CUA-rect-right-side) (or pad (eolp) (bolp)))
	(CUA-rect-right (1- (CUA-rect-right)))
	(move-to-column (CUA-rect-right) pad))
       ((CUA-rect-right-side)
	(backward-char 1)
	(CUA-rect-right (current-column)))
       ((or pad (eolp) (bolp))
	(CUA-rect-left (1- (CUA-rect-left)))
	(move-to-column (CUA-rect-right) pad))
       (t
	(backward-char 1)
	(CUA-rect-left (current-column)))))
     ((eq cmd 'next-line)
      (cond
       ((>= (CUA-rect-corner) 2)
	(goto-char (CUA-rect-bot))
	(when (CUA-forward-line 1 pad)
	  (move-to-column (CUA-rect-column) pad)
	  (CUA-rect-bot t)))
       (t
	(goto-char (CUA-rect-top))
	(when (CUA-forward-line 1 pad)
	  (move-to-column (CUA-rect-column) pad)
	  (CUA-rect-top t)))))
     ((eq cmd 'previous-line)
      (cond
       ((>= (CUA-rect-corner) 2)
	(goto-char (CUA-rect-bot))
	(when (CUA-forward-line -1 pad)
	  (move-to-column (CUA-rect-column) pad)
	  (CUA-rect-bot t)))
       (t
	(goto-char (CUA-rect-top))
	(when (CUA-forward-line -1 pad)
	  (move-to-column (CUA-rect-column) pad)
	  (CUA-rect-top t)))))
     ((memq cmd '(end-of-line end-of-line-or-backward-char))
      (cond
       ((not (eolp))
	(end-of-line)
	(if (> (current-column) (CUA-rect-right))
	    (CUA-rect-right (current-column)))
	(if (not (CUA-rect-right-side))
	    (CUA-rect-corner 1)))))
     ((memq cmd '(beginning-of-line beginning-of-line-or-indent))
      (cond
       ((not (bolp))
	(beginning-of-line)
	(CUA-rect-left (current-column))
	(if (CUA-rect-right-side)
	    (CUA-rect-corner -1)))))
     ((eq cmd 'end-of-buffer)
      (goto-char (point-max))
      (move-to-column (CUA-rect-column) pad)
      (CUA-rect-bot t))
     ((eq cmd 'beginning-of-buffer)
      (goto-char (point-min))
      (move-to-column (CUA-rect-column) pad)
      (CUA-rect-top t))
     ((memq cmd '(scroll-down scroll-up))
      (funcall cmd)
      (move-to-column (CUA-rect-column) pad)
      (if (>= (CUA-rect-corner) 2)
	  (CUA-rect-bot t)
	(CUA-rect-top t)))
     (t
      (setq resized nil)))
    (when resized
      (CUA-pad-rectangle pad)
      (CUA-rect-insert-col 0)
      (CUA-rect-set-corners)
      (CUA-keep-active t))
    resized))

(defun CUA-rect-move (dir)
  (let ((pad (CUA-rect-padding))
	(moved t)
	(top (CUA-rect-top))
	(bot (CUA-rect-bot))
	(l (CUA-rect-left))
	(r (CUA-rect-right)))
    (cond
     ((eq dir 'up)
      (goto-char top)
      (when (CUA-forward-line -1 pad)
	(CUA-rect-top t)
	(goto-char bot)
	(forward-line -1)
	(CUA-rect-bot t)))
     ((eq dir 'down)
      (goto-char bot)
      (when (CUA-forward-line 1 pad)
	(CUA-rect-bot t)
	(goto-char top)
	(CUA-forward-line 1 pad)
	(CUA-rect-top t)))
     ((eq dir 'left)
      (when (> l 0)
	(CUA-rect-left (1- l))
	(CUA-rect-right (1- r))))
     ((eq dir 'right)
      (CUA-rect-right (1+ r))
      (CUA-rect-left (1+ l)))
     (t
      (setq moved nil)))
    (when moved
      (CUA-pad-rectangle)
      (CUA-rect-set-corners)
      (CUA-keep-active t))))

(defun CUA-rect-operation (cmd visible undo pad &optional fct post-fct)
  ;; Call FCT for each line of region with 4 parameters:
  ;; Region start, end, left-col, right-col
  ;; Point is at start when FCT is called
  ;; Rectangle is padded if PAD = t or numeric and (CUA-rect-padding)
  ;; Mark is kept if keep == t and cleared if keep numeric
  (let* ((start (CUA-rect-top))
	 (end   (CUA-rect-bot))
	 (l (CUA-rect-left))
	 (r (1+ (CUA-rect-right)))
	 (m (make-marker))
	 (tabpad (and (integerp pad) (= pad 2)))
	 (sel (CUA-rect-restriction))
	 (keep-clear (and cmd (get cmd 'CUA-rect))))
    (if undo
	(CUA-rect-undo-boundary))
    (if (integerp pad)
	(setq pad (CUA-rect-padding)))
    (save-excursion
      (save-restriction
	(widen)
	(when (> (CUA-rect-corner) 1)
	  (goto-char end)
	  (and (bolp) (not (eolp)) (not (eobp))
	       (setq end (1+ end))))
	(when visible
	  (setq start (max (window-start) start))
	  (setq end   (min (window-end) end)))
	(goto-char end)
	(setq end (line-end-position))
	(goto-char start)
	(setq start (line-beginning-position))
	(narrow-to-region start end)
	(goto-char (point-min))
	(while (< (point) (point-max))
	  (move-to-column r pad)
	  (and (not pad) (not visible) (> (current-column) r)
	       (backward-char 1))
	  (if (and tabpad (not pad) (looking-at "\t"))
	      (forward-char 1))
	  (set-marker m (point))
	  (move-to-column l pad)
	  (if fct
	      (let ((v t) (p (point)))
		(when sel
		  (if (car (cdr sel))
		      (setq v (looking-at (car sel)))
		    (setq v (re-search-forward (car sel) m t))
		    (goto-char p))
		  (if (car (cdr (cdr sel)))
		      (setq v (null v))))
		(if visible
		    (funcall fct p m l r v)
		  (if v
		      (funcall fct p m l r)))))
	  (set-marker m nil)
	  (forward-line 1))
	(if (not visible)
	    (CUA-rect-bot t))
	(if post-fct
	    (funcall post-fct l r))))
    (cond
     ((eq keep-clear 'keep)
      (CUA-keep-active t))
     ((eq keep-clear 'clear)
      (CUA-keep-active nil))
     ((eq keep-clear 'corners)
      (CUA-rect-set-corners)
      (CUA-keep-active t)))))

(put 'CUA-rect-operation 'lisp-indent-function 4)

(defun CUA-pad-rectangle (&optional pad)
  (if (or pad (CUA-rect-padding))
      (CUA-rect-operation nil nil t t)))

(defun CUA-delete-rectangle ()
  (CUA-rect-operation nil nil t 2
    '(lambda (s e l r)
       (delete-region s (if (> e s) e (1+ e))))))

(defun CUA-extract-rectangle ()
  (let (rect)
    (CUA-rect-operation nil nil nil 1
     '(lambda (s e l r)
	(setq rect (cons (buffer-substring-no-properties s e) rect))))
      (nreverse rect)))

(defun CUA-insert-rectangle (rect &optional below)
  ;; Insert rectangle as insert-rectangle, but don't set mark and exit with
  ;; point at either next to top right or below bottom left corner
  ;; Notice: In overwrite mode, the rectangle is inserted as separate text lines.
  (if (and below (eq below 'auto))
      (setq below (and (bolp)
		       (or (eolp) (eobp) (= (1+ (point)) (point-max))))))
  (let ((lines rect)
	(insertcolumn (current-column))
	(first t)
	p)
    (while (or lines below)
      (or first
	  (if overwrite-mode
	      (insert ?\n)
	    (forward-line 1)
	    (or (bolp) (insert ?\n))
	    (move-to-column insertcolumn t)))
      (if (not lines)
	  (setq below nil)
	(insert (car lines))
	(setq lines (cdr lines))
	(and first (not below)
	     (setq p (point))))
      (setq first nil))
    (and p (not overwrite-mode)
	 (goto-char p))))

(defun CUA-rect-copy-as-kill (&optional ring)
  (if (CUA-register)
      (set-register (CUA-register) (CUA-extract-rectangle))
    (setq killed-rectangle (CUA-extract-rectangle))
    (setq CUA-rect-last-killed (cons (and kill-ring (car kill-ring)) killed-rectangle))
    (if ring
	(kill-new (mapconcat
		   (function (lambda (row) (concat row "\n")))
		   killed-rectangle "")))))

(defun CUA-rect-activate (&optional force)
  ;; Turn on rectangular marking mode by disabling transient mark mode
  ;; and manually handling highlighting from a post command hook.
  ;; Be careful if we are already marking a rectangle.
  (setq CUA-rectangle 
	(if (and CUA-last-rectangle 
		 (eq (car CUA-last-rectangle) (current-buffer))
		 (eq (car (cdr CUA-last-rectangle)) (point)))
	    (cdr (cdr CUA-last-rectangle))
	  (CUA-rect-get-corners
	   (and (not buffer-read-only)
		(or CUA-mode-auto-expand-rectangles
		    force
		    (eq major-mode 'picture-mode)))))
	CUA-mode-status (if (CUA-rect-padding) " Pad" "")
	CUA-last-rectangle nil))

(defvar CUA-save-point nil)

(defun CUA-rect-deactivate ()
  ;; This is used to clean up after `CUA-rect-activate'.
  (mapcar (function delete-overlay) CUA-rect-overlays)
  (setq CUA-last-rectangle (cons (current-buffer) (cons CUA-save-point CUA-rectangle))
	CUA-rectangle nil
	CUA-rect-overlays nil
	CUA-mode-status nil))

(defun CUA-rect-highlight ()
  ;; This function is used to highlight the rectangular region.
  ;; We do this by putting an overlay on each line within the rectangle.
  ;; Each overlay extends across all the columns of the rectangle.
  ;; We try to reuse overlays where possible because this is more efficient
  ;; and results in less flicker.
  ;; If CUA-rect-padding is nil and the buffer contains tabs or short lines,
  ;; the higlighted region may not be perfectly rectangular.
  (let ((deactivate-mark deactivate-mark)
	(old CUA-rect-overlays)
	(new nil)
	(left (CUA-rect-left))
	(right (1+ (CUA-rect-right))))
    (when (/= left right)
      (sit-for 0)  ; make window top/bottom reliable
      (CUA-rect-operation nil t nil nil
	'(lambda (s e l r v)
	   (let ((rface (if v 'CUA-rectangle-face 'CUA-rectangle-noselect-face))
		 overlay)
	     ;; Trim old leading overlays.
	     (while (and old
			 (setq overlay (car old))
			 (< (overlay-start overlay) s)
			 (/= (overlay-end overlay) e))
	       (delete-overlay overlay)
	       (setq old (cdr old)))
	     ;; Reuse an overlay if possible, otherwise create one.
	     (if (and old
		      (setq overlay (car old))
		      (or (= (overlay-start overlay) s)
			  (= (overlay-end overlay) e)))
		 (progn
		   (move-overlay overlay s e)
		   (setq old (cdr old)))
	       (setq overlay (make-overlay s e)))
	     (overlay-put overlay 'face rface)
	     (setq new (cons overlay new))))))
    ;; Trim old trailing overlays.
    (mapcar (function delete-overlay) old)
    (setq CUA-rect-overlays (nreverse new))))

(defun CUA-rect-indent-rectangle (&optional ch)
  "Indent current rectangle."
  (let ((col (CUA-rect-insert-col))
	(pad (CUA-rect-padding))
	indent)
    (CUA-rect-operation 'CUA-rect-indent-rectangle nil t pad
      '(lambda (s e l r)
	 (move-to-column col pad)
	 (if (and (eolp)
		  (< (current-column) col))
	     (move-to-column col t))
	 (if ch
	     (insert ch)
	   (tab-to-tab-stop))
	 (if (CUA-rect-right-side t)
	     (CUA-rect-insert-col (current-column))
	   (setq indent (- (current-column) l))))
      '(lambda (l r)
	 (when (and indent (> indent 0))
	   (aset CUA-rectangle 2 (+ l indent))
	   (aset CUA-rectangle 3 (+ r indent -1)))))))
(put 'CUA-rect-indent-rectangle 'CUA-rect 'corners)


;;; User functions.

;;;###autoload
(defun CUA-exchange-point-and-mark (arg)
  "Exchanges point and mark, but don't activate the mark.
Activates the mark if a prefix argument is given."
  (interactive "P")
  (if arg
      (setq mark-active t)
    (let (mark-active)
      (exchange-point-and-mark)
      (if CUA-rectangle
	  (CUA-rect-corner 0)))))

;;; Global Marker 

(defvar CUA-global-mark nil
  "Global mark position marker.")

(defvar CUA-global-mark-overlay nil
  "Overlay for global mark position.")

(defun CUA-global-mark-active ()
  (and (markerp CUA-global-mark) (marker-buffer CUA-global-mark)))

(defun CUA-global-mark-deactivate (&optional msg)
  (when CUA-global-mark-overlay
    (delete-overlay CUA-global-mark-overlay)
    (setq CUA-global-mark-overlay nil))
  (if (markerp CUA-global-mark)
      (move-marker CUA-global-mark nil))
  (if msg
      (message "Global Mark Cleared")))

(defun CUA-global-mark-activate (&optional msg)
  (if (not (markerp CUA-global-mark))
      (setq CUA-global-mark (make-marker)))
  (when (eobp)
    (insert " ")
    (backward-char 1))
  (move-marker CUA-global-mark (point))
  (if (overlayp CUA-global-mark-overlay)
      (move-overlay CUA-global-mark-overlay (point) (1+ (point)))
    (setq CUA-global-mark-overlay 
	  (make-overlay (point) (1+ (point))))
    (overlay-put CUA-global-mark-overlay 'face 'CUA-global-mark-face))
  (if msg
      (message "Global Mark Set")))

(defun CUA-cmd-toggle-global-mark (stay)
  "Set or cancel the global marker.
When the global marker is set, CUA cut and copy commands will automatically
insert the deleted or copied text before the global marker, even when the
global marker is in another buffer.
If the global marker isn't set, set the global marker at point in the current
buffer. Otherwise jump to the global marker position and cancel it.
With prefix argument, don't jump to global mark when cancelling it."
  (interactive "P")
  (if (not (CUA-global-mark-active))
      (if (not buffer-read-only)
	  (CUA-global-mark-activate t)
	(ding)
	(message "Cannot set global mark in read-only buffer."))
    (when (not stay)
      (pop-to-buffer (marker-buffer CUA-global-mark))
      (goto-char CUA-global-mark))
    (CUA-global-mark-deactivate t)))

(defun CUA-global-mark-insert (str &optional msg)
  ;; Insert string at global marker and move marker
  (save-excursion
    (set-buffer (marker-buffer CUA-global-mark))
    (goto-char (marker-position CUA-global-mark))
    (insert str)
    (CUA-global-mark-activate))
  (if msg
      (message "%s %d to global mark in %s:%d" msg
	       (length str)
	       (buffer-name (marker-buffer CUA-global-mark))
	       (marker-position CUA-global-mark))))

(defun CUA-global-mark-delete-char (arg &optional msg)
  ;; Delete chars at global marker
  (save-excursion
    (set-buffer (marker-buffer CUA-global-mark))
    (goto-char (marker-position CUA-global-mark))
    (delete-char arg))
  (if msg
      (message "%s %d chars at global mark in %s:%d" msg arg
	       (buffer-name (marker-buffer CUA-global-mark))
	       (marker-position CUA-global-mark))))

(defun CUA-global-mark-copy-region (start end)
  "Copy region to global mark buffer/position."
  (interactive "r")
  (if (CUA-global-mark-active)
      (let ((src-buf (current-buffer)))
	(save-excursion
	  (if (equal (marker-buffer CUA-global-mark) src-buf)
	      (let ((text (buffer-substring-no-properties start end)))
		(goto-char (marker-position CUA-global-mark))
		(insert text))
	    (set-buffer (marker-buffer CUA-global-mark))
	    (goto-char (marker-position CUA-global-mark))
	    (insert-buffer-substring src-buf start end))
	  (CUA-global-mark-activate)
	  (message "Copied %d to global mark in %s:%d"
		   (abs (- end start))
		   (buffer-name (marker-buffer CUA-global-mark))
		   (marker-position CUA-global-mark))))
    (CUA-global-mark-deactivate)
    (message "No Global Mark")))

(defun CUA-global-mark-move-region (start end)
  "Move region to global buffer/position."
  (interactive "r")
  (if (CUA-global-mark-active)
      (let ((src-buf (current-buffer)))
	(save-excursion
	  (if (equal (marker-buffer CUA-global-mark) src-buf)
	      (if (and (< start (marker-position CUA-global-mark))
		       (< (marker-position CUA-global-mark) end))
		  (message "Can't move region into itself.")
		(let ((text (buffer-substring-no-properties start end))
		      (p1 (copy-marker start))
		      (p2 (copy-marker end)))
		  (goto-char (marker-position CUA-global-mark))
		  (insert text)
		  (CUA-global-mark-activate)
		  (delete-region (marker-position p1) (marker-position p2))
		  (move-marker p1 nil)
		  (move-marker p2 nil)))
	    (set-buffer (marker-buffer CUA-global-mark))
	    (goto-char (marker-position CUA-global-mark))
	    (insert-buffer-substring src-buf start end)
	    (message "Moved %d to global mark in %s:%d"
		     (abs (- end start))
		     (buffer-name (marker-buffer CUA-global-mark))
		     (marker-position CUA-global-mark))
	    (CUA-global-mark-activate)
	    (set-buffer src-buf)
	    (delete-region start end))))
    (CUA-global-mark-deactivate)
    (message "No Global Mark")))

(defvar CUA-global-mark-do-rect-as-text nil)

(defun CUA-global-mark-copy-rect ()
  "Copy rectangle to global mark buffer/position."
  (if (CUA-global-mark-active)
      (let ((src-buf (current-buffer))
	    (text (CUA-extract-rectangle)))
	(save-excursion
	  (set-buffer (marker-buffer CUA-global-mark))
	  (goto-char (marker-position CUA-global-mark))
	  (if CUA-global-mark-do-rect-as-text
	      (while text
		(insert (car text))
		(if (setq text (cdr text))
		    (insert "\n")))
	    (CUA-insert-rectangle text 'auto))
	  (CUA-global-mark-activate)
	  (message "Copied rectangle to global mark in %s:%d"
		   (buffer-name (marker-buffer CUA-global-mark))
		   (marker-position CUA-global-mark))))
    (CUA-global-mark-deactivate)
    (message "No Global Mark")))

(defun CUA-global-mark-move-rect ()
  "Move rectangle to global buffer/position."
  (if (CUA-global-mark-active)
      (let ((src-buf (current-buffer)))
	(save-excursion
	  (if (equal (marker-buffer CUA-global-mark) src-buf)
	      (let ((olist (overlays-at (marker-position CUA-global-mark)))
		    in-rect)
		(while olist
		  (if (eq (overlay-get (car olist) 'face) 'CUA-rectangle-face)
		      (setq in-rect t olist nil)
		    (setq olist (cdr olist))))
		(if in-rect
		    (message "Can't move rectangle into itself.")
		  (let ((text (CUA-extract-rectangle)))
		    (CUA-delete-rectangle)
		    (goto-char (marker-position CUA-global-mark))
		    (if CUA-global-mark-do-rect-as-text
			(while text
			  (insert (car text))
			  (if (setq text (cdr text))
			      (insert "\n")))
		      (CUA-insert-rectangle text 'auto))
		    (CUA-global-mark-activate))))
	    (let ((text (CUA-extract-rectangle)))
	      (CUA-delete-rectangle)
	      (set-buffer (marker-buffer CUA-global-mark))
	      (goto-char (marker-position CUA-global-mark))
	      (CUA-insert-rectangle text 'auto))
	    (message "Moved rectangle to global mark in %s:%d"
		     (buffer-name (marker-buffer CUA-global-mark))
		     (marker-position CUA-global-mark))
	    (CUA-global-mark-activate))))
    (CUA-global-mark-deactivate)
    (message "No Global Mark")))

;;; Enhanced undo - restore rectangle selections

(defvar CUA-undo-list nil
  "Per-buffer CUA mode undo list.")

(defvar CUA-undo-max 64
  "*Max no of undoable CUA rectangle changes (including undo).")

(defun CUA-rect-undo-boundary ()
  (when (listp buffer-undo-list)
    (if (> (length CUA-undo-list) CUA-undo-max)
	(setcdr (nthcdr (1- CUA-undo-max) CUA-undo-list) nil))
    (undo-boundary)
    (setq CUA-undo-list
	  (cons (cons (cdr buffer-undo-list) (copy-sequence CUA-rectangle)) CUA-undo-list))))

(defun CUA-undo (&optional arg)
  "Undo some previous changes.
Knows about CUA rectangle highlighting in addition to standard undo."
  (interactive "*P")
  (if CUA-rectangle
      (CUA-rect-undo-boundary))
  (undo arg)
  (let ((l CUA-undo-list))
    (while l
      (if (eq (car (car l)) pending-undo-list)
	  (setq CUA-next-rectangle 
		(and (vectorp (cdr (car l))) (cdr (car l)))
		l nil)
	(setq l (cdr l)))))
  (setq CUA-start-point nil))

(defvar CUA-tidy-undo-counter 0
  "Number of times `CUA-tidy-undo-lists' have run successfully.")

(defun CUA-tidy-undo-lists (&optional clean)
  (let ((buffers (buffer-list)) (cnt CUA-tidy-undo-counter))
    (while (and buffers (or clean (not (input-pending-p))))
      (with-current-buffer (car buffers)
	(when (local-variable-p 'CUA-undo-list)
	  (if (or clean (null CUA-undo-list) (eq buffer-undo-list t))
	      (progn
		(kill-local-variable 'CUA-undo-list)
		(setq CUA-tidy-undo-counter (1+ CUA-tidy-undo-counter)))
	    (let* ((bul buffer-undo-list)
		   (cul (cons nil CUA-undo-list))
		   (cc (car (car (cdr cul)))))
	      (while (and bul cc)
		(if (setq bul (memq cc bul))
		    (setq cul (cdr cul)
			  cc (and (cdr cul) (car (car (cdr cul)))))))
	      (when cc
		(if CUA-debug
		    (setq cc (length (cdr cul))))
		(if (eq (cdr cul) CUA-undo-list)
		    (setq CUA-undo-list nil)
		  (setcdr cul nil))
		(setq CUA-tidy-undo-counter (1+ CUA-tidy-undo-counter))
		(if CUA-debug
		    (message "Clean undo list in %s (%d)" 
			     (buffer-name) cc)))))))
      (setq buffers (cdr buffers)))
    (/= cnt CUA-tidy-undo-counter)))
		  
;;; Aux functions

(defun CUA-indent-active-region (start end backw)
  (message "Indenting...")
  (let (amount (arg current-prefix-arg))
    (save-excursion
      (goto-char start)
      (setq start (line-beginning-position)))
    (if (equal arg '(4))
	(indent-region start end nil)
      (setq amount (if arg (prefix-numeric-value arg) tab-width))
      (indent-rigidly start end (if backw (- amount) amount)))))

(defun CUA-set-mark (&optional jump)
  "Set mark at where point is, clear mark, or jump to mark.
With no prefix argument, set mark, push old mark position on local mark
ring, and push mark on global mark ring, or if mark is already set, clear mark.
With argument, jump to mark, and pop a new position for mark off the ring."
  (interactive "P")
  (cond
   (jump
    (set-mark-command t))
   (mark-active
    (setq mark-active nil
	  CUA-explicit-region-start nil)
    (message "Mark Cleared"))
   (t
    (set-mark-command nil)
    (setq CUA-explicit-region-start t)
    (if CUA-mode-auto-help
	(CUA-help-for-region t)))))

(defun CUA-keep-active (keep)
  (if keep
      (setq mark-active t
	    deactivate-mark nil)
    (setq mark-active nil
	  CUA-explicit-region-start nil)
    (run-hooks 'deactivate-mark-hook)))

;;
;; region functions / actions
;;

(defun CUA-cmd-copy-region ()
  (interactive)
  (setq CUA-rect-last-killed nil)
  (let ((start (mark)) (end (point)))
    (or (<= start end)
	(setq start (prog1 end (setq end start))))
    (if (CUA-register)
	(copy-to-register (CUA-register) start end nil)
      (copy-region-as-kill start end))
    (CUA-keep-active CUA-mode-keep-region-after-copy)))

(defun CUA-cmd-cut-region ()
  (interactive)
  (setq CUA-rect-last-killed nil)
  (if buffer-read-only
      (CUA-cmd-copy-region)
    (let ((start (mark)) (end (point)))
      (or (<= start end)
	  (setq start (prog1 end (setq end start))))
      (if (CUA-register)
	  (copy-to-register (CUA-register) start end t)
	(kill-region start end)))
    (CUA-keep-active nil)))

(defun CUA-cmd-delete-region ()
  (interactive)
  (let ((start (mark)) (end (point)))
    (or (<= start end)
	(setq start (prog1 end (setq end start))))
    (if CUA-mode-delete-to-register-0
	(copy-to-register ?0 start end nil))
    (delete-region start end)
    (CUA-keep-active nil)))

(defun CUA-cmd-indent-region-left ()
  (interactive)
  (let ((start (mark)) (end (point)))
    (or (<= start end)
	(setq start (prog1 end (setq end start))))
    (CUA-indent-active-region start end nil)
    (CUA-keep-active t)))

(defun CUA-cmd-indent-region-right ()
  (interactive)
  (let ((start (mark)) (end (point)))
    (or (<= start end)
	(setq start (prog1 end (setq end start))))
    (CUA-indent-active-region start end t)
    (CUA-keep-active t)))

;;
;; rectangle functions / actions
;;

(defun CUA-cmd-begin-rectangle (&optional reopen)
  "Set mark and start in CUA rectangle mode.
With prefix argument, activate previous rectangle if possible."
  (interactive "P")
  (when (not CUA-rectangle)
    (if (and reopen
	     CUA-last-rectangle
	     (eq (car CUA-last-rectangle) (current-buffer)))
	(goto-char (car (cdr CUA-last-rectangle)))
      (if (not mark-active)
	  (set-mark-command nil)))
    (CUA-rect-activate)
    (CUA-rect-set-corners)
    (setq mark-active t
	  CUA-explicit-region-start t)
    (if CUA-mode-auto-help
	(CUA-help-for-rectangle t))))

(defun CUA-cmd-end-rectangle ()
  "Cancel current rectangle."
  (interactive)
  (when CUA-rectangle
    (setq mark-active nil
	  CUA-explicit-region-start nil)
    (CUA-rect-deactivate)))

(defun CUA-cmd-restrict-regexp-rectangle (arg)
  "Restrict rectangle to lines (not) matching REGEXP.
With prefix argument, the toggle restriction."
  (interactive "P")
  (CUA-absorb-prefix-arg)
  (let ((r (CUA-rect-restriction)) regexp)
    (if (and r (null (car (cdr r))))
      (if arg
	  (CUA-rect-restriction (car r) nil (not (car (cdr (cdr r)))))
	(CUA-rect-restriction "" nil nil))
      (CUA-rect-restriction
       (read-from-minibuffer "Restrict rectangle (regexp): "
			     nil nil nil nil) nil arg))))

(defun CUA-cmd-restrict-prefix-rectangle (arg)
  "Restrict rectangle to lines (not) starting with CHAR.
With prefix argument, the toggle restriction."
  (interactive "P")
  (CUA-absorb-prefix-arg)
  (let ((r (CUA-rect-restriction)) regexp)
    (if (and r (car (cdr r)))
      (if arg
	  (CUA-rect-restriction (car r) t (not (car (cdr (cdr r)))))
	(CUA-rect-restriction "" nil nil))
      (CUA-rect-restriction
       (format "[%c]" 
	       (read-char "Restrictive rectangle (char): ")) t arg))))

(defun CUA-cmd-move-rectangle-up ()
  (interactive)
  (CUA-rect-move 'up))

(defun CUA-cmd-move-rectangle-down ()
  (interactive)
  (CUA-rect-move 'down))

(defun CUA-cmd-move-rectangle-left ()
  (interactive)
  (CUA-rect-move 'left))

(defun CUA-cmd-move-rectangle-right ()
  (interactive)
  (CUA-rect-move 'right))

(defun CUA-cmd-copy-rectangle ()
  (interactive)
  (CUA-rect-copy-as-kill)
  (CUA-keep-active CUA-mode-keep-region-after-copy))

(defun CUA-cmd-cut-rectangle ()
  (interactive)
  (if buffer-read-only
      (CUA-cmd-copy-rectangle)
    (goto-char (min (mark) (point)))
    (CUA-rect-copy-as-kill)
    (CUA-delete-rectangle))
  (CUA-keep-active nil))

(defun CUA-cmd-delete-rectangle ()
  (interactive)
  (goto-char (min (point) (mark)))
  (if CUA-mode-delete-to-register-0
      (set-register ?0 (CUA-extract-rectangle)))
  (CUA-delete-rectangle)
  (CUA-keep-active nil))

(defun CUA-cmd-toggle-rectangle ()
  (interactive)
  (if CUA-rectangle
      (CUA-rect-deactivate)
    (CUA-rect-activate))
  (if CUA-mode-auto-help
      (if CUA-rectangle
	  (CUA-help-for-rectangle t)
	(CUA-help-for-region t))))

(defun CUA-cmd-rotate-rectangle ()
  (interactive)
  (CUA-rect-corner (if (= (CUA-rect-left) (CUA-rect-right)) 0 1))
  (CUA-rect-set-corners))

(defun CUA-cmd-toggle-rectangle-padding ()
  (interactive)
  (if buffer-read-only
      (message "Cannot do padding in read-only buffer.")
    (CUA-rect-padding t (not (CUA-rect-padding)))
    (CUA-pad-rectangle)
    (CUA-rect-set-corners))
  (setq CUA-mode-status (and (CUA-rect-padding) " Pad"))
  (CUA-keep-active t))

(defun CUA-cmd-do-rectangle-padding ()
  (interactive)
  (if buffer-read-only
      (message "Cannot do padding in read-only buffer.")
    (CUA-pad-rectangle t)
    (CUA-rect-set-corners))
  (CUA-keep-active t))

(defun CUA-cmd-open-rectangle ()
  "Blank out CUA rectangle, shifting text right.
The text previously in the region is not overwritten by the blanks,
but instead winds up to the right of the rectangle."
  (interactive)
  (CUA-rect-operation 'CUA-cmd-open-rectangle nil t 1
   '(lambda (s e l r)
      (skip-chars-forward " \t")
      (let ((ws (- (current-column) l))
	    (p (point)))
	(skip-chars-backward " \t")
	(delete-region (point) p)
	(indent-to (+ r ws))))))
(put 'CUA-cmd-open-rectangle 'CUA-rect 'corners)

(defun CUA-cmd-close-rectangle (arg)
  "Delete all whitespace starting at left edge of CUA rectangle.
On each line in the rectangle, all continuous whitespace starting
at that column is deleted.
With prefix arg, also delete whitespace to the left of that column."
  (interactive "P")
  (CUA-absorb-prefix-arg)
  (CUA-rect-operation 'CUA-cmd-close-rectangle nil t 1
   '(lambda (s e l r)
      (when arg
	(skip-syntax-backward " " (line-beginning-position))
	(setq s (point)))
      (skip-syntax-forward " " (line-end-position))
      (delete-region s (point)))))
(put 'CUA-cmd-close-rectangle 'CUA-rect 'clear)

(defun CUA-cmd-blank-rectangle ()
  "Blank out CUA rectangle.
The text previously in the rectangle is overwritten by the blanks."
  (interactive)
  (CUA-rect-operation 'CUA-cmd-blank-rectangle nil nil 1
   '(lambda (s e l r)
      (goto-char e)
      (skip-syntax-forward " " (line-end-position))
      (setq e (point))
      (let ((column (current-column)))
	(goto-char s)
	(skip-syntax-backward " " (line-beginning-position))
	(delete-region (point) e)
	(indent-to column)))))
(put 'CUA-cmd-blank-rectangle 'CUA-rect 'keep)

(defun CUA-cmd-align-rectangle ()
  "Align rectangle lines to left column."
  (interactive)
  (let (x)
    (CUA-rect-operation 'CUA-cmd-align-rectangle nil t t
     '(lambda (s e l r)
	(let ((b (line-beginning-position)))
	  (skip-syntax-backward "^ " b)
	  (skip-syntax-backward " " b)
	  (setq s (point)))
        (skip-syntax-forward " " (line-end-position))
        (delete-region s (point))
        (indent-to l))
     '(lambda (l r)
	(move-to-column l)
	(setq CUA-save-point (point))))))
(put 'CUA-cmd-align-rectangle 'CUA-rect 'clear)

(defun CUA-cmd-copy-rectangle-as-text (&optional delete)
  "Copy rectangle, but store as normal text."
  (interactive)
  (if (CUA-global-mark-active)
      (let ((CUA-global-mark-do-rect-as-text t))
	(if (eq delete 'delete)
	    (CUA-global-mark-move-rect)
	  (CUA-global-mark-copy-rect)))
    (let* ((rect (CUA-extract-rectangle))
	   (text (mapconcat
		  (function (lambda (row) (concat row "\n")))
		  rect "")))
      (if (CUA-register)
	  (set-register (CUA-register) text)
	(kill-new text)))
    (if (eq delete 'delete)
	(CUA-delete-rectangle))
    (CUA-keep-active nil)))

(defun CUA-cmd-cut-rectangle-as-text ()
  "Kill rectangle, but store as normal text."
  (interactive)
  (CUA-cmd-copy-rectangle-as-text (or buffer-read-only 'delete)))

(defun CUA-cmd-string-rectangle (string)
  "Replace CUA rectangle contents with STRING on each line.
The length of STRING need not be the same as the rectangle width."
  (interactive "sString rectangle: ")
  (CUA-rect-operation 'CUA-cmd-string-rectangle nil t t
     '(lambda (s e l r)
	(delete-region s e)
	(skip-chars-forward " \t")
	(let ((ws (- (current-column) l)))
	  (delete-region s (point))
	  (insert string)
	  (indent-to (+ (current-column) ws))))
     (unless (CUA-rect-restriction)
       '(lambda (l r)
	  (CUA-rect-right (max l (+ l (length string) -1)))))))
(put 'CUA-cmd-string-rectangle 'CUA-rect 'keep)

(defun CUA-cmd-fill-char-rectangle (ch)
  "Replace CUA rectangle contents with CHARACTER."
  (interactive "cFill rectangle with character: ")
  (CUA-rect-operation 'CUA-cmd-fill-char-rectangle nil t 1
   '(lambda (s e l r)
      (delete-region s e)
      (insert-char ch (- r l)))))
(put 'CUA-cmd-fill-char-rectangle 'CUA-rect 'clear)

(defun CUA-cmd-replace-in-rectangle (regexp newtext)
  "Replace REGEXP with NEWTEXT in each line of CUA rectangle."
  (interactive "sReplace regexp: \nsNew text: ")
  (if buffer-read-only
      (message "Cannot replace in read-only buffer")
    (CUA-rect-operation 'CUA-cmd-replace-in-rectangle nil t 1
     '(lambda (s e l r)
	(if (re-search-forward regexp e t)
	    (replace-match newtext nil nil))))))
(put 'CUA-cmd-replace-in-rectangle 'CUA-rect 'keep)

(defun CUA-cmd-incr-rectangle (increment)
  "Increment each line of CUA rectangle by prefix amount."
  (interactive "p")
  (CUA-absorb-prefix-arg)
  (CUA-rect-operation 'CUA-cmd-incr-rectangle nil t 1
     '(lambda (s e l r)
	(cond
	 ((re-search-forward "0x\\([0-9a-fA-F]+\\)" e t)
	  (let* ((txt (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
		 (n (string-to-number txt 16))
		 (fmt (concat "0x%0" (length txt) "x")))
	    (replace-match (format fmt (+ n increment)))))
	 ((re-search-forward "\\( *-?[0-9]+\\)" e t)
	  (let* ((txt (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
		 (n (string-to-number txt 10))
		 (fmt (concat "%" (length txt) "d")))
	    (replace-match (format fmt (+ n increment)))))
	 (t nil))))
  (CUA-keep-active t))
(put 'CUA-cmd-incr-rectangle 'CUA-rect 'clear)

(defvar CUA-rect-seq-format "%d"
  "Last format used by CUA-cmd-sequence-rectangle.")

(defun CUA-cmd-sequence-rectangle (first incr fmt)
  "Resequence each line of CUA rectangle starting from FIRST.
The numbers are formatted according to the FORMAT string."
  (interactive 
   (list (if current-prefix-arg
	     (prefix-numeric-value current-prefix-arg)
	   (string-to-number
	    (read-string "Start value: (0) " nil nil "0")))
	 (string-to-number
	  (read-string "Increment: (1) " nil nil "1"))
	 (read-string (concat "Format: (" CUA-rect-seq-format ") "))))
  (if (= (length fmt) 0)
      (setq fmt CUA-rect-seq-format)
    (setq CUA-rect-seq-format fmt))
  (CUA-rect-operation 'CUA-cmd-sequence-rectangle nil t 1
     '(lambda (s e l r)
	 (delete-region s e)
	 (insert (format fmt first))
	 (setq first (+ first incr)))))
(put 'CUA-cmd-sequence-rectangle 'CUA-rect 'clear)

(defun CUA-cmd-upcase-rectangle ()
  "Convert the rectangle to upper case."
  (interactive)
  (CUA-rect-operation 'CUA-cmd-upcase-rectangle nil nil nil
     '(lambda (s e l r)
	(upcase-region s e))))
(put 'CUA-cmd-upcase-rectangle 'CUA-rect 'clear)

(defun CUA-cmd-downcase-rectangle ()
  "Convert the rectangle to lower case."
  (interactive)
  (CUA-rect-operation 'CUA-cmd-downcase-rectangle nil nil nil
     '(lambda (s e l r)
	(downcase-region s e))))
(put 'CUA-cmd-downcase-rectangle 'CUA-rect 'clear)

(defun CUA-rect-aux-replace (width adjust keep replace pad format-fct &optional insert-fct)
  ;; Process text inserted by calling INSERT-FCT or current rectangle if nil.
  ;; Then call FORMAT-FCT on text (if non-nil); takes two args: start and end.
  ;; Fill to WIDTH characters if > 0 or fill to current width if == 0.
  ;; Don't fill if WIDTH < 0.
  ;; Replace current rectangle by filled text if REPLACE is non-nil
  (let ((m (get-buffer-create "*CUA temp*"))
	(w (- (CUA-rect-right) (CUA-rect-left) -1))
	(r (or insert-fct (CUA-extract-rectangle)))
	y z)
    (if (> width 1)
	(setq w width))
    (save-excursion
      (set-buffer m)
      (erase-buffer)
      (if insert-fct
	  (funcall insert-fct)
	(CUA-insert-rectangle r))
      (if format-fct
	  (let ((fill-column w))
	    (funcall format-fct (point-min) (point-max))))
      (when replace
	(goto-char (point-min))
	(while (not (eobp))
	  (setq z (cons (buffer-substring (point) (line-end-position)) z)
		w (if (> (setq y (length (car z))) w) y w))
	  (forward-line 1))))
    (kill-buffer m)
    (when replace
      (setq z (reverse z))
      (CUA-rect-operation nil nil t pad
	'(lambda (s e l r)
	   (let (cc)
	     (goto-char e)
	     (skip-chars-forward " \t")
	     (setq cc (current-column))
	     (delete-region s e)
	     (if (not z)
		 (setq y 0)
	       (insert (car z))
	       (setq y (length (car z)))
	       (setq z (cdr z)))
	     ;(if(> w y)
		;(insert-char ?  (- w y)))
	     (indent-to cc))))
      (if adjust
	  (CUA-rect-right (+ (CUA-rect-left) w -1)))
      (if keep
	  (CUA-keep-active t)))))

(put 'CUA-rect-aux-replace 'lisp-indent-function 4)

(defun CUA-cmd-text-fill-rectangle (width text)
  "Replace rectagle with filled TEXT read from minibuffer.
A numeric prefix argument is used a new width for the filled rectangle."
  (interactive (list
		(prog1 (prefix-numeric-value current-prefix-arg)
		  (CUA-absorb-prefix-arg))
		(read-from-minibuffer "Enter text: "
				      nil nil nil nil)))
  (CUA-rect-aux-replace width t t t 1
    'fill-region
    '(lambda () (insert text))))

(defun CUA-cmd-self-fill-rectangle (width)
  "Fill contents of current rectagle.
A numeric prefix argument is used a new width for the filled rectangle."
  (interactive "p")
  (CUA-absorb-prefix-arg)
  (CUA-rect-aux-replace width t t t 1 'fill-region))

(defun CUA-cmd-shell-command-on-rectangle (replace command)
  "Run shell command on rectangle like `shell-command-on-region'.
With prefix arg, replace rectangle with output from command."
  (interactive (list
		(prog1 current-prefix-arg (CUA-absorb-prefix-arg))
		(read-from-minibuffer "Shell command on rectangle: "
				      nil nil nil 
				      'shell-command-history)))
  (CUA-rect-aux-replace -1 t t replace 1
    '(lambda (s e)
       (shell-command-on-region s e command
				replace replace nil))))

(defun CUA-cmd-reverse-rectangle ()
  "Reverse the lines of the rectangle."
  (interactive)
  (CUA-rect-aux-replace 0 t t t t 'reverse-region))

(defun CUA-cmd-scroll-rectangle-up ()
  "Remove the first line of the rectangle and scroll remaining lines up."
  (interactive)
  (CUA-rect-aux-replace 0 t t t t
    '(lambda (s e) 
       (if (= (forward-line 1) 0)
	   (delete-region s (point))))))

(defun CUA-cmd-scroll-rectangle-down ()
  "Insert a blank line at the first line of the rectangle.
The remaining lines are scrolled down, losing the last line."
  (interactive)
  (CUA-rect-aux-replace 0 t t t t
    '(lambda (s e)
       (goto-char s)
       (insert "\n"))))

(defun CUA-action-insert-char-rectangle ()
  (if buffer-read-only
      (ding)
    (CUA-rect-indent-rectangle 
     (aref (this-single-command-keys) 0))
    (CUA-keep-active t))
  t)

(defun CUA-cmd-delete-char-rectangle ()
  "Delete char to left or right of rectangle."
  (interactive)
  (let ((col (CUA-rect-insert-col))
	(pad (CUA-rect-padding))
	indent)
    (CUA-rect-operation 'CUA-cmd-delete-char-rectangle nil t pad
     '(lambda (s e l r)
	(move-to-column 
	 (if (CUA-rect-right-side t)
	     (max (1+ r) col) l)
	 pad)
	(if (bolp)
	    nil
	  (delete-backward-char 1)
	  (if (CUA-rect-right-side t)
	      (CUA-rect-insert-col (current-column))
	    (setq indent (- l (current-column))))))
     '(lambda (l r)
	(when (and indent (> indent 0))
	  (aset CUA-rectangle 2 (- l indent))
	  (aset CUA-rectangle 3 (- r indent 1)))))))
(put 'CUA-cmd-delete-char-rectangle 'CUA-rect 'corners)

(defun CUA-cmd-mouse-set-rectangle-corner (event)
  "Set rectangle corner at mouse click position."
  (interactive "e")
  (mouse-set-point event)
  (if (CUA-rect-padding)
      (move-to-column (car (posn-col-row (event-end event))) t))
  (if (CUA-rect-right-side)
      (CUA-rect-right (current-column))
    (CUA-rect-left (current-column)))
  (if (>= (CUA-rect-corner) 2)
      (CUA-rect-bot t)
    (CUA-rect-top t))
  (CUA-pad-rectangle)
  (CUA-rect-insert-col 0)
  (CUA-rect-set-corners)
  (CUA-keep-active t)
  (setq CUA-start-point nil))



;;
;; global mark actions
;;
(defun CUA-cmd-copy-to-global-mark ()
  (interactive)
  (setq CUA-rect-last-killed nil)
  (if CUA-rectangle
      (CUA-global-mark-copy-rect)
    (let ((start (mark)) (end (point)))
      (or (<= start end)
	  (setq start (prog1 end (setq end start))))
      (CUA-global-mark-copy-region start end))))

(defun CUA-cmd-copy1-to-global-mark (n)
  (interactive "p")
  (CUA-absorb-prefix-arg)
  (setq CUA-rect-last-killed nil)
  (or (eobp)
      (let ((p (point)))
	(goto-char (+ p n))
	(CUA-global-mark-copy-region p (point)))))

(defun CUA-cmd-cut-to-global-mark ()
  (interactive)
  (if buffer-read-only
      (CUA-cmd-copy-to-global-mark)
    (setq CUA-rect-last-killed nil)
    (if CUA-rectangle
	(CUA-global-mark-move-rect)
      (let ((start (mark)) (end (point)))
	(or (<= start end)
	    (setq start (prog1 end (setq end start))))
	(CUA-global-mark-move-region start end)))))

(defun CUA-cmd-cut1-to-global-mark (n)
  (interactive "p")
  (CUA-absorb-prefix-arg)
  (setq CUA-rect-last-killed nil
	current-prefix-arg nil
	overriding-terminal-local-map nil)
  (or (eobp)
      (let ((p (point)))
	(goto-char (+ p n))
	(CUA-global-mark-move-region p (point)))))

(defun CUA-cmd-delete-char-at-global-mark (arg)
  (interactive "p")
  (CUA-absorb-prefix-arg)
  (if (window-minibuffer-p)
      nil
    (CUA-global-mark-delete-char arg "Deleted")
    t))

(defun CUA-cmd-delete-backward-char-at-global-mark (arg)
  (interactive "p")
  (CUA-absorb-prefix-arg)
  (if (window-minibuffer-p)
      nil
    (CUA-global-mark-delete-char (- arg) "Deleted backward")
    t))

(defun CUA-action-insert-char-at-global-mark ()
  (if (window-minibuffer-p)
      nil
    (CUA-global-mark-insert (char-to-string (aref (this-single-command-keys) 0)) "Inserted")
    t))

(defun CUA-action-insert-newline-at-global-mark ()
  (if (window-minibuffer-p)
      nil
    (CUA-global-mark-insert "\n")
    t))


(defun CUA-cmd-insert-newline-at-global-mark ()
  (interactive)
  (if (not (CUA-action-insert-newline-at-global-mark))
      (call-interactively CUA-orig-command)))

;;
;; misc functions
;;

(defun CUA-cmd-ignore (arg)
  (interactive "P")
  (CUA-absorb-prefix-arg))
(defun CUA-lookup-key (map key)
  (let ((k (lookup-key map key)))
    (and k
	 (not (integerp k))
	 k)))

;;
;; command specific CUA actions
;;

(defvar CUA-region-commands
  '((CUA-action-delete-before	; delete current region before command
     self-insert-command self-insert-iso
     insert-register newline-and-indent newline open-line)
    (CUA-action-delete		; delete current region and ignore command
     delete-backward-char backward-delete-char 
     backward-delete-char-untabify delete-char)
    (CUA-action-cut	; kill region and ignore command
     kill-region)
    (CUA-action-copy	; copy region and ignore command
     copy-region-as-kill kill-ring-save)
    (CUA-action-paste	; replace region with rectangle or element on kill ring
     yank clipboard-yank)
    (CUA-action-paste-pop ; replace current yank with previous kill ring element
     yank-pop)
    (CUA-action-cancel	; cancel current region
     keyboard-escape-quit keyboard-quit))
  "Specifies how various editing functions behave in CUA mode.
The value is a list of lists. For each element in the list, the
first element is an action function and the rest of the list are names of
editing commands which shall perform the specified action if the
region is active and CUA mode is enabled.
Note: These actions are only used if an action is not specified
in the state specific CUA keymap for the current command.
The action functions are called without arguments, and if they return
a non-nil value, the original command is not executed.")

(defun CUA-action-move ()
  ;; Action handler for cursor movement keys.
  ;; If region is not active, region is activated if key is shifted.
  ;; If region is active, region is cancelled if key is unshifted (and region not started with C-SPC).
  ;; If rectangle is active, expand rectangle in specified direction and ignore the movement.
  (if (or CUA-explicit-region-start
	  CUA-rectangle
	  (memq 'shift (event-modifiers (aref (this-single-command-keys) 0))))
      (and (not mark-active) (set-mark-command nil))
    (setq mark-active nil))
  (and mark-active CUA-rectangle
       (CUA-rect-resize this-command)))

(eval-when-compile
  (defvar CUA-this-action nil))

(defun CUA-action-cancel ()
  ;; Action handler which cancels the region.
  (setq mark-active nil
	CUA-explicit-region-start nil)
  (if CUA-rectangle
      (CUA-rect-deactivate))
  (setq CUA-last-rectangle nil)
  (if (CUA-global-mark-active)
      (CUA-global-mark-deactivate t)))

(defun CUA-action-delete-before ()
  ;; Action handler which deletes the region before the command is executed.
  (if mark-active
      (if CUA-rectangle
	  (CUA-cmd-delete-rectangle)
	(CUA-cmd-delete-region)))
  nil)

(defun CUA-action-delete ()
  ;; Action handler which deletes the region and ignores the command.
  (if (not mark-active)
      nil
    (if CUA-rectangle
	(CUA-cmd-delete-rectangle)
      (CUA-cmd-delete-region))
    t))

(defun CUA-action-cut ()
  ;; Action handler which cuts the region before executing the command.
  ;; If the region is not active, ignore the command.
  (if (not mark-active)
      t
    (setq this-command 
	  (cond ((CUA-global-mark-active) 'CUA-cmd-cut-to-global-mark)
		(CUA-rectangle 'CUA-cmd-cut-rectangle)
		(t 'CUA-cmd-cut-region)))
    nil))

(defun CUA-action-copy ()
  ;; Action handler which copies the region before executing the command.
  ;; If the region is not active, ignore the command.
  (if (not mark-active)
      t
    (setq this-command 
	  (cond ((CUA-global-mark-active) 'CUA-cmd-copy-to-global-mark)
		(CUA-rectangle 'CUA-cmd-copy-rectangle)
		(t 'CUA-cmd-copy-region)))
    nil))

(defun CUA-action-paste ()
  ;; Action handler which pastes the last cut or copy.
  ;; An active region is deleted before executing the command.
  (if buffer-read-only
      nil   ; fall back to original action
    ;; Must save register here, since delete may override reg 0.
    (let ((reg (and (CUA-register) (get-register (CUA-register)))))
      (if mark-active
	  ;; Before a yank command, make sure we don't yank
	  ;; the same region that we are going to delete.
	  ;; That would make yank a no-op.
	  (if CUA-rectangle
	      (CUA-cmd-delete-rectangle)
	    (if (string= (buffer-substring (point) (mark))
			 (car kill-ring))
		(current-kill 1))
	    (CUA-cmd-delete-region)))
      (cond
       ((CUA-register)
	(cond
	 ((consp reg) (CUA-insert-rectangle reg))
	 ((stringp reg) (insert reg))
	 (t (message "Nothing in register %c" (CUA-register))))
	t)
       ((and CUA-rect-last-killed (eq (and kill-ring (car kill-ring)) (car CUA-rect-last-killed)))
	(when (not (eq buffer-undo-list t))
	  (setq CUA-this-action 'CUA-action-paste-rect) ;; dynamic binding
	  (undo-boundary)
	  (setq buffer-undo-list (cons (point) buffer-undo-list)))
	(CUA-insert-rectangle (cdr CUA-rect-last-killed))
	t)
       ((and (CUA-global-mark-active)
	     (not (eobp)))
	(CUA-global-mark-copy-region (point) (1+ (point)))
	(forward-char 1)
	t)
       (t nil)))))

(defun CUA-action-paste-pop ()
  (when (eq CUA-last-action 'CUA-action-paste-rect)
    (undo)
    (setq this-command 'yank))
  nil)

;;
;; CUA state indications
;;
(defvar CUA-ind-do-initialize t)

(defun CUA-ind-init-indications (&optional reset)
  (unless (face-background 'CUA-rectangle-face)
    (copy-face 'region 'CUA-rectangle-face)
    (set-face-background 'CUA-rectangle-face "maroon")
    (set-face-foreground 'CUA-rectangle-face "white"))
  (unless (face-background 'CUA-rectangle-noselect-face)
    (copy-face 'region 'CUA-rectangle-noselect-face)
    (set-face-background 'CUA-rectangle-noselect-face "dimgray")
    (set-face-foreground 'CUA-rectangle-noselect-face "white"))
  (unless (face-background 'CUA-global-mark-face)
    (copy-face 'region 'CUA-global-mark-face)
    (set-face-foreground 'CUA-global-mark-face "black")
    (set-face-background 'CUA-global-mark-face "cyan"))
  (setq CUA-ind-do-initialize nil))

;; change cursor color according to overwrite-mode...

(require 'timer)

(defvar CUA-ind-cursor-blink-timer nil)
(defvar CUA-ind-cursor-blink-off nil)

(defun CUA-ind-cursor-blink-toggle ()
  (setq CUA-ind-cursor-blink-off (not CUA-ind-cursor-blink-off))
  (let ((cursor 
	 (cond
	  (CUA-ind-cursor-blink-off
	   (frame-parameter nil 'foreground-color))
	  (CUA-rectangle
	   (if (CUA-rect-padding)
	       CUA-mode-overwrite-cursor-color
	     CUA-mode-normal-cursor-color))
	  (overwrite-mode CUA-mode-overwrite-cursor-color)
	  (t CUA-mode-normal-cursor-color))))
    (if cursor
	(set-cursor-color cursor))))

(defun CUA-ind-update-indications ()
  (if CUA-ind-do-initialize
      (CUA-ind-init-indications))
  (if CUA-mode-use-cursor-colors
      (let ((cursor
	     (cond
	      (buffer-read-only CUA-mode-read-only-cursor-color)
	      (CUA-rectangle
	       (if (CUA-rect-padding)
		   CUA-mode-overwrite-cursor-color
		 CUA-mode-normal-cursor-color))
	      (overwrite-mode CUA-mode-overwrite-cursor-color)
	      (t CUA-mode-normal-cursor-color))))
	(cond
	 ((and (CUA-global-mark-active)
	       (or CUA-mode-global-mark-cursor-blink
		   CUA-mode-global-mark-cursor-color))
	  (if CUA-mode-global-mark-cursor-color
	      (setq cursor CUA-mode-global-mark-cursor-color))
	  (if (and CUA-mode-global-mark-cursor-blink
		   cursor (not CUA-ind-cursor-blink-timer))
	      (setq CUA-ind-cursor-blink-timer
		    (run-at-time t 0.25 'CUA-ind-cursor-blink-toggle))))
	 (CUA-ind-cursor-blink-timer
	  (cancel-timer CUA-ind-cursor-blink-timer)
	  (setq CUA-ind-cursor-blink-timer nil)
	  (setq CUA-ind-cursor-blink-off nil)))
	(if (and cursor
		 (not (equal cursor (frame-parameter nil 'cursor-color))))
	    (set-cursor-color cursor)))))

;;
;; prefix key handling
;;

(defun CUA-absorb-prefix-arg ()
  ;; If a CUA command has an optional prefix arg, use this command to clear it.
  (setq current-prefix-arg nil
	overriding-terminal-local-map nil))

(defun CUA-kbd-macro-fixup (e1 &optional e2)
  (when defining-kbd-macro
    (cancel-kbd-macro-events)
    (store-kbd-macro-event e1)
    (if e2
	(store-kbd-macro-event e2)))
  nil)

(defun CUA-prefix-handler (prompt)
  (let ((prefix last-input-char)
	(keys (this-command-keys))
	ev map)
    (if (not
	 (and
	  CUA-mode
	  mark-active
	  (or CUA-rectangle
	      transient-mark-mode
	      (and CUA-mode-highlight-shift-only CUA-explicit-region-start))
	  (not executing-kbd-macro)
	  (not (and CUA-mode-remap-cx-shift-only CUA-explicit-region-start))
	  (or (= (length keys) 1)
	      (eq (CUA-lookup-key global-map (substring keys 0 -1)) 'digit-argument))
	  (not (memq this-command '(describe-key describe-key-briefly)))
	  (or (not (memq CUA-mode-inhibit-method '(delay twice)))
	      (= CUA-mode-inhibit-delay 0)
	      (sit-for 0 CUA-mode-inhibit-delay t)
	      (symbolp (event-basic-type (setq ev (read-event))))
	      (eq CUA-mode-inhibit-method 'delay)
	      ;; We got the next key before the timeout.
	      ;; Don't perform CUA remapping of prefix.
	      ;; Also, if prefix == ev, drop ev
	      (and (= prefix ev)
		   (setq ev (CUA-kbd-macro-fixup prefix))))
	  (setq map (assq prefix CUA-prefix-key-mappings))))
	(if ev
	    (vector prefix ev)
	  (vector prefix))
      (CUA-kbd-macro-fixup (cdr map) ev)
      (if ev
	  (setq unread-command-events (cons ev unread-command-events)))
      (vector (cdr map)))))

(defun CUA-ctl-x-8-prefix-handler ()
  "Explicit handing of C-x 8 prefix when CUA mode is used.
This is necessary since CUA mode installs its own handler for C-x
in key-translation-map."
  (interactive)
  (unless CUA-ctl-x-8-prefix-key
    (setq CUA-ctl-x-8-prefix-key
	  (cond ((not (lookup-key global-map [?\H-8])) ?\H-8)
		((not (lookup-key global-map [?\s-8])) ?\s-8)
		((not (lookup-key global-map [?\C-\H-8])) ?\C-\H-8)
		(t ?\C-\s-\H-8)))
    (unless (and (boundp 'iso-transl-ctl-x-8-map) (lookup-key iso-transl-ctl-x-8-map [?/ ?o]))
      (let ((key-translation-map nil))  ;; Avoid barfing about CUA's C-x binding
	(require 'iso-transl))
      (define-key key-translation-map (vector CUA-ctl-x-8-prefix-key) 'iso-transl-ctl-x-8-map)))
  (setq unread-command-events (cons CUA-ctl-x-8-prefix-key unread-command-events)))

(defvar CUA-overriding-region-map nil
  "Keymap that overrides other keymaps when region is active.")

(defvar CUA-overriding-rectangle-map nil
  "Keymap that overrides other keymaps when rectangle is active.")

(defvar CUA-overriding-global-mark-map nil
  "Keymap that overrides other keymaps when global mark is active.")

(defvar CUA-overriding-global-mark-commands 
  '(self-insert-command self-insert-iso indent-for-tab-command)
  "List of commands whose global mapping should override mode specific mappings.")

(defun CUA-lookup-overriding-maps (keys &optional command)
  (cond
   ((and overriding-terminal-local-map
	 (CUA-lookup-key overriding-terminal-local-map keys))
    nil)
   ((and (window-minibuffer-p)
	 (current-local-map)
	 (CUA-lookup-key (current-local-map) keys))
    nil)
   (t
    (if command
	(let ((gcmd (CUA-lookup-key global-map keys)))
	  (setq keys 
		(vector
		 (if (memq gcmd CUA-overriding-global-mark-commands)
		     gcmd
		   command)))))
    (or (and (CUA-global-mark-active) 
	     (CUA-lookup-key CUA-overriding-global-mark-map keys))
	(and CUA-rectangle
	     (CUA-lookup-key CUA-overriding-rectangle-map keys))
	(and mark-active
	     (CUA-lookup-key CUA-overriding-region-map keys))))))

(defun CUA-delete-selection (command)
  ;; Convert delete-selection property to corresponding CUA action
  (let ((ds (or (get command 'delete-selection) (get command 'pending-delete))))
    (and ds
	 (cond
	  ((eq ds 'yank) 'CUA-action-paste)
	  ((eq ds 'kill) 'CUA-action-copy)
	  ((eq ds 'supersede) 'CUA-action-delete)
	  (t 'CUA-action-delete-before)))))

(defun CUA-pre-hook ()
  "Function run prior to command to check for special region handling.
If current command is a movement and the key is shifted, set or expand
the region."
  (condition-case nil
      (let* ((keys (this-single-command-raw-keys))
	     (new-cmd (and (= (length keys) 1)
		       	   (CUA-lookup-overriding-maps keys)))
	     action CUA-this-action ignore)
	(setq CUA-save-point (point))
	(if new-cmd
	    (setq CUA-orig-command this-command
		  this-command new-cmd))
	(if (setq action 
		  (and (symbolp this-command) 
		       (or (CUA-lookup-overriding-maps keys this-command)
			   (get this-command 'CUA)
			   (CUA-delete-selection this-command))))
	    (if (and action (fboundp action))
		(setq ignore (funcall (setq CUA-this-action action)))
	      (if action
		  (message "Unknown function: %S" action)
		(setq action nil))))
	(setq CUA-last-action CUA-this-action)
	(setq CUA-start-point nil)
	(if ignore
	    (setq this-command 'CUA-cmd-ignore
		  prefix-arg nil
		  current-prefix-arg nil)
	  (if CUA-rectangle
	      (setq CUA-start-point (cons (current-buffer) (point))))))
    (error nil)))

(defun CUA-post-hook ()
  "Function run after command to check for rectangle region handling."
  (condition-case nil
      (progn
	(when (and CUA-mode-global-mark-visible (CUA-global-mark-active))
	  (sit-for 0)
	  (if (or (not (eq (current-buffer) (marker-buffer CUA-global-mark)))
		  (not (pos-visible-in-window-p (marker-position CUA-global-mark))))
	      (let ((w (selected-window)) (p (point)) h)
		;; The following code is an attempt to keep the global mark visible in 
		;; other window -- but it doesn't work.
		(switch-to-buffer-other-window (marker-buffer CUA-global-mark) t)
		(goto-char (marker-position CUA-global-mark))
		(if (not (pos-visible-in-window-p (marker-position CUA-global-mark)))
		    (recenter (if (> (setq h (- (window-height) 4)) 1) h '(4))))
		(select-window w)
		(goto-char p))))
	(if CUA-next-rectangle
	    (setq CUA-rectangle CUA-next-rectangle
		  CUA-next-rectangle nil
		  mark-active t
		  deactivate-mark nil)
	  (when (and CUA-rectangle CUA-start-point
		     (equal (car CUA-start-point) (current-buffer))
		     (not (= (cdr CUA-start-point) (point))))
	    (if (CUA-rect-right-side)
		(CUA-rect-right (current-column))
	      (CUA-rect-left (current-column)))
	    (if (>= (CUA-rect-corner) 2)
		(CUA-rect-bot t)
	      (CUA-rect-top t))
	    (if (CUA-rect-padding)
		(setq unread-command-events 
		      (cons (if CUA-mode-use-hyper-key ?\H-P ?\M-P) unread-command-events)))))
	(setq CUA-start-point nil)
	(if CUA-rectangle
	    (if (and mark-active
		     (not deactivate-mark))
		(CUA-rect-highlight)
	      (CUA-rect-deactivate)))
	(if (or (not mark-active) deactivate-mark)
	    (setq CUA-explicit-region-start nil))
	(if CUA-debug
	    (cond 
	     (CUA-rectangle (CUA-rect-assert))
	     (mark-active (message "Mark=%d Point=%d Expl=%s"
				   (mark) (point) CUA-explicit-region-start))))
	;; Disable transient-mark-mode if rectangle active in current buffer.
	(if (not (window-minibuffer-p (selected-window)))
	    (setq transient-mark-mode (and (not CUA-rectangle)
					   (if CUA-mode-highlight-shift-only
					       (not CUA-explicit-region-start)
					     t))
		  CUA-cur-register nil))
	(CUA-ind-update-indications))
      (error nil)))


(defvar CUA-movement-keys
  '((forward-char	right)
    (backward-char	left)
    (next-line		down)
    (previous-line	up)
    (forward-word	control right)
    (backward-word	control left)
    (end-of-line	end)
    (beginning-of-line	home)
    (end-of-buffer	control end)
    (beginning-of-buffer control home)
    (scroll-up		next)
    (scroll-down	prior)
    (forward-paragraph	control down)
    (backward-paragraph	control up))
  "List of cursor movement functions for which to create CUA key mappings.
Each element in the list is a list where the first element is the name of
the cursor movement function, and the rest of the list are keys to which
the function shall be bound.  For each key listed, both the key itself and
the shifted version S-key are bound to the specified function.")

(defun CUA-help-for-region (&optional help)
  (interactive)
  (message 
   (concat (if help "C-?:help " "")
	   "C-z:undo C-x:cut C-c:copy C-v:paste S-ret:rect")))

(defun CUA-help-for-rectangle (&optional help)
  (interactive)
  (message 
   (concat (if help "C-?:help " "")
	   "M-p:pad M-o:open M-c:close M-b:blank M-s:string M-f:fill M-i:incr M-n:seq")))

(defun CUA-hyper-key (map key fct &optional other)
  (if (eq key 'space) (setq key ? ))
  (unless (listp key) (setq key (list key)))
  (setq key
	(cond
	 (CUA-mode-use-hyper-key (vector (cons 'hyper key)))
	 (other other)
	 (t (vector (cons 'meta key)))))
  (define-key map key fct)
  (if (and other (eq CUA-mode-use-hyper-key 'also))
      (define-key map other fct)))

(defun CUA-mode-init-maps (emacs-bindings)
  (if (not CUA-overriding-region-map)
      (let ((m (make-sparse-keymap)))
	(unless emacs-bindings
	  (define-key m [(control insert)] 'CUA-cmd-copy-region)
	  (define-key m [(shift delete)]   'CUA-cmd-cut-region))
	(CUA-hyper-key m 'space		   'CUA-cmd-toggle-rectangle [(shift return)])
	(define-key m [(control ?i)]	   'CUA-cmd-indent-region-left)
	(define-key m [(control shift ?i)] 'CUA-cmd-indent-region-right)
	(define-key m [(control ??)]	   'CUA-help-for-region)
	(setq CUA-overriding-region-map m)))
  (if (not CUA-overriding-rectangle-map)
      (let ((m (make-keymap)))
	(unless emacs-bindings
	  (define-key m [(control insert)] 'CUA-cmd-copy-rectangle)
	  (define-key m [(shift delete)]   'CUA-cmd-cut-rectangle))
	(define-key m [(control ? )]	 'CUA-cmd-toggle-rectangle)
	(CUA-hyper-key m 'space		 'CUA-cmd-end-rectangle [(shift return)])
	(define-key m [return]		 'CUA-cmd-rotate-rectangle)
	(define-key m "\r"		 'CUA-cmd-rotate-rectangle)
	(define-key m [mouse-1]		 'CUA-cmd-mouse-set-rectangle-corner)
	(CUA-hyper-key m 'up		 'CUA-cmd-move-rectangle-up)
	(CUA-hyper-key m 'down		 'CUA-cmd-move-rectangle-down)
	(CUA-hyper-key m 'left		 'CUA-cmd-move-rectangle-left)
	(CUA-hyper-key m 'right		 'CUA-cmd-move-rectangle-right)
	(CUA-hyper-key m '(control up)	 'CUA-cmd-scroll-rectangle-up)
	(CUA-hyper-key m '(control down) 'CUA-cmd-scroll-rectangle-down)
	(CUA-hyper-key m ?a		 'CUA-cmd-align-rectangle)
	(CUA-hyper-key m ?b		 'CUA-cmd-blank-rectangle)
	(CUA-hyper-key m ?c		 'CUA-cmd-close-rectangle)
	(CUA-hyper-key m ?f		 'CUA-cmd-fill-char-rectangle)
	(CUA-hyper-key m ?F		 'CUA-cmd-self-fill-rectangle)
	(CUA-hyper-key m ?i		 'CUA-cmd-incr-rectangle)
	(CUA-hyper-key m ?k		 'CUA-cmd-cut-rectangle-as-text)
	(CUA-hyper-key m ?l		 'CUA-cmd-downcase-rectangle)
	(CUA-hyper-key m ?m		 'CUA-cmd-copy-rectangle-as-text)
	(CUA-hyper-key m ?n		 'CUA-cmd-sequence-rectangle)
	(CUA-hyper-key m ?o		 'CUA-cmd-open-rectangle)
	(CUA-hyper-key m ?p		 'CUA-cmd-toggle-rectangle-padding)
	(CUA-hyper-key m ?P		 'CUA-cmd-do-rectangle-padding)
	(CUA-hyper-key m ?r		 'CUA-cmd-replace-in-rectangle)
	(CUA-hyper-key m ?R		 'CUA-cmd-reverse-rectangle)
	(CUA-hyper-key m ?s		 'CUA-cmd-string-rectangle)
	(CUA-hyper-key m ?t		 'CUA-cmd-text-fill-rectangle)
	(CUA-hyper-key m ?u		 'CUA-cmd-upcase-rectangle)
	(CUA-hyper-key m ?|		 'CUA-cmd-shell-command-on-rectangle)
	(CUA-hyper-key m ?'		 'CUA-cmd-restrict-prefix-rectangle)
	(CUA-hyper-key m ?/		 'CUA-cmd-restrict-regexp-rectangle)
	(define-key m [(control ??)]	 'CUA-help-for-rectangle)
	(define-key m [backspace]	 'CUA-cmd-delete-char-rectangle)
	(define-key m "\d"		 'CUA-cmd-delete-char-rectangle)
	(define-key m [self-insert-command]    'CUA-action-insert-char-rectangle)
	(define-key m [self-insert-iso]	       'CUA-action-insert-char-rectangle)
	(define-key m [indent-for-tab-command] 'CUA-action-insert-char-rectangle)	
	(setq CUA-overriding-rectangle-map m)))
  (if (not CUA-overriding-global-mark-map)
      (let ((m (make-sparse-keymap)))
	(if emacs-bindings
	    (define-key m [(control ?y)]    'CUA-cmd-copy1-to-global-mark)
	  (define-key m [(control insert)]  'CUA-cmd-copy-to-global-mark)
	  (define-key m [(shift delete)]    'CUA-cmd-cut-to-global-mark)
	  (define-key m [(control ?v)]	    'CUA-cmd-copy1-to-global-mark))
	(define-key m [(control ?d)]	    'CUA-cmd-cut1-to-global-mark)
	(define-key m [backspace]	    'CUA-cmd-delete-backward-char-at-global-mark)
	(define-key m "\d"		    'CUA-cmd-delete-backward-char-at-global-mark)
	(define-key m [delete]		    'CUA-cmd-delete-char-at-global-mark)
	(define-key m [self-insert-command] 'CUA-action-insert-char-at-global-mark)
	(define-key m [self-insert-iso]	    'CUA-action-insert-char-at-global-mark)
	(define-key m [newline]		    'CUA-action-inset-newline-at-global-mark)
	(define-key m [newline-and-indent]  'CUA-action-insert-newline-at-global-mark)
	(define-key m [return]		    'CUA-cmd-insert-newline-at-global-mark)
	(define-key m "\r"		    'CUA-cmd-insert-newline-at-global-mark)
	(setq CUA-overriding-global-mark-map m)))
  (unless (get 'forward-char 'CUA)
    (let ((list CUA-region-commands) act l)
      (while list
	(setq l (car list)
	      act (car l)
	      l (cdr l)
	      list (cdr list))
	(while l
	  (put (car l) 'CUA act)
	  (setq l (cdr l)))))
    (let ((list CUA-movement-keys) cmd)
      (while list
	(setq cmd (car (car list))
	      list (cdr list))
	(put cmd 'CUA 'CUA-action-move)))))

(defun CUA-define-key (map key cmd)
  "Like define-key with specific short-cuts for CUA maps.
In MAP, define KEY to run command CMD.
Special values for MAP are 'region, 'rect, 'gm, and 'all to bind into
the CUA maps for the active region, active rectangle, and active
global marker resp., or all of them."
  (if (symbolp key)
      (setq key (vector key)))
  (if (memq map '(region all))
      (define-key CUA-overriding-region-map key cmd))
  (if (memq map '(rect all))
      (define-key CUA-overriding-rectangle-map key cmd))
  (if (memq map '(gm all))
      (define-key CUA-overriding-global-mark-map key cmd)))

(defun CUA-define-key-as (map key key2)
  "Define a key to run the same CUA command as another key."
  (if (symbolp key)
      (setq key (vector key)))
  (if (symbolp key2)
      (setq key2 (vector key2)))
  (if (memq map '(region all))
      (let ((cmd (CUA-lookup-key CUA-overriding-region-map key2)))
	(if cmd
	    (define-key CUA-overriding-region-map key cmd))))
  (if (memq map '(rect all))
      (let ((cmd (CUA-lookup-key CUA-overriding-rectangle-map key2)))
	(if cmd
	    (define-key CUA-overriding-rectangle-map key cmd))))
  (if (memq map '(gm all))
      (let ((cmd (CUA-lookup-key CUA-overriding-global-mark-map key2)))
	(if cmd
	    (define-key CUA-overriding-global-mark-map key cmd)))))

;;;###autoload
(defun CUA-movement-key (key command)
  "Like `global-set-key' but also binds shifted KEY to COMMAND.
KEY should be a simple symbol or character, like home or ?\\C-e,
or a list like (control home)."
  (if (vectorp key)
      (setq key (aref key 0)))
  (if (not (listp key))
      (setq key (list key)))
  (global-set-key (vector key) command)
  (global-set-key (vector (cons 'shift key)) command)
  (put command 'CUA 'CUA-action-move))

;;;###autoload
(defun CUA-mode-bindings (&optional bind)
  "Define even more compatibility bindings.
Optional argument BIND identifies what bindings to add."
  (cond
   ((eq bind 'sun3)
    ;; The following bindings are useful on Sun Type 3 keyboards
    ;; They implement the Get-Delete-Put (copy-cut-paste)
    ;; functions from sunview on the L6, L8 and L10 keys
    (define-key global-map [f16]  'yank)
    (define-key global-map [f18]  'copy-region-as-kill)
    (define-key global-map [f20]  'kill-region))
   ((eq bind 'pc-select)
    ;; The following bindings are made by pc-select
    ;; I [KFS] personally don't like them, so I made them extra.
    (global-set-key [f1] 'help)			; KHelp         F1
    ;; The following bindings are from Pete Forman.
    (global-set-key [f6] 'other-window)		; KNextPane     F6
    (global-set-key [delete] 'delete-char)	; KDelete       Del
    (global-set-key [(meta backspace)] 'undo)	; KUndo         aBS
    ;; The following bindings are taken from pc-mode.el as suggested by RMS.
    (define-key function-key-map  [(meta delete)] [(meta ?d)])
    (global-set-key [(control meta delete)]  'kill-sexp)
    (global-set-key [(control backspace)] 'backward-kill-word)
    ;; Next line proposed by Eli Barzilay
    (global-set-key [(control escape)]    'electric-buffer-list))
   ((eq bind 'windows-nt)
    ;; From: Kari Heinola <kph@dp.com>
    (define-key global-map [(control ?a)]  'mark-whole-buffer)
    (define-key global-map [(control ?p)]  'print-buffer)
    (define-key global-map [(control ?s)]  'save-buffer)
    (define-key global-map [(control ?n)]  'find-file)
    (if (fboundp 'dlgopen-open-files)
	(define-key global-map [(control ?o)]  'dlgopen-open-files)
      (define-key global-map [(control ?o)]  'find-file))
    (define-key global-map [(control ?f)]  'isearch-forward)
    (define-key isearch-mode-map [(control ?f)] 'isearch-repeat-forward)
    (define-key global-map [(control ?h)]  'query-replace)
    (define-key global-map [f5]            'insert-time-stamp))
   ((eq bind 'emacs) ; CUA functionality for normal emacs bindings
    (substitute-key-definition 'undo 'CUA-undo global-map)
    (substitute-key-definition 'advertised-undo 'CUA-undo global-map)
    (define-key global-map [(control delete)] 'kill-word)
    (define-key global-map [(control backspace)] 'backward-kill-word)
    (define-key global-map [delete]	      'delete-char)
    (define-key global-map [(control ? )]     'CUA-set-mark)
    (define-key global-map [(shift control ? )] 'CUA-cmd-toggle-global-mark)
    (CUA-hyper-key global-map 'space	      'CUA-cmd-begin-rectangle [(shift return)]))
   ((eq bind 'CUA) ; default CUA mappings
    ;; Compatibility mappings
    ;; Note: These are required since CUA-prefix-key-mappings maps
    ;; C-x into S-delete and C-c into C-insert.
    (define-key global-map [(control insert)] 'copy-region-as-kill)
    (define-key global-map [(shift delete)]   'kill-region)
    (define-key global-map [(shift insert)]   'yank)
    (define-key global-map [(meta insert)]    'yank-pop)
    (define-key global-map [(control delete)] 'kill-word)
    (define-key global-map [(control backspace)] 'backward-kill-word)
    (define-key global-map [delete]	      'delete-char)
    (define-key global-map [(control ? )]     'CUA-set-mark)
    (define-key global-map [(shift control ? )] 'CUA-cmd-toggle-global-mark)
    (CUA-hyper-key global-map 'space	      'CUA-cmd-begin-rectangle [(shift return)]))
   ((eq bind 'zxcv)
    (define-key global-map [(control ?z)]     'CUA-undo)
    (define-key ctl-x-map  [(control ?x)]     'CUA-exchange-point-and-mark)
    (define-key global-map [(control ?v)]     'yank)
    (or key-translation-map
	(setq key-translation-map (make-sparse-keymap)))
    (let ((map CUA-prefix-key-mappings))
      (while map
	(define-key key-translation-map
	  (vector (car (car map))) 'CUA-prefix-handler)
	(if (stringp (cdr (car map)))
	    (setcdr (car map) (read-kbd-macro (cdr (car map)))))
	(setq map (cdr map))))
    (define-key ctl-x-map  [?8]     'CUA-ctl-x-8-prefix-handler))
   ((eq bind 'shift)
    (let ((list CUA-movement-keys) cmd elt key)
      (while list
	(setq elt (car list)
	      cmd (car elt)
	      key (cdr elt)
	      list (cdr list))
	(define-key global-map (vector key) cmd)
	(define-key global-map (vector (cons 'shift key)) cmd))))))

;;;###autoload
(defun CUA-mode (&optional arg extra nobind)
  "Toggle CUA keybinding mode.
When ON, C-x and C-c will cut and copy the selection if the selection
is active (i.e. the region is highlighted), and typed text replaces
the active selection. When OFF, typed text is just inserted at point.
If non-nil, the optional second argument EXTRA specifies additional
key bindings as defined by CUA-mode-bindings.
The following key bindings are made unless optional third argument 
NOBIND is non-nil:
 C-z  is undo
 C-v  is yank
 C-x C-x is CUA-exchange-point-and-mark which doesn't enable the mark
 C-space   starts/cancels the normal region 
 S-C-space sets/cancels the global marker
 S-return  starts a rectangular region, if repeated toggles between
          rectangle and normal region."
  (interactive "P")
  (setq CUA-mode
	(cond
	 ((null arg) (not CUA-mode))
	 ((symbolp arg) t)
	 (t (> (prefix-numeric-value arg) 0))))
  (let ((emacs-bindings (or CUA-mode-emacs-bindings (equal arg 'emacs))))
    (CUA-mode-init-maps emacs-bindings)
    (if emacs-bindings
	(CUA-mode-bindings 'emacs)
      (CUA-mode-bindings 'CUA)
      (CUA-mode-bindings 'zxcv)
      (CUA-mode-bindings 'shift))
    (if extra
	(CUA-mode-bindings extra)))
  (setq mark-even-if-inactive t)
  (setq highlight-nonselected-windows nil)
  (make-variable-buffer-local 'CUA-explicit-region-start)
  (make-variable-buffer-local 'CUA-rectangle)
  (make-variable-buffer-local 'CUA-rect-overlays)
  (make-variable-buffer-local 'CUA-mode-status)
  (make-variable-buffer-local 'CUA-undo-list)
  (cancel-function-timers 'CUA-tidy-undo-lists)
  (if CUA-mode
      (progn
	(add-hook 'pre-command-hook 'CUA-pre-hook)
	(add-hook 'post-command-hook 'CUA-post-hook)
	(run-with-idle-timer 10 t 'CUA-tidy-undo-lists)
	(if (and CUA-mode-use-modeline (not (assoc 'CUA-mode minor-mode-alist)))
	    (setq minor-mode-alist (cons '(CUA-mode CUA-mode-status) minor-mode-alist))))
    (remove-hook 'pre-command-hook 'CUA-pre-hook)
    (remove-hook 'post-command-hook 'CUA-post-hook)
    (CUA-tidy-undo-lists t))
  (setq transient-mark-mode (and CUA-mode
				 (if CUA-mode-highlight-shift-only
				     (not CUA-explicit-region-start)
				   t))))

;;;###autoload
(defun CUA-mode-on ()
  (interactive)
  (CUA-mode t))

(defun CUA-debug ()
  (interactive)
  (setq CUA-debug (not CUA-debug)))

;;; Register commands prefix remapping [C-x r ...]

(defun CUA-remap-ctl-x-commands (ctl-x-key prefix &optional no-orig)
  "Remap ctl-x commands [C-x r ...] onto [PREFIX ...].
Unless the optional third arguments NO-ORIG is non-nil, the original
binding of [PREFIX] is remapped to [PREFIX PREFIX]."
  (let ((org-prefix-cmd	(CUA-lookup-key global-map prefix))
	(new-prefix-cmd (CUA-lookup-key ctl-x-map ctl-x-key)))
    (if new-prefix-cmd
	(global-set-key prefix new-prefix-cmd))
    (if (and (not no-orig) 
	     new-prefix-cmd org-prefix-cmd
	     (not (eq new-prefix-cmd org-prefix-cmd)))
	(global-set-key (concat prefix prefix) org-prefix-cmd))))

;;; KEYPAD REMAPPING

(defun CUA-keypad-bind (kp bind)
  "Bind the keys in KP list to BIND list in function-key-map.
If BIND is 'unbind, all bindings for the keys are removed."
  (if (not (boundp 'function-key-map))
      (setq function-key-map (make-sparse-keymap)))
  (if (eq bind 'unbind)
      (while kp
	(define-key function-key-map (vector (car kp)) nil)
	(setq kp (cdr kp)))
    (while (and kp bind)
      (define-key function-key-map (vector (car kp)) (vector (car bind)))
      (setq kp (cdr kp)
	    bind (cdr bind)))))

  
;;;###autoload
(defun CUA-keypad-mode (mode &optional numlock decimal)
  "Set keypad bindings in function-key-map according to MODE.
If optional second argument NUMLOCK is non-nil, the NumLock On bindings
are changed. Otherwise, the NumLock Off binding are changed.

 Mode      Binding
 -------------------------------------------------------------
 'prefix   Command prefix argument, i.e.  M-0 .. M-9 and M--
 'S-cursor Bind shifted keypad keys to the shifted cursor movement keys.
 'cursor   Bind keypad keys to the cursor movement keys.
 'numeric  Plain numeric, i.e. 0 .. 9 and .  (or DECIMAL arg)
 'none     Removes all bindings for keypad keys in function-key-map.

If mode is 'numeric and the optional third argument DECIMAL is non-nil,
the decimal key on the keypad i<s mapped to DECIMAL instead of [.]."
  (let ((kp (if numlock
		'(kp-decimal kp-0 kp-1 kp-2 kp-3 kp-4 kp-5 kp-6 kp-7 kp-8 kp-9)
	      '(kp-delete kp-insert kp-end kp-down kp-next kp-left
			  kp-space kp-right kp-home kp-up kp-prior))))
    (CUA-keypad-bind
     kp 
     (cond
      ((eq mode 'none)
       'unbind)
      ((eq mode 'prefix)
       '(?\M-- ?\M-0 ?\M-1 ?\M-2 ?\M-3 ?\M-4 ?\M-5 ?\M-6 ?\M-7 ?\M-8 ?\M-9))
      ((eq mode 'cursor)
       '(delete insert end down next left space right home up prior))
      ((eq mode 'S-cursor)
       '(S-delete S-insert S-end S-down S-next S-left S-space S-right S-home S-up S-prior))
      ((eq mode 'numeric)
       (cons (or decimal ?.) '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
      (t
       (signal 'error (list "Unknown keypad mode: " mode)))))))


(if CUA-mode
    (CUA-mode 1))

(provide 'CUA-mode)
(provide 'cua-mode)
(provide 'cua)

;;; cua.el ends here
