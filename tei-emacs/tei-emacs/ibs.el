;;; ibs.el --- windows like buffer selection mode by C-TAB.

;; Copyright (C) 2000 Olaf Sylvester

;; Author: Olaf Sylvester <olaf@geekware.de>
;; Maintainer: Olaf Sylvester <olaf@geekware.de>
;; Keywords: convenience

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:
;; This Emacs package provides a minor mode for buffer cycling;
;; more exact: to switch by key C-TAB between Emacs buffers like
;; MS-Windows IDEs.
;; C-TAB starts buffer cycling.  Every other key terminates cycling
;; and sets the current buffer at the top of Emacs buffer list.
;; The buffer we started buffer cycling won't be buried !!!

;; You can configure the keys for cycling.
;; Therefore set global `ibs-cycling-key' before loading ibs.el.

;; You can define which buffers will be used for buffer cycling.
;; Set global `ibs-cycle-buffer-function' to a function which
;; returns a buffer list. The default is buffer-list, which returns
;; all buffers in recently order.
;; If package bs is loaded the cycling list of this package
;; will be used.

;;; History:
;; 22.12.2000
;; Version 0.11
;; - problems with generic-character-list (XEMACS 21.x)
;; - no more occurrence of eval-when
;; 
;; 17.12.2000
;; First Version 0.1
;;; Code:

;;; Global variables for customization.
(defvar ibs-cycling-key "<C-tab>"
  "Key sequence for buffer cycling.")

(defvar ibs-cycle-buffer-function nil
  "Function to calculate buffers for cycling.
When nil use `buffer-list'.
The function needs no arguments and must return a list of buffers.")

(defvar ibs-timeout 4
  "Seconds of inactivity for deactivating cycling mode.")

(defvar ibs-mode-hook nil
  "Function(s) to call after invoking mode ibs.")

(defvar ibs-mode-end-hook nil
  "Function(s) to call after terminating mode ibs.")

(defvar ibs-buffer-list nil
  "Current buffer list for cycling.")

(defvar ibs-start-buffer nil
  "Buffer we started cycling.")

;;; Define ibs-mode keymap.

(defvar ibs-mode-map nil
  "Keymap for function `ibs-mode'.
Derived from `isearch-mode-map'.")
;;(setq ibs-mode-map nil)
(or ibs-mode-map
    (let* ((i 0)
	   (map (make-keymap)))
      ;; Make characters stop cycling.
      (if (fboundp 'generic-character-list)
	  (let ((l (generic-character-list))
		(table (nth 1 map)))
	    (while l
	      (set-char-table-default table
				      (car l)
				      'ibs-select-buffer-and-quit)
	      (setq l (cdr l)))))
      ;; Make function keys, etc, stop cycling.
      (define-key map [t] 'ibs-select-buffer-and-quit)
      ;; Control chars, by default, end ibs mode transparently.
      ;; We need these explicit definitions because, in a dense
      ;; keymap, the binding for t does not affect characters.
      ;; We use a dense keymap to save space.
      (while (< i ?\ )
	(define-key map
	            (make-string 1 i)
		    'ibs-select-buffer-and-quit)
	(setq i (1+ i)))

      ;; Single-byte printing chars stop cycling.
      (setq i ?\ )
      (while (< i 256)
	(define-key map (vector i) 'ibs-select-buffer-and-quit)
	(setq i (1+ i)))

      ;; To handle local bindings with meta char prefix keys, define
      ;; another full keymap.  This must be done for any other prefix
      ;; keys as well, one full keymap per char of the prefix key.
      ;; It would be simpler to disable the global keymap, and/or
      ;; have adefault local key binding for any key not otherwise
      ;; bound.
      (let ((meta-map (make-sparse-keymap)))
	(define-key map (char-to-string meta-prefix-char) meta-map)
	(define-key map [escape] meta-map))
      (define-key map (vector meta-prefix-char t)
	              'ibs-other-meta-char)

      ;; Several non-printing chars change the searching behavior.
      (define-key map "\C-g" 'ibs-abort)
      ;; This assumes \e is the meta-prefix-char.
      (or (= ?\e meta-prefix-char)
	  (error "Inconsistency in ibs.el"))
      (define-key map "\e\e\e" 'ibs-cancel)
      (define-key map  [escape escape escape] 'ibs-cancel)
      
      (define-key map
	          (read-kbd-macro ibs-cycling-key)
		  'ibs-next-buffer)
    
      ;; Pass frame events transparently so they won't exit
      ;; the search. In particular, if we have more than one
      ;; display open, then a switch-frame might be generated
      ;; by someone typing at another keyboard.
      (define-key map [switch-frame]       nil)
      (define-key map [delete-frame]       nil)
      (define-key map [iconify-frame]      nil)
      (define-key map [make-frame-visible] nil)
      
      (setq ibs-mode-map map)
      ))


;; The value of input-method-function when ibs is invoked.
(defvar ibs-input-method-function nil)

;; A flag to tell if input-method-function is locally bound when
;; ibs is invoked.
(defvar ibs-input-method-local-p nil)

;; Register minor mode
(or (assq 'ibs-mode minor-mode-alist)
    (nconc minor-mode-alist
	   (list '(ibs-mode ibs-mode))))

(defvar ibs-mode nil)

(define-key global-map (read-kbd-macro ibs-cycling-key) 'ibs-select)

(defun ibs-cancel ()
  "Terminate cycling and signal quit."
  (interactive)
  (ibs-done)
  (signal 'quit nil))

(defun ibs-abort ()
  "Terminate cycling and reselect starting buffer."
  (interactive)
  (ibs-done)
  (switch-to-buffer ibs-start-buffer t))

(defun ibs-select ()
  "Do buffer selection."
  (interactive)
  (setq ibs-start-buffer (current-buffer))
  (setq ibs-buffer-list (mapcar
			 'identity
			 (funcall (or ibs-cycle-buffer-function
				      (function buffer-list)))))
  (if (not (memq (current-buffer) ibs-buffer-list))
      (setq ibs-buffer-list (cons (current-buffer) ibs-buffer-list)))
  (setq ibs-buffer-list (ibs-step-right ibs-buffer-list))
  (ibs-mode)
  (ibs-next-buffer)
  )


(defun ibs-cancel-after-timeout ()
  "Wait `ibs-timeout' seconds for terminating cycling."
  (when (sit-for ibs-timeout)
    (ibs-done t)
    (message "")))

(defun ibs-mode ()
  "Start ibs minor mode.  
Called by `ibs-select', etc.
\\{ibs-mode-map}"
  ;; Initialize global vars.
  (setq ibs-input-method-function
	input-method-function)
  (setq ibs-input-method-local-p
	(local-variable-p 'input-method-function))

  ;; We must bypass input method while reading key.
  ;; When a user type printable character, appropriate input
  ;; method is turned on in minibuffer to read multibyte
  ;; charactes.
  (or ibs-input-method-local-p
      (make-local-variable 'input-method-function))
  (setq input-method-function nil)
  (setq	ibs-mode " I-BS")
  (force-mode-line-update)
  (setq overriding-terminal-local-map ibs-mode-map)
  (run-hooks 'ibs-mode-hook)
  (add-hook 'mouse-leave-buffer-hook 'ibs-done)
  t)

(defun ibs-done (&optional select-buffer-p)
  "Terminate ibs normally."
  (remove-hook 'mouse-leave-buffer-hook 'ibs-done)
  (setq overriding-terminal-local-map nil)
  (setq ibs-mode nil)
  (if ibs-input-method-local-p
      (setq input-method-function ibs-input-method-function)
    (kill-local-variable 'input-method-function))
  (if select-buffer-p
      (switch-to-buffer (car (last ibs-buffer-list))))
  (force-mode-line-update)
  (run-hooks 'ibs-mode-end-hook)
  t)

(defun ibs-select-buffer-and-quit ()
  "Exit the cycling normally and reread this key sequence."
  (interactive)
  (let* ((key (this-command-keys))
	 (main-event (aref key 0))
	 (keylist (listify-key-sequence key)))
    (cond ((and (= (length key) 1)
		(let ((lookup (lookup-key function-key-map key)))
		  (not (or (null lookup) (integerp lookup)
			   (keymapp lookup)))))
	   ;; Handle a function key that translates into something
	   ;; else. If the key has a global definition too,
	   ;; exit and unread the key itself, so its global
	   ;; definition runs. Otherwise, unread the translation,
	   ;; so that the translated key takes effect within ibs.
	   (cancel-kbd-macro-events)
	   (if (lookup-key global-map key)
	       (progn
		 (ibs-done t)
		 (apply 'ibs-unread keylist))
	     (apply 'ibs-unread
		    (listify-key-sequence
		     (lookup-key function-key-map
				 key)))))
	  (
	   ;; Handle an undefined shifted control character
	   ;; by downshifting it if that makes it defined.
	   ;; (As read-key-sequence would normally do,
	   ;; if we didn't have a default definition.)
	   (let ((mods (event-modifiers main-event)))
	     (and (integerp main-event)
		  (memq 'shift mods)
		  (memq 'control mods)
		  (lookup-key ibs-mode-map
			      (let ((copy (copy-sequence key)))
				(aset copy 0
				      (- main-event (- ?\C-\S-a
						       ?\C-a)))
				copy)
			      nil)))
	   (setcar keylist (- main-event (- ?\C-\S-a ?\C-a)))
	   (cancel-kbd-macro-events)
	   (apply 'ibs-unread keylist))

	  (t
	   (let (window)
	     (cancel-kbd-macro-events)
	     (apply 'ibs-unread keylist)
	     ;; Properly handle scroll-bar and mode-line clicks
	     ;; for which a dummy prefix event was generated as (aref key 0).
	     (and (> (length key) 1)
		  (symbolp (aref key 0))
		  (listp (aref key 1))
		  (not (numberp (posn-point (event-start (aref key 1)))))
		  ;; Convert the event back into its raw form,
		  ;; with the dummy prefix implicit in the mouse event,
		  ;; so it will get split up once again.
		  (progn (setq unread-command-events
			       (cdr unread-command-events))
			 (setq main-event (car unread-command-events))
			 (setcar (cdr (event-start main-event))
				 (car (nth 1 (event-start main-event))))))
	     ;; If we got a mouse click, maybe it was read with the buffer
	     ;; it was clicked on.  If so, that buffer, not the current one,
	     ;; is in ibs mode.  So end the search in that buffer.
	     (if (and (listp main-event)
		      (setq window (posn-window (event-start main-event)))
		      (windowp window)
		      (or (> (minibuffer-depth) 0)
			  (not (window-minibuffer-p window))))
		 (save-excursion
		   (set-buffer (window-buffer window))
		   (ibs-done t)
		   )
	       (ibs-done t)
	       )))	  
	  )))

(defun ibs-unread (&rest char-or-events)
  "Unread all input events in CHAR-OR-EVENTS."
  (mapcar 'store-kbd-macro-event char-or-events)
  (setq unread-command-events
	(append char-or-events unread-command-events)))

(defun ibs-next-buffer ()
  "Switch to next buffer."
  (interactive)
  (let ((buff (car ibs-buffer-list)))
    (switch-to-buffer buff t)
    (ibs-mode)
    (setq ibs-buffer-list (ibs-step-right ibs-buffer-list))
    (message "%S" (mapcar (function buffer-name)
			  ibs-buffer-list))
    (ibs-cancel-after-timeout)
    ))

(defun ibs-step-right (alist)
  "Return ALIST rotated right."
  (append (cdr alist)
	  (list (car alist))))

(if (featurep 'bs)
    (progn
      (defun bs-cycling-list ()
	"Return buffer list for buffer cycling.
The buffers taking part in buffer cycling are defined
by buffer configuration `bs-cycle-configuration-name'."
	(interactive)
	(let ((bs--buffer-coming-from (current-buffer))
	      (bs-dont-show-regexp   bs-dont-show-regexp)
	      (bs-must-show-regexp   bs-must-show-regexp)
	      (bs-dont-show-function bs-dont-show-function)
	      (bs-must-show-function bs-must-show-function)
	      (bs--show-all          bs--show-all))
	  (if bs-cycle-configuration-name
	      (bs-set-configuration bs-cycle-configuration-name))
	  (let ((bs-buffer-sort-function nil)
		(bs--current-sort-function nil))
	    (let* ((tupel (bs-next-buffer)))
	      (cdr tupel)))))
      
      (setq ibs-cycle-buffer-function
	    (or ibs-cycle-buffer-function 'bs-cycling-list))))
(provide 'ibs)
;;; ibs.el ends here
