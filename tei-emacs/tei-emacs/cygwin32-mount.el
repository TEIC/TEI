;;; cygwin32-mount.el --- Teach EMACS about cygwin32 mount points.
;;; Michael Cook <mcook@cognex.com>.
;;; modified Jun 18 1998 by Keisuke Mori <ksk@ntts.com> 
;;;  to make it work with ange-ftp and enable mapping a drive letter
;;; modified Oct 25 1999 by Drew Moseley (drewmoseley@mindspring.com)
;;;  to support /cygdrive style drive maps and for better coexistence
;;;  with ange-ftp.
;;; modified Feb  7 2000 by James Ganong (jeg@bigseal.ucsc.edu)
;;;  to work when / is mounted in a subdirectory instead of top level of a
;;;  drive, and to check longest directory names first, to a / mount does
;;;  not shadow.
;;; modified Mar 23 2000 by Jeff Juliano <juliano@cs.unc.edu>
;;;  to make a user-callable function that performs the mapping.  I use
;;;  this in my .emacs file to find my non-version-specific site-lisp
;;;  directory since NTemacs doesn't have one in its search path.
;;; modified July 2000 by Jeff Juliano <juliano@cs.unc.edu>
;;;  ps-print to //machine/printer-name was broken.  cygwin-mount would
;;;  intercept and try to convert the name to a local path, even though one
;;;  doesn't exist.  Also, don't do mount table lookup as early as used to.
;;;  warning: this isn't extensively tested, and may not be correct.


(defun cygwin32-mount-build-table ()
  ;; Determine the cygwin mount points.

  (let ((buf (get-buffer-create " *mount*"))
	(case-fold-search t)
        mounts)
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (call-process "mount" nil t)

      
      ;; first pass tags each line with the length of the directory
      (goto-char (point-min))
      (while (search-forward-regexp
              "^\\([a-z]:[^ \t\n]*\\) +\\([^ \t\n]+\\)" nil t)
        (let ((device (buffer-substring (match-beginning 1)
                                        (match-end 1)))
              (direct (file-name-as-directory (buffer-substring (match-beginning 2)
                                                                (match-end 2)))))
	  (end-of-line) (insert ( format "\t%d" (length direct )))

	  ))

      ;; now sort the lines numerically
      (sort-numeric-fields -1 (point-min) (point-max))

      ;; 2nd pass builds the mount list

      (goto-char (point-min))
      (while (search-forward-regexp
              "^\\([a-z]:[^ \t\n]*\\) +\\([^ \t\n]+\\)" nil t)
        (let ((device (buffer-substring (match-beginning 1)
                                        (match-end 1)))
              (direct (file-name-as-directory(buffer-substring (match-beginning 2)
                                                               (match-end 2)))))

          (setq mounts (cons (cons device direct)
                             mounts))))

      )

    (kill-buffer buf)
    mounts))

(defvar cygwin32-mount-table (cygwin32-mount-build-table)
  "Alist of cygwin32 mount points.")

(or (assoc "^/[^:@]*$\\|[^/:]+\\(\\'\\|/\\)" file-name-handler-alist)
    (setq file-name-handler-alist
          (cons '("^/[^:@]*$\\|^/|/[^/:]+\\(\\'\\|/\\)" . cygwin32-mount-name-hook-function)
                file-name-handler-alist)))

(or (assoc "^//[A-Za-z]/" file-name-handler-alist)
    (setq file-name-handler-alist
          (cons '("^//[A-Za-z]/" . cygwin32-mount-map-drive-hook-function)
                file-name-handler-alist)))

;;; Support cygdrive style drive maps.
(or (assoc "^/cygdrive/[A-Za-z]" file-name-handler-alist)
    (setq file-name-handler-alist
          (cons '("^/cygdrive/[A-Za-z]" . cygwin32-mount-map-drive-hook-function)
                file-name-handler-alist)))

(defun cygwin32-mount-name-hook-function (operation &rest args)
  (let ((fn (get operation 'cygwin32-mount-name)))
    (if fn (apply fn operation args)
      (cygwin32-mount-run-real-handler operation args))))

(defun cygwin32-mount-map-drive-hook-function (operation &rest args)
  (let ((fn (get operation 'cygwin32-mount-map-drive)))
    (if fn (apply fn operation args)
      (cygwin32-mount-run-real-handler operation args))))

(defun cygwin32-mount-run-real-handler (operation args)
  (let ((inhibit-file-name-handlers
         (cons 'cygwin32-mount-name-hook-function
               (cons 'cygwin32-mount-map-drive-hook-function
                     (and (eq inhibit-file-name-operation operation)
                          inhibit-file-name-handlers))))
        (inhibit-file-name-operation operation))
    (apply operation args)))

;;;
;;; Unbound the ange-ftp-run-real-handler and rebind it to ours
;;; This version also inhibits the cygwin file name expansion when
;;; we are doing ange-ftp expansion.
;;;
;;; This is a real hack.  If the real definition of this function
;;; changes, we have to modify this function
;;;
(require 'ange-ftp)
(if (fboundp 'ange-ftp-run-real-handler)
    (fmakunbound 'ange-ftp-run-real-handler))

(defun ange-ftp-run-real-handler (operation args)
  (let ((inhibit-file-name-handlers
         (cons 'ange-ftp-hook-function
               (cons 'ange-ftp-completion-hook-function
                     (cons 'cygwin32-mount-name-hook-function
                           (cons 'cygwin32-mount-map-drive-hook-function
                                 (and (eq inhibit-file-name-operation operation)
                                      inhibit-file-name-handlers))))))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(put 'substitute-in-file-name 'cygwin32-mount-name 'cygwin32-mount-name-expand)
(put 'expand-file-name 'cygwin32-mount-name 'cygwin32-mount-name-expand)

(put 'substitute-in-file-name
     'cygwin32-mount-map-drive 'cygwin32-mount-map-drive)
(put 'expand-file-name 'cygwin32-mount-map-drive 'cygwin32-mount-map-drive)

(require 'cl)

(defun cygwin32-mount-name-expand (operation name &rest args)
  ;; If NAME uses a mount directory, substitute the mount device
  ;; and return the resulting string.  Otherwise, return NAME.
  (let ((mounts cygwin32-mount-table)
        (len (length (file-name-as-directory name)))
        match)
    (while mounts
      (let ((mount (file-name-as-directory (cdar mounts))))
        (and (>= len (length mount))
             (string= mount (file-name-as-directory(substring (file-name-as-directory name) 0 (length mount))))
             (or (null match)
                 (> (length (cdar mounts)) (length (cdr match))))
             (setq match (car mounts))))
      (setq mounts (cdr mounts)))
    (if match
        (concat (file-name-as-directory (car match))
                (substring name (length (file-name-as-directory (cdr match)))))
      (cygwin32-mount-run-real-handler operation (cons name args)))))

(defun cygwin32-convert-to-long-name (name)
  ;; If NAME uses a mount directory, substitute the mount device
  ;; and return the resulting string.  Otherwise, return NAME.
  (let ((mounts cygwin32-mount-table)
        (len (length (file-name-as-directory name)))
        match)
    (while mounts
      (let ((mount (file-name-as-directory (cdar mounts))))
        (and (>= len (length mount))
             (string= mount (file-name-as-directory(substring (file-name-as-directory name) 0 (length mount))))
             (or (null match)
                 (> (length (cdar mounts)) (length (cdr match))))
             (setq match (car mounts))))
      (setq mounts (cdr mounts)))
    (if match
        (concat (file-name-as-directory (car match))
                (substring name (length (file-name-as-directory (cdr match)))))
      name)))

(defun cygwin32-mount-map-drive (operation name &rest args)
  ;; NAME must have the format looks like "^//[A-Za-z]/" here.
  ;; Support cygdrive style drive maps.
  (cygwin32-mount-run-real-handler
   operation
   (if (string-equal (substring name 0 2) "//")
       (cons (concat (substring name 2 3) ":" (substring name 3 nil))
             args)
     (cons (concat (substring name 10 11) ":" (substring name 11 nil))
           args)
     )
   )
  )

(provide 'cygwin32-mount)
