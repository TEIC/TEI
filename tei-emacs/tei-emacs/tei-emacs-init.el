;; basic loading for tei-emacs project
;; C Wittern/S Rahtz April 2001
;; Revised: $Date$

(defmacro parent-directory (f)
  "Return safe parent directory of the directory given as argument."
  `(directory-file-name
    (file-name-directory
     (directory-file-name ,f))))

(if (eq system-type 'windows-nt)    (load-library "windoze"))

;; general setup
(setq-default frame-title-format '("%f"))
(setq-default icon-title-format '("%f"))
(setq line-number-display-limit 100000)
(setq column-number-mode t)
(setq next-line-add-newlines nil)
(global-set-key "\eg" 'goto-line)
(global-set-key "\er" 'replace-string)
(global-set-key "\em" 'set-mark-command)


;; timestamp

(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-line-limit 20)

(autoload 'all "all" nil t)

(defun insert-date-time()
  (interactive)
  (insert (format-time-string "[%Y-%m-%dT%T%z]" (current-time))))


;; line number into modeline
(add-hook 'text-mode-hook '(lambda () (line-number-mode 1)))


;; autofill mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq default-major-mode 'text-mode)

;;;  coloring syntax for LaTeX sources (and other languages)
(global-font-lock-mode t)
(add-hook 'help-mode-hook 'turn-on-font-lock)
(setq font-lock-maximum-decoration 3)

;; URL browsing
(require 'browse-url)
;; to let url loading work
;(setq browse-url-browser-function
;      ;; No window system at build time, ie.  site-start.el, ~/.emacs
;      ;; or ~/.gnus
;      (if window-system
;          (if (eq system-type 'windows-nt) 'shell-execute-url
;            'browse-url-mozilla)
;        'browse-url-w3))

(require 'func-doc)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "tp6")

(auto-compression-mode t)

;;; Cursor position, line number

(defun what-char-and-line ()
  "Print info on cursor position, line the point is on,
 and the total number of lines in the buffer."
  (interactive)
  (let* ((char (following-char))
         (beg (point-min))
         (end (point-max))
         (pos (point))
         (total (buffer-size))
         (percent (if (> total 50000)
                      ;; Avoid overflow from multiplying by 100!
                      (/ (+ (/ total 200) (1- pos)) (max (/ total 100) 1))
                    (/ (+ (/ total 2) (* 100 (1- pos))) (max total 1))))
         (hscroll (if (= (window-hscroll) 0)
                      ""
                    (format " Hscroll=%d" (window-hscroll))))
         (col (current-column)))
    (if (= pos end)
        (if (or (/= beg 1) (/= end (1+ total)))
            (message "point=%d/%d(%d%%) <%d - %d>  col %d %s End of buffer. Last line %d."
                     pos total percent beg end col hscroll (count-lines 1 (point-max)))
          (message "point=%d/%d(%d%%)  col %d %s End of buffer. Last line %d."
                   pos total percent col hscroll (count-lines 1 (point-max))))
      (if (or (/= beg 1) (/= end (1+ total)))
          (message "Car= %s (0%o)  point=%d/%d(%d%%) <%d - %d>  col %d %s ligne %d/%d"
                   (single-key-description char) char pos total percent beg end col hscroll
                   (count-lines 1 (1+ (point))) (count-lines 1 (point-max)))
        (message "Car = %s (0%o)  point=%d/%d(%d%%)  col %d %s ligne %d/%d"
                 (single-key-description char) char pos total percent col hscroll
                 (count-lines 1 (1+ (point))) (count-lines 1 (point-max)))))))

;; Paren mode
(load "paren")
(defun kill-something( ) (interactive)
    (if (and mark-active transient-mark-mode)
        (kill-region (point) (mark)) 
        (delete-backward-char 1)
    ))

;; XML material

(load "xmlsgml-setup")

;; character-related
(setq unicode-data-path (concat teiemacsdir "/unicode/UnicodeData-Latest.txt"))

;; define extra quail input method
(register-input-method
 "sanskrit-romanized-postfix" "Latin-1" 'quail-use-package
 "SK>" "Romanized Sanskrit postfix modifiers"
 "quail-sanskrit")

(if (> emacs-major-version 20)        
(progn
(require 'un-define)
))


;; initial window
(if (memq window-system '(x w32))
 (progn
   (set-frame-height (selected-frame) 40)      
   (set-frame-width (selected-frame) 100)
   (require 'color-theme)
	))

