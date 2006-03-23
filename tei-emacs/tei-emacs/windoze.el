;; Windows-specific loading for tei-emacs project
;; C Wittern/S Rahtz April 2001, May 2004
;; Revised: $Date$


;; dired stuff to open files a la Windows from Howard Melman
(defun dired-execute-file (&optional arg)
  (interactive "P")
  (mapcar #'(lambda (file)
      (w32-shell-execute "open" (convert-standard-filename file)))
          (dired-get-marked-files nil arg)))

(defun dired-mouse-execute-file (event)
  "In dired, execute the file or goto directory name you click on."
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-end event))))
  (goto-char (posn-point (event-end event)))
  (if (file-directory-p (dired-get-filename))
      (dired-find-file)
    (dired-execute-file)))
(global-set-key [?\C-x mouse-2] 'dired-mouse-execute-file)

(defun hrm-dired-mode-hook ()
  "Hook run when entering dired-mode."
    (define-key dired-mode-map "X" 'dired-execute-file)
    (define-key dired-mode-map [M-down-mouse-1] 'dired-mouse-mark-file))

(add-hook 'dired-mode-hook 'hrm-dired-mode-hook)


;; swap mouse-2 and mouse-3
(setq w32-swap-mouse-buttons t) 
(global-unset-key (kbd "<mouse-2>"))

;; Use standard Windows dialog box to open files
(load "dlgopen") 
;; (global-set-key "\C-x\C-f" 'dlgopen-open-files)



(setq w32-bdf-filename-alist
      (w32-find-bdf-fonts (concat teischemadir "/ucsfonts")))

(create-fontset-from-fontset-spec
 "-*-Courier New-normal-r-*-*-15-*-*-*-c-*-fontset-courier,
 arabic-iso8859-6:-*-Courier New-normal-r-*-*-15-*-*-*-c-*-iso8859-6
 chinese-big5-1:-*-MingLiU-normal-r-*-*-15-*-*-*-c-*-big5-*,
 chinese-big5-2:-*-MingLiU-normal-r-*-*-15-*-*-*-c-*-big5-*
 chinese-gb2312:-*-NSimSun-normal-r-*-*-15-*-*-*-c-*-gb2312-*,
 cyrillic-iso8859-5:-*-Courier New-normal-r-*-*-15-*-*-*-c-*-iso8859-5,
 greek-iso8859-7:-*-Courier New-normal-r-*-*-15-*-*-*-c-*-iso8859-7,
 indian-is13194:-*-Courier New-normal-r-*-*-15-*-*-*-c-*-indian-is13194,
 japanese-jisx0208-1978:-*-MS Gothic-normal-r-*-*-15-*-*-*-c-*-jisx0208-sjis,
 japanese-jisx0208:-*-MS Gothic-normal-r-*-*-15-*-*-*-c-*-jisx0208-sjis,
 japanese-jisx0212:-*-MS Gothic-normal-r-*-*-15-*-*-*-c-*-jisx0212-sjis,
 katakana-jisx0201:-*-MS Gothic-normal-r-*-*-15-*-*-*-c-*-jisx0208-sjis,
 korean-ksc5601:-*-Gungsuh-normal-r-*-*-15-*-*-*-c-*-ksc5601-*,
 latin-iso8859-2:-*-Courier New-normal-r-*-*-15-*-*-*-c-*-iso8859-2,
 latin-iso8859-3:-*-Courier New-normal-r-*-*-15-*-*-*-c-*-iso8859-3,
 latin-iso8859-4:-*-Courier New-normal-r-*-*-15-*-*-*-c-*-iso8859-4,
 latin-iso8859-9:-*-Courier New-normal-r-*-*-15-*-*-*-c-*-iso8859-9,
 latin-jisx0201:-*-MS Gothic-normal-r-*-*-15-*-*-*-c-*-jisx0208-sjis,"
 t 'noerror)

(setq w32-enable-synthesized-fonts t)

(message "done loading windoze")
