; 
; library to provide standard open-file-dialog in Windoze machines
;
; The executable getfile.exe should be in the system path somewhere
; and/or the variable 'dlgopen-file-path' should have the executable [path]name
;
; load this file from .emacs or use load-file `dlgopen` 
; "C-x C-f" or "M-x dlgopen-open-files" to open file(s)
;
; If multiple files are selected they will be given focus depending
; on the value of the variable 'dlgopen-give-focus-always'
;
; author: Binu Jose Philip
; do whatever you want with this code - except make it better :^)
;

(defvar dlgopen-executable-path "getfile.exe"
  "*Executable path for open dialog")

(defvar dlgopen-give-focus-always t
  "*If multiple file selected give focus when opening or not")

(defun dlgopen-open-files ()
  "*Provides standard file-open dialog to open files.
  Set the variable 'dlgopen-executable-path' to the path of the 
  executable 'getfile.exe'. If it is not in any of the system PATHs. 
  If a single file is selected it will be given focus always, if multiple 
  files are selected, depending on the value of 'dlgopen-give-focus-always' 
  files will be brought to foreground"
  
  (interactive)
  (let ((buffer "") (file-fqn "") (lines-in-page 0) (dir-path ""))
    (setq buffer (generate-new-buffer "files-to-open"))
    (if (call-process dlgopen-executable-path nil buffer)
        (save-excursion 
          (set-buffer buffer) (goto-line 1)
          (setq dir-path (get-current-line))

; if buffer empty user has cancelled or open failed
; if only one line in buffer only one file selected so give it focus
          (if (> (buffer-size) 0)
              (if (= (setq lines-in-page (count-lines 1 (buffer-size))) 1)
                  (find-file dir-path)            
                (while (> lines-in-page 1)
                  (setq lines-in-page (- lines-in-page 1))
                  (next-line 1)
                  (setq file-fqn (concat dir-path "/" (get-current-line)))
                  (save-excursion 
                    (if (eq dlgopen-give-focus-always t) 
                        (find-file file-fqn)
                      (find-file-noselect file-fqn))))))))
    (kill-buffer buffer))
  )

(defun get-current-line()
  (buffer-substring (save-excursion (beginning-of-line) (point))
                    (save-excursion (end-of-line) (point)))
  )

; define a short-cut key
;(global-set-key "\C-x\C-f" 'dlgopen-open-files)

