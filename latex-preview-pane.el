;;; latex-preview-pane.el --- Makes LaTeX editing less painful by providing a updatable preview pane

;; Copyright (C) 2013 John L. Singleton <jsinglet@gmail.com>

;; Author: John L. Singleton <jsinglet@gmail.com>
;; Keywords: latex, preview
;; Version: 20140204
;; URL: http://www.emacswiki.org/emacs/LaTeXPreviewPane

;; This file is *NOT* part of GNU Emacs.
;;
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
;;
;; latex-preview-pane is a minor mode for Emacs that enables you to preview your LaTeX files directly in Emacs. 
;; It supports PDF previews, your choice of pdflatex or xelatex, and it highlights errors in your LaTeX buffer.
;; 
;; The latest version of latex-preview-pane can always be found at
;; https://github.com/jsinglet/latex-preview-pane
;;
;; You can find the documentation for latex-preview-pane either on GitHub (above) or on EmacsWiki at: http://www.emacswiki.org/emacs/LaTeXPreviewPane
;;
;;; Code:


;;
;; Mode definition
;;

;;; try to automatically load this when we open a .tex file
;;;###autoload
(add-hook 'latex-mode-hook (lambda () (latex-preview-pane-mode t)))



(defun window-containing-preview () 
  (let (windows i docViewWindow)
    (setq windows (window-list))
    (setq i 0)
    (progn
    (while (and (not docViewWindow) (<= i (length windows)))
      (let ((currentWindow (pop windows)))
	(if (window-parameter currentWindow 'is-latex-preview-pane)
	    (setq docViewWindow currentWindow)
	  ))
      (setq i (1+ i))
      )
    docViewWindow)))

;;
;; Init procedure:
;; 1) Find a window with doc-view-mode turned on in this frame.
;; 2) If no such window can be found, split this window vertically. 
;; 2a) Display startup message, shortcuts, etc. Pause for 3 seconds.  
;; 3) TeX the current file. (that is, start the refresh loop)
;;

;;;###autoload
(defun init-latex-preview-pane ()
  (progn
    ;; make sure the current window isn't the preview pane
    (set-window-parameter nil 'is-latex-preview-pane nil)
    (if (eq (window-containing-preview) nil)
    ;; tag the newly created window
      (set-window-parameter (split-window nil nil 'right) 'is-latex-preview-pane t)
    )
    (display-startup (window-containing-preview))
    ;; add the save hook
    (add-hook 'after-save-hook 'latex-preview-pane-update nil 'make-it-local)
    ;; refresh that pane
    
    (run-at-time "0 min 3 sec" nil 'latex-preview-pane-update)
    )
)


(defun get-message (f)
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-string)))



(defun display-startup (where)
  (let ((old-buff (current-buffer)))
  (progn
  (set-window-buffer where (get-buffer-create "*Latex Preview Pane Welcome*"))
  (set-buffer (get-buffer "*Latex Preview Pane Welcome*"))
  (erase-buffer)
  (insert  message-latex-preview-pane-welcome)
  (set-buffer old-buff)
  )))


;;
;; System specific configuration. 
;;

;;;###autoload
(if (eq window-system 'w32)
    (progn
      ;(setq pdf-latex-command "pdflatex")
      (setq view-buffer-command "start")
      
    )
)

;;;###autoload
(if (eq system-type 'darwin)
    (progn
      ;(setq pdf-latex-command "pdflatex")
      (setq view-buffer-command "open")
    )
)

;;;###autoload
(if (eq system-type 'gnu/linux)
    (progn
      ;(setq pdf-latex-command "pdflatex")
      (setq view-buffer-command "xdg-open")
    )
)

;;;###autoload
(if (eq system-type 'gnu/kfreebsd)
    (progn
      ;(setq pdf-latex-command "pdflatex")
      (setq view-buffer-command "xdg-open")
    )
)




;;
;; Updates an external preview program of the current latex file
;;
;;;###autoload
(defun latex-preview-update () 
(interactive)
(let ( (pdf-file (replace-regexp-in-string ".tex" ".pdf" buffer-file-name)))
(if (not (file-exists-p pdf-file))
    (message (concat "File " pdf-file " does not exist. Save your current buffer to generate it."))
  (if (eq window-system 'w32)
      (w32-shell-execute "open" pdf-file nil nil)
    (start-process "Preview"
		   (get-buffer-create "*pdflatex-buffer*")
		   view-buffer-command
		   (replace-regexp-in-string ".tex" ".pdf" buffer-file-name)
		   )))))


;;
;; If a preview pane is open, updates the preview pane on save.
;;
;;;###autoload
(defun latex-preview-pane-update ()
  (interactive)
  (when  (and (boundp 'latex-preview-pane-mode) latex-preview-pane-mode)
    (if (eq (window-containing-preview) nil)
	(init-latex-preview-pane)
      (progn 
	(if (not (eq (get-buffer "*pdflatex-buffer*") nil))
	    (let ((old-buff (current-buffer)))
	    (progn
	      (set-buffer "*pdflatex-buffer*")
	      (erase-buffer)
	      (set-buffer old-buff)
	    )))
	(message "Updating LaTeX Preview Pane")
	(latex-preview-pane-update-p)))))



(defun last-backtrace ()
 (let ((old-buff (current-buffer)))
  (set-buffer (get-buffer "*pdflatex-buffer*"))
  (let ((error-msg (buffer-substring (point-min) (point-max))))
    (set-buffer old-buff)
    (mapconcat  'identity (reverse (split-string error-msg "\n")) "\n"))))


(defun latex-pp-filter (condp lst)
    (delq nil
          (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defface bad-face
  '((t (:foreground "White" :background "Red")))
  "Face for errors")


(defun chomp (str)
      "Chomp leading and tailing whitespace from STR."
      (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                           str)
        (setq str (replace-match "" t t str)))
      str)

(defun line-errors ()
 (let ((old-buff (current-buffer)))
  (set-buffer (get-buffer "*pdflatex-buffer*"))
  (let ((error-msg (buffer-substring (point-min) (point-max))))
    (set-buffer old-buff)
    ;; get all the line numbers.
    (mapcar (lambda (what) (chomp (substring what 2))) (latex-pp-filter (lambda (what) (eq (string-match "l\\.[0-9]*" what) 0))  (split-string error-msg "\n"))))))


(defun line-errors-to-layovers (errors)
  (mapcar (lambda (what) (let ( (line (string-to-number what)))
			   (let (layoverStart layoverEnd)

			     (goto-line line)
			     (setq layoverStart (point))
			     (setq layoverEnd (+ 1 (line-end-position)))
			     ;;(message (format "Adding Layover On Line: %d, Start: %d, End: %d" line layoverStart layoverEnd))			     
			     ;; create the layover
			     (overlay-put (make-overlay layoverStart layoverEnd) 'face 'bad-face)))) errors))

(defun display-backtrace ()
  (let ((old-buff (current-buffer)))
  (progn
  (set-window-buffer (window-containing-preview) (get-buffer-create "*Latex Preview Pane Errors*"))
  (set-buffer (get-buffer "*Latex Preview Pane Errors*"))
  (erase-buffer)
  (insert  message-no-preview-yet)
  (set-buffer (get-buffer "*Latex Preview Pane Errors*"))
  (insert  (last-backtrace))  
  (set-buffer old-buff)
  )))



;;;###autoload
(defun latex-preview-pane-update-p () 
(if (eq (call-process pdf-latex-command nil "*pdflatex-buffer*" nil buffer-file-name) 1)
    ;; TODO: highlight errors 
    (progn
      (display-backtrace)
      (remove-overlays)
      (line-errors-to-layovers (line-errors))
      )
  
  (let ((pdf-filename (replace-regexp-in-string ".tex" ".pdf" buffer-file-name))
	(tex-buff (current-buffer))
	(pdf-buff (replace-regexp-in-string ".tex" ".pdf" (buffer-name))))
    (remove-overlays)
    ;; if the file doesn't exist, say that the file isn't available due to error messages
    (if (file-exists-p pdf-filename)
	  (if (eq (get-buffer pdf-buff) nil)
	      (set-window-buffer (window-containing-preview) (find-file-noselect pdf-filename))
	    (progn 
	      (set-window-buffer (window-containing-preview) pdf-buff) 
	      (switch-to-buffer-other-window pdf-buff)
	      (doc-view-revert-buffer nil t)
	      (switch-to-buffer-other-window tex-buff) 
	      ))
	
      ))))

;;
;; Mode definition
;;

(defvar latex-preview-pane-mode-map (make-keymap) "Latex preview pane keymap")

(easy-menu-define words-menu latex-preview-pane-mode-map
       "Menu for working with Latex Preview Pane"
       '("LaTeX Preview Pane"
          ["Refresh Preview" latex-preview-pane-update]
	  ["Open Preview in External Viewer" latex-preview-update]
	  ["Disable LaTeX Preview Pane in this Buffer" (latex-preview-pane-mode 'toggle)]
	  ["Customize LaTeX Preview Pane" (customize-group 'latex-preview-pane)]
	  
	  ))

(define-key latex-preview-pane-mode-map (kbd "M-p") 'latex-preview-pane-update)
(define-key latex-preview-pane-mode-map (kbd "s-p") 'latex-preview-pane-update)
(define-key latex-preview-pane-mode-map (kbd "M-P") 'latex-preview-update)
(define-key latex-preview-pane-mode-map (kbd "s-P") 'latex-preview-update)

;;;###autoload
(define-minor-mode latex-preview-pane-mode
       "Toggle Latex Preview Pane Mode.
     Interactively with no argument, this command toggles the mode.
     A positive prefix argument enables the mode, any other prefix
     argument disables it.  From Lisp, argument omitted or nil enables
     the mode, `toggle' toggles the state.
     
     When Latex Preview Pane mode is enabled, saving a latex file will cause 
     a PDF preview pane of your document to appear."
       ;; The initial value.
       :init-value nil
       ;; The indicator for the mode line.
       :lighter " Latex Preview Pane"
       ;; The minor mode bindings.
       :keymap latex-preview-pane-mode-map
       :group 'latex-preview-pane
       ;; if we are turning on the mode, init the view
       (if (and (boundp 'latex-preview-pane-mode) latex-preview-pane-mode)
	   (init-latex-preview-pane)
	 ;; otherwise, kill the window
	 (delete-window (window-containing-preview))
	 ))


(defun get-install-dir () 
  (concat (package--dir "latex-preview-pane" "20140204") "/" ))

;; set some messages for later
(setq message-latex-preview-pane-welcome (get-message (concat (get-install-dir) "message-latex-preview-pane-welcome.txt")))

(setq message-no-preview-yet (get-message (concat (get-install-dir) "message-no-preview-yet.txt")))


(defgroup latex-preview-pane nil
  "Settings that are used in the Latex Preview Pane"
  :group 'latex-preview-pane)

(defcustom pdf-latex-command "pdflatex"
  "The command to produce a PDF file from a latex document."
  :type 'string
  :group 'latex-preview-pane)

(provide 'latex-preview-pane)


;;; latex-preview-pane.el ends here
