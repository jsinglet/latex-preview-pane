;;; latex-preview-pane.el --- Makes LaTeX editing less painful by providing a updatable preview pane

;; Copyright (C) 2015 John L. Singleton <jsinglet@gmail.com>

;; Author: John L. Singleton <jsinglet@gmail.com>
;; Keywords: latex, preview
;; Version: 20151021
;; URL: http://www.emacswiki.org/emacs/LaTeXPreviewPane

;;; Commentary:

;; latex-preview-pane is a minor mode for Emacs that enables you to preview your LaTeX files directly in Emacs. 
;; It supports PDF previews, your choice of pdflatex or xelatex, and it highlights errors in your LaTeX buffer.
;; 
;; To enable, place the following in your .emacs file:
;;
;; (latex-preview-pane-enable)
;;
;; As an alternative, you may enable it on the fly with:
;;
;; M-x latex-preview-pane-mode
;; 
;; The latest version of latex-preview-pane can always be found at
;; https://github.com/jsinglet/latex-preview-pane
;;
;; You can find the documentation for latex-preview-pane either on GitHub (above) or 
;; on EmacsWiki at: http://www.emacswiki.org/emacs/LaTeXPreviewPane

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'doc-view)

(defvar latex-preview-pane-current-version "20151021")
;;
;; Get rid of free variables warnings
;;

(defvar message-latex-preview-pane-welcome)
(defvar message-no-preview-yet)


;;;###autoload
(defun latex-preview-pane-enable ()
   "Enable `latex-preview-pane-mode' in `latex-mode'."
   (add-hook 'latex-mode-hook (lambda () (latex-preview-pane-mode 1))))



(defun lpp/window-containing-preview () 
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
    (if (eq (lpp/window-containing-preview) nil)
    ;; tag the newly created window
      (set-window-parameter (split-window nil nil preview-orientation) 'is-latex-preview-pane t)
    )
    (lpp/display-startup (lpp/window-containing-preview))
    ;; add the save hook
    (add-hook 'after-save-hook 'latex-preview-pane-update nil 'make-it-local)
    ;; refresh that pane
    
    (run-at-time "0 min 3 sec" nil 'latex-preview-pane-update)
    )
)


(defun lpp/get-message (f)
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-string)))



(defun lpp/display-startup (where)
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

(defvar lpp/view-buffer-command
  (pcase system-type
    (`windows-nt "start")
    (`darwin "open")
    (`gnu/linux "xdg-open")
    (`gnu/kfreebsd "xdg-open"))
  "Command used to view a file with the system's native tools.")


;;
;; Updates an external preview program of the current latex file
;;
;;;###autoload
(defun latex-preview-update () 
(interactive)
(let ( (pdf-file (replace-regexp-in-string "\.tex$" ".pdf" (lpp/buffer-file-name))))
(if (not (file-exists-p pdf-file))
    (message (concat "File " pdf-file " does not exist. Save your current buffer to generate it."))
  (if (eq system-type 'windows-nt)
      (w32-shell-execute "open" pdf-file nil nil)
    (start-process "Preview"
		   (get-buffer-create "*pdflatex-buffer*")
		   lpp/view-buffer-command
		   (replace-regexp-in-string "\.tex$" ".pdf" (lpp/buffer-file-name))
		   )))))


;;
;; If a preview pane is open, updates the preview pane on save.
;;
;;;###autoload
(defun latex-preview-pane-update ()
  (interactive)
  (when  (and (boundp 'latex-preview-pane-mode) latex-preview-pane-mode)
    (if (eq (lpp/window-containing-preview) nil)
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



(defun lpp/last-backtrace ()
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
  "Face for errors"
  :group 'latex-preview-pane)


(defun lpp/chomp (str)
      "Chomp leading and tailing whitespace from STR."
      (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                           str)
        (setq str (replace-match "" t t str)))
      str)

(defun lpp/line-errors ()
 (let ((old-buff (current-buffer)))
  (set-buffer (get-buffer "*pdflatex-buffer*"))
  (let ((error-msg (buffer-substring (point-min) (point-max))))
    (set-buffer old-buff)
    ;; get all the line numbers.
    (mapcar (lambda (what) (lpp/chomp (substring what 2))) (latex-pp-filter (lambda (what) (eq (string-match "l\\.[0-9]*" what) 0))  (split-string error-msg "\n"))))))


(defun lpp/line-errors-to-layovers (errors)
  (mapcar (lambda (what) (let ( (line (string-to-number what)))
			   (let (layoverStart layoverEnd)
			     (goto-char (point-min)) (forward-line (1- line))
			     (setq layoverStart (point))
			     (setq layoverEnd (+ 1 (line-end-position)))
			     ;;(message (format "Adding Layover On Line: %d, Start: %d, End: %d" line layoverStart layoverEnd))			     
			     ;; create the layover
			     (overlay-put (make-overlay layoverStart layoverEnd) 'face 'bad-face)))) errors))

(defun lpp/display-backtrace ()
  (let ((old-buff (current-buffer)))
  (progn
  (set-window-buffer (lpp/window-containing-preview) (get-buffer-create "*Latex Preview Pane Errors*"))
  (set-buffer (get-buffer "*Latex Preview Pane Errors*"))
  (erase-buffer)
  (insert  message-no-preview-yet)
  (set-buffer (get-buffer "*Latex Preview Pane Errors*"))
  (insert  (lpp/last-backtrace))  
  (set-buffer old-buff)
  )))

(boundp 'TeX-master)


(defun lpp/is-tex (string)
  (and (string-match (rx-to-string `(: ,".tex" eos) t)
		     string)
       t))


(defun lpp/auctex-buffer-file-name ()
  (cond
   ((eq (boundp 'TeX-master) nil) (message "The TeX master variable is not defined. To use this mode you must be using AUCTeX on this buffer."))
   ((eq TeX-master nil) (message "AUCTeX is enabled but TeX-master is not yet set. Please set it."))
   ((eq TeX-master t) buffer-file-name)
   (t (if (lpp/is-tex TeX-master) TeX-master (concat TeX-master ".tex")))
   )
  )

(defun lpp/get-file ()
  "Prompt user to enter a file path, with file name completion and input history support."
  (read-file-name "Location of Master TeX File:"))


(defun lpp/prompt-and-save-buffer-file-name ()
  (progn
    (cond
     ((eq (boundp 'lpp-TeX-master) nil)   (set (make-local-variable 'lpp-TeX-master) (lpp/get-file)))
     ((eq lpp-TeX-master nil)   (set (make-local-variable 'lpp-TeX-master) (lpp/get-file)))

     )
    (if (lpp/is-tex lpp-TeX-master) lpp-TeX-master (concat lpp-TeX-master ".tex"))))

(defun lpp/buffer-file-name  ()
  (if (eq latex-preview-pane-multifile-mode 'off) buffer-file-name
    (if (eq latex-preview-pane-multifile-mode 'auctex) (lpp/auctex-buffer-file-name)
      (lpp/prompt-and-save-buffer-file-name)))
)  

;;
;; Take a string like "../main" and extract: the path leading UP
;;




(defun lpp/invoke-pdf-latex-command ()
  (let ((buff (expand-file-name (lpp/buffer-file-name))) (default-directory (file-name-directory (expand-file-name (lpp/buffer-file-name)))))
    (if shell-escape-mode
	(call-process pdf-latex-command nil "*pdflatex-buffer*" nil shell-escape-mode buff)
      (call-process pdf-latex-command nil "*pdflatex-buffer*" nil buff)
      )
    )
  )


;;;###autoload
(defun latex-preview-pane-update-p () 
(if (eq (lpp/invoke-pdf-latex-command) 1)
    (progn
      (lpp/display-backtrace)
      (remove-overlays)
      (lpp/line-errors-to-layovers (lpp/line-errors))
      )
  
  (let ((pdf-filename (replace-regexp-in-string "\.tex$" ".pdf" (lpp/buffer-file-name)))
	(tex-buff (current-buffer))
	(pdf-buff (replace-regexp-in-string "\.tex" ".pdf" (buffer-name (get-file-buffer (lpp/buffer-file-name))))))
    (remove-overlays)
    ;; if the file doesn't exist, say that the file isn't available due to error messages
    (if (file-exists-p pdf-filename)
	  (if (eq (get-buffer pdf-buff) nil)
	      (set-window-buffer (lpp/window-containing-preview) (find-file-noselect pdf-filename))
	    (progn 
	      (set-window-buffer (lpp/window-containing-preview) pdf-buff) 
	      (switch-to-buffer-other-window pdf-buff)
	      (doc-view-revert-buffer nil t)
	      (switch-to-buffer-other-window tex-buff) 
	      ))
	
      ))))

;;
;; Mode definition
;;

(defvar latex-preview-pane-mode-map (make-keymap) "Latex preview pane keymap")

(defun lpp/set-multifile-mode (mode) (progn
				       (customize-set-variable 'latex-preview-pane-multifile-mode mode)
				       (set (make-local-variable 'lpp-TeX-master) nil)
				       ))



(easy-menu-define words-menu latex-preview-pane-mode-map
       "Menu for working with Latex Preview Pane"
       '("LaTeX Preview Pane"
	 ["LaTeX Preview Pane Actions" nil :active nil]

          ["Refresh Preview" latex-preview-pane-update]
	  ["Open Preview in External Viewer" latex-preview-update]
	  ["Disable LaTeX Preview Pane in this Buffer" (latex-preview-pane-mode 'toggle)]
	  ["Customize LaTeX Preview Pane" (customize-group 'latex-preview-pane)]
	  ["--" nil]
	  ["Multi-File Mode" nil :active nil]
	  ["Off" (lpp/set-multifile-mode 'off) :style radio :selected (eq latex-preview-pane-multifile-mode 'off)]
	  ["Use AUCTeX/TeX-master" (lpp/set-multifile-mode 'auctex) :style radio :selected (eq latex-preview-pane-multifile-mode 'auctex)]

	  ["Prompt" (lpp/set-multifile-mode 'prompt) :style radio :selected (eq latex-preview-pane-multifile-mode 'prompt)]
	  
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
	 (delete-window (lpp/window-containing-preview))
	 ))


;; set some messages for later
(let ((installation-dir (if load-file-name (file-name-as-directory (file-name-directory load-file-name)) nil)))
  (defvar message-latex-preview-pane-welcome (lpp/get-message (expand-file-name "message-latex-preview-pane-welcome.txt" installation-dir)))
  (defvar message-no-preview-yet (lpp/get-message (expand-file-name "message-no-preview-yet.txt" installation-dir))))


(defgroup latex-preview-pane nil
  "Settings that are used in the Latex Preview Pane"
  :group 'latex-preview-pane)

(defcustom pdf-latex-command "pdflatex"
  "The command to produce a PDF file from a latex document."
  :type 'string
  :group 'latex-preview-pane)

(defcustom shell-escape-mode nil
  "Should the pdflatex command use shell escaping?"
  :type '(choice (const :tag "Use shell escaping (-shell-escape)" "-shell-escape")
                 (const :tag "Do not use shell escaping" nil)
                 )
  :group 'latex-preview-pane)


(defcustom preview-orientation 'right
  "Which orientation to display the preview pane. Valid values are above, below, left, or right. Defaults to value right."
  :type '(choice (const :tag "Display preview on right" right)
                 (const :tag "Display preview on left" left)
		 (const :tag "Display preview above" above)
                 (const :tag "Display preview below" below)
                 )
  :group 'latex-preview-pane)


(defcustom latex-preview-pane-multifile-mode 'off
  "LaTeX Preview Pane can support multifile TeX projects. This variable tells LPP how to behave in different situations."
  :type '(choice (const :tag "Off" off)
                 (const :tag "Use AUCTeX (via the TeX-master variable)" auctex)
		 (const :tag "Prompt" prompt)
                 )
  :group 'latex-preview-pane)



;;
;; Some utility functions
;;

(defun lpp/packing-list ()
  '("README"
    "README.md" 
    "latex-preview-pane-pkg.el" 
    "latex-preview-pane.el"
    "message-latex-preview-pane-welcome.txt"
    "message-no-preview-yet.txt"
    "ss-error.PNG"
    "ss.PNG")
)

;; for making distributions
(defun lpp/make-dist ()
  (let ((dist-dir (concat "latex-preview-pane-" latex-preview-pane-current-version)))
    (let ((dist-file (concat dist-dir ".tar")))

    ;; (call-process "rm" nil "*dist-buffer*" nil ("-fr" dist-dir))
    (call-process "mkdir" nil "*dist-buffer*" nil dist-dir)

    ;; copy it over
    (mapc (lambda (f) 
	    (progn
	      (message (concat "Copying " f "..."))
	      (call-process "cp" nil "*dist-buffer*" nil f dist-dir)
	      ))
	  (lpp/packing-list))
	  

    (call-process "tar" nil "*dist-buffer*" nil  "-cvf" dist-file (concat dist-dir "/"))
    (message (concat "Package " dist-file " created."))
    )

))

;; (lpp/make-dist)



(provide 'latex-preview-pane)


;;; latex-preview-pane.el ends here
