;;; latex-preview-pane.el --- Makes LaTeX editing less painfull by providing a updatable preview pane

;; Copyright (C) 2013 John L. Singleton <jsinglet@gmail.com>

;; Author: John L. Singleton <jsinglet@gmail.com>
;; Keywords: latex, preview
;; Version: 20131020

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

;; The latest version of cygwin-mount.el can always be found at
;; https://github.com/jsinglet/latex-preview-pane


;;
;; System specific configuration. 
;;

;;;###autoload
(if (eq window-system 'w32)
    (progn
      (setq pdf-latex-command "pdflatex")
      (setq view-buffer-command "start")
      
    )
)

;;;###autoload
(if (eq system-type 'darwin)
    (progn
      (setq pdf-latex-command "pdflatex")
      (setq view-buffer-command "open")
    )
)

;;;###autoload
(if (eq system-type 'gnu/linux)
    (progn
      (setq pdf-latex-command "pdflatex")
      (setq view-buffer-command "xdg-open")
    )
)

;;;###autoload
(if (eq system-type 'gnu/kfreebsd)
    (progn
      (setq pdf-latex-command "pdflatex")
      (setq view-buffer-command "xdg-open")
    )
)




;;
;; Updates an external preview program of the current latex file
;;
;;;###autoload
(defun latex-preview-update () 
(interactive)
(if (eq (call-process pdf-latex-command nil "*pdflatex-buffer*" nil buffer-file-name) 1)
    (if (y-or-n-p "PDF Generation Failed. View Errors?") (switch-to-buffer "*pdflatex-buffer*"))
  (start-process "Preview"
		     (get-buffer-create "*pdflatex-buffer*")
		     view-buffer-command
		     (replace-regexp-in-string ".tex" ".pdf" buffer-file-name)
		     ))
)


;;
;; If a preview pane is open, updates the preview pane on save.
;;
;;;###autoload
(defun latex-preview-pane-update ()
  (interactive)
  (when (eq major-mode 'latex-mode)
  (progn 
    (message "Updating LaTeX Preview Pane")
    ;;(save-buffer)
    (latex-preview-pane-update-p))))


;;;###autoload
(defun latex-preview-pane-update-p () 
(if (eq (call-process pdf-latex-command nil "*pdflatex-buffer*" nil buffer-file-name) 1)
    (if (y-or-n-p "PDF Generation Failed. View Errors?") (switch-to-buffer "*pdflatex-buffer*"))
  ;; if we are currently viewing the document in a pane, we refresh it.
  (let ((tex-buff (current-buffer))
       (pdf-buff (replace-regexp-in-string ".tex" ".pdf" (buffer-name))))

  (if (not (eq (get-buffer-window pdf-buff) nil))
      (progn
	(switch-to-buffer-other-window pdf-buff) 
	(doc-view-revert-buffer nil t)
	(switch-to-buffer-other-window tex-buff) 
	)
    ))))


;;;###autoload
(add-hook 'after-save-hook 'latex-preview-pane-update)

;;;###autoload
(eval-after-load 'latex-mode
                     '(define-key LaTeX-mode-map (kbd "s-p") 'latex-preview-update))

;;;###autoload
(eval-after-load 'latex-mode
                     '(define-key LaTeX-mode-map (kbd "M-p") 'latex-preview-update))

;;;###autoload
(eval-after-load 'latex-mode
                     '(define-key LaTeX-mode-map (kbd "<f15>") 'latex-preview-update))




(provide 'latex-preview-pane)


;;; latex-preview-pane.el ends here
