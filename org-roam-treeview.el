;;; org-roam-treeview.el --- Tree view display for Org-roam

;; Author: Oleg Domanov <odomanov@yandex.ru>
;; Version: 1.0
;; Keywords: org-roam tree

;;; Commentary:

;;;  First customize or setup the `org-roam-treeview-startids'.  It should contain
;;;  a list of ids for the initial display.  Then launch `org-roam-treeview'.
;;;
;;;  Commands:
;;;    TAB   - Expand/contract the current node.
;;;    RET   - Open the file corresponding to the current node.
;;;    <, >  - Enlarge/Shrink the window.
;;;    q     - Bury the buffer
;;;    Q     - Kill the buffer
;;;
;;;  You may use the mouse as well.

;;; Code:

(require 'org-roam)

(defcustom org-roam-treeview-startids nil
  "The initial IDs for Org-roam treeview."
  :type '(repeat string)
  :group 'org-roam-treeview)

(defcustom org-roam-treeview-indent 2
  "Level indent for Org-roam treeview."
  :type 'integer
  :group 'org-roam-treeview)

(defvar org-roam-treeview-window nil
  "Org-roam treeview window.")

(define-button-type 'org-roam-treeview-expand
    'action #'org-roam-treeview--expand/contract
    'face nil
    'help-echo nil) 

(define-button-type 'org-roam-treeview-link
    'action #'org-roam-treeview--open
    'face 'org-link
    'help-echo #'org-roam-treeview--info-function)

(defface org-roam-treeview-title
    '((t (:weight bold
          :foreground "IndianRed"
          )))
  "Face for the title."
  :group 'org-roam-treeview)

(defmacro org-roam-treeview-with-writable (&rest forms)
  "Allow the buffer to be writable and evaluate FORMS."
  (declare (indent 0) (debug t))
  `(let ((inhibit-read-only t))
     ,@forms))

(defun org-roam-treeview--make-line (id level)
  "Create a line with buttons for ID and LEVEL."
  (let* ((item (car (org-roam-db-query
                     `[:select  [file title id] :from nodes
                                :where (= nodes:id $s1)] id)))
         (file (car item))
         (text (cadr item)))
    (save-excursion
      (beginning-of-line)
      (insert (make-string (* level org-roam-treeview-indent) ?\s))
      (insert-button "[+]" :type 'org-roam-treeview-expand :id id :level level)
      (insert " ")
      (insert-button text :type 'org-roam-treeview-link :file file :id id :level level)
      (insert "\n"))))

(defun org-roam-treeview-change-expand-button-char (char)
  "Change the expansion button character to CHAR for the current line."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "\\([-+?]\\)" (line-end-position) t)
	(org-roam-treeview-with-writable
	  (goto-char (match-end 1))
          (backward-char)
	  (insert-char char 1 t)
	  (delete-char 1)))))

(defun org-roam-treeview-delete-subblock (level)
  "Delete text from point to levelation level LEVEL or greater.
Handles end-of-sublist smartly."
  (org-roam-treeview-with-writable
    (save-excursion
      (end-of-line) (forward-char 1)
      (let ((start (point)))
	(while (and (not (eobp))
                    (> (button-get (next-button (point)) :level) level))
	  (forward-line 1)
	  (beginning-of-line))
	(delete-region start (point))))))

(defun org-roam-treeview--expand/contract (button)
  "Expanding/contracting the BUTTON item."
  (let* ((text  (button-label button))
         (id    (button-get button :id))
         (level (button-get button :level)))
    (cond ((string-match "\\+" text)	;we have to expand this node
	   (org-roam-treeview-change-expand-button-char ?-)
           (let ((dests (org-roam-db-query
                         `[:select  [dest] :from links
                                    :where (= links:source $s1)] id)))
             (dolist (dest dests)
               (setq item (org-roam-db-query
                           `[:select  [file title id] :from nodes
                                      :where (= nodes:id $s1)] (car dest)))
	       (org-roam-treeview-with-writable
	        (save-excursion
	          (end-of-line) (forward-char)
                  (dolist (i item)
                    (org-roam-treeview--make-line (caddr i) (1+ level))))))))
	  ((string-match "-" text)	;we have to contract this node
	   (org-roam-treeview-change-expand-button-char ?+)
	   (org-roam-treeview-delete-subblock level))
	  (t (error "Ooops...  not sure what to do")))))

(defun org-roam-treeview--expand/contract-line ()
  "Expand/contract the current line."
  (interactive)
  (beginning-of-line)
  (org-roam-treeview--expand/contract (next-button (point) t)))

(defun org-roam-treeview-get-focus ()
  "Get the focus"
  (select-window org-roam-treeview-window))

(defun org-roam-treeview--open (button)
  "Open file with id extracted from BUTTON"
  (let* (;(file (button-get button :file))
         (id (button-get button :id)))
    ;; from org-roam
    (let ((node (org-roam-populate (org-roam-node-create :id id))))
      (cond
        ((org-roam-node-file node)
         (org-mark-ring-push)
         (org-roam-node-visit node nil 'force)
         (org-roam-treeview-get-focus)
         t)
        (t nil)))))

(defun org-roam-treeview--open-line ()
  "Find the file corresponding to the current line and open it."
  (interactive)
  (end-of-line)
  (org-roam-treeview--open (previous-button (point) t)))

(defun org-roam-treeview--info-function (window obj pos)
  "OBJ should be a button."
  (org-roam-treeview-with-writable
   (button-label obj)))


;;;;;;;; Install function

(defun org-roam-treeview--install ()
  "Install Org-roam treeview."
  (setq org-roam-treeview-map (make-sparse-keymap))
  (let ((map org-roam-treeview-map))
    (set-keymap-parent map button-buffer-map)
    (define-key map "q" #'bury-buffer)
    (define-key map "Q" #'kill-this-buffer)
    (define-key map "<" #'enlarge-window-horizontally)
    (define-key map ">" #'shrink-window-horizontally)
    (define-key map (kbd "<return>") #'org-roam-treeview--open-line)
    (define-key map (kbd "<tab>") #'org-roam-treeview--expand/contract-line)))

;;;###autoload
(defun org-roam-treeview ()
  "Main entrance to Org-roam treeview."
  (interactive)
  (let ((buf (get-buffer-create "Org-roam treeview")))
    (setq org-roam-treeview-window
          (display-buffer-in-side-window buf
                                         '((side . right)
                                           (slot . 0)
                                           (window-height . fit-window-to-buffer)
                                           (window-width . 50)
                                           (dedicated . t)))))
  (select-window org-roam-treeview-window)
  (use-local-map org-roam-treeview-map)
  (erase-buffer)
  (toggle-truncate-lines 1)
  (setq-local mode-line-format (format "Org-roam: %s" org-roam-directory))
  (let ((start (point)))
    (insert "  =-= Org-roam =-=  \n")
    (set-text-properties start (point) '(face org-roam-treeview-title)))
  ;; (insert (propertize "  =-= Org-roam =-=  \n" 'face 'org-roam-treeview-title))
  (dolist (id (reverse org-roam-treeview-startids))
    (org-roam-treeview--make-line id 0))
  (setq buffer-read-only t
        cursor-type nil))

(org-roam-treeview--install)

(provide 'org-roam-treeview)

;;; org-roam-treeview.el ends here
