;;; org-roam-treeview.el --- Tree view display for Org-roam -*- lexical-binding:t -*-

;; Author: Oleg Domanov <odomanov@yandex.ru>
;; Package-Version: 1.0
;; Package-Requires: (org-roam dash)
;; Keywords: outlines org-roam 
;; URL: https://github.com/odomanov/org-roam-treeview

;;; Commentary:

;;;  First customize or setup the `org-roam-treeview-startids'.  It should contain
;;;  a list of ids for the initial display.  Then launch `org-roam-treeview'.
;;;
;;;  Commands:
;;;    TAB   - Expand/contract the current node.
;;;    RET   - Open the file corresponding to the current node.
;;;    <, >  - Enlarge/Shrink the window.
;;;    q     - Hide the buffer
;;;    Q     - Kill the buffer
;;;
;;;  You may use the mouse as well.

;;; Code:

(require 'org-roam)
(require 'dash)

(defcustom org-roam-treeview-startids nil
  "A list of initial IDs for Org-roam Treeview."
  :type '(repeat string)
  :group 'org-roam-treeview)

(defcustom org-roam-treeview-indent 2
  "Level indent for Org-roam Treeview."
  :type 'integer
  :group 'org-roam-treeview)

(defcustom org-roam-treeview-width .25
  "The starting width of Org-roam Treeview window.
Can be Integer or Real [0.0..1.0]."
  :type '(set integer float)
  :group 'org-roam-treeview)


(defvar org-roam-treeview-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-buffer-map)
    (define-key map "q" #'org-roam-treeview-hide-buffer)
    (define-key map "Q" #'org-roam-treeview-kill-buffer)
    (define-key map "<" #'enlarge-window-horizontally)
    (define-key map ">" #'shrink-window-horizontally)
    (define-key map (kbd "<return>") #'org-roam-treeview--open-line)
    (define-key map (kbd "<tab>") #'org-roam-treeview--expand/contract-line)
    map)
  "Org-roam Treeview keymap.")

(defconst org-roam-treeview--buffer-name "Org-roam Treeview"
  "The name of the Org-roam Treeview buffer.")

(defvar org-roam-treeview--current-width nil 
  "The current width of the Org-roam Treeview window.  Always integer.")

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

(defun org-roam-treeview-hide-buffer ()
  "Set the current window-width and bury the buffer"
  (interactive)
  (setq org-roam-treeview--current-width (window-width))
  (bury-buffer))

(defun org-roam-treeview-kill-buffer ()
  "Set the current window-width and kill the buffer"
  (interactive)
  (setq org-roam-treeview--current-width (window-width))
  (kill-this-buffer))

(defun org-roam-treeview--make-line (id level)
  "Create a line with buttons for ID on the level LEVEL."
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

(defun org-roam-treeview--change-expand-button-char (char)
  "Change the expansion button character to CHAR for the current line."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "\\([-+?]\\)" (line-end-position) t)
	(org-roam-treeview-with-writable
	  (goto-char (match-end 1))
          (backward-char)
	  (insert-char char 1 t)
	  (delete-char 1)))))

(defun org-roam-treeview--delete-subblock (level)
  "Delete text from point to levelation level LEVEL or greater.
Handles end-of-sublist smartly."
  (org-roam-treeview-with-writable
    (save-excursion
      (end-of-line) (forward-char)
      (let ((start (point)))
	(while (and (not (eobp))
                    (> (button-get (next-button (point)) :level) level))
	  (forward-line)
	  (beginning-of-line))
	(delete-region start (point))))))

(defun org-roam-treeview--expand/contract (button)
  "Expand or contract the BUTTON item."
  (let* ((text  (button-label button))
         (id    (button-get button :id))
         (level (button-get button :level)))
    (cond ((string-match "\\+" text)	;we have to expand this node
	   (org-roam-treeview--change-expand-button-char ?-)
           (let ((dests (org-roam-db-query
                         `[:select  [dest] :from links
                                    :where (= links:source $s1)] id)))
             (dolist (dest dests)
               (let ((items (org-roam-db-query
                             `[:select  [id] :from nodes
                                        :where (= nodes:id $s1)] (car dest))))
	         (org-roam-treeview-with-writable
	           (save-excursion
	             (end-of-line) (forward-char)
                     (dolist (item items)
                       (org-roam-treeview--make-line (car item) (1+ level)))))))))
	  ((string-match "-" text)	;we have to contract this node
	   (org-roam-treeview--change-expand-button-char ?+)
	   (org-roam-treeview--delete-subblock level))
	  (t (error "Ooops...  not sure what to do")))))

(defun org-roam-treeview--expand/contract-line ()
  "Expand or contract the current line."
  (interactive)
  (beginning-of-line)
  (org-roam-treeview--expand/contract (next-button (point) t)))

(defun org-roam-treeview--get-focus ()
  "Get the focus.  Org-roam Treeview window should be visible."
  (let ((win (get-buffer-window org-roam-treeview--buffer-name)))
    (select-window win)))

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
         (org-roam-treeview--get-focus)
         t)
        (t nil)))))

(defun org-roam-treeview--open-line ()
  "Find the file corresponding to the current line and open it."
  (interactive)
  (end-of-line)
  (org-roam-treeview--open (previous-button (point) t)))

(defun org-roam-treeview--info-function (_window obj _pos)
  "Get tooltip info.  OBJ should be a button."
  (org-roam-treeview-with-writable
   (button-label obj)))

(defun org-roam-treeview-set-width (width)
  "Set the width of Org-roam Treeview window to WIDTH."
  (let ((w (max width window-min-width)))
    (cond
      ((> (window-width) w)
       (shrink-window-horizontally  (- (window-width) w)))
      ((< (window-width) w)
       (enlarge-window-horizontally (- w (window-width)))))))

(defun org-roam-treeview--popup-window (buffer-or-name)
  "Create and select the Org-roam Treeview window for existing BUFFER-OR-NAME."
  (let* ((buf (get-buffer buffer-or-name))
         (ww (window-width))
         (win (display-buffer-in-side-window
               buf '((side . right)
                     (slot . -1)
                     (window-height . fit-window-to-buffer)
                     ;; (window-width . 0.25) ;org-roam-treeview-width)
                     (window-parameters . ((no-other-window . t)
                                           (no-delete-other-windows . t)))
                     (dedicated . t)
                     ))))
    (select-window win)
    (when (not org-roam-treeview--current-width)
      (setq org-roam-treeview--current-width (if (floatp org-roam-treeview-width)
                                                 (floor (* org-roam-treeview-width ww))
                                               org-roam-treeview-width)))
    (org-roam-treeview-set-width org-roam-treeview--current-width))
  (use-local-map org-roam-treeview-map))

(defun org-roam-treeview--init ()
  "Initialize the buffer."
  (let ((buf (get-buffer-create org-roam-treeview--buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (toggle-truncate-lines 1)
      (show-paren-local-mode -1)
      (font-lock-mode -1)
      (setq-local header-line-format
                  (format-mode-line (format "    Org-roam") 'org-roam-treeview-title))
      (setq-local mode-line-format (format "Org-roam: %s" org-roam-directory))
      (dolist (id (reverse org-roam-treeview-startids))
        (org-roam-treeview--make-line id 0))
      (setq buffer-read-only t
            cursor-type nil))
    buf))



;;;;;;;; Install function

;;;###autoload
(defun org-roam-treeview ()
  "Main entrance to Org-roam Treeview."
  (interactive)
  (let* ((buf-name org-roam-treeview--buffer-name)
         (buf (get-buffer buf-name))
         (win (get-buffer-window buf-name)))
    (cond (win                           ;window is visible
           (select-window win))
          (buf                           ;buffer exists
           (org-roam-treeview--popup-window buf-name))
          (t                             ;no buffer
           (org-roam-treeview--init)
           (org-roam-treeview--popup-window buf-name)))))


(provide 'org-roam-treeview)

;;; org-roam-treeview.el ends here
