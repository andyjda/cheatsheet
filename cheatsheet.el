;;; cheatsheet.el --- create your own cheatsheet
;; I (Andrew De Angelis) am working on this package for possible enhancements
;;    bobodeangelis@gmail.com
;;
;; Package-Requires: ((emacs "28") (cl-lib "0.5"))
;; Version: 1.0
;; Keywords: convenience, usability

;; This file is not part of GNU Emacs

;;; Licence:

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; Quick start:
;; Load package
;; Add your first cheat:
;; (cheatsheet-add :group 'Common
;;                 :key "C-x C-c"
;;                 :description "leave Emacs.")
;; Run (cheatsheet-show) and enjoy looking at your own Emacs cheatsheet.

;;; Code:

(require 'cl-lib)

(defgroup cheatsheet nil
  "Display a cheatsheet of emacs command."
  :group 'convenience)

(defface cheatsheet-group-face
  '((t :foreground "red"))
  "Group name font face."
  :group 'cheatsheet)

(defface cheatsheet-key-face
  '((t :foreground "orange"))
  "Cheat key font face."
  :group 'cheatsheet)


(defvar cheatsheet--cheat-list '()
  "List of cheats.")

;; Getters for CHEAT and GROUP plists
(defun cheatsheet--if-symbol-to-string (string-like)
  "Convert STRING-LIKE to string."
  (if (symbolp string-like) (symbol-name string-like) string-like))

(defun cheatsheet--group-name (group)
  "Get GROUP name."
  (cheatsheet--if-symbol-to-string (plist-get group :name)))

(defun cheatsheet--group-cheats (group)
  "Get GROUP cheats."
  (cheatsheet--if-symbol-to-string (plist-get group :cheats)))

(defun cheatsheet--cheat-key (cheat)
  "Get CHEAT key."
  (cheatsheet--if-symbol-to-string (plist-get cheat :key)))

(defun cheatsheet--cheat-group (cheat)
  "Get CHEAT group."
  (cheatsheet--if-symbol-to-string (plist-get cheat :group)))

(defun cheatsheet--cheat-description (cheat)
  "Get CHEAT description."
  (cheatsheet--if-symbol-to-string (plist-get cheat :description)))

(defun cheatsheet--cheat-name (cheat)
  ; this will be useful later
  "Get CHEAT name."
  (cheatsheet--if-symbol-to-string (plist-get cheat :name)))

;; Functions to get data from CHEATSHEET in convenient format
(defun cheatsheet--cheat-groups ()
  "Get all groups, submitted to cheatsheet."
  (reverse (delete-dups
            (mapcar 'cheatsheet--cheat-group
                    cheatsheet--cheat-list))))

(defun cheatsheet--get-group (group)
  "Get group struct with all cheats, belonging to GROUP."
  (cl-flet ((is-current-group (cheat)
                              (if (string= (cheatsheet--cheat-group cheat)
                                           group)
                                  cheat
                                nil)))
    (delq nil (mapcar #'is-current-group cheatsheet--cheat-list))))

;; Functions to format cheatsheet items and prepare to print
(defun cheatsheet--format-cheat (cheat key-cell-length)
  "Format CHEAT row with KEY-CELL-LENGTH key cell length."
  (let* ((format-string (format "%%%ds - %%s\n" key-cell-length))
         (key (cheatsheet--cheat-key cheat))
         (description (cheatsheet--cheat-description cheat))
         (faced-key (propertize key 'face 'cheatsheet-key-face)))
    (format format-string faced-key description)))

(defun cheatsheet--format-group (group)
  "Format GROUP to table."
  (cl-flet ((key-length (cheat) (length (cheatsheet--cheat-key cheat)))
            (format-cheat (key-cell-length cheat)
                          (cheatsheet--format-cheat cheat key-cell-length)))

    (let* ((name (cheatsheet--group-name group))
           (cheats (cheatsheet--group-cheats group))
           (key-max-length (apply 'max (mapcar #'key-length cheats)))
           (key-cell-length (+ 2 key-max-length))
           (format-cheat (apply-partially #'format-cheat key-cell-length))
           (formatted-cheats (apply 'concat (mapcar format-cheat cheats)))
           (faced-group-name (propertize name 'face 'cheatsheet-group-face)))
      (concat faced-group-name "\n" formatted-cheats "\n"))))

(defun cheatsheet--format ()
  "Print the whole cheatsheet."
  (let* ((cheatsheet (cheatsheet-get))
         (formatted-groups (mapcar 'cheatsheet--format-group cheatsheet))
         (formatted-cheatsheet (apply 'concat formatted-groups)))
    formatted-cheatsheet))

(defvar cheatsheet--modifiedp nil
  "Keep track of whether the cheatsheet list has been modified.")

(defvar cheatsheet--path-to-list-file
  "/Users/andrewdeangelis/.emacs.d/elpa/cheatsheet-20170126.2150/cheatsheet.el"
  "Variable for the path to the file where the cheatsheet list is kept")

(defvar set-list-rgx "^(setq cheatsheet--cheat-list '(+.*)+"
  "regex used to find where the value of our cheat list is set")

;; can be used to ensure the list is saved when exiting emacs,
;; by adding:
;; (add-hook 'kill-emacs-hook #'cheatsheet--save-list-to-elisp-file)
;; to the init file
;;;###autoload
(defun cheatsheet-save-list-to-elisp-file ()
  "Save the current cheatsheet list.
Check if the list has been modified, and if so,
re-write the 'setq' call in this file"
  (if cheatsheet--modifiedp
      (with-current-buffer (find-file-noselect cheatsheet--path-to-list-file)
	;; (replace-regexp set-list-rgx
	;; 		(format "(setq cheatsheet--cheat-list '%s)"
	;; 			cheatsheet--cheat-list)))))
	(re-search-forward set-list-rgx)
	(replace-match (format "(setq cheatsheet--cheat-list '%s)"
			       (if cheatsheet--cheat-list
				   (prin1-to-string cheatsheet--cheat-list)
				 "()")))
	(save-buffer)
	(setq cheatsheet--modifiedp nil))))

(defun cheatsheet-clear-list ()
  (interactive)
  (setq cheatsheet--cheat-list '())
  (setq cheatsheet--modifiedp t))

;; this variable is custom-set by the above function.
;; It should not be broken up with newlines
;; (unless you want to change the above regexp accordingly)
(setq cheatsheet--cheat-list '((:group Common :key "C-x C-c" :description "leave Emacs.")))

;; it will probably be better to instead store the list in its own file
(defun get-lisp-object-from-file (filePath)
  (with-temp-buffer
    (insert-file-contents filePath)
    (read (current-buffer))))

;; Interface
;;;###autoload
(defun cheatsheet-add (&rest cheat)
  "Add CHEAT to cheatsheet."
  (setq cheatsheet--modifiedp t)
  (add-to-list 'cheatsheet--cheat-list cheat))

(defun cheatsheet-get ()
  "Get cheatsheet as list of group structs, keeping defining order."
  (cl-flet ((make-group (group)
                        (list :name group
                              :cheats (cheatsheet--get-group group))))
    (mapcar #'make-group (cheatsheet--cheat-groups))))

;;;###autoload
(defun cheatsheet-add-group (group &rest cheats)
  "Add cheats to the same group."
  (mapcar #'(lambda (cheat)
              (apply 'cheatsheet-add
                     (append `(:group ,group) cheat)))
          cheats))

;;;###autoload
(defun cheatsheet-show ()
  "Create buffer and show cheatsheet."
  (interactive)
  (switch-to-buffer-other-window "*cheatsheet*")
  (cheatsheet-mode)
  (erase-buffer)
  (insert (cheatsheet--format))
  (setq buffer-read-only t))

(define-derived-mode cheatsheet-mode fundamental-mode "Cheat Sheet"
  "Set major mode for viewing cheat sheets.")

(define-key cheatsheet-mode-map (kbd "C-q") 'kill-buffer-and-window)

(provide 'cheatsheet)
;;; cheatsheet.el ends here
