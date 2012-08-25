;;; helm-mercurial.el --- helm interface for mercurial qpatch.
;;
;; Copyright (C) 2008, 2009 Thierry Volpiatto, all rights reserved
;;
;; Filename: helm-mercurial.el
;; Description: 
;; Author: thierry
;; Maintainer: 
;; URL: 
;; Keywords: 

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Need DVC and helm-config.el.
 
;;; Code:
(require 'xhg)
(require 'helm-config)

;; Internal use only!
(defvar helm-qapplied-alist nil)
(defvar helm-c-qpatch-directory nil)
(defvar helm-c-qunpatch-directory nil)
(defvar helm-c-qapplied-show-headers t)
(defvar helm-c-qunapplied-show-headers nil)

;; User variables
(defvar helm-hg-default-export-fname ".hg-helm-export"
  "*You can put in this file the path of your usual dir to export patchs.")

;; Clean up
(add-hook 'helm-before-initialize-hook #'(lambda ()
                                               (setq helm-c-qunpatch-directory nil)))

(add-hook 'helm-before-initialize-hook #'(lambda ()
                                               (setq helm-qapplied-alist nil)))

(add-hook 'helm-before-initialize-hook #'(lambda ()
                                               (setq helm-c-qpatch-directory nil)))


;;; Applied patchs
(defun helm-hg-init-applied ()
  (condition-case nil
      (setq helm-c-qpatch-directory
            (xhg-tree-root (expand-file-name
                            (if (eq major-mode 'dired-mode)
                                (dired-current-directory)
                                default-directory))))
    (error nil)))

(defun helm-hg-applied-candidates ()
  (condition-case nil
      (let ((applied-patchs
             (with-temp-buffer
               (apply #'call-process "hg" nil t nil
                      (if helm-c-qapplied-show-headers
                          `("qapplied" "-s" "-R" ,helm-c-qpatch-directory)
                          `("qapplied" "-R" ,helm-c-qpatch-directory)))
               (buffer-string)))
            (top
             (with-temp-buffer
               (apply #'call-process "hg" nil t nil
                      `("tip" "--template" "{rev}" "-R" ,helm-c-qpatch-directory))
               (buffer-string))))
        (setq top (string-to-number top))
        (setq applied-patchs (remove "" (split-string applied-patchs "\n" t)))
        (setq helm-qapplied-alist
              (loop for i in (reverse applied-patchs)
                 collect (list i top)
                 and do
                 (incf top -1)))
        (unless (or (string-match "abort:" (car applied-patchs))
                    (zerop (length applied-patchs)))
          (setq applied-patchs (reverse applied-patchs))))
    (error nil)))

(defun helm-hg-applied-persistent-action (elm)
  (let ((default-directory helm-c-qpatch-directory))
    (xhg-qpop)
    (helm-delete-current-selection)))

(defun helm-hg-applied-show-patch (elm)
  (let ((default-directory helm-c-qpatch-directory))
    (if (helm-hg-need-refresh)
        (xhg-qdiff)
        (xhg-log (cadr (assoc elm helm-qapplied-alist))
                 nil t default-directory))))

(defun helm-hg-applied-refresh (elm)
  (let ((default-directory helm-c-qpatch-directory))
    (xhg-qrefresh)))

(defun helm-hg-applied-rename-header (elm)
  (let ((default-directory helm-c-qpatch-directory)
        (win-conf (current-window-configuration)))
    (xhg-qrefresh-header)
    (save-window-excursion
      (when (get-buffer "*xhg-log*")
        (kill-buffer "*xhg-log*"))
      (xhg-log
       (cadr (assoc elm helm-qapplied-alist)) nil t))
    (save-excursion
      (display-buffer "*xhg-log*"))
    (dvc-buffer-push-previous-window-config win-conf)))

(defun helm-hg-applied-qnew (elm)
  (let ((default-directory helm-c-qpatch-directory)
        (patch (xhg-qnew-name-patch)))
    (xhg-qnew patch "New patch")
    (message "Now at patch `%s'" patch)))

(defun helm-hg-applied-export (elm)
  (let* ((default-directory helm-c-qpatch-directory)
         (abs-export-fname (expand-file-name
                            helm-hg-default-export-fname
                            helm-c-qpatch-directory))
         (export-dir-name (if (file-exists-p abs-export-fname)
                              (with-temp-buffer
                                (insert-file-contents abs-export-fname)
                                (replace-regexp-in-string "\n" "" (buffer-string)))
                              helm-c-qpatch-directory))
         (patch-name (expand-file-name (if (string-match "^patch-r[0-9]+" elm)
                                           (match-string 0 elm)
                                           "Initial-patch")
                                       export-dir-name)))
    (if (file-exists-p patch-name)
        (error "Error: Patch `%s' already exists" (helm-c-basename patch-name))
        (xhg-export
         (int-to-string (cadr (assoc elm helm-qapplied-alist)))
         (read-from-minibuffer "Destination: "
                               nil nil nil nil
                               patch-name)))))

(defun helm-hg-applied-export-via-mail (elm)
  (let ((default-directory helm-c-qpatch-directory))
    (xhg-export-via-mail
     (int-to-string (cadr (assoc elm helm-qapplied-alist))))))

(defun helm-hg-applied-apply-all-patchs (elm)
  (let ((default-directory helm-c-qpatch-directory))
    (xhg-qconvert-to-permanent)))

(defun helm-hg-applied-uniquify (elm)
  (let* ((default-directory helm-c-qpatch-directory)
         (patch-name (if (string-match "^patch-r[0-9]+" elm)
                         (match-string 0 elm)
                         "Initial-patch"))
         (dest (concat (file-name-as-directory
                        helm-c-qpatch-directory)
                       "Single"
                       patch-name
                       "ToTip.patch")))
    (if (file-exists-p dest)
        (error "Error: Patch `%s' already exists" dest)
        (xhg-qsingle dest patch-name))))

(defun helm-hg-applied-export-single-via-mail (elm)
  (let ((patch-name (if (string-match "^patch-r[0-9]+" elm)
                        (match-string 0 elm)
                        "Initial-patch"))
        (default-directory helm-c-qpatch-directory))
    (xhg-mq-export-via-mail patch-name t)))

(defun helm-hg-applied-qpop (elm)
  (let ((default-directory helm-c-qpatch-directory))
    (xhg-qpop)))

(defun helm-hg-applied-qpop-all (elm)
  (let ((default-directory helm-c-qpatch-directory))
    (xhg-qpop t)))

(defun helm-hg-need-refresh ()
  (not (string= (shell-command-to-string "hg diff") "")))

(defun helm-hg-applied-transformer (candidates source)
  (let ((state (helm-hg-need-refresh))
        (cur-patch (car candidates)))
    (if state
        (append (list (cons (concat "[R] "
                                    (propertize
                                     cur-patch
                                     'face 'font-lock-comment-face))
                            cur-patch))
                (cdr candidates))
        candidates)))

(defvar helm-c-source-qapplied-patchs
  '((name . "Hg Qapplied Patchs")
    (volatile)
    (init . helm-hg-init-applied)
    (candidates . helm-hg-applied-candidates)
    (filtered-candidate-transformer . helm-hg-applied-transformer)
    (persistent-action . helm-hg-applied-persistent-action)
    (action . (("Show Patch" . helm-hg-applied-show-patch)
               ("Hg Qrefresh" . helm-hg-applied-refresh)
               ("Rename Header" . helm-hg-applied-rename-header)
               ("hg rename patch" . helm-hg-qrename)
               ("Hg Qnew" . helm-hg-applied-qnew)
               ("Export" . helm-hg-applied-export)
               ("Export via Mail" . helm-hg-applied-export-via-mail)
               ("Apply all patchs" . helm-hg-applied-apply-all-patchs)
               ("Uniquify all patchs from rev" . helm-hg-applied-uniquify)
               ("Export Single Patch via mail"
                . helm-hg-applied-export-single-via-mail)
               ("Hg-Qpop (top of stack)" . helm-hg-applied-qpop)
               ("Hg-Qpop-All" . helm-hg-applied-qpop-all)))))
;; (helm 'helm-c-source-qapplied-patchs)


;;; Unapplied patchs
(defun helm-c-qunapplied-delete (elm)
  (if helm-c-qunapplied-show-headers
      (progn
        (string-match "^patch-r[0-9]+" elm)
        (xhg-qdelete (match-string 0 elm)))
      (xhg-qdelete elm)))

(defun helm-hg-unapplied-init ()
  (condition-case nil
      (setq helm-c-qunpatch-directory
            (xhg-tree-root (expand-file-name
                            (if (eq major-mode 'dired-mode)
                                (dired-current-directory)
                                default-directory))))
    (error nil)))

(defun helm-hg-unapplied-candidates ()
  (condition-case nil
      (let ((unapplied-patchs
             (with-temp-buffer
               (apply #'call-process "hg" nil t nil
                      (if helm-c-qunapplied-show-headers
                          `("qunapplied" "-s" "-R" ,helm-c-qunpatch-directory)
                          `("qunapplied" "-R" ,helm-c-qunpatch-directory)))
               (buffer-string))))
        (setq unapplied-patchs (split-string unapplied-patchs "\n" t))
        (unless (or (string-match "abort:" (car unapplied-patchs))
                    (zerop (length unapplied-patchs)))
          unapplied-patchs))
    (error nil)))

(defun helm-hg-unapplied-persistent-action (elm)
  (let ((default-directory helm-c-qpatch-directory))
    (xhg-qpush)
    (helm-delete-current-selection)))

(defun helm-hg-unapplied-push (elm)
  (let ((default-directory helm-c-qpatch-directory))
    (xhg-qpush)))

(defun helm-hg-unapplied-push-all (elm)
  (let ((default-directory helm-c-qpatch-directory))
    (xhg-qpush t)))

(defun helm-hg-unapplied-delete (elm)
  (let ((default-directory helm-c-qpatch-directory))
    (dolist (i (helm-marked-candidates))
      (helm-c-qunapplied-delete i))))

(defun helm-hg-qrename (elm)
  (let ((default-directory helm-c-qpatch-directory))
    (xhg-qrename elm (read-string "New patch name: " elm))))

(defvar helm-c-source-qunapplied-patchs
  '((name . "Hg Qunapplied Patchs")
    (volatile)
    (init . helm-hg-unapplied-init)
    (candidates . helm-hg-unapplied-candidates)
    (persistent-action . helm-hg-unapplied-persistent-action)
    (action . (("hg-qpush" . helm-hg-unapplied-push)
               ("hg-qpush-all" . helm-hg-unapplied-push-all)
               ("hg rename patch" . helm-hg-qrename)
               ("hg-qdelete patch(s)" . helm-hg-unapplied-delete)))))

;;;###autoload
(defun helm-qpatchs ()
  (interactive)
  (helm :sources '(helm-c-source-qapplied-patchs
                   helm-c-source-qunapplied-patchs)
        :buffer "*helm hg qpatchs*"))

(provide 'helm-mercurial)


;;; helm-mercurial.el ends here
