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
;; (helm 'helm-c-source-qunapplied-patchs)

;;; List hg files in hg project.
;;
;;
(defvar helm-hg-files-cache (make-hash-table :test 'equal))
(defvar helm-ls-hg-default-directory nil)
(defvar helm-ls-hg-status-command 'vc-dir)

(defun helm-hg-root ()
  (with-temp-buffer
    (when (= 0 (call-process "hg" nil t nil "root"))
      (file-name-as-directory
       (replace-regexp-in-string "\n" "" (buffer-string))))))

(defun helm-hg-root-p (candidate)
  ;; Check for file existence in case of creation
  ;; of file or directory.
  (when (or (file-exists-p candidate)
            (file-directory-p candidate))
  (let ((default-directory (if (file-directory-p candidate)
                               (file-name-as-directory candidate)
                               (file-name-as-directory
                                helm-ff-default-directory))))
    (stringp (helm-hg-root)))))

(defun helm-hg-list-files ()
  (let ((dir (helm-hg-root)))
    (if (and dir (file-directory-p dir))
        (helm-aif (gethash dir helm-hg-files-cache)
            it
          (with-temp-buffer
            (call-process "hg" nil t nil "manifest")
            (loop with ls = (split-string (buffer-string) "\n" t)
                  for f in ls
                  collect (concat dir f) into tmpls
                  finally return (puthash dir tmpls helm-hg-files-cache))))
        (error "Error: Not an hg repo (no .hg found)"))))

(defvar helm-c-source-hg-list-files
  `((name . "Hg files list")
    (init . (lambda ()
              (helm-init-candidates-in-buffer
               "*helm hg*" (helm-hg-list-files))))
    (keymap . ,helm-generic-files-map)
    (candidates-in-buffer)
    (type . file)))

(defun helm-ff-hg-find-files (candidate)
  (let ((default-directory (file-name-as-directory
                            (if (file-directory-p candidate)
                                (expand-file-name candidate)
                                (file-name-directory candidate)))))
    (helm-run-after-quit
     #'(lambda (d)
         (let ((default-directory d))
           (helm-hg-find-files-in-project)))
     default-directory)))

(defun helm-ls-hg-status ()
  (with-output-to-string
      (with-current-buffer standard-output
        (apply #'process-file
               "hg"
               nil t nil
               (list "status")))))

(defvar helm-c-source-ls-hg-status
  '((name . "Hg status")
    (init . (lambda ()
              (helm-init-candidates-in-buffer
               "*hg status*"
               (helm-ls-hg-status))))
    (candidates-in-buffer)
    (filtered-candidate-transformer . helm-ls-hg-status-transformer)
    (action-transformer . helm-ls-hg-status-action-transformer)
    (persistent-action . helm-ls-hg-diff)
    (persistent-help . "Diff")
    (action . (("Find file" . helm-find-many-files)
               ("Hg status" . (lambda (_candidate)
                                 (funcall helm-ls-hg-status-command
                                          (helm-hg-root))))))))

(defun helm-ls-hg-status-transformer (candidates source)
  (loop with root = (let ((default-directory
                           (or helm-ls-hg-default-directory
                               (with-helm-current-buffer
                                 default-directory))))
                      (helm-hg-root))
        for i in candidates
        collect
        (cond ((string-match "^\\(M \\)\\(.*\\)" i)
               (cons (propertize i 'face '((:foreground "yellow")))
                     (expand-file-name (match-string 2 i) root)))
               ((string-match "^\\([?] \\{1\\}\\)\\(.*\\)" i)
                (cons (propertize i 'face '((:foreground "red")))
                      (expand-file-name (match-string 2 i) root)))
               ((string-match "^\\([ARC] ?+\\)\\(.*\\)" i)
                (cons (propertize i 'face '((:foreground "green")))
                      (expand-file-name (match-string 2 i) root)))
               ((string-match "^\\([!] \\)\\(.*\\)" i)
                (cons (propertize i 'face '((:foreground "Darkgoldenrod3")))
                      (expand-file-name (match-string 2 i) root)))
               (t i))))

(defvar helm-ls-vc-delete-buffers-list nil)
(defun helm-ls-vc-commit (candidate backend)
  (let* ((marked (helm-marked-candidates))
         (default-directory
          (file-name-directory (car marked))))
    (loop for f in marked
          unless (or (find-buffer-visiting f)
                     (not (file-exists-p f)))
          do (push (find-file-noselect f)
                   helm-ls-vc-delete-buffers-list))
    (add-hook 'vc-checkin-hook 'helm-vc-checkin-hook)
    (vc-checkin marked backend)))

(defun helm-vc-checkin-hook ()
  (when helm-ls-vc-delete-buffers-list
    (loop for b in helm-ls-vc-delete-buffers-list
          do (kill-buffer b)
          finally (setq helm-ls-vc-delete-buffers-list nil))))

(defun helm-ls-hg-commit (candidate)
  (helm-ls-vc-commit candidate 'Hg))

(defun helm-ls-hg-status-action-transformer (actions candidate)
  (let ((disp (helm-get-selection nil t)))
    (cond ((string-match "^[?]\\{1\\}" disp)
           (append actions
                   (list '("Add file(s)"
                           . (lambda (candidate)
                               (let ((default-directory
                                      (file-name-directory candidate))
                                     (marked (helm-marked-candidates)))
                                 (vc-hg-register marked)))))))
          ((string-match "^M" disp)
           (append actions (list '("Diff file" . helm-ls-hg-diff)
                                 '("Commit file(s)" . helm-ls-hg-commit)
                                 '("Revert file" . vc-hg-revert))))
          ((string-match "^[!]" disp)
           (append actions (list '("Hg delete"
                                   . (lambda (candidate)
                                       (let ((default-directory
                                              (file-name-directory candidate))
                                             (marked (helm-marked-candidates)))
                                         (loop for f in marked
                                               do (vc-hg-delete-file f))))))))
          (t actions))))

(defun helm-ls-hg-diff (candidate)
  (with-current-buffer (find-file-noselect candidate)
    (call-interactively #'vc-diff)))

;;;###autoload
(defun helm-hg-find-files-in-project ()
  (interactive)
  (setq helm-ls-hg-default-directory default-directory)
  (unwind-protect
       (helm :sources '(helm-c-source-ls-hg-status
                        helm-c-source-hg-list-files)
             :buffer "*helm hg files*")
    (setq helm-ls-hg-default-directory nil)))

;;;###autoload
(defun helm-qpatchs ()
  (interactive)
  (helm :sources '(helm-c-source-qapplied-patchs
                   helm-c-source-qunapplied-patchs)
        :buffer "*helm hg qpatchs*"))

(provide 'helm-mercurial)


;;; helm-mercurial.el ends here
