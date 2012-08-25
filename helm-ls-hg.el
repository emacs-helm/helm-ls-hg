;;; helm-ls-hg.el --- List hg files in hg project.

;; Copyright (C) 2012 Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code

(require 'helm-locate)
(require 'helm-files)

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
        (with-temp-buffer
          (call-process "hg" nil t nil "manifest")
          (loop with ls = (split-string (buffer-string) "\n" t)
                for f in ls
                collect (concat dir f)))
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

(provide 'helm-ls-hg)

;;; helm-ls-hg.el ends here
