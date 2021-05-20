(require 'org)
(require 'ox)
(require 'ob-shell)

(setq eserver-blog (expand-file-name "blog" eserver-root))

;; Do not show temporary buffer after export org to html
(setq org-export-show-temporary-export-buffer nil)
;; Evaluate codeblocks without confirmation when export
(setq org-confirm-babel-evaluate nil)

(defun httpd/blog (proc path &rest args)
  (let* ((default-directory eserver-blog)
         (available-files (directory-files-recursively
                           ;; currently, only .org is allowed to visit
                           "." (rx ".org" eos))))
    ;; if path ends with directory, then use index.org under path
    (if (string-suffix-p "/" path)
        (setq path (concat path "index.org")))
    (if (string-equal path "/blog")
        (setq path "/blog/index.org"))
    (setq path (concat "." (string-remove-prefix "/blog" path)))
    (httpd-log `(getting file ,path))
    (if (not (member path available-files))
        ;; file not exist or not allowed to visit
        (with-httpd-buffer proc "text/plain"
          (insert "ERROR: This file does not exist.\nAvailable files are:\n")
          (dolist (file available-files)
            (insert (format "  %s\n" file))))
      (let ((buffer (get-buffer-create "*tbt: org html*")))
        (with-temp-buffer
          (insert-file-contents path)
          ;; (httpd-log `(inserted file contents under ,path))
          (org-export-to-buffer 'html buffer))
        (with-httpd-buffer proc "text/html"
          (insert-buffer buffer))))))
