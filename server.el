(require 'org)
(require 'ox)
(require 'ob-shell)

(defcustom eserver-blog (expand-file-name "blog" eserver-root)
  "Root directory of blog."
  :group 'eserver
  :type 'directory)

;; Do not show temporary buffer after export org to html
(setq org-export-show-temporary-export-buffer nil)
;; Evaluate codeblocks without confirmation when export
(setq org-confirm-babel-evaluate nil)

(defun eserver-blog-available-files ()
  "Return a list of available files under `eserver-blog'."
  (let ((default-directory eserver-blog))
    (directory-files-recursively
     "."                                ; scan current directory
     ;; match regexp
     (rx string-start
         ;; do not start with "." (hidden file, current / parent dir)
         (any alnum ?- ?_ ? )
         (* (any alnum ?- ?_ ?  ?.))       ; a-z A-Z 0-9 - _ space .
         string-end)
     nil                 ; do not include directory
     (lambda (file)      ; do not enter / use hidden directory or file
       ;; (not (and (file-name-directory file)
       ;;           (string-prefix-p "." (file-name-base file))))))))
       (not (string-prefix-p "." (file-name-base file)))))))

(defun httpd/blog (proc path &rest args)
  (let* ((default-directory eserver-blog)
         (available-files (eserver-blog-available-files)))
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
      ;; file exists, check for extension
      (if (string-suffix-p ".org" path)
          ;; if .org file, convert to html before sending
          (let ((buffer (get-buffer-create "*tbt: org html*")))
            (with-temp-buffer
              (insert-file-contents path)
              ;; (httpd-log `(inserted file contents under ,path))
              (org-export-to-buffer 'html buffer))
            (with-httpd-buffer proc "text/html"
              (insert-buffer buffer)))
        ;; regular file, simple send file
        (httpd-send-file proc path)))))

