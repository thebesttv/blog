(require 'simple-httpd)

(setq httpd-host "0.0.0.0")
(httpd-start)

(require 'org)
(require 'ox)
(require 'ob-shell)

;; (setq root-dir (directory-file-name (buffer-file-name)))

;; Do not show temporary buffer after export org to html
(setq org-export-show-temporary-export-buffer nil)
;; Evaluate all codeblocks when export
(setq org-confirm-babel-evaluate nil)

(defun httpd/blog (proc path &rest args)
  (let ((available-files (directory-files-recursively
                          "." (rx ".org" eos))))
    ;; if path ends with directory, then use index.org under path
    (if (string-suffix-p "/" path)
        (setq path (concat path "index.org")))
    (if (string-equal path "/blog")
        (setq path "/blog/index.org"))
    (setq path (concat "." (string-remove-prefix "/blog" path)))
    (message (format "trying to get file %s" path))
    (if (not (member path available-files))
        (with-httpd-buffer proc "text/plain"
          (insert "ERROR: This file does not exist.\nAvailable files are:\n")
          (dolist (file available-files)
            (insert (format "  %s\n" file))))
      (let ((buffer (get-buffer-create "*tbt: org html*")))
        (with-temp-buffer
          (insert-file-contents path)
          (org-export-to-buffer 'html buffer))
        (with-httpd-buffer proc "text/html"
          (insert-buffer buffer))))))
