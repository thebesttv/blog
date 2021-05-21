(require 'org)
(require 'ox)
(require 'ob-shell)

(defcustom eserver-blog (expand-file-name "blog" eserver-root)
  "Root directory of blog."
  :group 'eserver
  :type 'directory)

(defcustom eserver-blog-image-dataurl 'nil
  "If t, output image as base64 dataurl inside html."
  :group 'eserver
  :type 'boolean)

(defvar eserver-blog-current-path ""
  "Current .org document path.")

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
      (if (not (string-suffix-p ".org" path))
          ;; regular file, simple send file
          (httpd-send-file proc path)
        ;; if .org file, convert to html before sending
        (setq eserver-blog-current-path path)
        (let ((buffer (get-buffer-create "*tbt: org html*")))
          (with-temp-buffer
            (insert-file-contents path)
            ;; (httpd-log `(inserted file contents under ,path))
            (org-export-to-buffer 'html buffer))
          (with-httpd-buffer proc "text/html"
            (insert-buffer buffer)))))))

(defun base64-image-encode-cons (file)
  "Return a cons cell whose car is image type and cdr is base64 encoded string.
This functions uses `image-type-from-file-header' to determine
image type.  If unable to determine image type or file does not
exist, it returns (nil . nil)."
  (let ((type (image-type-from-file-header file)))
    (if (null type)
        (cons nil nil)
      (cons (symbol-name type)
            (with-temp-buffer
              (insert-file-contents-literally file)
              (base64-encode-region (point-min) (point-max) t)
              (buffer-string))))))

(defun html-image-dataurl-src (source)
  (let* ((image-cons-cell
          (base64-image-encode-cons
           (expand-file-name source
                             (file-name-directory
                              eserver-blog-current-path))))
         (image-type (car image-cons-cell))
         (image-data (cdr image-cons-cell)))
    (if (not (and image-type image-data))
        source
      (format "data:image/%s;base64,%s"
              image-type image-data))))

(defun org-html--format-image (source attributes info)
  "Return \"img\" tag with given SOURCE and ATTRIBUTES.
SOURCE is a string specifying the location of the image.
ATTRIBUTES is a plist, as returned by
`org-export-read-attribute'.  INFO is a plist used as
a communication channel."
  (when eserver-blog-image-dataurl
    (setq source (html-image-dataurl-src source)))
  (if (string= "svg" (file-name-extension source))
      (org-html--svg-image source attributes info)
    (org-html-close-tag
     "img"
     (org-html--make-attribute-string
      (org-combine-plists
       (list :src source
	     :alt (if (string-match-p
		       (concat "^" org-preview-latex-image-directory) source)
		      (org-html-encode-plain-text
		       (org-find-text-property-in-string 'org-latex-src source))
		    (file-name-nondirectory source)))
       attributes))
     info)))

