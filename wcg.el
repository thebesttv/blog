(defmacro wcg-bind-children (node children &rest body)
  "Bind all symbols in CHILDREN to corresponding child node in NODE.
All elements in CHILDREN are either a symbol or (cons symbol
nil).  If it's only a symbol, then bind to the text
value (aka. the third element of the node).  However, if it's a
cons cell, then bind to the node."
  (declare (indent 2))
  (append (list 'let*
                (mapcar (lambda (child)
                          (if (symbolp child)
                              `(,child (cl-third (car (xml-get-children
                                                       ,node (quote ,child)))))
                            (setq child (car child))
                            `(,child (car (xml-get-children
                                           ,node (quote ,child))))))
                        (eval children)))
          body))

(defun wcg-sec-to-human (sec)
  "Convert elapsed seconds to human readable y:d:h:m:s format."
  (let (div)
    (defun div (x y)
      (let* ((res (truncate (/ x y)))
             (lft (- x (* res y))))
        (cons res lft)))
    (let* ((sec-y (* 365 24 60 60))
           (sec-d (* 24 60 60))
           (sec-h (* 60 60))
           (y (div sec sec-y))
           (d (div (cdr y) sec-d))
           (h (div (cdr d) sec-h))
           (m (div (cdr h) 60)))
      (format "%d:%03d:%02d:%02d:%02d"
              (car y) (car d) (car h) (car m) (cdr m)))))

(defun wcg-row (key value)
  "Return a HTML table row with key & value as elements."
  (format "
<tr>
  <td class=\"org-left\">%s</td>
  <td class=\"org-right\">%s</td>
</tr>"
          key value))

(defun wcg-badge (src title)
  "Return a HTML img element representing the badge."
  (format "<img src=\"%s\" alt=\"%s\" title=\"%s\" style=\"display: inline-block\">"
          src title title))

(defvar wcg-url "https://www.worldcommunitygrid.org/stat/viewMemberInfo.do?userName=%s&xml=true"
  "XML format WCG statistics")

(defvar wcg-template "
<table>
  <tbody>
    <tr>
      <th colspan=\"2\" align=\"center\">
        <img src=\"https://www.worldcommunitygrid.org/images/homepage-images/wcglogo.png\" alt=\"World Community Grid Logo\" class=\"org-center\">
      </th>
    </tr>
%s
%s
%s
%s
%s
%s

  </tbody>
</table>"
  "HTML template for WCG")

(defun wcg-html (username)
  "Return a HTML formated table of the World Community Grid statistics of the user."
  (let ((xml (with-current-buffer (url-retrieve-synchronously
                                   (format wcg-url username))
               (xml-parse-region url-http-end-of-headers)))
        badges-html)
    (if (null (xml-get-children (car xml) 'MemberStat))
        "<p>Error, try refreshing the page.</p>"
      (let* ((MemberStat (car (xml-get-children (car xml) 'MemberStat))))
        (wcg-bind-children MemberStat
            '(Name MemberId (StatisticsTotals) (Badges))
          (wcg-bind-children StatisticsTotals
              '(RunTime RunTimeRank Points PointsRank Results ResultsRank)
            (setq badges-html
                  (mapconcat (lambda (Badge)
                               (wcg-bind-children Badge '((Resource))
                                 (wcg-bind-children Resource '(Url Description)
                                   (wcg-badge Url Description))))
                             (xml-get-children Badges 'Badge) "\n"))
            (format wcg-template
                    (wcg-row "Name" Name)
                    (wcg-row "Member ID" MemberId)
                    (wcg-row "Total Run Time (y:d:h:m:s) (Rank)"
                             (format "%s (#%s)"
                                     (wcg-sec-to-human (string-to-number RunTime))
                                     RunTimeRank))
                    (wcg-row "Points Generated (Rank)"
                             (format "%s (#%s)" Points PointsRank))
                    (wcg-row "Results Returned (Rank)"
                             (format "%s (#%s)" Results ResultsRank))
                    (wcg-row "Project Badges" badges-html))))))))
