(in-package :dereport)

(defvar *prefixes-per-category* '(("Done" . ("- [X]" "- [x]" "*** DONE" "*** WAITING" "*** DELEGATED"))
                     ("Discarded" . ("*** CANCELLED" "*** FORWARDED"))
                     ("Doing" . ("- [-]" "- [o]" "*** DOING"))
                     ("Next" . ("- [ ]" "*** TODO" "*** NEXT")))
  "Association List with the group and the list of patterns to group them")

(defun sanitize-task (task prefix)
  (-<> task
    (str:trim)
    (replace-first prefix "-" <>) ; Homologate prefix
    (regex-replace-all " :[\\w:]+:$" <> "") ; Remove org-mode tags
    (regex-replace-all "\\[(\\w+)\\]\\(\\w+\\)" <> "\\1") ; Markdown links
    (regex-replace-all "\\[\\[(\\w+)\\]\\[(\\w+)]\\]" <> "\\2"))) ; Org-mode links

(defun get-valid-prefix (task prefixes)
  "Returns the prefix that applies to the given TASK"
  (loop :for prefix :in prefixes
        :when (str:starts-with-p prefix task)
        :return prefix))

(defun identify-category-prefix (task)
  "Returns the category and prefix that applies to the given TASK"
  (loop :for (category . prefixes) :in *prefixes-per-category*
        :for prefix = (get-valid-prefix task prefixes)
        :when prefix
        :return (values category prefix)))

(defun shorter-p (a b)
  (< (length a) (length b)))

(defun print-tasks-per-category (category tasks)
  "Print the TASKS grouped by the CATEGORY"
  (format t "~&~a:~&~{~a~&~}~%" category (sort tasks #'shorter-p)))

(defun get-tasks-per-category-from-stream (&optional (stream *standard-input*))
  (let ((tasks-per-category (make-hash-table :test #'equal)))
    (loop :for line = (read-line stream nil :eof)
          :until (eq line :eof)
          :do (multiple-value-bind (category prefix) (identify-category-prefix line)
                (when category
                  (setf (gethash category tasks-per-category)
                        (adjoin (sanitize-task line prefix)
                                (gethash category tasks-per-category) :test #'equal)))))
    (maphash #'print-tasks-per-category tasks-per-category)
    tasks-per-category))
