(in-package :dereport)

(defvar *prefixes-per-category* '(("Done" . ("- [x]"))
                     ("Doing" . ("- [-]"))
                     ("Next" . ("- [ ]")))
  "Association List with the group and the list of patterns to group them")

(defvar *regex-replacements* '(("\\[(\\w+)\\]\\(\\w+\\)" . "\\1")) ; Markdown links
  "List of Regex replacements")

(defun sanitize-task (task prefix)
  "Clean the TASK and return it without unnecesary context, removing the PREFIX"
  (-<>> task
    (str:trim)
    (replace-first prefix "-") ; Homologate prefix
    (do ((replacement-list *regex-replacements* (cdr replacement-list))
         (sanitized-task <> (str:trim (regex-replace-all (caar replacement-list) sanitized-task (cdar replacement-list)))))
        ((null replacement-list) sanitized-task))))

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

(defun print-tasks-per-category (category tasks)
  "Print the TASKS grouped by the CATEGORY"
  (labels ((less-p (a b) (< (length a) (length b))))
    (format t "~&~a:~&~{~a~&~}~%" category (sort tasks #'less-p))))

(defun get-tasks-per-category-from-stream (&optional (stream *standard-input*))
  "Split tasks per category from the given STREAM or the STDIN"
  (let ((tasks-per-category (make-hash-table :test #'equal)))
    (loop :for line = (read-line stream nil :eof)
          :until (eq line :eof)
          :do (multiple-value-bind (category prefix) (identify-category-prefix line)
                (when category
                  (let ((sanitized-task (sanitize-task line prefix))
                        (processed-tasks (gethash category tasks-per-category)))
                    (setf (gethash category tasks-per-category)
                          (adjoin sanitized-task processed-tasks :test #'equal))))))
    (maphash #'print-tasks-per-category tasks-per-category)
    tasks-per-category))
