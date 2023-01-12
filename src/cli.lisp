(in-package :dereport)

(opts:define-opts
    (:name :help
           :description "print this help"
           :short #\h
           :long "help")
    (:name :input
           :description "Read from file"
           :short #\i
           :long "input"
           :arg-parser #'identity
           :meta-var "FILE"))

(defun handle-arg-condition (condition)
  "Return information when there was a problem parsing the arguments"
  (format t "Problem while parsing option ~s: ~a .~%" (opts:option condition) condition)
  (opts:describe))

(defun get-cli-arguments ()
  "Gets arguments when CLI is called"
  (multiple-value-bind (arguments)
      (handler-bind ((opts:unknown-option #'handle-arg-condition)
                     (opts:arg-parser-failed #'handle-arg-condition)
                     (opts:missing-arg #'handle-arg-condition))
         (opts:get-opts))
    arguments))

(defun main ()
  (load-init-file #P"~/.config/dereport/cli.lisp")
  (let ((arguments (get-cli-arguments)))
    (if (getf arguments :help)
        (opts:describe)
        (if-let (input-file (uiop:file-exists-p (getf arguments :input)))
          (with-open-file (stream input-file)
            (get-tasks-per-category-from-stream stream))
          (get-tasks-per-category-from-stream)))))
