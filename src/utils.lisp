(in-package :dereport)

(defun load-init-file (file-path)
  "Load the FILE-PATH lisp file"
  (log:debug "Loading Init File")
  (when-let (config-file (uiop:file-exists-p file-path))
    (load config-file)
    (log:debug "Loaded file")))
