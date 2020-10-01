(require 'signpost)
(require 'json)

(let ((log-obj (signpost-log-create "json-read")))
  (dotimes (_ 10000)
    (with-signpost log-obj
      (json-read-file "data/test.json"))))
