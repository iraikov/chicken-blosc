
(use chicken test blosc)

(initialize!)

(test-group "blosc basic test"
            (test-assert
             (let* ((s "0123456789")
                    (b (string->blob s))
                    (c (compress b))
                    (d (decompress c)))
               (equal? b d))))


