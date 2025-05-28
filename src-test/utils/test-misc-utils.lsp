
(in-package :misc-utils)

(test-lib:test-fn "boolean-p"
  (boolean-p t)
  (boolean-p nil)
  (not (boolean-p 3))
)

(test-lib:test-fn "to-keyword"
  (test-lib:test-equal :hello (to-keyword "hello"))
  (test-lib:test-equal :hello (to-keyword 'hello))
  (test-lib:test-equal :hello (to-keyword :hello))
)
