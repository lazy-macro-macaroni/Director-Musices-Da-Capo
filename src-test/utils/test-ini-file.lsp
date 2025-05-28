
(in-package :ini-file)

(test-lib:test-macro "read-file"
  (let* ((def
          (ini-definition:create
            '((:test-setting-string :string "")
              (:test-setting-int :int 0)
              (:test-setting-float :float 0.0)
              (:test-setting-string-two :string "")
              (:test-setting-string-default :string "default value")
              (:test-setting-multiline-string :string "")

              (:test-setting-form :form "")
              (:test-setting-form-two :form "")
            )
            '(:testini-testfile :testini-testfile-two)))
         (file (ini-file:create def)))
    (read-ini-from-file file (file-utils:jfile "." "src-test" "utils" "testini.ini"))

    (test-lib:tests
      (test-lib:test-equal (get-setting file :test-setting-string) "hello world")
      (test-lib:test-equal (get-setting file :test-setting-int) 394)
      (test-lib:test-equal (get-setting file :test-setting-float) 6.32)
      (test-lib:test-equal (get-setting file :test-setting-string-two) "bye world")
      (test-lib:test-equal '(1 2 3) (get-setting file :test-setting-form))
      (test-lib:test-equal '(1 (2 3 (4 5))) (get-setting file :test-setting-form-two))
      (test-lib:test-equal (get-setting file :test-setting-string-default) "default value")
      (test-lib:test-equal (get-setting file :test-setting-multiline-string) (globals:format-string "hello~%world"))

      (test-lib:test-equal
        '("hello world" "hello world number 2!")
        (ini-file:get-files file :TESTINI-TESTFILE))

      (test-lib:test-equal
        '("hello two")
        (ini-file:get-files file :TESTINI-TESTFILE-two))
    )))
