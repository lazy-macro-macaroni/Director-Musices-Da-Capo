
(in-package :ini-file)

(defun test-ini-file (file)
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
  ))

(let ((def nil)
      (file1 nil))
  (test-lib:test-no-errors "ini-definition:create"
    (setf def
      (ini-definition:create
        "test-file"
        '((:test-setting-string :string "")
          (:test-setting-int :int 0)
          (:test-setting-float :float 0.0)
          (:test-setting-string-two :string "")
          (:test-setting-string-default :string "default value")
          (:test-setting-multiline-string :string "")

          (:test-setting-form :form "")
          (:test-setting-form-two :form "")
        )
        '(:testini-testfile :testini-testfile-two))))

  (test-lib:test-no-errors "create"
    (setf file1 (ini-file:create def)))

  (test-lib:test-no-errors "read-ini-from-file"
    (read-ini-from-file file1 (file-utils:jfile "." "src-test" "utils" "testini.ini"))
    (test-ini-file file1))

  (test-lib:test-no-errors "save-ini-to-string/read-ini-from-string"
    (let ((file2 (ini-file:create def)))
      (read-ini-from-string file2 (save-ini-to-string file1))
      (test-ini-file file2)))

  (test-lib:test-expect-error ini-definition:ini-bad-file-type-error
    (let ((file2 (ini-file:create def)))
      (read-ini-from-string file2 (globals:format-string ""))))

  (test-lib:test-expect-error ini-definition:ini-bad-file-type-error
    (let ((file2 (ini-file:create def)))
      (read-ini-from-string file2 (globals:format-string "INIFIL"))))

  (test-lib:test-expect-error ini-definition:ini-bad-file-type-error
    (let ((file2 (ini-file:create def)))
      (read-ini-from-string file2 (globals:format-string "INIFILE~%test-file-lol~%"))))

  (test-lib:test-expect-error ini-definition:ini-bad-setting-name-error
    (let ((file2 (ini-file:create def)))
      (read-ini-from-string file2 (globals:format-string "INIFILE~%test-file~%bla=3~%"))))
)
