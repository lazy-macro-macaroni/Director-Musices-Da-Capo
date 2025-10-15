
(in-package :string-utils)

(test-lib:test-fn "starts-with-p"
  (starts-with-p "hallo" "hal")
  (starts-with-p "hello world" "hello")
  (not (starts-with-p "hello world" "world"))
  (not (starts-with-p "HELLO world" "hello"))
  (starts-with-p "" "")
  (starts-with-p "anything" "")
  (not (starts-with-p "" "non-empty"))
)

(test-lib:test-fn "ends-with-p"
  (ends-with-p "hallo" "lo")
  (not (ends-with-p "hallo" "ll"))
  (ends-with-p "hallo" "")
  (ends-with-p "" "")
)

(test-lib:test-fn "alphanumeric-p"
  (string-utils:alphanumeric-p "abc123AZNF")
  (alphanumeric-p "XYEEeeeeZzzXzxAabB1234567890")
  (not (alphanumeric-p ",\\"))
  (not (alphanumeric-p "@å"))
)

(test-lib:test-fn "remove-prefix"
  (string= ".lsp" (remove-prefix "bla.lsp" "bla"))
  (not (string= "bla.lsp" (remove-prefix "bla.lsp" "bla")))
  (not (remove-prefix "bla.lsp" ".lsp"))
)

(test-lib:test-fn "remove-suffix"
  (string= "bla" (remove-suffix "bla.lsp" ".lsp"))
  (not (string= "bla.lsp" (remove-suffix "bla.lsp" ".lsp")))
)

(test-lib:test-fn "split-string-on-char"
  (test-lib:test-equal (split-string-on-char "a,b" #\,) (cons "a" "b"))
  (test-lib:test-equal (split-string-on-char "ab" #\,) (cons "ab" nil))
  (test-lib:test-equal (split-string-on-char "a,b,c" #\,) (cons "a" "b,c"))
  (test-lib:test-equal (split-string-on-char "a,b,c" #\, :from-end t) (cons "a,b" "c"))
  (test-lib:test-equal (car (split-string-on-char "a,b,c" #\, :from-end t)) "a,b")
)

(test-lib:test-fn "split-string-on-all-char"
  (test-lib:test-equal (split-string-on-all-char "a,b" #\,) '("a" "b"))
  (test-lib:test-equal (split-string-on-all-char "ab" #\,) '("ab"))
  (test-lib:test-equal (split-string-on-all-char "a,b,c" #\,) '("a" "b" "c"))
)

(test-lib:test-fn "uppercase"
  (test-lib:test-equal (uppercase "hello") "HELLO")
)

(test-lib:test-fn "join-strings"
  (test-lib:test-equal (join-strings '("a" "b" "c") #\newline) (globals:format-string "a~%b~%c"))
)

(test-lib:test-class "regex-matcher"
  (match (regex-matcher "abc[0-9]+") "abc32423")
  (not (match (regex-matcher "abc[0-9]+") "abc32423_"))
  (test-lib:test-equal (match (regex-matcher "(abc)(edf)") "abcedf") '("abc" "edf"))
)
