
(in-package :string-utils)

(test-lib:test-fn "starts-with-p"
  (starts-with-p "hallo" "hal")
)

(test-lib:test-fn "ends-with-p"
  (ends-with-p "hallo" "lo")
  (not (ends-with-p "hallo" "ll"))
  (ends-with-p "hallo" "")
  (ends-with-p "" "")
)

(test-lib:test-fn "remove-suffix"
  (string= "bla" (remove-suffix "bla.lsp" ".lsp"))
  (not (string= "bla.lsp" (remove-suffix "bla.lsp" ".lsp")))
)
