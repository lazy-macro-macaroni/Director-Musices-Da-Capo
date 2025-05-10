
(load "src/startup/load-files.lsp")

(load-files:load-files
  nil
  (load-files:dm-files)
  (load-files:src-files)
  (load-files:test-files)
  (load-files:build-files))
