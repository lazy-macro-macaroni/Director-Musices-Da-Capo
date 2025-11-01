
; (in-package :data-value)

(let ((data nil)
      (data-list nil)
      (listener-value nil)
      (listener-value-2 nil))
  (test-lib:test-fn "create-data-value"
    (test-lib:test-no-errors "Creating Value"
      (setf data (data-value:create-data-value "test-value" 0 'integer)))
    (test-lib:test-equal (data-value:get-value data) 0)
    (test-lib:test-no-errors "Setting Value" (data-value:set-value data 3))
    (test-lib:test-equal (data-value:get-value data) 3)

    (test-lib:test-equal listener-value nil)

    (test-lib:test-no-errors "Adding Listener"
      (data-value:add-listener data (lambda (v) (setf listener-value v))))

    ; Listener should be called with the current value
    (test-lib:test-equal listener-value 3)

    (test-lib:test-no-errors "Setting Value 2"
      (data-value:set-value data 4))

    (test-lib:test-equal (data-value:get-value data) 4)
    (test-lib:test-equal listener-value 4)

    (test-lib:test-expect-error data-value-common:data-value-bad-type-error
      (data-value:set-value data "hello"))
    )

  (test-lib:test-fn "create-data-value-list"
    (test-lib:test-no-errors "Creating Value"
      (setf data-list (data-value-list:create-data-value-list "test-value" 'integer)))
    (test-lib:test-equal (data-value-list:get-list data-list) '())
    (test-lib:test-no-errors "Adding Value" (data-value-list:add-value data-list 3))
    (test-lib:test-equal (data-value-list:get-value data-list 0) 3)

    (test-lib:test-equal listener-value-2 nil)

    (test-lib:test-no-errors "Adding Listener"
      (data-value-list:add-listener data-list (lambda (l) (setf listener-value-2 l))))

    ; Listener should be called with the current value
    (test-lib:test-equal (nth 0 listener-value-2) 3)

    (test-lib:test-no-errors "Adding Value 2"
      (data-value-list:add-value data-list 4))

    (test-lib:test-equal (nth 1 listener-value-2) 4)

    (test-lib:test-no-errors "Remove Value"
      (data-value-list:remove-value data-list 3 #'eq))

    (test-lib:test-equal (nth 0 listener-value-2) 4)

    (test-lib:test-expect-error data-value-common:data-value-bad-type-error
      (data-value-list:add-value data-list "hello"))
    ))
