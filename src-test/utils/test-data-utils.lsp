
; (in-package :data-utils)

(let ((data nil)
      (listener-value nil))
  (test-lib:test-fn "create-data-value"
    (test-lib:test-no-errors "Creating Value"
      (setf data (data-utils:create-data-value "test-value" 0 'integer)))
    (test-lib:test-equal (data-utils:get-value data) 0)
    (test-lib:test-no-errors "Setting Value" (data-utils:set-value data 3))
    (test-lib:test-equal (data-utils:get-value data) 3)

    (test-lib:test-equal listener-value nil)

    (test-lib:test-no-errors "Adding Listener"
      (data-utils:add-listener data (lambda (v) (setf listener-value v))))

    (test-lib:test-equal listener-value 3)

    (test-lib:test-no-errors "Setting Value 2"
      (data-utils:set-value data 4))

    (test-lib:test-equal (data-utils:get-value data) 4)
    (test-lib:test-equal listener-value 4)

    (test-lib:test-expect-error data-utils:data-value-bad-type-error
      (data-utils:set-value data "hello"))
    ))
