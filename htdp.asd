(defsystem "htdp"
  :version "0.1.0"
  :author "Jason Robinson"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "htdf")
		 (:file "htdd")
		 (:file "guess")
		 (:file "compound-data")
		 (:file "main")
		 )))
  :description "Learning How to Design Programs"
  :in-order-to ((test-op (test-op "htdp/tests"))))

(defsystem "htdp/tests"
  :author "Jason Robinson"
  :license ""
  :depends-on (:htdp
	       :ptester)
  :components ((:module "tests"
                :components
                ((:file "guess")
		 (:file "htdf")
		 (:file "htdd")
		 (:file "main"))))
  :description "Test system for htdp"
  ;; :perform (test-op (op c) (symbol-call :rove :run c))
  :perform (test-op (op c) () (symbol-call :htdp/tests :probe))
  )
