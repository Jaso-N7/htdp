(defsystem "htdp"
  :version "0.1.0"
  :author "Jason Robinson"
  :license ""
  :depends-on ()
  :components ((:module "src"
		:components
			((:file "htdf")
			 (:file "htdd")
			 (:file "compound-data")
			 (:file "mixed-data")
			 (:file "main")
			 (:file "guess"))))
  :description "Learning How to Design Programs"
  :in-order-to ((test-op (test-op "htdp/tests"))))

(defsystem "htdp/tests"
  :author "Jason Robinson"
  :license ""
  :depends-on (:htdp
	       :ptester)
  :components ((:module "tests"
		:components
		((:file "mixed-data")
		 (:file "compound-data")
		 (:file "guess")
		 (:file "htdf")
		 (:file "htdd")
		 (:file "main"))))
  :description "Test system for htdp"
  :perform (test-op (op c) () (symbol-call :htdp/tests :probe)))
