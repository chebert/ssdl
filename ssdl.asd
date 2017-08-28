;;;; ssdl.asd
;;;;
;;;; Copyright (c) 2017 Christopher Hebert <hebert.christopherj@gmail.com>

(asdf:defsystem #:ssdl
  :description "Describe ssdl here"
  :author "Christopher Hebert <hebert.christopherj@gmail.com>"
  :license "All Rights Reserved"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "package")
               (:file "ssdl")))

