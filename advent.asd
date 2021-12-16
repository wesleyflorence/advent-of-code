(asdf:defsystem #:advent
  :serial t
  :description "Advent of Code"
  :author "Wes Florence <hello@wesflorence.com>"
  :depends-on ("cl-ppcre" "split-sequence" "alexandria" "trivia")
  :components ((:file "package")
               (:file "01" :depends-on ("package"))
               (:file "02" :depends-on ("package"))
               (:file "03" :depends-on ("package"))
               (:file "04" :depends-on ("package"))
               (:file "05" :depends-on ("package"))
               (:file "06" :depends-on ("package"))
               (:file "07" :depends-on ("package"))
               (:file "08" :depends-on ("package"))))
