;;Test file to demonstrate finding symbols that name tests.

(defun a-previous-defun ()
   "")

;;!Before all foo definitions.
(defun foo ()
   ""
   ;;!In function.
   (let*
      ()))

;;!Between function and test.

(emt:deftest-2 foo
   ;;!In test definition.
   ("A dummy test definition."
      ;;!In test clause.
      (progn)))

;;!After all foo definitions.

(defun a-later-defun ()
   "")

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + no-byte-compile: t
;;;_  + mode: allout
;;;_  + End:
