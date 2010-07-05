;;An example file to help test libversion

(defconst foo:load-file-name 
   load-file-name 
   "The load-file name" )

(defconst foo:var1 "Old foo var 1" 
   "Old foo:var1's docstring" )

(defconst foo:var2 "Old foo var 2" 
   "Old foo:var2's docstring" )

(defconst foo:old:unshared "Old foo unshared variable" 
   "Old foo:old:unshared's docstring" )

(defun foo:fun1 ()
   "Old foo:fun1's docstring"
   "Old foo fun 1")

(put 'foo:var2 'foo:properties "Old foo")

;;Deliberately, both old foo and new foo provide this feature.
(provide 'foo)

;;To guarantee that these files are always found in .el form
;;;_  + Local variables:
;;;_  + no-byte-compile: t
;;;_  + End:
