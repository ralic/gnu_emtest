;;An example file to help test libversion

(defconst foo:var1 "New foo var 1" 
   "New foo:var1's docstring")

(defconst foo:var2 "New foo var 2" 
   "New foo:var2's docstring" )

(defconst foo:new:unshared "New foo unshared variable" 
   "New foo:new:unshared's docstring" )

(defun foo:fun1 ()
   "New foo:fun1's docstring"
   "New foo fun 1")

(put 'foo:var2 'foo:properties "New foo")

;;Deliberately, both old foo and new foo provide this feature.
(provide 'foo)

