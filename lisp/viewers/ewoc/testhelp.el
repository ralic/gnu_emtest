;;;_ viewers/ewoc/testhelp.el --- Test support for ewoc

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, internal, maint

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;_ , Commentary:

;; 


;;;_ , Requires

(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))
(require 'ewoc)

;;;_. Body
;;;_ , ewoc:th:linked-p
(defun ewoc:th:linked-p (node)
   ""
   
   (let
      ((curr-node node))
      (catch 'ewoc-result
	 (while t
	    (setq curr-node
	       (ewoc--node-right curr-node))
	    (cond
	       ((eq curr-node node)
		  (throw 'ewoc-result t))
	       ;;Looks like an ewoc node
	       ((vectorp curr-node))
	       ;;Definitely not an ewoc node
	       (t
		  (throw 'ewoc-result nil)))))))


;;;_ , ewoc-debug-get-position-skeleton

(defun ewoc-debug-get-position-skeleton (ewoc &optional func)
   "Return a list showing the position that each node starts at.
EWOC must be an ewoc.

FUNC must be nil or a function to call on each ewoc node (including
header and footer nodes). Its return value will be incorporated into
the return value. It can be used, for instance, to identify nodes."
   (ewoc--set-buffer-bind-dll ewoc
      (let 
	 ((func (or func #'ignore)))
	 (do ((node (ewoc--node-right dll) (ewoc--node-right node))
		(rv-output ()))
	    ((eq dll node) (nreverse rv-output)) 
	    (push
	       (list
		  (let
		     ((marker (ewoc--node-start-marker node)))
		     (when marker (marker-position marker)))
		  (funcall func (ewoc--node-data node)))
	       rv-output)))))

;;;_. Footers
;;;_ , Provides

(provide 'viewers/ewoc/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/ewoc/testhelp.el ends here
