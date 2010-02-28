;;;_ tester/testhelp/mocks/dirtree.el --- Directory tree mock

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, maint, internal

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

;;;_. Body
;;;_ , emtmd:root

;;A function, which inits if needed

'(make-temp-name)

;;And make a directory there.
;;Config which dir to point at.
;;Since we don't know when to erase, we'll leave things there.

;;;_ , emtmd:with-dirtree
;;(Set up a dirtree.  For now, always from a master dir)
;;     * make-temp-name under emtmd:root
;;     * copy master dir recursively to there
;;     * Or for tree, set each buffer filename to there.

;;When done, remove that dir tree, `unwind-protect'.
(defmacro emtmd:with-dirtree (spec &rest body)
   ""
   
   `(let
       ;;Needs a value
       ((default-directory))
       ,@body))


;;;_ , emtmd:get-nametree   Get representation
;;Bunch of filenames.  Sort them into canonical order.
;;directory-files, recursively on directories
;;directory-files-and-attributes, so we can tell directories, which
;;we'll explore.
;; Neat - this sorts them, and they can be relative, and one can
;;recognize directories.
(defun emtmd:get-nametree (root-dir)
   ""
   
   (let*
      ()
      
      ))


;;;_ , Compare representations
;;But that could be just `equal'.
;;Might be nice to indicate when a comparand is out of order, since
;;we're comparing sets.

;;;_. Footers
;;;_ , Provides

(provide 'tester/testhelp/mocks/dirtree)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; tester/testhelp/mocks/dirtree.el ends here
