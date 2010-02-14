;;;_ wysiwyg.el --- WYSIWYG for chewie

;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: maint

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

;;;_  . Structures
;;;_   , Within wookie
;;For now, the only structure written
(defstruct wysiwyg:in-wookie
   ""
   ;;Direction of mastering.  Can be mixed, {text,fixed-extractor,fixed-all}
   mastering
   ;;Nil if no extractor has been defined
   extractor
   ;;Which will be the data chewie formerly placed in wookie
   data
   )

;;The operation will be to place an extra piece at the beginning of
;;wookie, whereby the user can define the extractor.  May try to use
;;trail for this.

;;;_. Body

;;;_. Footers
;;;_ , Provides

(provide 'wysiwyg)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; wysiwyg.el ends here
