;;;_ emtest/editing/versioning.el --- Versioning assistance for Emtest

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, maint, tools

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

;;;_. Body

;;In an old rtest form, before a clause:
;;Read the string.
;;Replace it with ()
;;For each line of the string:
;;If it matches Word: create an emt:doc line for it.
;;Place that where?  Or just kill-ring them all?
;;Inside a new progn around the whole thing.

;;If the last thing in it is `t', remove that.  (for understood calls)

;;Supplementary for that, editor to kill a doc form and yank it
;;elsewhere?  Seems to not add much to kill and yank, especially to
;;kill-sexp, C-M-k.


;;In an emtest form: Where each clause begins with something
;;recognized, such as `emt:eg:narrow', move that into surrounders.

;;Where each clause differs by eg flags, replace them all with an eg
;;iterator. 


;;;_. Footers
;;;_ , Provides

(provide 'emtest/editing/versioning)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/editing/versioning.el ends here
