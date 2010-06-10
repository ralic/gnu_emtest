;;;_ emtest/testhelp/mocks/libversion/vc/git.el --- Git version-control support for libversion

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

;; Git version-control support for libversion


;;;_ , Requires

(require 'magit)


;;;_. Body
;;;_ , emtmv:vc:git:checkout
(defun emtmv:vc:git:checkout (branch-name dir)
   "Checkout out branch BRANCH-NAME in repo of DIR.
Return the old state."

   ;;Set up or find magit in DIR
   (magit-status dir)

   ;;$$IMPROVE ME This assumes that such a buffer is available.  It
   ;;probably is, since magit-status tries hard, but it's possible it
   ;;isn't.
   (with-current-buffer
      (magit-find-buffer 'status (magit-get-top-dir dir))
      (let
	 ((old-branch-name (magit-get-current-branch)))
	 (magit-checkout (magit-rev-to-git stable-branch))
	 (list old-branch-name dir))))

;;;_ , emtmv:vc:git:start
(defun emtmv:vc:git:start (stable-branch lib-path)
   "Switch to branch STABLE-BRANCH in the repo that manages LIB-PATH.

Return the old state, which `emtmv:vc:git:switch' can use."
   (emtmv:vc:git:checkout 
      stable-branch 
      ;;Don't use `magit-get-top-dir' here because `magit-status'
      ;;sometimes sets that up (ie, if there was no repo)
      (file-name-directory lib-path)))

;;;_ , emtmv:vc:git:switch
(defun emtmv:vc:git:switch (new-state)
   "Switch to NEW-STATE in the respective repo.
Return the previous state.
NEW-STATE should be an object returned by `emtmv:vc:git:start'"
   (emtmv:vc:git:checkout (first new-state) (second new-state)))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/mocks/libversion/vc/git)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/mocks/libversion/vc/git.el ends here
