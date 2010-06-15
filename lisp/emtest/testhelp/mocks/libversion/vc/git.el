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

;;Rethink: This might better use git-show.  Still find repo and its
;;top dir, but then just call that and load the buffer.

;;The buffer might be given a special name like
;;libversion:repo-dir:tree-ish:relative-filename Filename is relative
;;to the repo's top dir.

;;;_ , Requires

(require 'magit)


;;;_. Body
;;;_ , emtmv:vc:git:filename
(defun emtmv:vc:git:filename (top-dir branch-name relative-file-name)
   "Return the canonical pseudo-filename of RELATIVE-FILE-NAME in
BRANCH-NAME in the repo at TOP-DIR"
   (concat
      "libversion:git:"
      top-dir
      ":"
      branch-name
      ":"
      relative-file-name))

;;;_ , emtmv:vc:git:insert-file
(defun emtmv:vc:git:insert-file (buf branch-name lib-path)
   "Get file LIB-PATH from branch BRANCH-NAME in a git repo.
Sets the buffer's file-name to a pseudo-filename reflecting its repo.
Sets the buffer's default-directory to the directory in the repo.
LIB-PATH should be absolute."
   ;;Could try to absolutize lib-path but YAGNI
   (let*
      ((dir (file-name-directory lib-path))
	 (top-dir 
	    (progn
	       ;;Set up or find magit in DIR.  May not strictly be
	       ;;needed but it's useful because it tries hard to find
	       ;;or make a repo there.
	       (magit-status dir)
	       (magit-get-top-dir dir)))
	 (relative-file-name
	    (file-relative-name lib-path top-dir)))

	 (with-current-buffer buf
	    ;;We could be less aggressive with setting buffer
	    ;;variables, but YAGNI
	    (setq default-directory dir)
	    (setq buffer-file-name
	       (emtmv:vc:git:filename
		  top-dir
		  branch-name
		  relative-file-name))
	    (insert
	       (magit-git-string
		  "show %s:%s"
		  branch-name
		  relative-file-name)))))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/mocks/libversion/vc/git)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/mocks/libversion/vc/git.el ends here
