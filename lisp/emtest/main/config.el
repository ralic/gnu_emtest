;;;_ emtest/main/config.el --- Customizations for Emtest

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, maint

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
(require 'custom)

;;;_. Body
;;;_ , Constants
(defconst emtt:version "4.5" 
   "Current version of Emtest" )

;;;_ , Customizations
(defgroup emtest ()
   "Customization for Emtest testhelp"
   :package-version `(emtest . ,emtt:version)
   :group 'development
   :group 'programming)

;;;_  . Viewer/display configuration

;;;_   , emtl:receiver-f
;;Cheat for now: Always know to use emviewer2.  Later use a
;;customizable variable.
(defconst emtl:receiver-f
   #'emtv2:tester-cb
   "Which viewer to use to display results" )

;;;_. Footers
;;;_ , Provides

(provide 'emtest/main/config)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/main/config.el ends here
