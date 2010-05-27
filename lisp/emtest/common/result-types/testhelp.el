;;;_ emtest/common/result-types/testhelp.el --- Examples of emtest result types

;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: lisp

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

(require 'emtest/common/result-types)
(require 'emtest/testhelp/tagnames)

;;;_. Body

;;;_ , Example definitions


(defconst emt:result:thd:examples
   (emtg:define+ ;;xmp:ebd26a0a-1392-45c5-a137-0395cb079589
   ((project emtest)(library result-types))
   (transparent-tags () (type name))
   
   (group ((type grade-event))
      (type-must-be () emt:result:event:grade)
      (item ((name fail))
	 (make-emt:result:event:grade
	    ;;:id ()
	    :grade 'fail
	    :info-about ()
	    :diagnostic-info ()))
      (item ((name pass))
	 (make-emt:result:event:grade
	    ;;:id ()
	    :grade 'pass
	    :info-about ()
	    :diagnostic-info ()))
      
      (item ((name ungraded))
	 (make-emt:result:event:grade
	    ;;:id ()
	    :grade 'ungraded
	    :info-about ()
	    :diagnostic-info ())))
   
   (group ((type event-list))
      (type-must-be () emt:result:event:group)
      (item ((name simple))
	 (make-emt:result:event:group
	    ;; :name "A very simple event-group"
	    :info-about 
	    (list 
	       (make-emt:result:info-about:name
		  :name "Event-List X"))
	    :children ()
	    :aborted-p nil)
   
	 (doc () "Example of a result event-group")
	 )

      (item ((name has-info-about))
	 (make-emt:result:event:group
	    ;; :name "An event-group with some information about it"
	    :info-about () ;;$$Add me
	    :children ()
	    :aborted-p nil)
   
	 (doc () "Result event-group with various info-about")
	 )

      (item ((name has-child-events))
	 (make-emt:result:event:group
	    ;; :name "An event-group in which some events happened"
	    :info-about ()
	    :children 
	    ;;If `eg' were more advanced, we could just map over names.
	    (emtg:narrow ((type grade-event))
	       (list 
		  (emtg (name fail))
		  (emtg (name pass))		  
		  (emtg (name ungraded))))
	    :aborted-p nil)
   
	 (doc () "Example of a result event-group with various event children")
	 )

      (item ((name has-child-event-groups))
	 (make-emt:result:event:group
	    ;; :name "An event-group which contained some other event-groups"
	    :info-about ()
	    :children 
	    (emtg:narrow ((type event-list))
	       (list 
		  (emtg (name simple))
		  (emtg (name has-info-about))
		  (emtg (name has-child-events))))
	    :aborted-p nil)
   
	 (doc () "Example of a result event-group with other event-groups as children")
	 )

      (item ((name has-mixed-children-1))
	 (make-emt:result:event:group
	    ;; :name "An event-group which contained some other event-groups and
	    ;; some loose events" 
	    :info-about ()
	    :children 
	    (list 
	       (emtg (type grade-event) (name pass))
	       (emtg (type event-list)       (name simple))
	       (emtg (type grade-event) (name ungraded))
	       (emtg (type event-list)       (name has-info-about))
	       (emtg (type grade-event) (name fail)))
	    :aborted-p nil)
   
	 (doc () "Example of a result event-group with other event-groups as children")
	 )

      (item ((name has-mixed-children-2))
	 (make-emt:result:event:group
	    ;; :name "An event-group which contained some other event-groups and some loose events"
	    :info-about ()
	    :children 
	    (list 
	       (emtg (type event-list)       (name simple))
	       (emtg (type grade-event) (name pass))
	       (emtg (type grade-event) (name ungraded))
	       (emtg (type event-list)       (name has-info-about))
	       (emtg (type grade-event) (name fail))
	       (emtg (type event-list)       (name has-child-events)))
	    
	    :aborted-p nil)
   
	 (doc () "Example of a result event-group with other event-groups as children")
	 )

      (item ((name aborted-no-events))
	 (make-emt:result:event:group
	    ;; :name "An event-group that aborts before any events"
	    :info-about ()
	    :children ()
	    :aborted-p t)
   
	 (doc () 
	    "Example of a result event-group that was aborted, no event children"))
      

      (item ((name aborted-after-events))
	 (make-emt:result:event:group
	    ;; :name "An event-group that aborts after some events"
	    :info-about ()
	    :children 
	    (list
	       (emtg (type grade-event) (name pass))
	       (emtg (type grade-event) (name ungraded)))
	    
	    :aborted-p t)
   
	 (doc () 
	    "Example of a result event-group that was aborted, some event children")
	 )
      )
   (group ((type result-group))
      (type-must-be () emt:result-group)
      (item
	 ((name simple))
	 (make-emt:result-group
	    :grouping 
	    (make-emt:test-ID
	       :explore-next
	       (emthow:make-unique-clause))
	    :info ()
	    :status
	    (make-emt:result:status:was-run
	       :events 
	       (emtg (type event-list) (name has-child-events)))))

      (item
	 ((name another))
	 (make-emt:result-group
	    :grouping 
	    (make-emt:test-ID
	       :context '((x 0))
	       :explore-next
	       (emthow:make-unique-clause))
	    :info ()
	    :status
	    (make-emt:result:status:was-run
	       :events ()))))
   
;;    (group ((type result-diag))
;;       (type-must-be () emt:result:diag:call)
;;       (group ((subtype call))
;; 	 (item
;; 	    ((name pass))
;; 	    (make-emt:result:diag:call
;; 	       :status t
;; 	       :info-about ()
;; 	       :call-sexp '(= 2 2)))
;; 	 (item
;; 	    ((name fail))
;; 	    (make-emt:result:diag:call
;; 	       :status nil
;; 	       :info-about ()
;; 	       :call-sexp '(= 2 5)))
;; 	 (item
;; 	    ((name ungraded))
;; 	    (make-emt:result:diag:call
;; 	       :status 'error
;; 	       :info-about ()
;; 	       :call-sexp '(= 2 'penguin)))
;; 	 (item
;; 	    ((name long-call))
;; 	    (make-emt:result:diag:call
;; 	       :status nil
;; 	       :info-about ()
;; 	       :call-sexp '(= 
;; 			      (a very 
;; 				 very-very 
;; 				 very-very-very-very
;; 				 long call) 
;; 			      (another very-very-very-very long call))))
;; 	 ))

      ))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/common/result-types/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/common/result-types/testhelp.el ends here
