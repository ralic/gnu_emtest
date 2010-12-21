;;; registrations.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (emtvf:TESTRAL-gov:comparison-w/persist) "emtest/viewer/testral/compare-w-persist"
;;;;;;  "compare-w-persist.el" (19726 48695))
;;; Generated autoloads from compare-w-persist.el

(autoload (quote emtvf:TESTRAL-gov:comparison-w/persist) "emtest/viewer/testral/compare-w-persist" "\
Formatter for TESTRAL note governed by `comparison-w/persist'

\(fn GOV-SYMBOL RESULT VALUE BACKEND ID)" nil nil)
 (unless (fboundp 'emtvf:TESTRAL:add-gov)
  (error "emtest/viewer/all-note-formatters must be loaded"))
 (emtvf:TESTRAL:add-gov
   'comparison-w/persist 
   #'emtvf:TESTRAL-gov:comparison-w/persist)
 (provide 'emtest/viewer/testral/registrations)

;;;***

;;;### (autoloads (emtvf:TESTRAL-gov:doc) "emtest/viewer/testral/doc"
;;;;;;  "doc.el" (19728 13504))
;;; Generated autoloads from doc.el

(autoload (quote emtvf:TESTRAL-gov:doc) "emtest/viewer/testral/doc" "\
Formatter for TESTRAL note governed by `doc'

\(fn GOV-SYMBOL DOC)" nil nil)
 (emtvf:TESTRAL:add-gov
   'doc 
   #'emtvf:TESTRAL-gov:doc)

;;;***

;;;### (autoloads (emtvf:TESTRAL-gov:error-raised) "emtest/viewer/testral/error-raised"
;;;;;;  "error-raised.el" (19728 63414))
;;; Generated autoloads from error-raised.el

(autoload (quote emtvf:TESTRAL-gov:error-raised) "emtest/viewer/testral/error-raised" "\
Formatter for TESTRAL note governed by `error-raised'

\(fn GOV-SYMBOL ERR)" nil nil)
 (emtvf:TESTRAL:add-gov
   'error-raised 
   #'emtvf:TESTRAL-gov:error-raised)

;;;***

;;;### (autoloads (emtvf:TESTRAL-gov:fail) "emtest/viewer/testral/fail"
;;;;;;  "fail.el" (19729 2506))
;;; Generated autoloads from fail.el

(autoload (quote emtvf:TESTRAL-gov:fail) "emtest/viewer/testral/fail" "\
Formatter for TESTRAL note governed by `fail'

\(fn GOV-SYMBOL STR)" nil nil)
 (emtvf:TESTRAL:add-gov
   'fail 
   #'emtvf:TESTRAL-gov:fail)

;;;***

;;;### (autoloads (emtvf:TESTRAL-gov:not-in-db) "emtest/viewer/testral/not-in-db"
;;;;;;  "not-in-db.el" (19728 63589))
;;; Generated autoloads from not-in-db.el

(autoload (quote emtvf:TESTRAL-gov:not-in-db) "emtest/viewer/testral/not-in-db" "\
Formatter for TESTRAL note governed by `not-in-db'

\(fn VALUE ID BACKEND)" nil nil)
 (emtvf:TESTRAL:add-gov
   'not-in-db 
   #'emtvf:TESTRAL-gov:not-in-db)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; registrations.el ends here
