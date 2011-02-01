;;; registrations.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (emt:vw:note:matched emt:vw:note:mismatched emt:vw:note:succeeded
;;;;;;  emt:vw:note:failed) "emtest/viewer/note-formatters/asserted"
;;;;;;  "asserted.el" (19783 34766))
;;; Generated autoloads from asserted.el

(autoload (quote emt:vw:note:failed) "emtest/viewer/note-formatters/asserted" "\
Formatter for TESTRAL note governed by `failed'

\(fn NOTE FORM)" nil nil)

(autoload (quote emt:vw:note:succeeded) "emtest/viewer/note-formatters/asserted" "\
Formatter for TESTRAL note governed by `succeeded'

\(fn NOTE FORM)" nil nil)

(autoload (quote emt:vw:note:mismatched) "emtest/viewer/note-formatters/asserted" "\
Formatter for TESTRAL note governed by `mismatched'

\(fn NOTE FORM)" nil nil)

(autoload (quote emt:vw:note:matched) "emtest/viewer/note-formatters/asserted" "\
Formatter for TESTRAL note governed by `matched'

\(fn NOTE FORM)" nil nil)
 (eval-after-load 'emtest/viewer/all-note-formatters
 '(emt:vw:note:add-gov
   'failed 
   #'emt:vw:note:failed))
 (eval-after-load 'emtest/viewer/all-note-formatters
 '(emt:vw:note:add-gov
   'succeeded
   #'emt:vw:note:succeeded))
 (eval-after-load 'emtest/viewer/all-note-formatters
 '(emt:vw:note:add-gov
   'mismatched 
   #'emt:vw:note:mismatched))
 (eval-after-load 'emtest/viewer/all-note-formatters
 '(emt:vw:note:add-gov
   'matched 
   #'emt:vw:note:matched))

;;;***

;;;### (autoloads (emt:vw:note:comparison-w/persist) "emtest/viewer/note-formatters/compare-w-persist"
;;;;;;  "compare-w-persist.el" (19783 34754))
;;; Generated autoloads from compare-w-persist.el

(autoload (quote emt:vw:note:comparison-w/persist) "emtest/viewer/note-formatters/compare-w-persist" "\
Formatter for TESTRAL note governed by `comparison-w/persist'

\(fn NOTE MATCHED-P VALUE BACKEND ID)" nil nil)
 (eval-after-load 'emtest/viewer/all-note-formatters
 '(emt:vw:note:add-gov
   'comparison-w/persist 
   #'emt:vw:note:comparison-w/persist))
 (provide 'emtest/viewer/note-formatters/registrations)

;;;***

;;;### (autoloads (emt:vw:note:doc) "emtest/viewer/note-formatters/doc"
;;;;;;  "doc.el" (19783 34777))
;;; Generated autoloads from doc.el

(autoload (quote emt:vw:note:doc) "emtest/viewer/note-formatters/doc" "\
Formatter for TESTRAL note governed by `doc'

\(fn NOTE DOC)" nil nil)
 (eval-after-load 'emtest/viewer/all-note-formatters
 '(emt:vw:note:add-gov
   'doc 
   #'emt:vw:note:doc))

;;;***

;;;### (autoloads (emt:vw:note:error-raised) "emtest/viewer/note-formatters/error-raised"
;;;;;;  "error-raised.el" (19783 34787))
;;; Generated autoloads from error-raised.el

(autoload (quote emt:vw:note:error-raised) "emtest/viewer/note-formatters/error-raised" "\
Formatter for TESTRAL note governed by `error-raised'

\(fn NOTE &rest ERR)" nil nil)
 (eval-after-load 'emtest/viewer/all-note-formatters
 '(emt:vw:note:add-gov
   'error-raised 
   #'emt:vw:note:error-raised))

;;;***

;;;### (autoloads (emt:vw:note:not-in-db) "emtest/viewer/note-formatters/not-in-db"
;;;;;;  "not-in-db.el" (19783 34798))
;;; Generated autoloads from not-in-db.el

(autoload (quote emt:vw:note:not-in-db) "emtest/viewer/note-formatters/not-in-db" "\
Formatter for TESTRAL note governed by `not-in-db'

\(fn NOTE VALUE ID BACKEND)" nil nil)
 (eval-after-load 'emtest/viewer/all-note-formatters
 '(emt:vw:note:add-gov
   'not-in-db 
   #'emt:vw:note:not-in-db))

;;;***

;;;### (autoloads (emt:vw:note:parameter) "emtest/viewer/note-formatters/parameter"
;;;;;;  "parameter.el" (19783 34807))
;;; Generated autoloads from parameter.el

(autoload (quote emt:vw:note:parameter) "emtest/viewer/note-formatters/parameter" "\
Formatter for TESTRAL note governed by `parameter'

\(fn NOTE ARG VAL)" nil nil)
 (eval-after-load 'emtest/viewer/all-note-formatters
 '(emt:vw:note:add-gov
   'parameter 
   #'emt:vw:note:parameter))

;;;***

;;;### (autoloads (emt:vw:note:scope) "emtest/viewer/note-formatters/scope"
;;;;;;  "scope.el" (19783 34820))
;;; Generated autoloads from scope.el

(autoload (quote emt:vw:note:scope) "emtest/viewer/note-formatters/scope" "\
Formatter for TESTRAL viewable governed by `scope'

\(fn NOTE &optional NAME)" nil nil)
 (eval-after-load 'emtest/viewer/all-note-formatters
 '(emt:vw:note:add-gov
   'scope 
   #'emt:vw:note:scope))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; registrations.el ends here