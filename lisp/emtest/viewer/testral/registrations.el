;;; registrations.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (emtvf:TESTRAL-gov:comparison-w/persist) "emtest/viewer/testral/compare-w-persist"
;;;;;;  "compare-w-persist.el" (19726 47099))
;;; Generated autoloads from compare-w-persist.el

(autoload (quote emtvf:TESTRAL-gov:comparison-w/persist) "emtest/viewer/testral/compare-w-persist" "\
Formatter for TESTRAL note governed by `comparison-w/persist'

\(fn RESULT VALUE BACKEND ID)" nil nil)
 (unless (fboundp 'emtvf:TESTRAL:add-gov)
  (error "emtest/viewer/all-note-formatters must be loaded"))
 (emtvf:TESTRAL:add-gov
   'comparison-w/persist 
   #'emtvf:TESTRAL-gov:comparison-w/persist)
 (provide 'emtest/viewer/testral/registrations)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; registrations.el ends here
