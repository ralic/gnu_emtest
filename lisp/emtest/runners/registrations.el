;;; def-runners.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (emtr:external) "emtest/runners/external"
;;;;;;  "external.el" (19739 53011))
;;; Generated autoloads from external.el

(autoload (quote emtr:external) "emtest/runners/external" "\
Run a test-case on external program and report the result.

\(fn PROPS FORM REPORT-F)" nil nil)
 (unless (fboundp 'emtt:add-runner)
  (error "A certain unwritten file must be loaded"))
 (emtt:add-runner 'external #'emtr:external
  "External runner") 
 (provide 'emtest/runners/registrations)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; def-runners.el ends here
