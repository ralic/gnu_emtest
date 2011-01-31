;;; def-runners.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (emtr:expect) "emtest/runners/expect" "expect.el"
;;;;;;  (19778 835))
;;; Generated autoloads from expect.el

(autoload (quote emtr:expect) "emtest/runners/expect" "\
Run a test-case on external program and report the result.

\(fn PROPS FORM REPORT-F)" nil nil)
 (unless (fboundp 'emtt:add-runner)
  (error "A certain unwritten file must be loaded"))
 (emtt:add-runner 'expect #'emtr:expect
  "Expect script runner") 
 (provide 'emtest/runners/registrations)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; def-runners.el ends here
