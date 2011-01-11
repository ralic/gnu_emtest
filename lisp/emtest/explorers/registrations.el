;;; registrations.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (emtt:explore-indexed-clause emtt:explore-literal-clause)
;;;;;;  "emtest/explorers/clause" "clause.el" (19752 56538))
;;; Generated autoloads from clause.el

(autoload (quote emtt:explore-literal-clause) "emtest/explorers/clause" "\
Explore a literal clause in Emtest.

\(fn TEST-ID PROPS PATH REPORT-F)" nil nil)
 (emtt:add-explorer #'emthow:form-p #'emtt:explore-literal-clause
 "Literal clause") 

(autoload (quote emtt:explore-indexed-clause) "emtest/explorers/clause" "\
Explore an indexed clause in a suite in Emtest.

\(fn TEST-ID PROPS PATH REPORT-F)" nil nil)
 (unless (fboundp 'emtt:add-explorer)
  (error "emtest/explorers/all must be loaded"))
 (emtt:add-explorer #'emthow:indexed-clause-p #'emtt:explore-indexed-clause
 "Indexed clause") 
 (provide 'emtest/explorers/registrations)

;;;***

;;;### (autoloads (emtt:explore-library) "emtest/explorers/library"
;;;;;;  "library.el" (19741 22165))
;;; Generated autoloads from library.el

(autoload (quote emtt:explore-library) "emtest/explorers/library" "\


\(fn TEST-ID PROPS PATH REPORT-F)" nil nil)
 (emtt:add-explorer #'emthow:library:elisp-load-p #'emtt:explore-library
 "Elisp library") 

;;;***

;;;### (autoloads (emtt:explore-suite) "emtest/explorers/suite" "suite.el"
;;;;;;  (19741 22165))
;;; Generated autoloads from suite.el

(autoload (quote emtt:explore-suite) "emtest/explorers/suite" "\


\(fn TEST-ID PROPS-UNUSED PATH REPORT-F)" nil nil)
 (emtt:add-explorer #'emthow:suite-p #'emtt:explore-suite
 "Suite") 

;;;***

;;;### (autoloads nil nil ("emtest/explorers/all") (19756 48260 384386))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; loadexplorers.el ends here
