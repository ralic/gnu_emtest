;;; registrations.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (emtt:explore-indexed-clause emtt:explore-literal-clause)
;;;;;;  "emtest/explorers/clause" "clause.el" (19778 173))
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

;;;### (autoloads (emtt:explore-fileset emt:fileset) "emtest/explorers/filesets"
;;;;;;  "filesets.el" (19783 15159))
;;; Generated autoloads from filesets.el

(autoload (quote emt:fileset) "emtest/explorers/filesets" "\
Run the tests defined in FILESET.

\(fn FILESET-NAME)" t nil)

(autoload (quote emtt:explore-fileset) "emtest/explorers/filesets" "\
Run the tests defined in fileset.

\(fn TEST-ID PROPS PATH REPORT-F)" nil nil)
 (eval-after-load 'emtest/explorers/all
 '(emtt:add-explorer #'emthow:fileset-p #'emtt:explore-fileset
 "Fileset"))

;;;***

;;;### (autoloads (emtt:explore-library) "emtest/explorers/library"
;;;;;;  "library.el" (19783 14314))
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

;;;### (autoloads nil nil ("emtest/explorers/all") (19783 15348 978483))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; loadexplorers.el ends here
