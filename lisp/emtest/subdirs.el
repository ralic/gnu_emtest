;; subdirs.el
;;
;; Generated on Thu 18 Mar, 2010  5:19 PM by Tom Breton (Tehom)
;; (tehom-wse-make-subdirs-el "~/projects/emtest/lisp/emtest/" (4) nil t)
;;
;; By write-subdirs-el, written by Tehom (Tom Breton)
;;
;;No, I had to temporarily edit it by hand.


;;;### (autoloads (emt:persist:accept-correct emt:persist:view-obj)
;;;;;;  "common/emt-persist" "common/emt-persist.el" (19362 37313))
;;; Generated autoloads from common/emt-persist.el

(autoload (quote emt:persist:view-obj) "emtest/common/emt-persist" "\


\(fn TRIED)" t nil)

(autoload (quote emt:persist:accept-correct) "emtest/common/emt-persist" "\


\(fn CALL TRIED)" t nil)

;;;***

;;;### (autoloads (emt:deftest-3 emt:deftest-2 emt:if-avail) "emtest/runner/define"
;;;;;;  "runner/define.el" (19362 38439))
;;; Generated autoloads from runner/define.el

(autoload (quote emt:if-avail) "emtest/runner/define" "\
Return (progn . BODY) just if emtest is available.

\(fn &rest BODY)" nil (quote macro))

(autoload (quote emt:deftest-2) "emtest/runner/define" "\
Define a test in Emtest

\(fn SYMBOL PROPS-OR-FIRST-CLAUSE &rest CLAUSES)" nil (quote macro))

(autoload (quote emt:deftest-3) "emtest/runner/define" "\
Define a test in Emtest

\(fn NAME-OR-PROPS &rest CLAUSES)" nil (quote macro))

;;;***

;;;### (autoloads (emt:library emt:defun-at-point) "emtest/runner/launch"
;;;;;;  "runner/launch.el" (19362 38201))
;;; Generated autoloads from runner/launch.el

(autoload (quote emt:defun-at-point) "emtest/runner/launch" "\
Run tests on the function or suite under point.

If prefix ARG is non-nil, eval it first.

Does nothing if the buffer is not in a known lisp mode.

\(fn ARG)" t nil)

(autoload (quote emt:library) "emtest/runner/launch" "\
Run the test suites of LIBRARY

\(fn LIBRARY)" t nil)

;;;***

;;;### (autoloads (emty:check emty:check-f) "emtest/testhelp/deep-type-checker"
;;;;;;  "testhelp/deep-type-checker.el" (19362 38439))
;;; Generated autoloads from testhelp/deep-type-checker.el

(autoload (quote emty:check-f) "emtest/testhelp/deep-type-checker" "\
Return non-nil if VALUE is deeply of type TYPE.
An alternate entry point for `emty:check'.
Here the args are values, not forms.

\(fn VALUE TYPE &optional STRING)" nil nil)

(autoload (quote emty:check) "emtest/testhelp/deep-type-checker" "\
Return non-nil if FORM evaluates to a value of type TYPE.

\(fn FORM TYPE &optional STRING)" nil (quote macro))

;;;***

;;;### (autoloads (emt:eg:map emt:eg:narrow emt:eg:value emt:eg)
;;;;;;  "testhelp/eg" "testhelp/eg.el" (19362 38439))
;;; Generated autoloads from testhelp/eg.el

(autoload (quote emt:eg) "emtest/testhelp/eg" "\


\(fn &rest KV-LIST)" nil (quote macro))

(autoload (quote emt:eg:value) "emtest/testhelp/eg" "\

Takes keywords
  NARROW - A tagset to narrow by.
  IGNORE-TAGS - tags to be ignored.

\(fn &key NARROW IGNORE-TAGS)" nil (quote macro))

(autoload (quote emt:eg:narrow) "emtest/testhelp/eg" "\
Execute BODY in a particular examples tagset.
Purpose: For consise use inside test code.

\(fn KV-FILTER &rest BODY)" nil (quote macro))

(autoload (quote emt:eg:map) "emtest/testhelp/eg" "\


\(fn TAG NAME &rest BODY)" nil (quote macro))

;;;***

;;;### (autoloads (emt:util:all-different emt:let-noprops emt:somewhere-in-tree
;;;;;;  emt:expand-filename-by-load-file uti:form-by-option) "emtest/testhelp/misc"
;;;;;;  "testhelp/misc.el" (19362 38439))
;;; Generated autoloads from testhelp/misc.el

(autoload (quote uti:form-by-option) "emtest/testhelp/misc" "\
Return a form that interprets KEY in OPTIONS.
MAKE-FORM and MAKE-FORM-ELSE should be functions of one variable that
return a form.

\(fn OPTIONS KEY MAKE-FORM &optional MAKE-FORM-ELSE)" nil (quote macro))

(autoload (quote emt:expand-filename-by-load-file) "emtest/testhelp/misc" "\


\(fn FILENAME)" nil nil)

(autoload (quote emt:somewhere-in-tree) "emtest/testhelp/misc" "\


\(fn FUNC TREE &rest ARGS)" nil nil)

(autoload (quote emt:let-noprops) "emtest/testhelp/misc" "\
Run BODY with symbols temporarily stripped of its properties.
When done, restore each symbol's original list of properties.
SYMS-FORM is a form to make a list of symbols.

\(fn SYMS-FORM &rest BODY)" nil (quote macro))

(autoload (quote emt:util:all-different) "emtest/testhelp/misc" "\
Return non-nil just if all members of SET are different.

\(fn SET &optional TEST)" nil nil)

;;;***

;;;### (autoloads (emt:persist) "emtest/testhelp/persist" "testhelp/persist.el"
;;;;;;  (19362 38439))
;;; Generated autoloads from testhelp/persist.el

(autoload (quote emt:persist) "emtest/testhelp/persist" "\
Return a persisting object or a placeholder

\(fn ID &optional BACKEND)" nil nil)

;;;***

;;;### (autoloads (should emt:doc) "emtest/testhelp/standard" "testhelp/standard.el"
;;;;;;  (19362 38439))
;;; Generated autoloads from testhelp/standard.el

(autoload (quote emt:doc) "emtest/testhelp/standard" "\


\(fn STR &rest R)" nil nil)

(autoload (quote should) "emtest/testhelp/standard" "\


\(fn FORM &key DOC)" nil (quote macro))

;;;***

;;;### (autoloads (emt:tp:collect emtp:eval emtp) "emtest/testhelp/testpoint"
;;;;;;  "testhelp/testpoint.el" (19362 38439))
;;; Generated autoloads from testhelp/testpoint.el

(autoload (quote emtp) "emtest/testhelp/testpoint" "\
A testpoint.
When not enabled, this simply runs BODY as if in a progn.
When enabled and there is a handler that matches ID, calls that
handler with arglist ARGS.
That handler can either fall thru to BODY or throw a return value to
`emtp:tag-return'.

\(fn ID ARGS &rest BODY)" nil (quote macro))

(autoload (quote emtp:eval) "emtest/testhelp/testpoint" "\


\(fn FORM &rest TESTPOINT-FORMS)" nil (quote macro))

(autoload (quote emt:tp:collect) "emtest/testhelp/testpoint" "\
Only meaningful inside `emtp:eval'

\(fn &rest ARGS)" nil nil)

;;;***

;;;### (autoloads (emt:insert) "emtest/editing/insert" "editing/insert.el"
;;;;;;  (19362 38561))
;;; Generated autoloads from editing/insert.el

(autoload (quote emt:insert) "emtest/editing/insert" "\
Insert a skeleton related to emtest

\(fn SKELETON)" t nil)

;;;***

;;;### (autoloads (pcomplete-emtest-setup pcomplete/emacs-lisp-mode/with-buffer-containing-object
;;;;;;  pcomplete/emacs-lisp-mode/emt:eg:narrow pcomplete/emacs-lisp-mode/emt:eg)
;;;;;;  "editing/pcmpl-emtest" "editing/pcmpl-emtest.el" (19362 38561))
;;; Generated autoloads from editing/pcmpl-emtest.el

(autoload (quote pcomplete/emacs-lisp-mode/emt:eg) "emtest/editing/pcmpl-emtest" "\


\(fn)" nil nil)

(autoload (quote pcomplete/emacs-lisp-mode/emt:eg:narrow) "emtest/editing/pcmpl-emtest" "\


\(fn)" nil nil)

(autoload (quote pcomplete/emacs-lisp-mode/with-buffer-containing-object) "emtest/editing/pcmpl-emtest" "\


\(fn)" nil nil)
 (add-hook 'emacs-lisp-mode-hook #'pcomplete-emtest-setup)

(autoload (quote pcomplete-emtest-setup) "emtest/editing/pcmpl-emtest" "\


\(fn)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
