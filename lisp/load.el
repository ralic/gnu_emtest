;;Temporary file to set the load-path.

(add-to-list 'load-path
   (expand-file-name "."
      (if load-file-name
	 (file-name-directory
	    (file-truename load-file-name)))))

;;;_  + Local variables:
;;;_  + no-byte-compile: t
;;;_  + no-update-autoloads: t
;;;_  + End:
