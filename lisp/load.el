;;Temporary file to set the load-path.

(add-to-list 'load-path
   (expand-file-name "."
      (if load-file-name
	 (file-name-directory
	    (file-truename load-file-name)))))