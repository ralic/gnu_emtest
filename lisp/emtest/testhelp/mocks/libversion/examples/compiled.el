;;Test file.  This file is to exist as ".el" and ".elc" forms, but
;;only to be available in ".el" form in the stable copy.
(defconst compiled:load-file-name 
   load-file-name 
   "The load-file name" )

;;Only to be loaded with load-path set to this directory.
(provide 'compiled)
