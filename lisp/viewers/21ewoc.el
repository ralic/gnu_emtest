;; This just wraps ewoc231.el, for emacs 21

;;Before loading, redefine defalias
(when (string-lessp emacs-version "22")
   (defadvice defalias (around defalias-3 (a b &optional docstring) activate)
      "Allow defalias 3 arguments, as later emacs' support"
      (ad-Orig-defalias a b)))

(require 'ewoc "231ewoc")
