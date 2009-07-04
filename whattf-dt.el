;;; whattf-dt.el --- WHAT Task Force datatype library for Emacs Lisp

;; Copyright (C) 2009  Edward O'Connor

;; Author: Edward O'Connor <hober0@gmail.com>
;; Keywords: hypermedia

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Eventually, this file will contain elisp definitions for the various
;; WHATTF datatypes <URL:http://hsivonen.iki.fi/html5-datatypes/>.

;; For now, this file provides a dummy datatype validator to
;; `nxml-mode', so that you can use the WHATTF HTML5 RELAX-NG schema for
;; validating your markup.

;;; Code:

(put 'http://whattf.org/datatype-draft
     'rng-dt-compile
     'whattf-dt-compile)

(defun whattf-dt-compile (name params)
    "Provides WHAT Task Force datatypes as a RELAX NG datatypes library.
NAME is a symbol giving the local name of the datatype. PARAMS is a list
of pairs (PARAM-NAME . PARAM-VALUE) where PARAM-NAME is a symbol giving
the name of the parameter and PARAM-VALUE is a string giving its value."
    (list t 'identity))

(provide 'whattf-dt)
;;; whattf-dt.el ends here
