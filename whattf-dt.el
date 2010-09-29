;;; whattf-dt.el --- WHAT Task Force datatype library for Emacs Lisp

;; Copyright (C) 2009, 2010  Edward O'Connor

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

;; Eventually, this file will contain elisp definitions for all of the
;; various WHATTF datatypes
;; <URL:http://hsivonen.iki.fi/html5-datatypes/>.

;; For now, this file implements just a few datatypes, and provides a
;; dummy datatype validator for the rest of them.

;;; Code:

(require 'html5-langs)

(autoload 'rng-dt-error "rng-dt")

(put 'http://whattf.org/datatype-draft
     'rng-dt-compile
     'whattf-dt-compile)

(defun whattf-dt-integer-non-negative (n)
  (if (not (string-match "^[0-9]+$" n))
      (rng-dt-error "Not a valid non-negative integer.")
    t))

(defun whattf-dt-browsing-context (str)
  (cond ((eq (length str) 0)
         (rng-dt-error "Browsing context name must be at least one character long."))
        ((eq (substring str 0 1) "_")
         (rng-dt-error "Browsing context name started with an underscore."))
        (t
         t)))

(defun whattf-dt-browsing-context-or-keyword (str)
  (cond ((eq (length str) 0)
         (rng-dt-error "Browsing context name must be at least one character long."))
        ((and (eq (substring str 0 1) "_")
              (not (member (substring str 1) '("blank" "self" "top" "parent"))))
         (rng-dt-error (format "Reserved keyword %s used." (substring str 1))))
        (t
         t)))

;; Is this too restrictive? Should we be allowing things like
;; en-GB-x-Hixie?
(defun whattf-dt-language (code)
  (let ((case-fold-search nil))
    (if (not (string-match html5-language-tag-re code))
        (rng-dt-error "Not a valid language code.")
      t)))

(defun whattf-dt-keylabellist (keylabellist)
  (let* ((keylabels (sort (split-string keylabellist) 'string<))
         (n (length keylabels)))
    (setq keylabels (delete-duplicates keylabels))
    (if (< (length keylabels) n)
        (rng-dt-error "Duplicate key labels.")
      t)))

(defun whattf-dt-zero (n)
  (if (not (string-match "^[0]+$" n))
      (rng-dt-error "Not zero.")
    t))

(defun whattf-dt-compile (name params)
    "Provides WHAT Task Force datatypes as a RELAX NG datatypes library.
NAME is a symbol giving the local name of the datatype. PARAMS is a list
of pairs (PARAM-NAME . PARAM-VALUE) where PARAM-NAME is a symbol giving
the name of the parameter and PARAM-VALUE is a string giving its value."
    (cond ((eq name 'browsing-context) '(t whattf-dt-browsing-context))
          ((eq name 'browsing-context-or-keyword) '(t whattf-dt-browsing-context-or-keyword))
          ((eq name 'integer-non-negative) '(nil whattf-dt-integer-non-negative))
          ((eq name 'keylabellist) '(t whattf-dt-keylabellist))
          ((eq name 'language) '(t whattf-dt-language))
          ((eq name 'zero) '(t whattf-dt-zero))
          ;; TODO: other datatypes below
          ((eq name 'circle) '(t identity))
          ((eq name 'color) '(t identity))
          ((eq name 'date) '(t identity))
          ((eq name 'date-or-time) '(t identity))
          ((eq name 'datetime-local) '(t identity))
          ((eq name 'datetime-tz) '(t identity))
          ((eq name 'email-address) '(t identity))
          ((eq name 'email-address-list) '(t identity))
          ((eq name 'float) '(t identity))
          ((eq name 'float-non-negative) '(t identity))
          ((eq name 'float-positive) '(t identity))
          ((eq name 'hash-name) '(t identity))
          ((eq name 'ID) '(t identity))
          ((eq name 'IDREF) '(t identity))
          ((eq name 'integer) '(t identity))
          ((eq name 'integer-positive) '(t identity))
          ((eq name 'iri) '(t identity))
          ((eq name 'iri-ref) '(t identity))
          ((eq name 'media-query) '(t identity))
          ((eq name 'mime-type) '(t identity))
          ((eq name 'mime-type-list) '(t identity))
          ((eq name 'month) '(t identity))
          ((eq name 'pattern) '(t identity))
          ((eq name 'string) '(t identity))
          ((eq name 'time) '(t identity))
          ((eq name 'week) '(t identity))
          (t ;; (rng-dt-error "Unknown datatype")
           '(t identity))))

(provide 'whattf-dt)
;;; whattf-dt.el ends here
