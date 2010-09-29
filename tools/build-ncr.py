#!/usr/bin/env python

# Copyright (C) 2009, 2010  Edward O'Connor
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
# BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
# ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

from __future__ import with_statement
import re
import sys

#     <tr> <td> <code title="">Tab;</code> </td> <td> U+00009 </td> </tr>
matcher = re.compile(r".*[>](.*)[<][/]code[>].*U[+]([0-9A-F]+).*")

header = """;;; html5-ncr.el --- Named Character References in HTML5

;; Copyright (C) 2009, 2010  Edward O'Connor
;; A derivative work of the HTML5 specification, which is
;; Copyright (C) 2004-2009 Apple Computer, Inc., Mozilla Foundation, and
;; Opera Software ASA.

;; Author: Edward O'Connor <hober0@gmail.com>
;; Keywords: hypermedia, wp

;; You are granted a license to use, reproduce and create
;; derivative works of this document.

;;; Commentary:

;; Table of named character entity references in HTML5.

;;; Code:

(require 'cl)

(defstruct h5-trie
  (code-point nil)
  (children nil))

(defun h5-trie-insert (trie str &optional code-point)
  (if (zerop (length str))
      (setf (h5-trie-code-point trie) code-point)
    (let* ((char (elt str 0))
           (child (gethash char (h5-trie-children trie))))
      (unless child
        (setq child (make-h5-trie
                     :code-point nil
                     :children (make-hash-table :test 'eq)))
        (puthash char child (h5-trie-children trie)))
      (h5-trie-insert child (substring str 1) code-point))))

(defun h5-trie-member-p (trie str)
  (if (zerop (length str))
      (h5-trie-code-point trie)
    (let* ((char (elt str 0))
           (child (gethash char (h5-trie-children trie))))
      (if child
          (h5-trie-member-p child (substring str 1))
        nil))))

(defun h5-trie-subtrie (trie prefix)
  (if (zerop (length prefix))
      trie
    (let* ((char (elt prefix 0))
           (child (gethash char (h5-trie-children trie))))
      (if child
          (h5-trie-subtrie child (substring prefix 1))
        nil))))

(defvar html5-named-character-references
  (let ((trie (make-h5-trie :code-point nil
                            :children (make-hash-table :test 'eq))))"""

footer = """    trie)
  "Alist mapping named character references to Unicode code points.")

(provide 'html5-ncr)
;;; html5-ncr.el ends here
"""

def extract(files):
    print header
    for filename in files:
        f = open(filename)
        for line in f:
            matched = matcher.match(line)
            if matched:
                print "    (h5-trie-insert trie \"%s\" #x%s)" % (matched.group(1), matched.group(2))
        f.close()
    print footer

if __name__ == '__main__':
    extract(sys.argv[1:])
