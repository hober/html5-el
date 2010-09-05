#!/usr/bin/env python

# Copyright (C) 2010  Edward O'Connor
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

matcher = re.compile(r"^(.*): (.*)$")

header = """;;; html5-langs.el --- Language tags

;; A derivative work of the IANA language subtag registry, which, as far
;; as I can tell, is in the public domain.

;; Author: Edward O'Connor <hober0@gmail.com>
;; Keywords: hypermedia, wp

;;; Commentary:

;; List of language tags.

;;; Code:

(defvar html5-language-tags
  '("""

footer = """)
  "List of valid language tags, from the IANA registry.")

(defvar html5-language-tag-re
  (regexp-opt html5-language-tags)
  "Regular expression matching valid language tags.")

(provide 'html5-langs)
;;; html5-langs.el ends here
"""

# Parser states
ST_IN_HEAD = 0
ST_BOUNDARY = 1
ST_IN_REC = 2

def extract(files):
    print header
    for filename in files:
        f = open(filename)
        state = ST_IN_HEAD
        currentLang = None
        for line in f:
            matched = matcher.match(line)
            if line == "%%\n":
                state = ST_BOUNDARY
                if currentLang is not None:
                    if 'Tag' in currentLang:
                        print "\"%s\"" % currentLang['Tag']
                    if 'Subtag' in currentLang:
                        print "\"%s\"" % currentLang['Subtag']
                currentLang = {}
            elif matched:
                if state != ST_IN_HEAD:
                    state = ST_IN_REC
                    currentLang[matched.group(1)] = matched.group(2)
        f.close()
    print footer

if __name__ == '__main__':
    extract(sys.argv[1:])
