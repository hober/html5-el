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

EMACS=emacs

dummy:
	echo "USAGE: $(make) [html5-ncr.el]"

clean:
	rm -f language-subtag-registry parsing-algorithm *~ *.elc

distclean:
	rm -rf relaxng webapps

parsing-algorithm: webapps tools/extract-parsing-algorithm.py
	python tools/extract-parsing-algorithm.py < webapps/source > parsing-algorithm

html5-ncr.el: webapps tools/build-ncr.py
	python tools/build-ncr.py webapps/entities-legacy.inc webapps/entities-unicode.inc > $@

html5-langs.el: tools/build-langs.py language-subtag-registry
	python tools/build-langs.py language-subtag-registry > html5-langs.el

test: html5lib/testdata/tokenizer/test1.test
	@$(EMACS) -batch -l h5-maint.el -f h5-run-tests html5lib/testdata/tokenizer/*.test

.el.elc:
	@$(EMACS) -batch -f batch-byte-compile $*.el \
		|| (echo "Perhaps you should specifcy LOAD_PATH to make?" \
		"(e.g. \"gmake LOAD_PATH=~/elisp\".)" \
		&& echo "Please see README for compilation instructions." \
		&& exit 1)

## External repositories

# (non-normative) RELAX NG schema for HTML5
relaxng:
	svn co http://whattf.svn.cvsdude.com/syntax/trunk/relaxng/ relaxng
# The HTML5 spec source
webapps:
	svn co http://svn.whatwg.org/webapps webapps
# Two- and three-letter language codes
language-subtag-registry:
	curl -O http://www.iana.org/assignments/language-subtag-registry
html5lib/testdata/tokenizer/test1.test: html5lib
html5lib:
	hg clone https://html5lib.googlecode.com/hg/ html5lib

update:
	cd relaxng; svn up
	cd webapps; svn up
