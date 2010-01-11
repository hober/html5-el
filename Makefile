# Copyright (C) 2009  Edward O'Connor
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

dummy:
	echo "USAGE: $(make) [html5-ncr.el]"

clean:
	rm parsing-algorithm *~ *.elc

distclean:
	rm -rf relaxng

parsing-algorithm: webapps tools/extract-parsing-algorithm.py
	python tools/extract-parsing-algorithm.py < webapps/source > parsing-algorithm

html5-ncr.el: webapps tools/build-ncr.py
	python tools/build-ncr.py webapps/entities-legacy.inc webapps/entities-unicode.inc > $@

## External repositories

# (non-normative) RELAX NG schema for HTML5
relaxng:
	svn co http://whattf.svn.cvsdude.com/syntax/trunk/relaxng/ relaxng
# The HTML5 spec source
webapps:
	svn co http://svn.whatwg.org/webapps webapps

update:
	cd relaxng; svn up
	cd webapps; svn up
