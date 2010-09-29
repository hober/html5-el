# html5-el

This is an umbrella repository for various projects of mine involving
working with HTML5 in Emacs Lisp.

## Edit XHTML5 documents with `nxml-mode`

This provides the ability to use `nxml-mode` to edit XHTML5 documents
with RELAX NG validation.

### How to use

First, you'll need to fetch the RELAX NG schema from the WHAT Task Force
subversion repository; a simple `make relaxng` in this directory should
do the trick.

Next, you'll need to add this directory to your `load-path`. Let's
assume you've placed it in `~/code/html5-el/`.

    (add-to-list 'load-path "~/code/html5-el/")

Finally, wire this stuff into `nxml-mode` like so, altering the path to
`schemas.xml` appropriately.

    (eval-after-load "rng-loc"
      '(add-to-list 'rng-schema-locating-files "~/code/html5-el/schemas.xml"))

    (require 'whattf-dt)

## Experimental Emacs Lisp HTML5 tokenizer (`html5-tok.el`)

This is an implementation of the HTML5 tokenizer in Emacs Lisp. The API
is relatively simple: with point at the location in a buffer where you'd
like to start tokenizing, call `html5-tok-forward` to move forward one
token. The token moved over is returned.

Despite the fact that all the tokenizer states are implemented, this is
very early on in its life and the interface is likely to change
significantly.
