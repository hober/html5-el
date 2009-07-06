This project provides the ability to use `nxml-mode` to edit XHTML5
documents with validation. First, you'll need to fetch the RELAX NG
schema from the WHAT Task Force subversion repository; a simple `make
relaxng` in this directory should do the trick.

Next, you'll need to add this directory to your `load-path`. Let's
assume you've placed it in `~/code/html5-el/`.

    (add-to-list 'load-path "~/code/html5-el/")

Finally, wire this stuff into `nxml-mode` like so, altering the path to
`schemas.xml` appropriately.

    (eval-after-load "rng-loc"
      '(add-to-list 'rng-schema-locating-files "~/code/html5-el/schemas.xml"))

    (require 'whattf-dt)
