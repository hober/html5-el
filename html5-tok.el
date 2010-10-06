;;; html5-tok.el --- HTML Tokenizer

;; Copyright (C) 2010  Edward O'Connor

;; Author: Edward O'Connor <hober0@gmail.com>
;; Keywords: wp, hypermedia, comm, languages

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

;;


;;; History:
;; 

;;; Code:

(require 'html5-ncr)

(defvar *h5-curtok* nil)
(defvar *h5-curattr* nil)
(defvar *h5-additional-allowed-character* nil)
(defvar *h5-last-tag-open* nil)
(defvar html5-tok-coalesce-character-tokens nil
  "If nil, the tokenizer emits each character token separately.
If non-nil, it collects character tokens and emits strings of
consecutive ones.")

(defun h5-inside (thing)
  "Mark the text from the last tag open state as a THING."
  (when *h5-last-tag-open*
    (put-text-property *h5-last-tag-open* (point)
                       'h5-inside thing))
  (setq *h5-last-tag-open* nil))

;; (defvar h5-charbuf nil)
;; (defvar h5-charbuf-nonempty nil)
;; (defun h5-clear-charbuf ()
;;   ""
;;   (let (contents)
;;     (unless (bufferp h5-charbuf)
;;       (setq h5-charbuf (get-buffer-create " *h5-chars*")))
;;     (with-current-buffer h5-charbuf
;;       (setq contents
;;             (buffer-substring-no-properties (point-min) (point-max)))
;;       (delete-region (point-min) (point-max)))
;;     contents))
;; (defun h5-append-to-charbuf (val)
;;   ""
;;   (with-current-buffer h5-charbuf
;;     (insert val)))

(defun h5-parse-error (&optional reason)
  ""
  (let ((here (point)))
    (put-text-property (1- here) here 'h5-parse-error (or reason t)))
  (h5-emit :parse-error))

(defsubst h5-space-p (str)
  ""
  (and (stringp str)
       (string-match "[\t\n\f ]" str)))

(defsubst h5-uppercase-p (str)
  ""
  (and (stringp str)
       (string-match "[A-Z]" str)))

(defsubst h5-lowercase-p (str)
  ""
  (and (stringp str)
       (string-match "[a-z]" str)))

(defun h5-consume-the-next-input-character ()
  ""
  (condition-case nil
      (progn
        (forward-char 1)
        (char-before))
    (end-of-buffer
     :eof)))

;; <h4><dfn>Tokenization</dfn></h4>
;;
;; Implementations must act as if they used the following state machine
;; to tokenize HTML.

(defvar *h5-curstate* nil)

(defvar *h5-prevstate* nil)

(defvar *h5-statestart* 1)

(defun h5-switch-state (state)
  ""
  (when (eq state :previous)
    (setq state *h5-prevstate*))
  ;; (message "STATE [[[%s]]]->[[[%s]]] at %d" *h5-curstate* state (point))
  (when *h5-curstate*
    (put-text-property *h5-statestart* (point)
                       'h5-state *h5-curstate*))
  (setq *h5-prevstate* *h5-curstate*)
  (setq *h5-curstate* state)
  (setq *h5-statestart* (point)))

(defun h5-current-state ()
  ""
  *h5-curstate*)

;; Most states consume a single character, which may have
;; various side-effects, and either switches the state machine to a new
;; state to <em>reconsume</em> the same character, or switches it to a
;; new state (to consume the next character), or repeats the same state
;; (to consume the next character). Some states have more complicated
;; behavior and can consume several characters before switching to
;; another state. In some cases, the tokenizer state is also changed by
;; the tree construction stage.
;;
;; The exact behavior of certain states depends on the <span>insertion
;; mode</span> and the <span>stack of open elements</span>.

(defvar h5-insertion-mode nil)
(defvar h5-stack-of-open-elements nil)

;; Certain states also use a <dfn><var>temporary buffer</var></dfn> to
;; track progress.

(defvar h5-tmpbuf nil)

(defun h5-clear-tmpbuf ()
  ""
  (unless (bufferp h5-tmpbuf)
    (setq h5-tmpbuf (get-buffer-create " *h5-tmp*")))
  (with-current-buffer h5-tmpbuf
    (delete-region (point-min) (point-max))))

(defun h5-append-to-tmpbuf (val)
  ""
  (with-current-buffer h5-tmpbuf
    (insert val)))

(defun h5-tmpbuf ()
  ""
  (with-current-buffer h5-tmpbuf
    (buffer-substring (point-min) (point-max))))

(defun h5-emit-tmpbuf ()
  ""
  (h5-emit-string
   (with-current-buffer h5-tmpbuf
    (buffer-substring-no-properties (point-min) (point-max)))))

;; The output of the tokenization step is a series of zero or more of
;; the following tokens: DOCTYPE, start tag, end tag, comment,
;; character, end-of-file.

(defstruct h5-doctype-token
  ;; DOCTYPE tokens have a name, a public identifier, a system
  ;; identifier, and a <i>force-quirks flag</i>.
  ;; When a DOCTYPE token is created, its name, public identifier, and
  ;; system identifier must be marked as missing (which is a distinct
  ;; state from the empty string),
  (name 'missing :type string)
  (public-id 'missing :type string)
  (system-id 'missing :type string)
  ;; and the <i>force-quirks flag</i> must be set to <i>off</i> (its
  ;; other state is <i>on</i>).
  (force-quirks nil :type boolean))

;; and a list of attributes, each of which has a name and a value.
(defstruct h5-attr
  (name "" :type string)
  (value "" :type string)
  ;; internal to html5-tok.el
  (duplicate nil :type boolean))

(defstruct h5-tag-token
  ;; Start and end tag tokens have a tag name,
  (name "" :type string)
  ;; When a start or end tag token is created, its <i>self-closing
  ;; flag</i> must be unset (its other state is that it be set),
  (self-closing nil :type boolean)
  ;; and its attributes list must be empty.
  (attributes nil :type list))

(defstruct (h5-start-tag-token (:include h5-tag-token)))
(defstruct (h5-end-tag-token (:include h5-tag-token)))

;; Comment and character tokens have data.
(defstruct h5-comment-token
  (data "" :type string))

;; (defun h5-emit-charbuf ()
;;   ""
;;   (let ((str (h5-clear-charbuf)))
;;     (when (> (length str) 0)
;;       (h5-emit str))))

(defun h5-emit (&optional token)
  ""
  (unless token
    (setq token *h5-curtok*))
  (when token
    (when (eq token *h5-curtok*)
      (setq *h5-curtok* nil)
      (setq *h5-curattr* nil))

    ;; We don't immediately emit character tokens, but accumulate them
    ;; for emitting en mass.
    ;; (when (and (not (numberp token))
    ;;            (not (stringp token))
    ;;            h5-charbuf-nonempty)
    ;;   (h5-emit-charbuf))

    (cond ((eq token :parse-error)
           (throw 'h5-emit :parse-error))
          ((h5-start-tag-token-p token)
           (h5-inside 'start-tag)
           (setq h5-last-start-tag-emitted token)
           ;; When a start tag token is emitted with its <i>self-closing
           ;; flag</i> set, if the flag is not <dfn title="acknowledge
           ;; self-closing flag">acknowledged</dfn> when it is processed
           ;; by the tree construction stage, that is a <span>parse
           ;; error</span>.
           (throw 'h5-emit token))
          ((h5-end-tag-token-p token)
           (h5-inside 'end-tag)
           ;; When an end tag token is emitted with attributes, that is
           ;; a <span>parse error</span>.
           (when (h5-tag-token-attributes token)
             (h5-parse-error))
           ;; When an end tag token is emitted with its <i>self-closing
           ;; flag</i> set, that is a <span>parse error</span>.
           (when (h5-tag-token-self-closing token)
             (h5-parse-error))
           (throw 'h5-emit token))
          ((h5-comment-token-p token)
           (h5-inside 'comment)
           (throw 'h5-emit token))
          ((h5-doctype-token-p token)
           (h5-inside 'doctype)
           (throw 'h5-emit token))
          ((eq token :eof)
           (throw 'h5-emit :eof))
          ((numberp token)
           ;; (setq h5-charbuf-nonempty t)
           ;; (h5-append-to-charbuf token)
           (throw 'h5-emit token)
           )
          (t
           (throw 'h5-emit token)))))

(defun h5-emit-string (str)
  ""
  (dolist (char (string-to-list str))
    (h5-emit char)))

(defvar h5-last-start-tag-emitted nil)

;; An <dfn>appropriate end tag token</dfn> is an end tag token whose tag
;; name matches the tag name of the last start tag to have been emitted
;; from this tokenizer, if any.
(defun h5-appropriate-end-tag-token (tok)
  ""
  (if h5-last-start-tag-emitted
      (equal (h5-tag-token-name h5-last-start-tag-emitted)
             (h5-tag-token-name tok))
    ;; If no start tag has been emitted from this tokenizer, then no end
    ;; tag token is appropriate.
    nil))

;; Before each step of the tokenizer, the user agent must first check
;; the <span>parser pause flag</span>. If it is true, then the tokenizer
;; must abort the processing of any nested invocations of the tokenizer,
;; yielding control back to the caller.

(defun h5-data-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let ((char (h5-consume-the-next-input-character)))
    (cond ((eq char ?&) ;; <dt>U+0026 AMPERSAND (&amp;)</dt>
           ;; <dd>Switch to the <span>character reference in data
           ;; state</span>.</dd>
           (h5-switch-state
            'h5-character-reference-in-data-state))
          ((eq char ?<) ;; <dt>U+003C LESS-THAN SIGN (&lt;)</dt>
           ;; <dd>Switch to the <span>tag open state</span>.</dd>
           (h5-switch-state 'h5-tag-open-state))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd>Emit an end-of-file token.</dd>
           (h5-emit :eof))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Emit the <span>current input character</span> as a
           ;; character token.</dd>
           (h5-emit char)))))

(defun h5-character-reference-in-data-state ()
  ""
  (let ((attempt
         ;; Attempt to <span>consume a character reference</span>,
         ;; with no <span>additional allowed character</span>.
         (let ((*h5-additional-allowed-character* nil))
           (h5-consume-a-character-reference))))
    (cond ((eq attempt nil) ;; If nothing is returned,
           ;; emit a U+0026 AMPERSAND character (&amp;) token.
           (h5-emit ?&))
          (t ;; Otherwise,
          ;; emit the character token that was returned.
           (h5-emit attempt))))
  ;; Finally, switch to the <span>data state</span>.
  (h5-switch-state 'h5-data-state))

(defun h5-RCDATA-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let ((char (h5-consume-the-next-input-character)))
    (cond ((eq char ?&) ;; <dt>U+0026 AMPERSAND (&amp;)</dt>
           ;; <dd>Switch to the <span>character reference in RCDATA
           ;; state</span>.</dd>
           (h5-switch-state
            'h5-character-reference-in-RCDATA-state))
          ((eq char ?<) ;; <dt>U+003C LESS-THAN SIGN (&lt;)</dt>
           ;; <dd>Switch to the <span>RCDATA less-than sign
           ;; state</span>.</dd>
           (h5-switch-state 'h5-RCDATA-less-than-sign-state))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd>Emit an end-of-file token.</dd>
           (h5-emit :eof))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Emit the <span>current input character</span> as a
           ;; character token.</dd>
           (h5-emit char)))))

(defun h5-character-reference-in-RCDATA-state ()
  ""
  (let ((attempt
         ;; Attempt to <span>consume a character reference</span>,
         ;; with no <span>additional allowed character</span>.
         (let ((*h5-additional-allowed-character* nil))
           (h5-consume-a-character-reference))))
    (cond ((eq attempt nil) ;; If nothing is returned,
           ;; emit a U+0026 AMPERSAND character (&amp;) token.
           (h5-emit ?&))
          (t ;; Otherwise,
           ;; emit the character token that was returned.
           (h5-emit attempt))))
  ;; Finally, switch to the <span>RCDATA state</span>.
  (h5-switch-state 'h5-RCDATA-state))

(defun h5-RAWTEXT-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let ((char (h5-consume-the-next-input-character)))
    (cond ((eq char ?<) ;; <dt>U+003C LESS-THAN SIGN (&lt;)</dt>
           ;; <dd>Switch to the <span>RAWTEXT less-than sign
           ;; state</span>.</dd>
           (h5-switch-state 'h5-RAWTEXT-less-than-sign-state))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd>Emit an end-of-file token.</dd>
           (h5-emit :eof))

          (t ;; <dt>Anything else</dt>
           ;; <dd>Emit the <span>current input character</span> as a
           ;; character token.</dd>
           (h5-emit char)))))

(defun h5-script-data-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let ((char (h5-consume-the-next-input-character)))
    (cond ((eq char ?<) ;; <dt>U+003C LESS-THAN SIGN (&lt;)</dt>
           ;; <dd>Switch to the <span>script data less-than sign
           ;; state</span>.</dd>
           (h5-switch-state 'h5-script-data-less-than-sign-state))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd>Emit an end-of-file token.</dd>
           (h5-emit :eof))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Emit the <span>current input character</span> as a
           ;; character token.</dd>
           (h5-emit char)))))

(defun h5-PLAINTEXT-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let ((char (h5-consume-the-next-input-character)))
    (cond ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd>Emit an end-of-file token.</dd>
           (h5-emit :eof))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Emit the <span>current input character</span> as a
           ;; character token.</dd>
           (h5-emit char)))))

(defun h5-tag-open-state ()
  ""
  (setq *h5-last-tag-open* (1- (point)))
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character))
         (case-fold-search nil)
         (char-str (if (numberp char) (string char) char)))
    (cond ((eq char ?!) ;; <dt>U+0021 EXCLAMATION MARK (!)</dt>
           ;; (h5-emit-charbuf)
           ;; <dd>Switch to the <span>markup declaration open
           ;; state</span>.</dd>
           (h5-switch-state 'h5-markup-declaration-open-state))
          ((eq char ?/) ;; <dt>U+002F SOLIDUS (/)</dt>
           ;; (h5-emit-charbuf)
           ;; <dd>Switch to the <span>end tag open state</span>.</dd>
           (h5-switch-state 'h5-end-tag-open-state))
          (;; <dt>U+0041 LATIN CAPITAL LETTER A through to U+005A LATIN
           ;; CAPITAL LETTER Z</dt>
           (h5-uppercase-p char-str)
           ;; (h5-emit-charbuf)
           (setq *h5-curtok*
            ;; <dd>Create a new start tag token,
            (make-h5-start-tag-token
             ;; set its tag name to the lowercase version of the
             ;; <span>current input character</span> (add 0x0020 to the
             ;; character's code point),
             :name (downcase char-str)))
           ;; then switch to the <span>tag name state</span>.
           (h5-switch-state 'h5-tag-name-state))
          (;; <dt>U+0061 LATIN SMALL LETTER A through to U+007A LATIN
           ;; SMALL LETTER Z</dt>
           (h5-lowercase-p char-str)
           ;; (h5-emit-charbuf)
           (setq *h5-curtok*
            ;; <dd>Create a new start tag token,
            (make-h5-start-tag-token
             ;; set its tag name to the <span>current input
             ;; character</span>,
             :name char-str))
           ;; then switch to the <span>tag name state</span>.
           (h5-switch-state 'h5-tag-name-state))
          ((eq char ??) ;; <dt>U+003F QUESTION MARK (?)</dt>
           ;; (h5-emit-charbuf)
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Switch to the <span>bogus comment state</span>.</dd>
           (h5-switch-state 'h5-bogus-comment-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Emit a U+003C LESS-THAN SIGN character token
           (h5-emit ?<)
           ;; and reconsume the <span>current input character</span>
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state)))))

(defun h5-end-tag-open-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond (;; <dt>U+0041 LATIN CAPITAL LETTER A through to U+005A LATIN
           ;; CAPITAL LETTER Z</dt>
           (h5-uppercase-p char-str)
           (setq *h5-curtok*
            ;; <dd>Create a new end tag token,
            (make-h5-end-tag-token
             ;; set its tag name to the lowercase version of the
             ;; <span>current input character</span> (add 0x0020 to the
             ;; character's code point),
             :name (downcase char-str)))
           ;; then switch to the <span>tag name state</span>.
           (h5-switch-state 'h5-tag-name-state))
          (;; <dt>U+0061 LATIN SMALL LETTER A through to U+007A LATIN
           ;; SMALL LETTER Z</dt>
           (h5-lowercase-p char-str)
           (setq *h5-curtok*
            ;; <dd>Create a new end tag token,
            (make-h5-end-tag-token
             ;; set its tag name to the <span>current input
             ;; character</span>,
             :name char-str))
           ;; then switch to the <span>tag name state</span>.
           (h5-switch-state 'h5-tag-name-state))

          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Switch to the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Emit a U+003C LESS-THAN SIGN character token
           (h5-emit ?<)
           ;; and a U+002F SOLIDUS character token.
           (h5-emit ?/)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Switch to the <span>bogus comment state</span>.</dd>
           (h5-switch-state 'h5-bogus-comment-state)))))

(defun h5-tag-name-state () ;; <dfn>Tag name state</dfn>
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char))
         (tok *h5-curtok*))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>Switch to the <span>before attribute name
           ;; state</span>.</dd>
           (h5-switch-state 'h5-before-attribute-name-state))
          ((eq char ?/) ;; <dt>U+002F SOLIDUS (/)</dt>
           ;; <dd>Switch to the <span>self-closing start tag
           ;; state</span>.</dd>
           (h5-switch-state 'h5-self-closing-start-tag-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd>Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit the current tag token.</dd>
           (h5-emit))
          (;; <dt>U+0041 LATIN CAPITAL LETTER A through to U+005A LATIN
           ;; CAPITAL LETTER Z</dt>
           (h5-uppercase-p char-str)
           ;; <dd>Append the lowercase version of the <span>current
           ;; input character</span> (add 0x0020 to the character's code
           ;; point) to the current tag token's tag name.</dd>
           (setf (h5-tag-token-name tok)
                 (concat (h5-tag-token-name tok)
                         (downcase char-str))))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Append the <span>current input character</span> to the
           ;; current tag token's tag name.</dd>
           (setf (h5-tag-token-name tok)
                 (concat (h5-tag-token-name tok)
                         char-str))))))

(defun h5-RCDATA-less-than-sign-state ()
  ""
  ;; <!-- identical to the RAWTEXT less-than sign state, except s/RAWTEXT/RCDATA/g -->
  ;; Consume the <span>next input character</span>:
  (let ((char (h5-consume-the-next-input-character)))
    (cond ((eq char ?/) ;; <dt>U+002F SOLIDUS (/)</dt>
           ;; <dd>Set the <var>temporary buffer</var> to the empty string.
           (h5-clear-tmpbuf)
           ;; Switch to the <span>RCDATA end tag open state</span>.</dd>
           (h5-switch-state 'h5-RCDATA-end-tag-open-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Emit a U+003C LESS-THAN SIGN character token
           (h5-emit ?<)
           ;; and reconsume the <span>current input character</span>
           (backward-char 1)
           ;; in the <span>RCDATA state</span>.</dd>
           (h5-switch-state 'h5-RCDATA-state)))))

(defun h5-RCDATA-end-tag-open-state ()
  ""
  ;; <!-- identical to the RAWTEXT (and Script data) end tag open state, except s/RAWTEXT/RCDATA/g -->
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond (;; <dt>U+0041 LATIN CAPITAL LETTER A through to U+005A LATIN
           ;; CAPITAL LETTER Z</dt>
           (h5-uppercase-p char-str)
           (setq *h5-curtok*
            ;; <dd>Create a new end tag token,
            (make-h5-end-tag-token
             ;; and set its tag name to the lowercase version of the
             ;; <span>current input character</span> (add 0x0020 to the
             ;; character's code point).
             :name (downcase char-str)))
           ;; Append the <span>current input character</span> to the
           ;; <var>temporary buffer</var>.
           (h5-append-to-tmpbuf char-str)
           ;; Finally, switch to the <span>RCDATA end tag name
           ;; state</span>.
           (h5-switch-state 'h5-RCDATA-end-tag-name-state))
          (;; <dt>U+0061 LATIN SMALL LETTER A through to U+007A LATIN
           ;; SMALL LETTER Z</dt>
           (h5-lowercase-p char-str)
           (setq *h5-curtok*
            ;; <dd>Create a new end tag token,
            (make-h5-end-tag-token
             ;; and set its tag name to the <span>current input
             ;; character</span>.
             :name char-str))
           ;; Append the <span>current input character</span> to the
           ;; <var>temporary buffer</var>.
           (h5-append-to-tmpbuf char-str)
           ;; Finally, switch to the <span>RCDATA end tag name
           ;; state</span>.
           (h5-switch-state 'h5-RCDATA-end-tag-name-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Emit a U+003C LESS-THAN SIGN character token,
           (h5-emit ?<)
           ;; a U+002F SOLIDUS character token,
           (h5-emit ?/)
           ;; and reconsume the <span>current input character</span>
           (backward-char 1)
           ;; in the <span>RCDATA state</span>.</dd>
           (h5-switch-state 'h5-RCDATA-state)))))

(defun h5-RCDATA-end-tag-name-state ()
  ""
  ;; <!-- identical to the RAWTEXT (and Script data) end tag name state, except s/RAWTEXT/RCDATA/g -->
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char))
         (tok *h5-curtok*)
         (anything-else
          (lambda ()
            ;; <dd>Emit a U+003C LESS-THAN SIGN character token,
           (h5-emit ?<)
           ;; a U+002F SOLIDUS character token,
           (h5-emit ?/)
           ;; a character token for each of the characters in the
           ;; <var>temporary buffer</var> (in the order they were added
           ;; to the buffer),
           (h5-emit-tmpbuf)
           ;; and reconsume the <span>current input character</span>
           (backward-char 1)
           ;; in the <span>RCDATA state</span>.</dd>
           (h5-switch-state 'h5-RCDATA-state))))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>If the current end tag token is an <span>appropriate
           ;; end tag token</span>,
           (if (h5-appropriate-end-tag-token tok)
               ;; then switch to the <span>before attribute name
               ;; state</span>.
               (h5-switch-state 'h5-before-attribute-name-state)
             ;; Otherwise, treat it as per the "anything else" entry
             ;; below.</dd>
             (funcall anything-else)))
          ((eq char ?/) ;; <dt>U+002F SOLIDUS (/)</dt>
           ;; <dd>If the current end tag token is an <span>appropriate
           ;; end tag token</span>,
           (if (h5-appropriate-end-tag-token tok)
               ;; then switch to the <span>self-closing start tag
               ;; state</span>.
               (h5-switch-state 'h5-self-closing-start-tag-state)
             ;; Otherwise, treat it as per the "anything else" entry
             ;; below.</dd>
             (funcall anything-else)))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd>If the current end tag token is an <span>appropriate
           ;; end tag token</span>,
           (if (h5-appropriate-end-tag-token tok)
               (progn
                 ;; then emit the current tag token
                 (h5-emit tok)
                 ;; and switch to the <span>data state</span>.
                 (h5-switch-state 'h5-data-state))
             ;; Otherwise, treat it as per the "anything else" entry
             ;; below.</dd>
             (funcall anything-else)))
          (;; <dt>U+0041 LATIN CAPITAL LETTER A through to U+005A LATIN
           ;; CAPITAL LETTER Z</dt>
           (h5-uppercase-p char-str)
           ;; <dd>Append the lowercase version of the <span>current
           ;; input character</span> (add 0x0020 to the character's code
           ;; point) to the current tag token's tag name.
           (setf (h5-tag-token-name tok)
                 (concat (h5-tag-token-name tok)
                         (downcase char-str)))
           ;; Append the <span>current input character</span> to the
           ;; <var>temporary buffer</var>.</dd>
           (h5-append-to-tmpbuf char-str))

          (;; <dt>U+0061 LATIN SMALL LETTER A through to U+007A LATIN
           ;; SMALL LETTER Z</dt>
           (h5-lowercase-p char-str)
           ;; <dd>Append the <span>current input character</span> to the
           ;; current tag token's tag name.
           (setf (h5-tag-token-name tok)
                 (concat (h5-tag-token-name tok)
                         char-str))
           ;; Append the <span>current input character</span> to the
           ;; <var>temporary buffer</var>.</dd>
           (h5-append-to-tmpbuf char-str))
          (t ;; <dt>Anything else</dt>
           (funcall anything-else)))))

(defun h5-RAWTEXT-less-than-sign-state ()
  ""
  ;; <!-- identical to the RCDATA less-than sign state, except s/RCDATA/RAWTEXT/g -->
  ;; Consume the <span>next input character</span>:
  (let ((char (h5-consume-the-next-input-character)))
    (cond ((eq char ?/) ;; <dt>U+002F SOLIDUS (/)</dt>
           ;; <dd>Set the <var>temporary buffer</var> to the empty
           ;; string.
           (h5-clear-tmpbuf)
           ;; Switch to the <span>RAWTEXT end tag open
           ;; state</span>.</dd>
           (h5-switch-state 'h5-RAWTEXT-end-tag-open-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Emit a U+003C LESS-THAN SIGN character token
           (h5-emit ?<)
           ;; and reconsume the <span>current input character</span>
           (backward-char 1)
           ;; in the <span>RAWTEXT state</span>.</dd>
           (h5-switch-state 'h5-RAWTEXT-state)))))

(defun h5-RAWTEXT-end-tag-open-state ()
  ""
  ;; <!-- identical to the RCDATA (and Script data) end tag open state, except s/RCDATA/RAWTEXT/g -->
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond (;; <dt>U+0041 LATIN CAPITAL LETTER A through to U+005A LATIN
           ;; CAPITAL LETTER Z</dt>
           (h5-uppercase-p char-str)
           (setq *h5-curtok*
            ;; <dd>Create a new end tag token,
            (make-h5-end-tag-token
             ;; and set its tag name to the lowercase version of the
             ;; <span>current input character</span> (add 0x0020 to the
             ;; character's code point).
             :name (downcase char-str)))
           ;; Append the <span>current input character</span> to the
           ;; <var>temporary buffer</var>.
           (h5-append-to-tmpbuf char-str)
           ;; Finally, switch to the <span>RAWTEXT end tag name
           ;; state</span>.
           (h5-switch-state 'h5-RAWTEXT-end-tag-name-state))
          (;; <dt>U+0061 LATIN SMALL LETTER A through to U+007A LATIN
           ;; SMALL LETTER Z</dt>
           (h5-lowercase-p char-str)
           (setq *h5-curtok*
            ;; <dd>Create a new end tag token,
            (make-h5-end-tag-token
             ;; and set its tag name to the <span>current input
             ;; character</span>.
             :name char-str))
           ;; Append the <span>current input character</span> to the
           ;; <var>temporary buffer</var>.
           (h5-append-to-tmpbuf char-str)
           ;; Finally, switch to the <span>RAWTEXT end tag name
           ;; state</span>.
           (h5-switch-state 'h5-RAWTEXT-end-tag-name-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Emit a U+003C LESS-THAN SIGN character token,
           (h5-emit ?<)
           ;; a U+002F SOLIDUS character token,
           (h5-emit ?/)
           ;; and reconsume the <span>current input character</span>
           (backward-char 1)
           ;; in the <span>RAWTEXT state</span>.</dd>
           (h5-switch-state 'h5-RAWTEXT-state)))))

(defun h5-RAWTEXT-end-tag-name-state ()
  ""
  ;; <!-- identical to the RCDATA (and Script data) end tag name state, except s/RCDATA/RAWTEXT/g -->
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char))
         (tok *h5-curtok*)
         (anything-else
          (lambda ()
            ;; <dd>Emit a U+003C LESS-THAN SIGN character token,
            (h5-emit ?<)
            ;; a U+002F SOLIDUS character token,
            (h5-emit ?/)
            ;; a character token for each of the characters in the
            ;; <var>temporary buffer</var> (in the order they were added
            ;; to the buffer),
            (h5-emit-tmpbuf)
            ;; and reconsume the <span>current input character</span>
            (backward-char 1)
            ;; in the <span>RAWTEXT state</span>.</dd>
            (h5-switch-state 'h5-RAWTEXT-state))))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>If the current end tag token is an <span>appropriate
           ;; end tag token</span>,
           (if (h5-appropriate-end-tag-token tok)
               ;; then switch to the <span>before attribute name
               ;; state</span>.
               (h5-switch-state 'h5-before-attribute-name-state)
             ;; Otherwise, treat it as per the "anything else" entry
             ;; below.</dd>
             (funcall anything-else)))
          ((eq char ?/) ;; <dt>U+002F SOLIDUS (/)</dt>
           ;; <dd>If the current end tag token is an <span>appropriate
           ;; end tag token</span>,
           (if (h5-appropriate-end-tag-token tok)
               ;; then switch to the <span>self-closing start tag
               ;; state</span>.
               (h5-switch-state 'h5-self-closing-start-tag-state)
             ;; Otherwise, treat it as per the "anything else" entry
             ;; below.</dd>
             (funcall anything-else)))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd>If the current end tag token is an <span>appropriate
           ;; end tag token</span>,
           (if (h5-appropriate-end-tag-token tok)
               (progn
                 ;; then emit the current tag token
                 (h5-emit)
                 ;; and switch to the <span>data state</span>.
                 (h5-switch-state 'h5-data-state))
             ;; Otherwise, treat it as per the "anything else" entry
             ;; below.</dd>
             (funcall anything-else)))
          (;; <dt>U+0041 LATIN CAPITAL LETTER A through to U+005A LATIN
           ;; CAPITAL LETTER Z</dt>
           (h5-uppercase-p char-str)
           ;; <dd>Append the lowercase version of the <span>current
           ;; input character</span> (add 0x0020 to the character's code
           ;; point) to the current tag token's tag name.
           (setf (h5-tag-token-name tok)
                 (concat (h5-tag-token-name tok)
                         (downcase char-str)))
           ;; Append the <span>current input character</span> to the
           ;; <var>temporary buffer</var>.</dd>
           (h5-append-to-tmpbuf char-str))
          (;; <dt>U+0061 LATIN SMALL LETTER A through to U+007A LATIN
           ;; SMALL LETTER Z</dt>
           (h5-lowercase-p char-str)
           ;; <dd>Append the <span>current input character</span> to the
           ;; current tag token's tag name.
           (setf (h5-tag-token-name tok)
                 (concat (h5-tag-token-name tok)
                         char-str))
           ;; Append the <span>current input character</span> to the
           ;; <var>temporary buffer</var>.</dd>
           (h5-append-to-tmpbuf char-str))
          (t ;; <dt>Anything else</dt>
           (funcall anything-else)))))

(defun h5-script-data-less-than-sign-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond ((eq char ?/) ;; <dt>U+002F SOLIDUS (/)</dt>
           ;; <dd>Set the <var>temporary buffer</var> to the empty
           ;; string.
           (h5-clear-tmpbuf)
           ;; Switch to the <span>script data end tag open
           ;; state</span>.</dd>
           (h5-switch-state 'h5-script-data-end-tag-open-state))
          ((eq char ?!) ;; <dt>U+0021 EXCLAMATION MARK (!)</dt>

           ;; <dd>Switch to the <span>script data escape start
           ;; state</span>.
           (h5-switch-state 'h5-script-data-escape-start-state)
           ;; Emit a U+003C LESS-THAN SIGN character token
           (h5-emit ?<)
           ;; and a U+0021 EXCLAMATION MARK character token.</dd>
           (h5-emit ?!))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Emit a U+003C LESS-THAN SIGN character token
           (h5-emit ?<)
           ;; and reconsume the <span>current input character</span>
           (backward-char 1)
           ;; in the <span>script data state</span>.</dd>
           (h5-switch-state 'h5-script-data-state)))))

(defun h5-script-data-end-tag-open-state ()
  ""
  ;; <!-- identical to the RCDATA (and RAWTEXT) end tag open state, except s/RCDATA/Script data/g -->
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond (;; <dt>U+0041 LATIN CAPITAL LETTER A through to U+005A LATIN
           ;; CAPITAL LETTER Z</dt>
           (h5-uppercase-p char-str)
           (setq *h5-curtok*
            ;; <dd>Create a new end tag token,
            (make-h5-end-tag-token
             ;; and set its tag name to the lowercase version of the
             ;; <span>current input character</span> (add 0x0020 to the
             ;; character's code point).
             :name (downcase char-str)))
           ;; Append the <span>current input character</span> to the
           ;; <var>temporary buffer</var>.
           (h5-append-to-tmpbuf char-str)
           ;; Finally, switch to the <span>script data end tag name
           ;; state</span>.
           (h5-switch-state 'h5-script-data-end-tag-name-state))
          (;; <dt>U+0061 LATIN SMALL LETTER A through to U+007A LATIN
           ;; SMALL LETTER Z</dt>
           (h5-lowercase-p char-str)
           (setq *h5-curtok*
            ;; <dd>Create a new end tag token,
            (make-h5-end-tag-token
             ;; and set its tag name to the <span>current input
             ;; character</span>.
             :name char-str))
           ;; Append the <span>current input character</span> to the
           ;; <var>temporary buffer</var>.
           (h5-append-to-tmpbuf char-str)
           ;; Finally, switch to the <span>script data end tag name
           ;; state</span>.
           (h5-switch-state 'h5-script-data-end-tag-name-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Emit a U+003C LESS-THAN SIGN character token,
           (h5-emit ?<)
           ;; a U+002F SOLIDUS character token,
           (h5-emit ?/)
           ;; and reconsume the <span>current input character</span>
           (backward-char 1)
           ;; in the <span>script data state</span>.</dd>
           (h5-switch-state 'h5-script-data-state)))))

(defun h5-script-data-end-tag-name-state ()
  ""
  ;; <!-- identical to the RCDATA (and RAWTEXT) end tag name state, except s/RCDATA/Script data/g -->
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char))
         (tok *h5-curtok*)
         (anything-else
          (lambda ()
            ;; <dd>Emit a U+003C LESS-THAN SIGN character token,
            (h5-emit ?<)
            ;; a U+002F SOLIDUS character token,
            (h5-emit ?/)
            ;; a character token for each of the characters in the
            ;; <var>temporary buffer</var> (in the order they were added
            ;; to the buffer),
            (h5-emit-tmpbuf)
            ;; and reconsume the <span>current input character</span>
            (backward-char 1)
            ;; in the <span>script data state</span>.</dd>
            (h5-switch-state 'h5-script-data-state))))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>If the current end tag token is an <span>appropriate
           ;; end tag token</span>,
           (if (h5-appropriate-end-tag-token tok)
               ;; then switch to the <span>before attribute name
               ;; state</span>.
               (h5-switch-state 'h5-before-attribute-name-state)
             ;; Otherwise, treat it as per the "anything else" entry
             ;; below.</dd>
             (funcall anything-else)))
          ((eq char ?/) ;; <dt>U+002F SOLIDUS (/)</dt> <dd>If the
           ;; current end tag token is an <span>appropriate end tag
           ;; token</span>,
           (if (h5-appropriate-end-tag-token tok)
               ;; then switch to the <span>self-closing start tag
               ;; state</span>.
               (h5-switch-state 'h5-self-closing-start-tag-state)
             ;; Otherwise, treat it as per the "anything else" entry
             ;; below.</dd>
             (funcall anything-else)))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd>If the current end tag token is an <span>appropriate
           ;; end tag token</span>,
           (if (h5-appropriate-end-tag-token tok)
               (progn
                 ;; then emit the current tag token
                 (h5-emit)
                 ;; and switch to the <span>data state</span>.
                 (h5-switch-state 'h5-data-state))
             ;; Otherwise, treat it as per the "anything else" entry
             ;; below.</dd>
             (funcall anything-else)))
          (;; <dt>U+0041 LATIN CAPITAL LETTER A through to U+005A LATIN
           ;; CAPITAL LETTER Z</dt>
           (h5-uppercase-p char-str)
           ;; <dd>Append the lowercase version of the <span>current
           ;; input character</span> (add 0x0020 to the character's code
           ;; point) to the current tag token's tag name.
           (setf (h5-tag-token-name tok)
                 (concat (h5-tag-token-name tok)
                         (downcase char-str)))
           ;; Append the <span>current input character</span> to the
           ;; <var>temporary buffer</var>.</dd>
           (h5-append-to-tmpbuf char-str))
          (;; <dt>U+0061 LATIN SMALL LETTER A through to U+007A LATIN
           ;; SMALL LETTER Z</dt>
           (h5-lowercase-p char-str)
           ;; <dd>Append the <span>current input character</span> to the
           ;; current tag token's tag name.
           (setf (h5-tag-token-name tok)
                 (concat (h5-tag-token-name tok)
                         char-str))
           ;; Append the <span>current input character</span> to the
           ;; <var>temporary buffer</var>.</dd>
           (h5-append-to-tmpbuf char-str))
          (t ;; <dt>Anything else</dt>
           (funcall anything-else)))))

(defun h5-script-data-escape-start-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character)))
    (cond ((eq char ?-) ;; <dt>U+002D HYPHEN-MINUS (-)</dt>
           ;; <dd>Switch to the <span>script data escape start dash
           ;; state</span>.
           (h5-switch-state 'h5-script-data-escape-start-dash-state)
           ;; Emit a U+002D HYPHEN-MINUS character token.</dd>
           (h5-emit ?-)
          (t ;; <dt>Anything else</dt>
           ;; <dd>Reconsume the <span>current input character</span>
           (backward-char 1)
           ;; in the <span>script data state</span>.</dd>
           (h5-switch-state 'h5-script-data-state))))))

(defun h5-script-data-escape-start-dash-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character)))
    (cond ((eq char ?-) ;; <dt>U+002D HYPHEN-MINUS (-)</dt>
           ;; <dd>Switch to the <span>script data escaped dash dash
           ;; state</span>.
           (h5-switch-state 'h5-script-data-escaped-dash-dash-state)
           ;; Emit a U+002D HYPHEN-MINUS character token.</dd>
           (h5-emit ?-))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Reconsume the <span>current input character</span>
           (backward-char 1)
           ;; in the <span>script data state</span>.</dd>
           (h5-switch-state 'h5-script-data-state)))))

(defun h5-script-data-escaped-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character)))
    (cond ((eq char ?-) ;; <dt>U+002D HYPHEN-MINUS (-)</dt>
           ;; <dd>Switch to the <span>script data escaped dash state</span>.
           (h5-switch-state 'h5-script-data-escaped-dash-state)
           ;; Emit a U+002D HYPHEN-MINUS character token.</dd>
           (h5-emit ?-))
          ((eq char ?<) ;; <dt>U+003C LESS-THAN SIGN (&lt;)</dt>
           ;; <dd>Switch to the <span>script data escaped less-than sign
           ;; state</span>.</dd>
           (h5-switch-state 'h5-script-data-escaped-less-than-sign-state))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Emit the <span>current input character</span> as a
           ;; character token.</dd>
           (h5-emit char)))))

(defun h5-script-data-escaped-dash-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character)))
    (cond ((eq char ?-) ;; <dt>U+002D HYPHEN-MINUS (-)</dt>
           ;; <dd>Switch to the <span>script data escaped dash dash
           ;; state</span>.
           (h5-switch-state 'h5-script-data-escaped-dash-dash-state)
           ;; Emit a U+002D HYPHEN-MINUS character token.</dd>
           (h5-emit ?-))

          ((eq char ?<) ;; <dt>U+003C LESS-THAN SIGN (&lt;)</dt>
           ;; <dd>Switch to the <span>script data escaped less-than sign
           ;; state</span>.</dd>
           (h5-switch-state 'h5-script-data-escaped-less-than-sign-state))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))

          (t ;; <dt>Anything else</dt>
           ;; <dd>Switch to the <span>script data escaped state</span>.
           (h5-switch-state 'h5-script-data-escaped-state)
           ;; Emit the <span>current input character</span> as a
           ;; character token.</dd>
           (h5-emit char)))))

(defun h5-script-data-escaped-dash-dash-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character)))
    (cond ((eq char ?-) ;; <dt>U+002D HYPHEN-MINUS (-)</dt>
           ;; <dd>Emit a U+002D HYPHEN-MINUS character token.</dd>
           (h5-emit ?-))
          ((eq char ?<) ;; <dt>U+003C LESS-THAN SIGN (&lt;)</dt>
           ;; <dd>Switch to the <span>script data escaped less-than sign
           ;; state</span>.</dd>
           (h5-switch-state 'h5-script-data-escaped-less-than-sign-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd>Switch to the <span>script data state</span>.
           (h5-switch-state 'h5-script-data-state)
           ;; Emit a U+003E GREATER-THAN SIGN character token.</dd>
           (h5-emit ?>))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Switch to the <span>script data escaped state</span>.
           (h5-switch-state 'h5-script-data-escaped-state)
           ;; Emit the <span>current input character</span> as a
           ;; character token.</dd>
           (h5-emit char)))))

(defun h5-script-data-escaped-less-than-sign-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond ((eq char ?/) ;; <dt>U+002F SOLIDUS (/)</dt>
           ;; <dd>Set the <var>temporary buffer</var> to the empty
           ;; string.
           (h5-clear-tmpbuf)
           ;; Switch to the <span>script data escaped end tag open
           ;; state</span>.</dd>
           (h5-switch-state 'h5-script-data-escaped-end-tag-open-state))
          ;; <dt>U+0041 LATIN CAPITAL LETTER A through to U+005A LATIN CAPITAL LETTER Z</dt>
          ((h5-uppercase-p char)
          ;; <dd>Set the <var>temporary buffer</var> to the empty string.
           (h5-clear-tmpbuf)
           ;; Append the lowercase version of the <span>current input
           ;; character</span> (add 0x0020 to the character's code
           ;; point) to the <var>temporary buffer</var>.
           (h5-append-to-tmpbuf (downcase (char-str)))
           ;; Switch to the <span>script data double escape start
           ;; state</span>.
           (h5-switch-state 'h5-script-data-double-escape-start-state)
           ;; Emit a U+003C LESS-THAN SIGN character token
           (h5-emit ?<)
           ;; and the <span>current input character</span> as a
           ;; character token.</dd>
           (h5-emit char))
          ;; <dt>U+0061 LATIN SMALL LETTER A through to U+007A LATIN SMALL LETTER Z</dt>
          ((h5-lowercase-p char)
          ;; <dd>Set the <var>temporary buffer</var> to the empty string.
           (h5-clear-tmpbuf)
           ;; Append the <span>current input character</span> to the
           ;; <var>temporary buffer</var>.
           (h5-append-to-tmpbuf (char-str))
           ;; Switch to the <span>script data double escape start
           ;; state</span>.
           (h5-switch-state 'h5-script-data-double-escape-start-state)
           ;; Emit a U+003C LESS-THAN SIGN character token
           (h5-emit ?<)
           ;; and the <span>current input character</span> as a
           ;; character token.</dd>
           (h5-emit char))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Emit a U+003C LESS-THAN SIGN character token
           (h5-emit ?<)
           ;; and reconsume the <span>current input character</span>
           (backward-char 1)
           ;; in the <span>script data escaped state</span>.</dd>
           (h5-switch-state 'h5-script-data-escaped-state)))))

(defun h5-script-data-escaped-end-tag-open-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond (;; <dt>U+0041 LATIN CAPITAL LETTER A through to U+005A LATIN
           ;; CAPITAL LETTER Z</dt>
           (h5-uppercase-p char)
           (setq *h5-curtok*
            ;; <dd>Create a new end tag token,
            (make-h5-end-tag-token
             ;; set its tag name to the lowercase version of the
             ;; <span>current input character</span> (add 0x0020 to the
             ;; character's code point),
             :name (downcase char-str)))
           ;; Append the <span>current input character</span> to the
           ;; <var>temporary buffer</var>.
           (h5-append-to-tmpbuf char)
           ;; Finally, switch to the <span>script data escaped end tag
           ;; name state</span>.
           (h5-switch-state 'h5-script-data-escaped-end-tag-name-state))
          (;; <dt>U+0061 LATIN SMALL LETTER A through to U+007A LATIN
           ;; SMALL LETTER Z</dt>
           (h5-lowercase-p char)
           (setq *h5-curtok*
            ;; <dd>Create a new end tag token,
            (make-h5-end-tag-token
             ;; set its tag name to the <span>current input
             ;; character</span>
             :name char-str))
           ;; Append the <span>current input character</span> to the
           ;; <var>temporary buffer</var>.
           (h5-append-to-tmpbuf char)
           ;; Finally, switch to the <span>script data escaped end tag
           ;; name state</span>.
           (h5-switch-state 'h5-script-data-escaped-end-tag-name-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Emit a U+003C LESS-THAN SIGN character token,
           (h5-emit ?<)
           ;; a U+002F SOLIDUS character token,
           (h5-emit ?/)
           ;; and reconsume the <span>current input character</span>
           (backward-char 1)
           ;; in the <span>script data escaped state</span>.</dd>
           (h5-switch-state 'h5-script-data-escaped-state)))))

(defun h5-script-data-escaped-end-tag-name-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char))
         (tok *h5-curtok*)
         (anything-else
          (lambda ()
            ;; <dd>Emit a U+003C LESS-THAN SIGN character token,
           (h5-emit ?<)
           ;; a U+002F SOLIDUS character token,
           (h5-emit ?/)
           ;; a character token for each of the characters in the
           ;; <var>temporary buffer</var> (in the order they were added
           ;; to the buffer),
           (h5-emit-tmpbuf)
           ;; and reconsume the <span>current input character</span>
           (backward-char 1)
           ;; in the <span>Script data escaped state</span>.</dd>
           (h5-switch-state 'h5-script-data-escaped-state))))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>If the current end tag token is an <span>appropriate
           ;; end tag token</span>,
           (if (h5-appropriate-end-tag-token tok)
               ;; then switch to the <span>before attribute name
               ;; state</span>.
               (h5-switch-state 'h5-before-attribute-name-state)
             ;; Otherwise, treat it as per the "anything else" entry
             ;; below.</dd>
             (funcall anything-else)))
          ((eq char ?/) ;; <dt>U+002F SOLIDUS (/)</dt>
           ;; <dd>If the current end tag token is an <span>appropriate
           ;; end tag token</span>,
           (if (h5-appropriate-end-tag-token tok)
               ;; then switch to the <span>self-closing start tag
               ;; state</span>.
               (h5-switch-state 'h5-self-closing-start-tag-state)
             ;; Otherwise, treat it as per the "anything else" entry
             ;; below.</dd>
             (funcall anything-else)))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd>If the current end tag token is an <span>appropriate
           ;; end tag token</span>,
           (if (h5-appropriate-end-tag-token tok)
               (progn
                 ;; then emit the current tag token
                 (h5-emit tok)
                 ;; and switch to the <span>data state</span>.
                 (h5-switch-state 'h5-data-state))
             ;; Otherwise, treat it as per the "anything else" entry
             ;; below.</dd>
             (funcall anything-else)))
          (;; <dt>U+0041 LATIN CAPITAL LETTER A through to U+005A LATIN
           ;; CAPITAL LETTER Z</dt>
           (h5-uppercase-p char-str)
           ;; <dd>Append the lowercase version of the <span>current
           ;; input character</span> (add 0x0020 to the character's code
           ;; point) to the current tag token's tag name.
           (setf (h5-tag-token-name tok)
                 (concat (h5-tag-token-name tok)
                         (downcase char-str)))
           ;; Append the <span>current input character</span> to the
           ;; <var>temporary buffer</var>.</dd>
           (h5-append-to-tmpbuf char-str))
          (;; <dt>U+0061 LATIN SMALL LETTER A through to U+007A LATIN
           ;; SMALL LETTER Z</dt>
           (h5-lowercase-p char-str)
           ;; <dd>Append the <span>current input character</span> to the
           ;; current tag token's tag name.
           (setf (h5-tag-token-name tok)
                 (concat (h5-tag-token-name tok)
                         char-str))
           ;; Append the <span>current input character</span> to the
           ;; <var>temporary buffer</var>.</dd>
           (h5-append-to-tmpbuf char-str))
          (t ;; <dt>Anything else</dt>
           (funcall anything-else)))))

(defun h5-script-data-double-escape-start-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond ((or
            ;; <dt>U+0009 CHARACTER TABULATION</dt>
            ;; <dt>U+000A LINE FEED (LF)</dt>
            ;; <dt>U+000C FORM FEED (FF)</dt>
            ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
            ;; <dt>U+0020 SPACE</dt>
            (h5-space-p char-str)
            ;; <dt>U+002F SOLIDUS (/)</dt>
            ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
            (and (stringp char-str)
                 (string-match "[/>]" char-str)))
           ;; <dd>If the <var>temporary buffer</var> is the string "<code
           ;; title="">script</code>",
           (if (string-equal (h5-tmpbuf) "script")
               ;; then switch to the <span>script data double escaped
               ;; state</span>.
               (h5-switch-state 'h5-script-data-double-escaped-state)
             ;; Otherwise, switch to the <span>script data escaped
             ;; state</span>.
             (h5-switch-state 'h5-script-data-escaped-state)
             ;; Emit the <span>current input character</span> as a
             ;; character token.</dd>
             (h5-emit char)))
          (;; <dt>U+0041 LATIN CAPITAL LETTER A through to U+005A LATIN
           ;; CAPITAL LETTER Z</dt>
           (h5-uppercase-p char-str)
           ;; <dd>Append the lowercase version of the <span>current
           ;; input character</span> (add 0x0020 to the character's code
           ;; point) to the <var>temporary buffer</var>.
           (h5-append-to-tmpbuf (downcase char-str))
           ;; Emit the <span>current input character</span> as a
           ;; character token.</dd>
           (h5-emit char))
          (;; <dt>U+0061 LATIN SMALL LETTER A through to U+007A LATIN
           ;; SMALL LETTER Z</dt>
           (h5-lowercase-p char-str)
           ;; <dd>Append the <span>current input character</span> to the
           ;; <var>temporary buffer</var>.
           (h5-append-to-tmpbuf char-str)
           ;; Emit the <span>current input character</span> as a
           ;; character token.</dd>
           (h5-emit char))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Reconsume the <span>current input character</span>
           (backward-char 1)
           ;; in the <span>script data escaped state</span>.</dd>
           (h5-switch-state 'h5-script-data-escaped-state)))))

(defun h5-script-data-double-escaped-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character)))
    (cond ((eq char ?-) ;; <dt>U+002D HYPHEN-MINUS (-)</dt>
           ;; <dd>Switch to the <span>script data double escaped dash
           ;; state</span>.
           (h5-switch-state 'h5-script-data-double-escaped-dash-state)
           ;; Emit a U+002D HYPHEN-MINUS character token.</dd>
           (h5-emit ?-))
          ((eq char ?<) ;; <dt>U+003C LESS-THAN SIGN (&lt;)</dt>
           ;; <dd>Switch to the <span>script data double escaped
           ;; less-than sign state</span>.
           (h5-switch-state 'h5-script-data-double-escaped-less-than-sign-state)
           ;; Emit a U+003C LESS-THAN SIGN character token.</dd>
           (h5-emit ?<))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Emit the <span>current input character</span> as a
           ;; character token.</dd>
           (h5-emit char)))))

(defun h5-script-data-double-escaped-dash-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character)))
    (cond ((eq char ?-) ;; <dt>U+002D HYPHEN-MINUS (-)</dt>
           ;; <dd>Switch to the <span>script data double escaped dash
           ;; dash state</span>.
           (h5-switch-state 'h5-script-data-double-escaped-dash-dash-state)
           ;; Emit a U+002D HYPHEN-MINUS character token.</dd>
           (h5-emit ?-))
          ((eq char ?<) ;; <dt>U+003C LESS-THAN SIGN (&lt;)</dt>
           ;; <dd>Switch to the <span>script data double escaped
           ;; less-than sign state</span>.
           (h5-switch-state 'h5-script-data-double-escaped-less-than-sign-state)
           ;; Emit a U+003C LESS-THAN SIGN character token.</dd>
           (h5-emit ?<))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Switch to the <span>script data double escaped
           ;; state</span>.
           (h5-switch-state 'h5-script-data-double-escaped-state)
           ;; Emit the <span>current input character</span> as a
           ;; character token.</dd>
           (h5-emit char)))))

(defun h5-script-data-double-escaped-dash-dash-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character)))
    (cond ((eq char ?-) ;; <dt>U+002D HYPHEN-MINUS (-)</dt>
           ;; <dd>Emit a U+002D HYPHEN-MINUS character token.</dd>
           (h5-emit ?-))
          ((eq char ?<) ;; <dt>U+003C LESS-THAN SIGN (&lt;)</dt>
           ;; <dd>Switch to the <span>script data double escaped
           ;; less-than sign state</span>.
           (h5-switch-state 'h5-script-data-double-escaped-less-than-sign-state)
           ;; Emit a U+003C LESS-THAN SIGN character token.</dd>
           (h5-emit ?<))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd>Switch to the <span>script data state</span>.
           (h5-switch-state 'h5-script-data-state)
           ;; Emit a U+003E GREATER-THAN SIGN character token.</dd>
           (h5-emit ?>))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Switch to the <span>script data double escaped
           ;; state</span>.
           (h5-switch-state 'h5-script-data-double-escaped-state)
           ;; Emit the <span>current input character</span> as a
           ;; character token.</dd>
           (h5-emit char)))))

(defun h5-script-data-double-escaped-less-than-sign-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character)))
    (cond ((eq char ?/) ;; <dt>U+002F SOLIDUS (/)</dt>
           ;; <dd>Set the <var>temporary buffer</var> to the empty
           ;; string.
           (h5-clear-tmpbuf)
           ;; Switch to the <span>script data double escape end
           ;; state</span>.
           (h5-switch-state 'h5-script-data-double-escape-end-state)
           ;; Emit a U+002F SOLIDUS character token.</dd>
           (h5-emit ?/))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Reconsume the <span>current input character</span>
           (backward-chaar 1)
           ;; in the <span>script data double escaped state</span>.</dd>
           (h5-switch-state 'h5-script-data-double-escaped-state)))))

(defun h5-script-data-double-escape-end-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond ((or
            ;; <dt>U+0009 CHARACTER TABULATION</dt>
            ;; <dt>U+000A LINE FEED (LF)</dt>
            ;; <dt>U+000C FORM FEED (FF)</dt>
            ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
            ;; <dt>U+0020 SPACE</dt>
            (h5-space-p char-str)
            ;; <dt>U+002F SOLIDUS (/)</dt>
            ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
            (and (stringp char-str)
                 (string-match "[/>]" char-str)))

           ;; <dd>If the <var>temporary buffer</var> is the string
           ;; "<code title="">script</code>",
           (if (string-equal (h5-tmpbuf) "script")
               ;; then switch to the <span>script data escaped state</span>.
               (h5-switch-state 'h5-script-data-escaped-state)
             ;; Otherwise, switch to the <span>script data double escaped
             ;; state</span>.
             (h5-switch-state 'h5-script-data-double-escaped-state)
             ;; Emit the <span>current input character</span> as a
             ;; character token.</dd>
             (h5-emit char)))
          (;; <dt>U+0041 LATIN CAPITAL LETTER A through to U+005A LATIN
           ;; CAPITAL LETTER Z</dt>
           (h5-uppercase-p char-str)
           ;; <dd>Append the lowercase version of the <span>current
           ;; input character</span> (add 0x0020 to the character's code
           ;; point) to the <var>temporary buffer</var>.
           (h5-append-to-tmpbuf (downcase char-str))
           ;; Emit the <span>current input character</span> as a
           ;; character token.</dd>
           (h5-emit char))
          (;; <dt>U+0061 LATIN SMALL LETTER A through to U+007A LATIN
           ;; SMALL LETTER Z</dt>
           (h5-lowercase-p char-str)
           ;; <dd>Append the <span>current input character</span> to the
           ;; <var>temporary buffer</var>.
           (h5-append-to-tmpbuf char-str)
           ;; Emit the <span>current input character</span> as a
           ;; character token.</dd>
           (h5-emit char))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Reconsume the <span>current input character</span>
           (backward-char 1)
           ;; in the <span>script data double escaped state</span>.</dd>
           (h5-switch-state 'h5-script-data-double-escaped-state)))))

(defun h5-before-attribute-name-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char))
         (anything-else
          (lambda ()
            (setq *h5-curattr*
             ;; <dd>Start a new attribute in the current tag token.
             (make-h5-attr
              ;; Set that attribute's name to the <span>current input
              ;; character</span>,
              :name char-str
              ;; and its value to the empty string.
              :value ""))
            (push *h5-curattr*
                  (h5-tag-token-attributes *h5-curtok*))
            ;; Switch to the <span>attribute name state</span>.</dd>
            (h5-switch-state 'h5-attribute-name-state))))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>Ignore the character.</dd>
           )
          ((eq char ?/) ;; <dt>U+002F SOLIDUS (/)</dt>
           ;; <dd>Switch to the <span>self-closing start tag
           ;; state</span>.</dd>
           (h5-switch-state 'h5-self-closing-start-tag-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd>Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit the current tag token.</dd>
           (h5-emit))
          (;; <dt>U+0041 LATIN CAPITAL LETTER A through to U+005A LATIN
           ;; CAPITAL LETTER Z</dt>
           (h5-uppercase-p char-str)
           (setq *h5-curattr*
             ;; <dd>Start a new attribute in the current tag token.
             (make-h5-attr
                  ;; Set that attribute's name to the lowercase version of the
                  ;; <span>current input character</span>
              :name (downcase char-str)
              ;; and its value to the empty string.
              :value ""))
            (push *h5-curattr*
                  (h5-tag-token-attributes *h5-curtok*))
            ;; Switch to the <span>attribute name state</span>.</dd>
            (h5-switch-state 'h5-attribute-name-state))
          (;; <dt>U+0022 QUOTATION MARK (&quot;)</dt>
           ;; <dt>U+0027 APOSTROPHE (')</dt>
           ;; <dt>U+003C LESS-THAN SIGN (&lt;)</dt>
           ;; <dt>U+003D EQUALS SIGN (=)</dt>
           (and (stringp char-str)
                (string-match "[\"'<=]" char-str))
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Treat it as per the "anything else" entry below.</dd>
           (funcall anything-else))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           (funcall anything-else)))))

(defun h5-attribute-name-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char))
         (leave
          (lambda ()
            ;; When the user agent leaves the attribute name state (and
            ;; before emitting the tag token, if appropriate),
            (let ((name (and *h5-curattr* (h5-attr-name *h5-curattr*)))
                  (attrs
                   (cond ((h5-start-tag-token-p *h5-curtok*)
                          (h5-start-tag-token-attributes *h5-curtok*))
                         ((h5-end-tag-token-p *h5-curtok*)
                          (h5-end-tag-token-attributes *h5-curtok*))
                         (t
                          ;; INCONCEIVABLE!!!!
                          (error "*h5-curtok* [[[%s]]] neither start nor end tag"
                                 *h5-curtok*)))))
              ;; the complete attribute's name must be compared to the
              ;; other attributes on the same token;
              (dolist (attr attrs)
                ;; if there is already an attribute on the token with
                ;; the exact same name,
                (when (and (not (eq attr *h5-curattr*))
                           (equal name (h5-attr-name attr)))
                  ;; then this is a <span>parse error</span>
                  (h5-parse-error)
                  ;; and the new attribute must be dropped, along with
                  ;; the value that gets associated with it (if
                  ;; any).
                  (setf (h5-attr-duplicate *h5-curattr*) t))))))
         (anything-else
          (lambda ()
            ;; <dd>Append the <span>current input character</span> to
            ;; the current attribute's name.</dd>
            (setf (h5-attr-name *h5-curattr*)
                  (concat (h5-attr-name *h5-curattr*)
                          char-str)))))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>Switch to the <span>after attribute name
           ;; state</span>.</dd>
           (funcall leave)
           (h5-switch-state 'h5-after-attribute-name-state))
          ((eq char ?/) ;; <dt>U+002F SOLIDUS (/)</dt>
           ;; <dd>Switch to the <span>self-closing start tag
           ;; state</span>.</dd>
           (funcall leave)
           (h5-switch-state 'h5-self-closing-start-tag-state))
          ((eq char ?=) ;; <dt>U+003D EQUALS SIGN (=)</dt>
           ;; <dd>Switch to the <span>before attribute value
           ;; state</span>.</dd>
           (funcall leave)
           (h5-switch-state 'h5-before-attribute-value-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd>Switch to the <span>data state</span>.
           (funcall leave)
           (h5-switch-state 'h5-data-state)
           ;; Emit the current tag token.</dd>
           (h5-emit))
          (;; <dt>U+0041 LATIN CAPITAL LETTER A through to U+005A LATIN
           ;; CAPITAL LETTER Z</dt>
           (h5-uppercase-p char-str)
           ;; <dd>Append the lowercase version of the <span>current input
           ;; character</span> (add 0x0020 to the character's code point) to
           ;; the current attribute's name.</dd>
           (setf (h5-attr-name *h5-curattr*)
                 (concat (h5-attr-name *h5-curattr*)
                         (downcase char-str))))
          (;; <dt>U+0022 QUOTATION MARK (&quot;)</dt>
           ;; <dt>U+0027 APOSTROPHE (')</dt>
           ;; <dt>U+003C LESS-THAN SIGN (&lt;)</dt>
           (and (stringp char-str)
                (string-match "[\"'<]" char-str))
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Treat it as per the "anything else" entry below.</dd>
           (funcall anything-else))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           (funcall anything-else)))))

(defun h5-after-attribute-name-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char))
         (anything-else
          (lambda ()
           (setq *h5-curattr*
                 ;; <dd>Start a new attribute in the current tag token.
                 (make-h5-attr
                  ;; Set that attribute's name to the <span>current
                  ;; input character</span>,
                  :name char-str
                  ;; and its value to the empty string.
                  :value ""))
           (push *h5-curattr*
                 (h5-tag-token-attributes *h5-curtok*))
           ;; Switch to the <span>attribute name state</span>.</dd>
            (h5-switch-state 'h5-attribute-name-state))))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>Ignore the character.</dd>
           )
          ((eq char ?/) ;; <dt>U+002F SOLIDUS (/)</dt>
           ;; <dd>Switch to the <span>self-closing start tag
           ;; state</span>.</dd>
           (h5-switch-state 'h5-self-closing-start-tag-state))
          ((eq char ?=) ;; <dt>U+003D EQUALS SIGN (=)</dt>
           ;; <dd>Switch to the <span>before attribute value
           ;; state</span>.</dd>
           (h5-switch-state 'h5-before-attribute-value-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd>Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit the current tag token.</dd>
           (h5-emit))
          (;; <dt>U+0041 LATIN CAPITAL LETTER A through to U+005A LATIN
           ;; CAPITAL LETTER Z</dt>
           (h5-uppercase-p char-str)
           (setq *h5-curattr*
                 ;; <dd>Start a new attribute in the current tag token.
                 (make-h5-attr
                  ;; Set that attribute's name to the lowercase version
                  ;; of the <span>current input character</span> (add
                  ;; 0x0020 to the character's code point),
                  :name (downcase char-str)
                  ;; and its value to the empty string.
                  :value ""))
           (push *h5-curattr*
                 (h5-tag-token-attributes *h5-curtok*))
           ;; Switch to the <span>attribute name state</span>.</dd>
            (h5-switch-state 'h5-attribute-name-state))
          (;; <dt>U+0022 QUOTATION MARK (&quot;)</dt>
           ;; <dt>U+0027 APOSTROPHE (')</dt>
           ;; <dt>U+003C LESS-THAN SIGN (&lt;)</dt>
           (and (stringp char-str)
                (string-match "[\"'<]" char-str))
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Treat it as per the "anything else" entry below.</dd>
           (funcall anything-else))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           (funcall anything-else)))))

(defun h5-before-attribute-value-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char))
         (anything-else
          (lambda ()
            ;; <dd>Append the <span>current input character</span> to
            ;; the current attribute's value.
            (setf (h5-attr-value *h5-curattr*)
                  (concat (h5-attr-value *h5-curattr*)
                          char-str))
            ;; Switch to the <span>attribute value (unquoted)
            ;; state</span>.</dd>
            (h5-switch-state 'h5-attribute-value-unquoted-state))))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>Ignore the character.</dd>
           )
          ((eq char ?\") ;; <dt>U+0022 QUOTATION MARK (&quot;)</dt>
           ;; <dd>Switch to the <span>attribute value (double-quoted)
           ;; state</span>.</dd>
           (h5-switch-state 'h5-attribute-value-double-quoted-state))
          ((eq char ?&) ;; <dt>U+0026 AMPERSAND (&amp;)</dt>
           ;; <dd>Switch to the <span>attribute value (double-quoted)
           ;; state</span>.</dd>
           (h5-switch-state 'h5-attribute-value-unquoted-state)
           ;; and reconsume this <span>current input character</span>.</dd>
           (backward-char 1))
          ((eq char ?') ;; <dt>U+0027 APOSTROPHE (')</dt>
           ;; <dd>Switch to the <span>attribute value (single-quoted)
           ;; state</span>.</dd>
           (h5-switch-state 'h5-attribute-value-single-quoted-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit the current tag token.</dd>
           (h5-emit))
          (;; <dt>U+003C LESS-THAN SIGN (&lt;)</dt>
           ;; <dt>U+003D EQUALS SIGN (=)</dt>
           ;; <dt>U+0060 GRAVE ACCENT (`)</dt>
           (and (stringp char-str)
                (string-match "[<=`]" char-str))
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Treat it as per the "anything else" entry below.</dd>
           (funcall anything-else))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           (funcall anything-else)))))

(defun h5-attribute-value-double-quoted-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond ((eq char ?\") ;; <dt>U+0022 QUOTATION MARK (&quot;)</dt>
           ;; <dd>Switch to the <span>after attribute value (quoted)
           ;; state</span>.</dd>
           (h5-switch-state 'h5-after-attribute-value-quoted-state))
          ((eq char ?&) ;; <dt>U+0026 AMPERSAND (&amp;)</dt>
           ;; <dd>Switch to the <span>character reference in attribute
           ;; value state</span>,
           (h5-switch-state 'h5-character-reference-in-attribute-value-state)
           ;; with the <span>additional allowed character</span>
           ;; being U+0022 QUOTATION MARK (&quot;).</dd>
           (setq *h5-additional-allowed-character* ?\"))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Append the <span>current input character</span> to the
           ;; current attribute's value.</dd>
            (setf (h5-attr-value *h5-curattr*)
                  (concat (h5-attr-value *h5-curattr*)
                          char-str))))))

(defun h5-attribute-value-single-quoted-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond ((eq char ?') ;; <dt>U+0027 APOSTROPHE (')</dt>
           ;; <dd>Switch to the <span>after attribute value (quoted)
           ;; state</span>.</dd>
           (h5-switch-state 'h5-after-attribute-value-quoted-state))
          ((eq char ?&) ;; <dt>U+0026 AMPERSAND (&amp;)</dt>
           ;; <dd>Switch to the <span>character reference in attribute
           ;; value state</span>,
           (h5-switch-state 'h5-character-reference-in-attribute-value-state)
           ;; with the <span>additional allowed character</span>
           ;; being U+0027 APOSTROPHE (').</dd>
           (setq *h5-additional-allowed-character* ?'))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Append the <span>current input character</span> to the
           ;; current attribute's value.</dd>
            (setf (h5-attr-value *h5-curattr*)
                  (concat (h5-attr-value *h5-curattr*)
                          char-str))))))

(defun h5-attribute-value-unquoted-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char))
         (anything-else
          (lambda ()
            ;; <dd>Append the <span>current input character</span> to
            ;; the current attribute's value.</dd>
            (setf (h5-attr-value *h5-curattr*)
                  (concat (h5-attr-value *h5-curattr*)
                          char-str)))))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>Switch to the <span>before attribute name
           ;; state</span>.</dd>
           (h5-switch-state 'h5-before-attribute-name-state))
          ((eq char ?&) ;; <dt>U+0026 AMPERSAND (&amp;)</dt>
           ;; <dd>Switch to the <span>character reference in attribute
           ;; value state</span>,
           (h5-switch-state 'h5-character-reference-in-attribute-value-state)
           ;; with the <span>additional allowed character</span>
           ;; being U+003E GREATER-THAN SIGN (&gt;).</dd>
           (setq *h5-additional-allowed-character* ?>))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd>Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit the current tag token.</dd>
           (h5-emit))
          (;; <dt>U+0022 QUOTATION MARK (&quot;)</dt>
           ;; <dt>U+0027 APOSTROPHE (')</dt>
           ;; <dt>U+003C LESS-THAN SIGN (&lt;)</dt>
           ;; <dt>U+003D EQUALS SIGN (=)</dt>
           ;; <dt>U+0060 GRAVE ACCENT (`)</dt>
           (and (stringp char-str)
                (string-match "[\"'<=`]" char-str))
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Treat it as per the "anything else" entry below.</dd>
           (funcall anything-else))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           (funcall anything-else)))))

(defun h5-character-reference-in-attribute-value-state ()
  ""
  (let ((attempt
         ;; Attempt to <span>consume a character reference</span>.
         (h5-consume-a-character-reference)))
    (cond ((eq attempt nil) ;; If nothing is returned,
           ;; append a U+0026 AMPERSAND character (&amp;) to the current
           ;; attribute's value.
           (setf (h5-attr-value *h5-curattr*)
                 (concat (h5-attr-value *h5-curattr*)
                         (string ?&))))
          (t ;; Otherwise,
           ;; append the returned character token to the current
           ;; attribute's value.
           (setf (h5-attr-value *h5-curattr*)
                 (concat (h5-attr-value *h5-curattr*)
                         (string attempt))))))
  ;; Finally, switch back to the attribute value state that switched
  ;; into this state.
  (h5-switch-state :previous))

(defun h5-after-attribute-value-quoted-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>Switch to the <span>before attribute name
           ;; state</span>.</dd>
           (h5-switch-state 'h5-before-attribute-name-state))
          ((eq char ?/) ;; <dt>U+002F SOLIDUS (/)</dt>
           ;; <dd>Switch to the <span>self-closing start tag
           ;; state</span>.</dd>
           (h5-switch-state 'h5-self-closing-start-tag-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd>Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit the current tag token.</dd>
           (h5-emit))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Reconsume the character
           (backward-char 1)
           ;; in the <span>before attribute name state</span>.</dd>
           (h5-switch-state 'h5-before-attribute-name-state)))))

(defun h5-self-closing-start-tag-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd>Set the <i>self-closing flag</i> of the current tag
           ;; token.
           (setf (h5-tag-token-self-closing *h5-curtok*) t)
           ;; Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit the current tag token.</dd>
           (h5-emit))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>before attribute name state</span>.</dd>
           (h5-switch-state 'h5-before-attribute-name-state)))))

(defun h5-bogus-comment-state ()
  ""
  (let ((beginning (point))
        char)
    ;; Consume every character up to and including the first U+003E
    ;; GREATER-THAN SIGN character (&gt;) or the end of the file (EOF),
    ;; whichever comes first.
    (while (not (memq char '(?> :eof)))
      (setq char (h5-consume-the-next-input-character)))

    ;; Emit a comment token
    (h5-emit
     (make-h5-comment-token
      ;; whose data is the concatenation of all the characters starting
      ;; from and including the character that caused the state machine
      ;; to switch into the bogus comment state, up to and including the
      ;; character immediately before the last consumed character (i.e.
      ;; up to the character just before the U+003E or EOF character).
      ;; (If the comment was started by the end of the file (EOF), the
      ;; token is empty.)
      :data (buffer-substring (1- beginning) (point))))

    ;; Switch to the <span>data state</span>.
    (h5-switch-state 'h5-data-state)

    ;; If the end of the file was reached, reconsume the EOF
    ;; character.
    (when (eq char :eof)
      (backward-char 1))))

(defun h5-markup-declaration-open-state ()
  ""
  (cond
   (;; If the next two characters are both U+002D HYPHEN-MINUS
    ;; characters (-),
    (looking-at "--")
    ;; consume those two characters,
    (forward-char 2)
    ;; create a comment token whose data is the empty string,
    (setq *h5-curtok*
     (make-h5-comment-token :data ""))
    ;; and switch to the <span>comment start state</span>.
    (h5-switch-state 'h5-comment-start-state))
   (;; Otherwise, if the next seven characters are an <span>ASCII
    ;; case-insensitive</span> match for the word "DOCTYPE",
    (let ((case-fold-search t))
      (looking-at "doctype"))
    ;; then consume those characters
    (forward-char 7)
    ;; and switch to the <span>DOCTYPE state</span>.
    (h5-switch-state 'h5-DOCTYPE-state))
   (;; Otherwise, if the <span>insertion mode</span> is "<span
    ;; title="insertion mode: in foreign content">in foreign
    ;; content</span>"
    (and (eq h5-insertion-mode :in-foreign-content)
    ;; and the <span>current node</span> is not an element in the
    ;; <span>HTML namespace</span>
         (throw 'not-implemented (point))
    ;; and the next seven characters are an <span>case-sensitive</span>
    ;; match for the string "[CDATA[" (the five uppercase letters
    ;; "CDATA" with a U+005B LEFT SQUARE BRACKET character before and
    ;; after),
         )
    ;; then consume those characters
    (throw 'not-implemented (point))
    ;; and switch to the <span>CDATA section state</span>.
    (throw 'not-implemented (point)))
   (t ;; Otherwise
    ;; this is a <span>parse error</span>.
    (h5-parse-error)
    ;; Switch to the <span>bogus comment state</span>.
    (h5-switch-state 'h5-bogus-comment-state)
    ;; The next character that is consumed, if any, is the first
    ;; character that will be in the comment.
    )))

(defun h5-comment-start-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let ((char (h5-consume-the-next-input-character)))
    (cond ((eq char ?-) ;; <dt>U+002D HYPHEN-MINUS (-)</dt>
           ;; <dd>Switch to the <span>comment start dash
           ;; state</span>.</dd>
           (h5-switch-state 'h5-comment-start-dash-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit the comment token.</dd> <!-- see comment in
           ;; comment end state -->
           (h5-emit))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Emit the comment token.
           (h5-emit)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Append the <span>current input character</span> to the
           ;; comment token's data.
           (setf (h5-comment-token-data *h5-curtok*)
                 (concat (h5-comment-token-data *h5-curtok*)
                         (string char)))
           ;; Switch to the <span>comment state</span>.</dd>
           (h5-switch-state 'h5-comment-state)))))

(defun h5-comment-start-dash-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character)))
    (cond ((eq char ?-) ;; <dt>U+002D HYPHEN-MINUS (-)</dt>
           ;; <dd>Switch to the <span>comment end state</span></dd>
           (h5-switch-state 'h5-comment-end-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit the comment token.</dd>
           (h5-emit))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Emit the comment token.
           (h5-emit)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd> <!-- see comment
           ;; in comment end state -->
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           (setf (h5-comment-token-data *h5-curtok*)
                 (concat (h5-comment-token-data *h5-curtok*)
                         ;; <dd>Append a U+002D HYPHEN-MINUS character
                         ;; (-) and the <span>current input
                         ;; character</span> to the comment token's
                         ;; data.
                         (string ?- char)))
           ;; Switch to the <span>comment state</span>.</dd>
           (h5-switch-state 'h5-comment-state)))))

(defun h5-comment-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character)))
    (cond ((eq char ?-) ;; <dt>U+002D HYPHEN-MINUS (-)</dt>
           ;; <dd>Switch to the <span>comment end dash state</span></dd>
           (h5-switch-state 'h5-comment-end-dash-state))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Emit the comment token.
           (h5-emit)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd> <!-- see comment in
           ;; comment end state -->
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Append the <span>current input character</span> to the
           ;; comment token's data.</dd>
           (setf (h5-comment-token-data *h5-curtok*)
                 (concat (h5-comment-token-data *h5-curtok*)
                         (string char)))))))

(defun h5-comment-end-dash-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character)))
    (cond ((eq char ?-) ;; <dt>U+002D HYPHEN-MINUS (-)</dt>
           ;; <dd>Switch to the <span>comment end state</span></dd>
           (h5-switch-state 'h5-comment-end-state))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Emit the comment token.
           (h5-emit)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd> <!-- see comment in
           ;; comment end state -->
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           (setf (h5-comment-token-data *h5-curtok*)
                 (concat (h5-comment-token-data *h5-curtok*)
                         ;; <dd>Append a U+002D HYPHEN-MINUS character
                         ;; (-) and the <span>current input
                         ;; character</span> to the comment token's
                         ;; data.
                         (string ?- char)))
           ;; Switch to the <span>comment state</span>.</dd>
           (h5-switch-state 'h5-comment-state)))))

(defun h5-comment-end-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd>Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit the comment token.</dd>
           (h5-emit))
          (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Append two U+002D HYPHEN-MINUS characters (-) and the
           ;; <span>current input character</span> to the comment
           ;; token's data.
           (setf (h5-comment-token-data *h5-curtok*)
                 (concat (h5-comment-token-data *h5-curtok*)
                         (string ?- ?- char)))
           ;; Switch to the <span>comment end space state</span>.</dd>
           (h5-switch-state 'h5-comment-end-space-state))
          ((eq char ?!) ;; <dt>U+0021 EXCLAMATION MARK (!)</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Switch to the <span>comment end bang state</span>.</dd>
           (h5-switch-state 'h5-comment-end-bang-state))
          ((eq char ?-) ;; <dt>U+002D HYPHEN-MINUS (-)</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Append a U+002D HYPHEN-MINUS character (-) to the comment
           ;; token's data.</dd>
           (setf (h5-comment-token-data *h5-curtok*)
                 (concat (h5-comment-token-data *h5-curtok*)
                         (string ?-))))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Emit the comment token.
           (h5-emit)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd> <!-- For security
           ;; reasons: otherwise, hostile user could put a <script> in a
           ;; comment e.g. in a blog comment and then DOS the server so
           ;; that the end tag isn't read, and then the commented
           ;; <script> tag would be treated as live code -->
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Append two U+002D HYPHEN-MINUS characters (-) and the
           ;; <span>current input character</span> to the comment
           ;; token's data.
           (setf (h5-comment-token-data *h5-curtok*)
                 (concat (h5-comment-token-data *h5-curtok*)
                         (string ?- ?- char)))
           ;; Switch to the <span>comment state</span>.</dd>
           (h5-switch-state 'h5-comment-state)))))

(defun h5-comment-end-bang-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character)))
    (cond ((eq char ?-) ;; <dt>U+002D HYPHEN-MINUS (-)</dt>
           ;; <dd>Append two U+002D HYPHEN-MINUS characters (-) and a
           ;; U+0021 EXCLAMATION MARK character (!) to the comment
           ;; token's data.
           (setf (h5-comment-token-data *h5-curtok*)
                 (concat (h5-comment-token-data *h5-curtok*)
                         "--!"))
           ;; Switch to the <span>comment end dash state</span>.</dd>
           (h5-switch-state 'h5-comment-end-dash-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd>Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit the comment token.</dd>
           (h5-emit))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Emit the comment token.
           (h5-emit)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd> <!-- see comment in
           ;; comment end state -->
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Append two U+002D HYPHEN-MINUS characters (-), a U+0021
           ;; EXCLAMATION MARK character (!), and the <span>current input
           ;; character</span> to the comment token's data.
           (setf (h5-comment-token-data *h5-curtok*)
                 (concat (h5-comment-token-data *h5-curtok*)
                         "--!" (string char)))
           ;; Switch to the <span>comment state</span>.</dd>
           (h5-switch-state 'h5-comment-state)))))

(defun h5-comment-end-space-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>Append the <span>current input character</span> to the
           ;; comment token's data.</dd>
           (setf (h5-comment-token-data *h5-curtok*)
                 (concat (h5-comment-token-data *h5-curtok*)
                         char-str)))
          ((eq char ?-) ;; <dt>U+002D HYPHEN-MINUS (-)</dt>
           ;; <dd>Switch to the <span>comment end dash
           ;; state</span>.</dd>
           (h5-switch-state 'h5-comment-end-dash-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd>Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit the comment token.</dd>
           (h5-emit))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Emit the comment token.
           (h5-emit)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd> <!-- see comment in
           ;; comment end state -->
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Append the <span>current input character</span> to the
           ;; comment token's data.
           (setf (h5-comment-token-data *h5-curtok*)
                 (concat (h5-comment-token-data *h5-curtok*)
                         char-str))
           ;; Switch to the <span>comment state</span>.</dd>
           (h5-switch-state 'h5-comment-state)))))

(defun h5-DOCTYPE-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>Switch to the <span>before DOCTYPE name
           ;; state</span>.</dd>
           (h5-switch-state 'h5-before-DOCTYPE-name-state))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           (setq *h5-curtok*
            ;; Create a new DOCTYPE token.
            (make-h5-doctype-token
             ;; Set its <i>force-quirks flag</i> to <i>on</i>.
             :force-quirks t))
           ;; Emit the token.
           (h5-emit)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Reconsume the character
           (backward-char 1)
           ;; in the <span>before DOCTYPE name state</span>.</dd>
           (h5-switch-state 'h5-before-DOCTYPE-name-state)))))

(defun h5-before-DOCTYPE-name-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>Ignore the character.</dd>
           )
          (;; <dt>U+0041 LATIN CAPITAL LETTER A through to U+005A LATIN
           ;; CAPITAL LETTER Z</dt>
           (h5-uppercase-p char-str)
           (setq *h5-curtok*
            ;; <dd>Create a new DOCTYPE token.
            (make-h5-doctype-token
             ;; Set the token's name to the lowercase version of the
             ;; <span>current input character</span> (add 0x0020 to the
             ;; character's code point).
             :name char-str))
           ;; Switch to the <span>DOCTYPE name state</span>.</dd>
           (h5-switch-state 'h5-DOCTYPE-name-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           (setq *h5-curtok*
            ;; Create a new DOCTYPE token.
            (make-h5-doctype-token
             ;; Set its <i>force-quirks flag</i> to <i>on</i>.
             :force-quirks t))
           ;; Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit the token.</dd>
           (h5-emit))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           (setq *h5-curtok*
            ;; Create a new DOCTYPE token.
            (make-h5-doctype-token
             ;; Set its <i>force-quirks flag</i> to <i>on</i>.
             :force-quirks t))
           ;; Emit the token.
           (h5-emit)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'data-state))
          (t ;; <dt>Anything else</dt>
           (setq *h5-curtok*
            ;; <dd>Create a new DOCTYPE token.
            (make-h5-doctype-token
             ;; Set the token's name to the <span>current input
             ;; character</span>.
             :name char-str))
           ;; Switch to the <span>DOCTYPE name state</span>.</dd>
           (h5-switch-state 'h5-DOCTYPE-name-state)))))

(defun h5-DOCTYPE-name-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((case-fold-search nil)
         (char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>Switch to the <span>after DOCTYPE name state</span>.</dd>
           (h5-switch-state 'h5-after-DOCTYPE-name-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd>Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit the current DOCTYPE token.</dd>
           (h5-emit))
          (;; <dt>U+0041 LATIN CAPITAL LETTER A through to U+005A LATIN
           ;; CAPITAL LETTER Z</dt>
           (h5-uppercase-p char-str)
           ;; <dd>Append the lowercase version of the <span>current
           ;; input character</span> (add 0x0020 to the character's code
           ;; point) to the current DOCTYPE token's name.</dd>
           (setf (h5-doctype-token-name *h5-curtok*)
                 (concat (h5-doctype-token-name *h5-curtok*)
                         char-str)))
           ((eq char :eof) ;; <dt>EOF</dt>
            ;; <dd><span>Parse error</span>.
            (h5-parse-error)
            ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
            ;; <i>on</i>.
            (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
            ;; Emit that DOCTYPE token.
            (h5-emit)
            ;; Reconsume the EOF character
            (backward-char 1)
            ;; in the <span>data state</span>.</dd>
            (h5-switch-state 'h5-data-state))
           (t ;; <dt>Anything else</dt>
            ;; <dd>Append the <span>current input character</span> to
            ;; the current DOCTYPE token's name.</dd>
           (setf (h5-doctype-token-name *h5-curtok*)
                 (concat (h5-doctype-token-name *h5-curtok*)
                         char-str))))))

(defun h5-after-DOCTYPE-name-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>Ignore the character.</dd>
           )
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd>Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit the current DOCTYPE token.</dd>
           (h5-emit))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Emit that DOCTYPE token.
           (h5-emit)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-stte 'h5-data-state))
          (;; If the six characters starting from the <span>current
           ;; input character</span> are an <span>ASCII
           ;; case-insensitive</span> match for the word "PUBLIC",
           (progn
             (backward-char 1)
             (looking-at "public"))
           ;; then consume those characters
           (forward-char 6)
           ;; and switch to the <span>after DOCTYPE public keyword
           ;; state</span>.
           (h5-switch-state 'h5-after-DOCTYPE-public-keyword-state))
          (;; Otherwise, if the six characters starting from the
           ;; <span>current input character</span> are an <span>ASCII
           ;; case-insensitive</span> match for the word "SYSTEM",
           (looking-at "system")
           ;; then consume those characters
           (forward-char 6)
           ;; and switch to the <span>after DOCTYPE system keyword
           ;; state</span>.
           (h5-switch-state 'h5-after-DOCTYPE-system-keyword-state))
          (t ;; Otherwise,
           (forward-char 1) ;; fix up position after moving back for p or s
           ;; this is the <span>parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Switch to the <span>bogus DOCTYPE state</span>.
           (h5-switch-state 'h5-bogus-DOCTYPE-state)))))

(defun h5-after-DOCTYPE-public-keyword-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>Switch to the <span>before DOCTYPE public identifier
           ;; state</span>.</dd>
           (h5-switch-state 'h5-before-DOCTYPE-public-identifier-state))
          ((eq char ?\") ;; <dt>U+0022 QUOTATION MARK (&quot;)</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's public identifier to the empty
           ;; string (not missing),
           (setf (h5-doctype-token-public-id *h5-curtok*) "")
           ;; then switch to the <span>DOCTYPE public identifier
           ;; (double-quoted) state</span>.</dd>
           (h5-switch-state 'h5-DOCTYPE-public-identifier-double-quoted-state))
          ((eq char ?') ;; <dt>U+0027 APOSTROPHE (')</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's public identifier to the empty
           ;; string (not missing),
           (setf (h5-doctype-token-public-id *h5-curtok*) "")
           ;; then switch to the <span>DOCTYPE public identifier
           ;; (single-quoted) state</span>.</dd>
           (h5-switch-state 'h5-DOCTYPE-public-identifier-single-quoted-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit that DOCTYPE token.</dd>
           (h5-emit))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Emit that DOCTYPE token.
           (h5-emit)
           ;; Reconsume the EOF character
           (backward-word 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Switch to the <span>bogus DOCTYPE state</span>.</dd>
           (h5-switch-state 'h5-bogus-DOCTYPE-state)))))

(defun h5-before-DOCTYPE-public-identifier-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>Ignore the character.</dd>
           )
          ((eq char ?\") ;; <dt>U+0022 QUOTATION MARK (&quot;)</dt>
           ;; <dd>Set the DOCTYPE token's public identifier to the empty
           ;; string (not missing),
           (setf (h5-doctype-token-public-id *h5-curtok*) "")
           ;; then switch to the <span>DOCTYPE public identifier
           ;; (double-quoted) state</span>.</dd>
           (h5-switch-state 'h5-DOCTYPE-public-identifier-double-quoted-state))
          ((eq char ?') ;; <dt>U+0027 APOSTROPHE (')</dt>
           ;; <dd>Set the DOCTYPE token's public identifier to the empty
           ;; string (not missing),
           (setf (h5-doctype-token-public-id *h5-curtok*) "")
           ;; then switch to the <span>DOCTYPE public identifier
           ;; (single-quoted) state</span>.</dd>
           (h5-switch-state 'h5-DOCTYPE-public-identifier-double-quoted-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit that DOCTYPE token.</dd>
           (h5-emit))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Emit that DOCTYPE token.
           (h5-emit)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Switch to the <span>bogus DOCTYPE state</span>.</dd>
           (h5-switch-state 'h5-bogus-DOCTYPE-state)))))

(defun h5-DOCTYPE-public-identifier-double-quoted-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character)))
    (cond ((eq char ?\") ;; <dt>U+0022 QUOTATION MARK (&quot;)</dt>
           ;; <dd>Switch to the <span>after DOCTYPE public identifier
           ;; state</span>.</dd>
           (h5-switch-state 'h5-after-DOCTYPE-public-identifier-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit that DOCTYPE token.</dd>
           (h5-emit))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Emit that DOCTYPE token.
           (h5-emit)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>

           ;; <dd>Append the <span>current input character</span> to the
           ;; current DOCTYPE token's public identifier.</dd>
           (setf (h5-doctype-token-public-id *h5-curtok*)
                 (concat (h5-doctype-token-public-id *h5-curtok*)
                         (string char)))))))

(defun h5-DOCTYPE-public-identifier-single-quoted-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character)))
    (cond ((eq char ?') ;; <dt>U+0027 APOSTROPHE (')</dt>
           ;; <dd>Switch to the <span>after DOCTYPE public identifier
           ;; state</span>.</dd>
           (h5-switch-state 'h5-after-DOCTYPE-public-identifier-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit that DOCTYPE token.</dd>
           (h5-emit))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Emit that DOCTYPE token.
           (h5-emit)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Append the <span>current input character</span> to the
           ;; current DOCTYPE token's public identifier.</dd>
           (setf (h5-doctype-token-public-id *h5-curtok*)
                 (concat (h5-doctype-token-public-id *h5-curtok*)
                         (string char)))))))

(defun h5-after-DOCTYPE-public-identifier-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>Switch to the <span>between DOCTYPE public and system
           ;; identifiers state</span>.</dd>
           (h5-switch-state 'h5-between-DOCTYPE-public-and-system-identifiers-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd>Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit the current DOCTYPE token.</dd>
           (h5-emit))
          ((eq char ?\") ;; <dt>U+0022 QUOTATION MARK (&quot;)</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's system identifier to the empty
           ;; string (not missing),
           (setf (h5-doctype-token-system-id *h5-curtok*) "")
           ;; then switch to the <span>DOCTYPE system identifier
           ;; (double-quoted) state</span>.</dd>
           (h5-swith-state 'h5-DOCTYPE-system-identifier-double-quoted-state))
          ((eq char ?') ;; <dt>U+0027 APOSTROPHE (')</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's system identifier to the empty
           ;; string (not missing),
           (setf (h5-doctype-token-system-id *h5-curtok*) "")
           ;; then switch to the <span>DOCTYPE system identifier
           ;; (single-quoted) state</span>.</dd>
           (h5-swith-state 'h5-DOCTYPE-system-identifier-single-quoted-state))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Emit that DOCTYPE token.
           (h5-emit)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Switch to the <span>bogus DOCTYPE state</span>.</dd>
           (h5-switch-state 'h5-bogus-DOCTYPE-state)))))

(defun h5-between-DOCTYPE-public-and-system-identifiers-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>Ignore the character.</dd>
           )
	((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
         ;; <dd>Switch to the <span>data state</span>.
         (h5-switch-state 'h5-data-state)
         ;; Emit the current DOCTYPE token.</dd>
         (h5-emit))
        ((eq char ?\") ;; <dt>U+0022 QUOTATION MARK (&quot;)</dt>
         ;; <dd>Set the DOCTYPE token's system identifier to the empty
         ;; string (not missing),
         (setf (h5-doctype-token-system-id *h5-curtok*) "")
         ;; then switch to the <span>DOCTYPE system identifier
         ;; (double-quoted) state</span>.</dd>
         (h5-switch-state 'h5-DOCTYPE-system-identifier-double-quoted-state))
        ((eq char ?') ;; <dt>U+0027 APOSTROPHE (')</dt>
         ;; <dd>Set the DOCTYPE token's system identifier to the empty
         ;; string (not missing),
         (setf (h5-doctype-token-system-id *h5-curtok*) "")
         ;; then switch to the <span>DOCTYPE system identifier
         ;; (single-quoted) state</span>.</dd>
         (h5-switch-state 'h5-DOCTYPE-system-identifier-single-quoted-state))
        ((eq char :eof) ;; <dt>EOF</dt>
         ;; <dd><span>Parse error</span>.
         (h5-parse-error)
         ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
         ;; <i>on</i>.
         (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
         ;; Emit that DOCTYPE token.
         (h5-emit)
         ;; Reconsume the EOF character
         (backward-char 1)
         ;; in the <span>data state</span>.</dd>
         (h5-switch-state 'h5-data-state))
        (t ;; <dt>Anything else</dt>
         ;; <dd><span>Parse error</span>.
         (h5-parse-error)
         ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
         ;; <i>on</i>.
         (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
         ;; Switch to the <span>bogus DOCTYPE state</span>.</dd>
         (h5-switch-state 'h5-bogus-DOCTYPE-state)))))

(defun h5-after-DOCTYPE-system-keyword-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>Switch to the <span>before DOCTYPE system identifier
           ;; state</span>.</dd>
           (h5-switch-state 'h5-before-DOCTYPE-system-identifier-state))
          ((eq char ?\") ;; <dt>U+0022 QUOTATION MARK (&quot;)</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's system identifier to the empty
           ;; string (not missing),
           (setf (h5-doctype-token-system-id *h5-curtok*) "")
           ;; then switch to the <span>DOCTYPE system identifier
           ;; (double-quoted) state</span>.</dd>
           (h5-switch-state 'h5-DOCTYPE-system-identifier-double-quoted-state))
          ((eq char ?') ;; <dt>U+0027 APOSTROPHE (')</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's system identifier to the empty
           ;; string (not missing),
           (setf (h5-doctype-token-system-id *h5-curtok*) "")
           ;; then switch to the <span>DOCTYPE system identifier
           ;; (single-quoted) state</span>.</dd>
           (h5-switch-state 'h5-DOCTYPE-system-identifier-single-quoted-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit that DOCTYPE token.</dd>
           (h5-emit))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Emit that DOCTYPE token.
           (h5-emit)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Switch to the <span>bogus DOCTYPE state</span>.</dd>
           (h5-switch-state 'h5-bogus-DOCTYPE-state)))))

(defun h5-before-DOCTYPE-system-identifier-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>Ignore the character.</dd>
           )
          ((eq char ?\") ;; <dt>U+0022 QUOTATION MARK (&quot;)</dt>
           ;; <dd>Set the DOCTYPE token's system identifier to the empty
           ;; string (not missing),
           (setf (h5-doctype-token-system-id *h5-curtok*) "")
           ;; then switch to the <span>DOCTYPE system identifier
           ;; (double-quoted) state</span>.</dd>
           (h5-switch-state 'h5-DOCTYPE-system-identifier-double-quoted-state))
          ((eq char ?') ;; <dt>U+0027 APOSTROPHE (')</dt>
           ;; <dd>Set the DOCTYPE token's system identifier to the empty
           ;; string (not missing),
           (setf (h5-doctype-token-system-id *h5-curtok*) "")
           ;; then switch to the <span>DOCTYPE system identifier
           ;; (single-quoted) state</span>.</dd>
           (h5-switch-state 'h5-DOCTYPE-system-identifier-single-quoted-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit that DOCTYPE token.</dd>
           (h5-emit))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Emit that DOCTYPE token.
           (h5-emit)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Switch to the <span>bogus DOCTYPE state</span>.</dd>
           (h5-switch-state 'h5-bogus-DOCTYPE-state)))))

(defun h5-DOCTYPE-system-identifier-double-quoted-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character)))
    (cond ((eq char ?\") ;; <dt>U+0022 QUOTATION MARK (&quot;)</dt>
           ;; <dd>Switch to the <span>after DOCTYPE system identifier
           ;; state</span>.</dd>
           (h5-switch-state 'h5-after-DOCTYPE-system-identifier-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit that DOCTYPE token.</dd>
           (h5-emit))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Emit that DOCTYPE token.
           (h5-emit)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Append the <span>current input character</span> to the
           ;; current DOCTYPE token's system identifier.</dd>
           (setf (h5-doctype-token-system-id *h5-curtok*)
                 (concat (h5-doctype-token-system-id *h5-curtok*)
                         (string char)))))))

(defun h5-DOCTYPE-system-identifier-single-quoted-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character)))
    (cond ((eq char ?') ;; <dt>U+0027 APOSTROPHE (')</dt>
           ;; <dd>Switch to the <span>after DOCTYPE system identifier
           ;; state</span>.</dd>
           (h5-switch-state 'h5-after-DOCTYPE-system-identifier-state))
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit that DOCTYPE token.</dd>
           (h5-emit))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Emit that DOCTYPE token.
           (h5-emit)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Append the <span>current input character</span> to the
           ;; current DOCTYPE token's system identifier.</dd>
           (setf (h5-doctype-token-system-id *h5-curtok*)
                 (concat (h5-doctype-token-system-id *h5-curtok*)
                         (string char)))))))

(defun h5-after-DOCTYPE-system-identifier-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character))
         (char-str (if (numberp char) (string char) char)))
    (cond (;; <dt>U+0009 CHARACTER TABULATION</dt>
           ;; <dt>U+000A LINE FEED (LF)</dt>
           ;; <dt>U+000C FORM FEED (FF)</dt>
           ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
           ;; <dt>U+0020 SPACE</dt>
           (h5-space-p char-str)
           ;; <dd>Ignore the character.</dd>
           )
          ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd>Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit the current DOCTYPE token.</dd>
           (h5-emit))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Set the DOCTYPE token's <i>force-quirks flag</i> to
           ;; <i>on</i>.
           (setf (h5-doctype-token-force-quirks *h5-curtok*) t)
           ;; Emit that DOCTYPE token.
           (h5-emit)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd><span>Parse error</span>.
           (h5-parse-error)
           ;; Switch to the <span>bogus DOCTYPE state</span>. (This does
           ;; <em>not</em> set the DOCTYPE token's <i>force-quirks
           ;; flag</i> to <i>on</i>.)</dd>
           (h5-switch-state 'h5-bogus-DOCTYPE-state)))))

(defun h5-bogus-DOCTYPE-state ()
  ""
  ;; Consume the <span>next input character</span>:
  (let* ((char (h5-consume-the-next-input-character)))
    (cond ((eq char ?>) ;; <dt>U+003E GREATER-THAN SIGN (&gt;)</dt>
           ;; <dd>Switch to the <span>data state</span>.
           (h5-switch-state 'h5-data-state)
           ;; Emit the DOCTYPE token.</dd>
           (h5-emit))
          ((eq char :eof) ;; <dt>EOF</dt>
           ;; <dd>Emit the DOCTYPE token.
           (h5-emit)
           ;; Reconsume the EOF character
           (backward-char 1)
           ;; in the <span>data state</span>.</dd>
           (h5-switch-state 'h5-data-state))
          (t ;; <dt>Anything else</dt>
           ;; <dd>Ignore the character.</dd>
           ))))

(defun h5-CDATA-section-state ()
  ""
  ;; Consume every character up to the next occurrence of the three
  ;; character sequence U+005D RIGHT SQUARE BRACKET U+005D RIGHT SQUARE
  ;; BRACKET U+003E GREATER-THAN SIGN (<code title="">]]></code>), or
  ;; the end of the file (EOF), whichever comes first.
  (let ((beginning (point))
        (found (re-search-forward "]]>" nil 'advance)))
    ;; Emit a series of character tokens consisting of all the
    ;; characters consumed except the matching three character sequence
    ;; at the end (if one was found before the end of the file).
    (h5-emit-string
     (buffer-substring beginning (if found (3- found) (point))))
    ;; Switch to the <span>data state</span>.
    (h5-switch-state 'h5-data-state)
    ;; If the end of the file was reached, reconsume the EOF character.
    (unless found
      (backward-char 1))))

;; Tokenizing character references

;; This section defines how to <dfn>consume a character reference</dfn>.
;; This definition is used when parsing character references <span
;; title="character reference in data state">in text</span> and <span
;; title="character reference in attribute value state">in
;; attributes</span>.

(defvar h5-charref-overrides
  '((#x00 . #xFFFD) (#x0D . #x000D) (#x80 . #x20AC) (#x81 . #x0081)
    (#x82 . #x201A) (#x83 . #x0192) (#x84 . #x201E) (#x85 . #x2026)
    (#x86 . #x2020) (#x87 . #x2021) (#x88 . #x02C6) (#x89 . #x2030)
    (#x8A . #x0160) (#x8B . #x2039) (#x8C . #x0152) (#x8D . #x008D)
    (#x8E . #x017D) (#x8F . #x008F) (#x90 . #x0090) (#x91 . #x2018)
    (#x92 . #x2019) (#x93 . #x201C) (#x94 . #x201D) (#x95 . #x2022)
    (#x96 . #x2013) (#x97 . #x2014) (#x98 . #x02DC) (#x99 . #x2122)
    (#x9A . #x0161) (#x9B . #x203A) (#x9C . #x0153) (#x9D . #x009D)
    (#x9E . #x017E) (#x9F . #x0178)))

(defun h5-consume-a-character-reference ()
  ""
  ;; The behavior depends on the identity of the next character (the
  ;; one immediately after the U+0026 AMPERSAND character):
  (let* ((char (char-after))
         (char-str (if (numberp char) (string char) char))
         (char2 (char-after (1+ (point))))
         hexadecimal-flag
         range
         num
         num-start
         num-end)
    (cond ((or ;; <dt>U+0009 CHARACTER TABULATION</dt>
            ;; <dt>U+000A LINE FEED (LF)</dt>
            ;; <dt>U+000C FORM FEED (FF)</dt>
            ;; <!--<dt>U+000D CARRIAGE RETURN (CR)</dt>-->
            ;; <dt>U+0020 SPACE</dt>
            (h5-space-p char-str)
            ;; <dt>U+003C LESS-THAN SIGN</dt>
            ;; <dt>U+0026 AMPERSAND</dt>
            (and (stringp char-str)
                 (string-match "[<&]" char-str))
            ;; <dt>EOF</dt>
            (eq (1+ (point)) (point-max))
            ;; <dt>The <dfn>additional allowed character</dfn>, if there
            ;; is one</dt>
            (eq char *h5-additional-allowed-character*))
           ;; <dd>Not a character reference. No characters are consumed,
           ;; and nothing is returned. (This is not an error,
           ;; either.)</dd>
           )
          ((eq char ?#) ;; <dt>U+0023 NUMBER SIGN (#)</dt>
           ;; Consume the U+0023 NUMBER SIGN.
           (forward-char 1)
           ;; The behavior further depends on the character after the
           ;; U+0023 NUMBER SIGN:
           (setq range
                 (cond ((or
                         ;; <dt>U+0078 LATIN SMALL LETTER X</dt>
                         (eq char2 ?x)
                         ;; <dt>U+0058 LATIN CAPITAL LETTER X</dt>
                         (eq char2 ?X))
                        ;; Consume the X.
                        (setq hexadecimal-flag t)
                        (forward-char 1)
                        ;; Follow the steps below, but using the range of
                        ;; characters U+0030 DIGIT ZERO (0) to U+0039 DIGIT NINE
                        ;; (9), U+0061 LATIN SMALL LETTER A to U+0066 LATIN
                        ;; SMALL LETTER F, and U+0041 LATIN CAPITAL LETTER A to
                        ;; U+0046 LATIN CAPITAL LETTER F (in other words, 0-9,
                        ;; A-F, a-f).
                        ;;
                        ;; When it comes to interpreting the number, interpret
                        ;; it as a hexadecimal number.
                        "[0-9A-Za-z]")
                       (t
                        "[0-9]")))

           ;; Consume as many characters as match the range of
           ;; characters given above.
           (setq num-start (point)
                 num-end nil)
           (while (looking-at range)
             (forward-char 1))

           (cond (;; If no characters match the range, then don't
                  ;; consume any characters
                  (eq num-start (point))
                  ;; (and unconsume the U+0023 NUMBER SIGN character
                  ;; and, if appropriate, the X character).
                  (backward-char (if hexadecimal-flag 2 1))
                  ;; This is a <span>parse error</span>
                  (h5-parse-error)
                  ;; nothing is returned.
                  nil)
                 (t ;; Otherwise,
                  (setq num-end (point))
                  ;; if the next character is a U+003B SEMICOLON,
                  (if (looking-at ";")
                      ;; consume that too.
                      (forward-char 1)
                    ;; If it isn't, there is a <span>parse error</span>.
                    (h5-parse-error))

                  ;; If one or more characters match the range, then
                  ;; take them all and interpret the string of
                  ;; characters as a number (either hexadecimal or
                  ;; decimal as appropriate).
                  (setq num
                        (string-to-number
                         (buffer-substring num-start num-end)
                         (if hexadecimal-flag 16 10)))

                  (let ((override (assoc num h5-charref-overrides)))
                    ;; If that number is one of the numbers in the first
                    ;; column of the following table,
                    (cond (override
                           ;; then this is a <span>parse error</span>.
                           (h5-parse-error)
                           ;; Find the row with that number in the first
                           ;; column, and return a character token for
                           ;; the Unicode character given in the second
                           ;; column of that row.
                           (cdr override))
                          ((or ;; Otherwise,
                            ;; if the number is in the range 0xD800 to
                            ;; 0xDFFF<!-- surrogates not allowed; see
                            ;; the comment in the "preprocessing the
                            ;; input stream" section for details -->
                            (and (>= num #xD800) (<= num #xDFFF))
                            ;; or is greater than 0x10FFFF,
                            (> num #x10FFFF))
                           ;; then this is a <span>parse error</span>.
                           (h5-parse-error)
                           ;; Return a U+FFFD REPLACEMENT CHARACTER.
                           #xFFFD)
                          (t ;; Otherwise
                           ;; return a character token for the Unicode
                           ;; character whose code point is that number.
                           num))))))
          (t
           ;; Consume the maximum number of characters possible, with
           ;; the consumed characters matching one of the identifiers in
           ;; the first column of the <span>named character
           ;; references</span> table (in a <span>case-sensitive</span>
           ;; manner).
           (let ((beginning (point))
                 (has-subtrie t)
                 match)
             (while has-subtrie
               (forward-char 1)
               (setq has-subtrie (h5-trie-subtrie
                                  html5-named-character-references
                                  (buffer-substring beginning (point)))))
             (backward-char 1)
             (setq match
                   (h5-trie-member-p html5-named-character-references
                                     (buffer-substring beginning (point))))

             (cond ((eq match nil) ;; If no match can be made
                    ;; then no characters are consumed,
                    (goto-char beginning)
                    ;; and nothing is returned.
                    (prog1 nil
                      ;; In this case, if the characters after the
                      ;; U+0026 AMPERSAND character (&amp;) consist of a
                      ;; sequence of one or more characters in the range
                      ;; U+0030 DIGIT ZERO (0) to U+0039 DIGIT NINE (9),
                      ;; U+0061 LATIN SMALL LETTER A to U+007A LATIN
                      ;; SMALL LETTER Z, and U+0041 LATIN CAPITAL LETTER
                      ;; A to U+005A LATIN CAPITAL LETTER Z, followed by
                      ;; a U+003B SEMICOLON character (;),
                      (when (looking-at "([0-9a-zA-Z]+);")
                        ;; then this is a <span>parse error</span>.
                        (h5-parse-error))))
                   ((and
                     ;; If the character reference is being consumed
                     ;; <span title="character reference in attribute
                     ;; value state">as part of an attribute</span>,
                     (eq (h5-current-state)
                         'h5-character-reference-in-attribute-value-state)
                     ;; and the last character matched is not a U+003B
                     ;; SEMICOLON character (;),
                     (not (eq (char-before) ?\;))
                     ;; and the next character is either
                     (or
                      ;; a U+003D EQUALS SIGN character (=) or in the
                      ;; range U+0030 DIGIT ZERO (0) to U+0039 DIGIT
                      ;; NINE (9), U+0041 LATIN CAPITAL LETTER A to
                      ;; U+005A LATIN CAPITAL LETTER Z, or U+0061 LATIN
                      ;; SMALL LETTER A to U+007A LATIN SMALL LETTER Z
                      (string-match "[=0-9A-Z-a-z]"
                       (string (char-after)))))
                    ;; then, for historical reasons, all the characters
                    ;; that were matched after the U+0026 AMPERSAND
                    ;; character (&amp;) must be unconsumed,
                    (goto-char beginning)
                    ;; and
                    ;; nothing is returned.
                    nil)

                   ;; Otherwise,
                   (t ;; a character reference is parsed.
                    ;; If the last character matched is not a U+003B
                    ;; SEMICOLON character (;),
                    (when (not (eq (char-before) ?\;))
                      ;; there is a <span>parse error</span>.
                      (h5-parse-error))

                    ;; Return a character token for the character
                    ;; corresponding to the character reference name (as
                    ;; given by the second column of the <span>named
                    ;; character references</span> table).
                    match)))))))

(defun html5-tok-forward (&optional from initial-state)
  ""
  (when from
    (goto-char from))
  (let ((*h5-curtok* nil)
        (*h5-curattr* nil)
        (*h5-curstate* nil)
        (*h5-prevstate* nil)
        (*h5-statestart* (or from (point))))
    ;; The state machine must start in the <span>data state</span>.
    (h5-switch-state (or initial-state 'h5-data-state))
    ;; (h5-clear-charbuf)
    (catch 'h5-emit
      (while t
        (if (fboundp (h5-current-state))
            (funcall (h5-current-state))
          ;; If this *ever* happens, it's because I'm an idiot.
          (error "Unknown state %s" (h5-current-state)))))))

(provide 'html5-tok)
;;; html5-tok.el ends here
