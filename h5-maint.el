(add-to-list 'load-path default-directory)
(mapc (lambda (dir) (add-to-list 'load-path dir))
      (parse-colon-path (getenv "LOAD_PATH")))

(require 'json)
(require 'html5-tok)

(defun h5-explore-text-properties ()
  (when (< (point) (point-max))
    (describe-text-properties (point))))

(define-minor-mode h5-explore-text-properties-mode
  "Explore the text properties in this buffer."
  nil " xtp" nil
  (if explore-text-properties-mode
      (add-hook 'post-command-hook 'h5-explore-text-properties nil t)
    (remove-hook 'post-command-hook 'h5-explore-text-properties t)))

(defun h5-munge-state-name (state-name)
  (intern (format "h5-%s"
                  (mapconcat 'identity (split-string state-name) "-"))))

(defsubst h5-test-input (test)
  (cdr (assoc 'input test)))
(defsubst h5-test-initial-states (test)
  (cdr (assoc 'initialStates test)))
(defsubst h5-test-last-start-tag (test)
  (cdr (assoc 'lastStartTag test)))
(defsubst h5-test-output (test)
  (cdr (assoc 'output test)))
(defsubst h5-test-description (test)
  (cdr (assoc 'description test)))

(defun h5-coalesce-chars (in-toks)
  (let ((out-toks '())
        (in-charrun nil)
        (charrun '()))
    (dolist (tok in-toks)
      (cond
       ((and (numberp tok) (not in-charrun))
        (setq in-charrun t
              charrun '())
        (push tok charrun))
       ((and (numberp tok) in-charrun)
        (push tok charrun))
       (in-charrun
        (push (apply 'string (nreverse charrun)) out-toks)
        (push tok out-toks)
        (setq in-charrun nil
              charrun '()))
       (t
        (push tok out-toks))))
    (if in-charrun
        (push (apply 'string (nreverse charrun)) out-toks))
    (nreverse out-toks)))

(defun h5-compare-doctype-to-expected (doctype expected)
  (and (listp expected)
       (string-equal (nth 0 expected) "DOCTYPE")
       (let ((expected-name (nth 1 expected))
             (expected-public-id (or (nth 2 expected) 'missing))
             (expected-system-id (or (nth 3 expected) 'missing))
             (expected-force-quirks (nth 4 expected)))
         (and
          (string-equal (h5-doctype-token-name actual) expected-name)
          (equal (h5-doctype-token-public-id actual) expected-public-id)
          (equal (h5-doctype-token-system-id actual) expected-system-id)
          (equal (h5-doctype-token-force-quirks actual)
                 expected-force-quirks)))))

(defun h5-compare-attr (actual expected)
  (and (consp expected)
       (h5-attr-p actual)
       (string-equal (h5-attr-name actual)
                     (symbol-name (car expected)))
       (string-equal (h5-attr-value actual)
                     (cdr expected))))

(defun h5-compare-attrs (actual expected)
  (and (listp actual)
       (listp expected)
       (eq (length actual) (length expected))
       (every 'identity
              (loop for a-attr in actual
                    for e-attr in expected
                    collect (h5-compare-attr a-attr e-attr)))))

(defun h5-compare-start-tag-to-expected (start-tag expected)
  (and (listp expected)
       (string-equal (nth 0 expected) "StartTag")
       (string-equal (h5-tag-token-name start-tag) (nth 1 expected))
       (h5-compare-attrs (h5-start-tag-token-attributes start-tag)
                         (nth 2 expected))))

(defun h5-compare-token-to-expected (actual expected)
  (cond ((and
          (stringp actual)
          (listp expected)
          (string-equal (car expected) "Character"))
         (string-equal actual (cadr expected)))
        ((h5-end-tag-token-p actual)
         (and (listp expected)
              (string-equal (car expected) "EndTag")
              (string-equal (h5-tag-token-name actual)
                            (cadr expected))))
        ((eq actual :parse-error)
         (and (stringp expected)
              (string-equal expected "ParseError")))
        ((h5-doctype-token-p actual)
         (h5-compare-doctype-to-expected actual expected))
        ((h5-start-tag-token-p actual)
         (h5-compare-start-tag-to-expected actual expected))
        ((h5-comment-token-p actual)
         (and (listp expected)
              (string-equal (car expected) "Comment")
              (string-equal (h5-comment-token-data actual)
                            (cadr expected))))
        (t
         ;; (princ (make-string 72 ?-))
         ;; (terpri)
         ;; (princ (format "actual %S\n" actual))
         ;; (princ (format "expected %S\n" expected))
         (error "comparing apples and oranges?"))))

(defun h5-compare-tokens-to-expected-output (actual expected)
  (every 'identity
         (loop for actual-tok in actual
               for expected-tok in expected
               collect (h5-compare-token-to-expected actual-tok expected-tok))))

(defun h5-run-test-1 (test initial-state)
  (let ((tokens '())
        (token t)
        (passed nil)
        (h5-last-start-tag-emitted
         (if (h5-test-last-start-tag test)
             (make-h5-start-tag-token
              :name (h5-test-last-start-tag test))
           nil)))
    (setq initial-state
          (if initial-state
              (h5-munge-state-name initial-state)
            nil))
    (with-temp-buffer
      (insert (h5-test-input test))
      (goto-char (point-min))
      (while (not (eq token :eof))
        (setq token (html5-tok-forward nil initial-state))
        (setq initial-state nil)
        (unless (eq token :eof)
          (push token tokens)))
      (setq tokens (h5-coalesce-chars (nreverse tokens)))
      (setq passed
            (h5-compare-tokens-to-expected-output tokens (h5-test-output test))))
    passed))

(defun h5-run-test (test)
  (let ((initial-states (or (h5-test-initial-states test) (list "data state"))))
    (condition-case e
        (every 'identity
               (mapcar
                (lambda (initial-state)
                  (h5-run-test-1 test initial-state))
                initial-states))
      (error
       ;; (princ (format "Blew up: %S!\n" e))
       nil))))

(defun h5-run-tests ()
  (let ((total 0)
        (passed 0)
        (PASSED "\033[32;1mPASS\033[0m")
        (FAILED "\033[31;1mFAIL\033[0m"))
    (dolist (testfile command-line-args-left)
      (princ (format "Running tests from %s:\n"
                     (file-name-nondirectory testfile)))
      (let ((testdata
             (let ((json-object-type 'alist)
                   (json-array-type 'list))
               (json-read-file testfile))))
        (dolist (test (cdr (assoc 'tests testdata)))
          (incf total)
          (princ (format "\t'%s': %s\n"
                         (h5-test-description test)
                         (if (h5-run-test test)
                             (prog1 PASSED (incf passed))
                           FAILED))))))
    (princ (format "Passed %d out of %d tests (%02.1f%%)"
                   passed total
                   (* 100 (/ (float passed) (float total))))))
  (setq command-line-args-left nil))
