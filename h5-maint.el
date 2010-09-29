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

(defun h5-compare-token-to-expected (actual expected)
  (cond
   ((stringp actual)
    (and (listp expected)
         (eq (car expected) 'Character)
         (string-equal actual (cadr expected))))
   (t
    (error "comparing apples and oranges?"))))

(defun h5-compare-tokens-to-expected-output (actual expected)
  (every
   'identity
   (loop for actual-tok in actual
         for expected-tok in expected
         collect (h5-compare-token-to-expected actual-tok expected-tok))))

(defun h5-run-test-1 (test initial-state)
  (let ((tokens '())
        (token t))
    (with-temp-buffer
      (insert (h5-test-input test))
      (goto-char (point-min))
      (while (not (eq token :eof))
        (setq token (html5-tok-forward
                     nil
                     (if initial-state (h5-munge-state-name initial-state))))
        (setq initial-state nil)
        (unless (eq token :eof)
          (push token tokens)))
      (setq tokens (h5-coalesce-chars (nreverse tokens)))
      (h5-compare-tokens-to-expected-output tokens (h5-test-output test)))))

(defun h5-run-test (test)
  (let ((initial-states (or (h5-test-initial-states test) (list "data state"))))
    (condition-case e
        (dolist (initial-state initial-states)
          (h5-run-test-1 test initial-state))
      (error
       (princ (format "Blew up: %s!\n" e))
       nil))))

(defun h5-run-tests ()
  (let ((total 0)
        (passed 0))
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
                             (prog1 "PASS" (incf passed))
                           "FAIL"))))))
    (princ (format "Passed %d out of %d tests (%02.1f%%)"
                   passed total
                   (* 100 (/ (float passed) (float total))))))
  (setq command-line-args-left nil))
