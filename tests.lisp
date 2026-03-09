;;; tests.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Test suite for the completions library using FiveAM.
;;; All tests use fresh *tools* hash tables for isolation and make no live API calls.

(defpackage #:completions/tests
  (:use #:cl #:fiveam)
  (:import-from #:str #:starts-with-p))

(in-package #:completions/tests)

(def-suite completions-suite
  :description "Test suite for the completions library.")

(in-suite completions-suite)

;;; ---- Tool rendering tests ----

(test render-openai-tool
  "render produces correct OpenAI tool JSON structure."
  (let ((completions::*tools* (make-hash-table :test 'equalp))
          (completions::*safe-tools* (make-hash-table :test 'equalp)))
    (completions::defun-tool test-tool ((city string "The city name")
                                        (units string "Temperature units"))
      "Get the weather"
      (format nil "Weather in ~A (~A)" city units))
    (let* ((tool (gethash "TEST-TOOL" completions::*tools*))
           (rendered (completions::render tool)))
      (is (string= "function" (cdr (assoc :type rendered))))
      (let ((func (cdr (assoc :function rendered))))
        (is (string= "TEST-TOOL" (cdr (assoc :name func))))
        (is (string= "Get the weather" (cdr (assoc :description func))))
        (let ((params (cdr (assoc :parameters func))))
          (is (string= "object" (cdr (assoc :type params))))
          (is (= 2 (length (cdr (assoc :properties params)))))
          (is (= 2 (length (cdr (assoc :required params))))))))))

(test render-openai-no-params
  "render works for tools with no parameters."
  (let ((completions::*tools* (make-hash-table :test 'equalp))
          (completions::*safe-tools* (make-hash-table :test 'equalp)))
    (completions::defun-tool no-param-tool ()
      "A tool with no params"
      "hello")
    (let* ((tool (gethash "NO-PARAM-TOOL" completions::*tools*))
           (rendered (completions::render tool)))
      (is (string= "function" (cdr (assoc :type rendered))))
      (let ((func (cdr (assoc :function rendered))))
        (is (string= "NO-PARAM-TOOL" (cdr (assoc :name func))))
        ;; No :parameters key when no params
        (is (null (assoc :parameters func)))))))

(test render-gemini-tool
  "render-gemini produces correct Gemini functionDeclaration structure."
  (let ((completions::*tools* (make-hash-table :test 'equalp))
          (completions::*safe-tools* (make-hash-table :test 'equalp)))
    (completions::defun-tool gemini-test ((query string "Search query"))
      "Search for something"
      query)
    (let* ((tool (gethash "GEMINI-TEST" completions::*tools*))
           (rendered (completions::render-gemini tool)))
      (is (string= "GEMINI-TEST" (cdr (assoc :name rendered))))
      (is (string= "Search for something" (cdr (assoc :description rendered))))
      (let ((params (cdr (assoc :parameters rendered))))
        (is (string= "object" (cdr (assoc :type params))))
        (is (= 1 (length (cdr (assoc :properties params)))))))))

(test render-anthropic-tool
  "render-anthropic produces correct Anthropic tool definition."
  (let ((completions::*tools* (make-hash-table :test 'equalp))
          (completions::*safe-tools* (make-hash-table :test 'equalp)))
    (completions::defun-tool anthropic-test ((name string "A name"))
      "Greet someone"
      (format nil "Hello ~A" name))
    (let* ((tool (gethash "ANTHROPIC-TEST" completions::*tools*))
           (rendered (completions::render-anthropic tool)))
      (is (string= "ANTHROPIC-TEST" (cdr (assoc :name rendered))))
      (is (string= "Greet someone" (cdr (assoc :description rendered))))
      (let ((schema (cdr (assoc "input_schema" rendered :test #'string=))))
        (is (string= "object" (cdr (assoc "type" schema :test #'string=))))
        (is (= 1 (length (cdr (assoc "properties" schema :test #'string=)))))))))

;;; ---- invoke-tool tests ----

(test invoke-tool-basic
  "invoke-tool calls the tool function with correct arguments."
  (let ((completions::*tools* (make-hash-table :test 'equalp))
          (completions::*safe-tools* (make-hash-table :test 'equalp)))
    (completions::defun-tool adder ((a number "First number")
                                    (b number "Second number"))
      "Add two numbers"
      (format nil "~A" (+ a b)))
    (let ((result (completions::invoke-tool "ADDER" '((:A . 3) (:B . 4)))))
      (is (string= "7" result)))))

(test invoke-tool-unknown
  "invoke-tool returns error string for unknown tools."
  (let ((completions::*tools* (make-hash-table :test 'equalp))
          (completions::*safe-tools* (make-hash-table :test 'equalp)))
    (let ((result (completions::invoke-tool "NONEXISTENT" nil)))
      (is (starts-with-p "Error:" result)))))

(test invoke-tool-error-handling
  "invoke-tool returns error string when tool signals an error."
  (let ((completions::*tools* (make-hash-table :test 'equalp))
          (completions::*safe-tools* (make-hash-table :test 'equalp)))
    (completions::defun-tool failing-tool ()
      "A tool that always fails"
      (error "Something went wrong"))
    (let ((result (completions::invoke-tool "FAILING-TOOL" nil)))
      (is (starts-with-p "Error:" result)))))

;;; ---- map-args-to-parameters tests ----

(test map-args-to-parameters-order
  "map-args-to-parameters maps in declared order regardless of alist key order."
  (let ((completions::*tools* (make-hash-table :test 'equalp))
          (completions::*safe-tools* (make-hash-table :test 'equalp)))
    (completions::defun-tool ordered-tool ((first string "First param")
                                           (second string "Second param")
                                           (third string "Third param"))
      "A tool with ordered params"
      (format nil "~A-~A-~A" first second third))
    (let* ((tool (gethash "ORDERED-TOOL" completions::*tools*))
           ;; Args in different order than declared
           (args '((:THIRD . "c") (:FIRST . "a") (:SECOND . "b")))
           (mapped (completions::map-args-to-parameters tool args)))
      (is (equal '("a" "b" "c") mapped)))))

;;; ---- extract-system-instruction tests ----

(test extract-system-instruction-present
  "extract-system-instruction extracts system messages into Gemini format."
  (let* ((messages '(((:role . "system") (:content . "Be helpful"))
                     ((:role . "user") (:content . "Hello"))))
         (result (completions::extract-system-instruction messages)))
    (is (not (null result)))
    (is (string= "user" (cdr (assoc :role result))))
    (let* ((parts (cdr (assoc :parts result)))
           (text (cdr (assoc :text (first parts)))))
      (is (string= "Be helpful" text)))))

(test extract-system-instruction-absent
  "extract-system-instruction returns nil when no system messages."
  (let* ((messages '(((:role . "user") (:content . "Hello"))))
         (result (completions::extract-system-instruction messages)))
    (is (null result))))

(test extract-system-instruction-multiple
  "extract-system-instruction concatenates multiple system messages."
  (let* ((messages '(((:role . "system") (:content . "Be helpful"))
                     ((:role . "system") (:content . "Be concise"))
                     ((:role . "user") (:content . "Hello"))))
         (result (completions::extract-system-instruction messages)))
    (is (not (null result)))
    (let* ((parts (cdr (assoc :parts result)))
           (text (cdr (assoc :text (first parts)))))
      (is (string= "Be helpful Be concise" text)))))

;;; ---- convert-messages-to-gemini tests ----

(test convert-messages-to-gemini-filters-system
  "convert-messages-to-gemini filters out system messages."
  (let* ((messages '(((:role . "system") (:content . "System msg"))
                     ((:role . "user") (:content . "Hello"))
                     ((:role . "assistant") (:content . "Hi"))))
         (result (completions::convert-messages-to-gemini messages)))
    (is (= 2 (length result)))))

(test convert-messages-to-gemini-role-mapping
  "convert-messages-to-gemini maps assistant to model role."
  (let* ((messages '(((:role . "user") (:content . "Hello"))
                     ((:role . "assistant") (:content . "Hi"))))
         (result (completions::convert-messages-to-gemini messages)))
    (is (string= "user" (cdr (assoc :role (first result)))))
    (is (string= "model" (cdr (assoc :role (second result)))))))

;;; ---- gemini-make-payload tests ----

(test gemini-make-payload-basic
  "gemini-make-payload creates basic payload structure."
  (let* ((contents '(((:role . "user") (:parts . (((:text . "Hello")))))))
         (payload (completions::gemini-make-payload contents nil 100)))
    (is (not (null (assoc :contents payload))))
    (is (not (null (assoc :generation-config payload))))
    (let ((config (cdr (assoc :generation-config payload))))
      (is (= 100 (cdr (assoc :max-output-tokens config)))))
    ;; No tools or system-instruction
    (is (null (assoc :tools payload)))
    (is (null (assoc :system-instruction payload)))))

(test gemini-make-payload-with-system
  "gemini-make-payload includes system instruction when provided."
  (let* ((contents '(((:role . "user") (:parts . (((:text . "Hello")))))))
         (sys-instr '((:role . "user") (:parts . (((:text . "Be helpful"))))))
         (payload (completions::gemini-make-payload contents nil 100 sys-instr)))
    (is (not (null (assoc :system-instruction payload))))))

(test gemini-make-payload-with-response-format
  "gemini-make-payload includes response-mime-type for json_object format."
  (let* ((contents '(((:role . "user") (:parts . (((:text . "Hello")))))))
         (payload (completions::gemini-make-payload contents nil 100 nil "json_object")))
    (let ((config (cdr (assoc :generation-config payload))))
      (is (string= "application/json" (cdr (assoc :response-mime-type config)))))))
