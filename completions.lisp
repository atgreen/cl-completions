;;; completions.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2024, 2025  Anthony Green <green@moxielogic.com>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.
;;;

(in-package :completions)

;; Set this to an output stream to see debug logs.
(defvar *debug-stream* nil)

;; 120 second read timeout by default
(defvar *read-timeout* 120)

(defvar *tools* (make-hash-table :test 'equalp))

;; Permission and safety system
(defvar *permission-callback* nil
  "Callback function for tool permission requests. Should accept (tool-name args description) and return boolean.")

(defvar *default-safety-level* :safe
  "Default safety level for tools: :safe, :requires-approval, or :dangerous")

(defvar *safe-tools* (make-hash-table :test 'equalp)
  "Hash table mapping tool names to their safety levels")

;; Tool categories
(defvar *tool-categories* (make-hash-table :test 'equalp)
  "Hash table mapping tool names to their categories")

;; Context binding support
(defvar *tool-context-vars* nil
  "List of dynamic variables to capture during tool execution")

;; Event hooks
(defvar *tool-start-hooks* nil
  "List of functions to call when a tool starts executing")

(defvar *tool-complete-hooks* nil
  "List of functions to call when a tool completes successfully")

(defvar *tool-error-hooks* nil
  "List of functions to call when a tool encounters an error")

(defclass completer ()
  ())

(defclass openai-completer (completer)
  ((endpoint :initform "https://api.openai.com/v1/chat/completions" :initarg :endpoint)
   (api-key :initarg :api-key)
   (prompt-token-count :initform 0)
   (completion-token-count :initform 0)
   (model :initarg :model :initform "gpt-4")
   (tools :initarg :tools :initform (list))))

(defclass anthropic-completer (completer)
  ((endpoint :initform "https://api.anthropic.com/v1/messages" :initarg :endpoint)
   (api-key :initarg :api-key)
   (model :initarg :model :initform "claude-3-opus-20240229")
   (tools :initarg :tools :initform (list))))

(defclass ollama-completer (completer)
  ((endpoint :initform "http://localhost:11434/api/chat" :initarg :endpoint)
   (model :initarg :model)))

(defclass tool ()
  ())

(defclass function-tool (tool)
  ((description :initarg :description)
   (name :initarg :name)
   (parameters :initarg :parameters)
   (fn :initarg :fn)
   (safety-level :initarg :safety-level :initform nil)
   (category :initarg :category :initform nil)
   (context-vars :initarg :context-vars :initform nil)
   (requires-approval :initarg :requires-approval :initform nil)
   (approval-description :initarg :approval-description :initform nil)
   (permission-callback :initarg :permission-callback :initform nil)
   (on-start :initarg :on-start :initform nil)
   (on-complete :initarg :on-complete :initform nil)
   (on-error :initarg :on-error :initform nil)
   (parameter-validators :initarg :parameter-validators :initform nil)))

(defmethod render ((tool function-tool))
  (with-slots (description name parameters) tool
    `((:type . "function")
      (:function . ((:name . ,name)
                    (:description . ,description)
                    ,(when parameters
                       `(:parameters . ((:type . "object")
                                        (:properties . ,(loop for p in parameters
                                                              collect (cons (first p)
                                                                            (list
                                                                             (cons :type (second p))
                                                                             (cons :description (third p))))))
                                        (:required . ,(loop for p in parameters
                                                            collect (first p)))))))))))

(defmacro defun-tool (name args description &rest options-and-body)
  ;; Parse options and body
  (let ((body nil)
        (safety-level nil)
        (category nil)
        (context-vars nil)
        (requires-approval nil)
        (approval-description nil)
        (permission-callback nil)
        (on-start nil)
        (on-complete nil)
        (on-error nil)
        (parameter-validators nil))

    ;; Parse keyword options and body
    (dolist (item options-and-body)
      (cond
        ((and (listp item) (keywordp (first item)))
         (case (first item)
           (:safety-level (setf safety-level (second item)))
           (:category (setf category (second item)))
           (:context-vars (setf context-vars (rest item)))
           (:requires-approval (setf requires-approval (second item)))
           (:approval-description (setf approval-description (second item)))
           (:permission-callback (setf permission-callback (second item)))
           (:on-start (setf on-start (second item)))
           (:on-complete (setf on-complete (second item)))
           (:on-error (setf on-error (second item)))
           (:parameter-validators 
            ;; Convert flat list (param1 validator1 param2 validator2...) to pairs
            (let ((validator-list (rest item)))
              (setf parameter-validators 
                    `',(loop for i from 0 below (length validator-list) by 2
                             when (< (1+ i) (length validator-list))
                             collect (list (nth i validator-list) (nth (1+ i) validator-list))))))))
        (t (push item body))))

    (setf body (nreverse body))

    ;; Compile time checks
    (unless (listp args)
      (error "ARGS must be a list."))
    (unless (stringp description)
      (error "DESCRIPTION must be a string."))
    (dolist (arg args)
      (unless (member (second arg) '(string number boolean) :test #'equal)
        (error "Unsupported defun-tool argument type: ~a" (second arg))))

    ;; Generate code
    (let ((name-str (if (symbolp name) (symbol-name name) name))
          (arg-names (mapcar #'first args)))
      `(progn
         ;; Define and set the function in *tools*
         (setf (gethash ,name-str *tools*)
               (make-instance 'function-tool
                              :name ,name-str
                              :description ,description
                              :parameters ',args
                              :safety-level ,(or safety-level '*default-safety-level*)
                              :category ',category
                              :context-vars ',context-vars
                              :requires-approval ,requires-approval
                              :approval-description ,approval-description
                              :permission-callback ,permission-callback
                              :on-start ,on-start
                              :on-complete ,on-complete
                              :on-error ,on-error
                              :parameter-validators ,parameter-validators
                              :fn ,(if (or safety-level category context-vars requires-approval
                                           approval-description permission-callback on-start on-complete
                                           on-error parameter-validators)
                                       ;; Enhanced tool using execute-tool
                                       `(lambda ,arg-names
                                          (execute-tool
                                           ,name-str
                                           (list ,@(mapcar (lambda (arg-name) `(cons ',arg-name ,arg-name)) arg-names))
                                           (lambda () ,@body)))
                                       ;; Basic tool
                                       `(lambda ,arg-names
                                          ,@body))))

         ;; Register tool metadata
         ,(when category
            `(setf (gethash ,name-str *tool-categories*) ',category))
         ,(when safety-level
            `(setf (gethash ,name-str *safe-tools*) ,safety-level))))))

(defun execute-tool (tool-name args body-fn)
  "Execute a tool with permission checks, context binding, hooks, etc."
  (let ((tool (gethash tool-name *tools*)))
    (unless tool
      (error "Unknown tool: ~A" tool-name))

    (with-slots (safety-level requires-approval approval-description permission-callback
                 context-vars on-start on-complete on-error parameter-validators) tool

      ;; Parameter validation
      (when parameter-validators
        (validate-tool-parameters tool-name args parameter-validators))

      ;; Permission check
      (when (or requires-approval
                (and safety-level (eq safety-level :requires-approval)))
        (let* ((description (if (functionp approval-description)
                                (funcall approval-description args)
                                (or approval-description
                                    (format nil "Execute ~A" tool-name))))
               (callback (or permission-callback *permission-callback*)))
          (unless (and callback (funcall callback tool-name args description))
            (error "Tool execution denied: ~A" tool-name))))

      ;; Execute with context binding and hooks
      (handler-case
          (let (result)
            ;; Start hooks
            (when on-start
              (funcall on-start args))
            (dolist (hook *tool-start-hooks*)
              (funcall hook tool-name args))

            ;; Context binding
            (if context-vars
                (progv context-vars
                       (mapcar #'symbol-value context-vars)
                  (setf result (funcall body-fn)))
                (setf result (funcall body-fn)))

            ;; Complete hooks
            (when on-complete
              (funcall on-complete result))
            (dolist (hook *tool-complete-hooks*)
              (funcall hook tool-name result))

            result)
        (error (e)
          ;; Error hooks
          (when on-error
            (funcall on-error e))
          (dolist (hook *tool-error-hooks*)
            (funcall hook tool-name e))
          (error e))))))

;; Tool discovery and introspection functions
(defun list-available-tools ()
  "Return a list of all available tool names"
  (loop for tool-name being the hash-keys of *tools*
        collect tool-name))

(defun get-tool-info (tool-name)
  "Get detailed information about a tool"
  (let ((tool (gethash (if (symbolp tool-name) (symbol-name tool-name) tool-name) *tools*)))
    (when tool
      (with-slots (description parameters safety-level category requires-approval) tool
        (list :name tool-name
              :description description
              :parameters parameters
              :safety-level safety-level
              :category category
              :requires-approval requires-approval)))))

(defun get-tools-by-category (category)
  "Get all tools in a specific category"
  (loop for tool-name being the hash-keys of *tool-categories*
        for tool-category being the hash-values of *tool-categories*
        when (eq tool-category category)
        collect tool-name))

(defun get-tools-by-safety-level (safety-level)
  "Get all tools with a specific safety level"
  (loop for tool-name being the hash-keys of *safe-tools*
        for tool-safety being the hash-values of *safe-tools*
        when (eq tool-safety safety-level)
        collect tool-name))

(defun get-tool-safety-level (tool-name)
  "Get the safety level of a tool"
  (gethash (if (symbolp tool-name) (symbol-name tool-name) tool-name) *safe-tools*))

(defun set-tool-safety-level (tool-name safety-level)
  "Set the safety level of a tool"
  (setf (gethash (if (symbolp tool-name) (symbol-name tool-name) tool-name) *safe-tools*) safety-level))

(defun validate-tool-parameters (tool-name args validators)
  "Validate tool parameters using provided validators"
  (dolist (validator validators)
    (let ((param-name (first validator))
          (validator-fn (second validator)))
      (let ((param-value (cdr (assoc param-name args))))
        (unless (funcall validator-fn param-value)
          (error "Parameter validation failed for ~A in tool ~A" param-name tool-name))))))

(defun read-streamed-json-objects (stream streaming-callback)
  (loop for line = (read-line stream nil 'eof)
        when *debug-stream*
          do (format *debug-stream* "~&completions line: ~A~%" line)
        until (or (eq line 'eof) (string= "data: [DONE]" line))
        when (and (> (length line) 6) (string= "data: {" (subseq line 0 7)))
        collect (let ((json (json:decode-json-from-string (subseq line 6))))
                  (let ((first-choice (cdr (assoc :delta (car (cdr (assoc :choices json)))))))
                    (unless (assoc :tool--calls first-choice)
                      (let ((text (cdr (assoc :content first-choice))))
                        (when text
                          (funcall streaming-callback text)))))
                  json)))

(defun concatenate-arguments (sexps)
  "Concatenate all arguments for the given index from the list of s-expressions."
  (let ((argument-strings '()))
    ;; Loop through each s-expression.
    (dolist (sexp sexps)
      ;; Navigate through the nested structure to find the :TOOL--CALLS.
      (let* ((choices (cdr (assoc :CHOICES sexp)))
             (delta (cdr (assoc :DELTA (car choices))))
             (tool-calls (cdr (assoc :TOOL--CALLS delta))))
        ;; Loop through each :TOOL--CALLS entry.
        (dolist (call tool-calls)
          ;; Add the argument string to the list if the index matches.
          (let ((args (cdr (assoc :ARGUMENTS (cdr (assoc :FUNCTION call))))))
            (push args argument-strings)))))
    ;; Concatenate all collected argument strings in reverse order to maintain the original order.
    (apply #'concatenate 'string (reverse argument-strings))))

(defun convert-byte-array-to-utf8 (byte-array)
  (let ((encodings '(:utf-8 :iso-8859-1 :windows-1252)))
    (loop for encoding in encodings
          do (handler-case
                 (return (babel:octets-to-string byte-array :encoding encoding :errorp t))
               (error () (when *debug-stream* (format *debug-stream* "~&*BARF* on string conversion~%")) nil)))))

(defun completions-loop (provider endpoint headers messages payload-format-string streaming-callback)
  (let ((payload (format nil payload-format-string (json:encode-json-to-string messages)))
        (objs))
    (if streaming-callback
        (let* ((response-stream (dex:post endpoint
                                          :read-timeout *read-timeout*
                                          :content payload
                                          :headers headers
                                          :want-stream t)))
          (unwind-protect
               (setf objs (read-streamed-json-objects response-stream streaming-callback))
            (close response-stream))
          (let ((tool-calls (cdr (assoc :tool--calls (cdr (assoc :delta (car (cdr (assoc :choices (car objs))))))))))
            (if tool-calls
                (let* ((arg-string (concatenate-arguments objs))
                       (tool-answers (loop for tool-call in tool-calls
                                           do (setf (cdr (assoc :arguments (cdr (assoc :function tool-call)))) arg-string)
                                           collect (let* ((args (json:decode-json-from-string arg-string))
                                                          (fn-name (cdr (assoc :NAME (cdr (assoc :FUNCTION tool-call)))))
                                                          (fn-tool (gethash fn-name *tools*)))
                                                     `((:role . "tool")
                                                       (:tool_call_id . ,(cdr (assoc :id tool-call)))
                                                       (:content . ,(apply (slot-value fn-tool 'fn)
                                                                           (loop for arg in args collect (cdr arg)))))))))
                  (completions-loop provider endpoint headers (append messages
                                                                      (loop for tool-call in tool-calls
                                                                            collect `(("role" . "assistant")
                                                                                      ("content" . nil)
                                                                                      ("tool_calls" . ,(list tool-call))))
                                                                      tool-answers)
                                    payload-format-string streaming-callback))
              (with-output-to-string (s)
                (loop for obj in objs
                      do (let ((content (cdr (assoc :content (cdr (assoc :delta (car (cdr (assoc :choices obj)))))))))
                           (when content
                             (princ content s))))))))

      ;; Non-streaming...
      (let ((objs (json:decode-json-from-string
                   (convert-byte-array-to-utf8
                    (let ((ba (dex:post endpoint
                                        :read-timeout *read-timeout*
                                        :content payload
                                        :headers headers
                                        :force-binary t
                                        :want-stream nil)))
                      (when *debug-stream* (print ba))
                      ba)))))
        (when *debug-stream*
          (format *debug-stream* "~&api call result: ~A~%" objs))
        (let ((tool-calls (cdr (assoc :tool--calls (cdr (assoc :message (car (cdr (assoc :choices objs)))))))))
          (if tool-calls
              (let ((tool-answers (loop for tool-call in tool-calls
                                        collect (let* ((args (json:decode-json-from-string (cdr (assoc :ARGUMENTS (cdr (assoc :FUNCTION tool-call))))))
                                                       (fn-name (cdr (assoc :NAME (cdr (assoc :FUNCTION tool-call)))))
                                                       (fn-tool (gethash fn-name *tools*)))
                                                  `((:role . "tool")
                                                    (:tool_call_id . ,(cdr (assoc :id tool-call)))
                                                    (:content . ,(apply (slot-value fn-tool 'fn)
                                                                        (loop for arg in args collect (cdr arg)))))))))
                (completions-loop provider endpoint headers (append messages
                                                                    (loop for tool-call in tool-calls
                                                                          collect `(("role" . "assistant")
                                                                                    ("content" . nil)
                                                                                    ("tool_calls" . ,(list tool-call))))
                                                                    tool-answers)
                                  payload-format-string nil))
            (cdr (assoc :content (cdr (assoc :message (cadr (assoc :choices objs))))))))))))

(defmethod get-completion ((provider openai-completer) messages &key (max-tokens 1024) (streaming-callback nil) (response-format nil))
  (when (stringp messages)
    (setf messages `(((:role . "user") (:content . ,messages)))))
  (with-slots (endpoint api-key model completion-token-count prompt-token-count tools) provider
    (let* ((tools-rendered
             (json:encode-json-to-string (loop for tool-symbol in tools
                                               collect (let ((tool (gethash (symbol-name tool-symbol) *tools*)))
                                                         (if tool
                                                             (render tool)
                                                             (error "Undefined tool function: ~A" tool-symbol))))))
           (payload-format-string
             (format nil "{ \"model\": ~S, \"stream\": ~A, ~A ~A \"messages\": ~~A, \"max_completion_tokens\": ~A }"
                     model
                     (if streaming-callback "true" "false")
                     (if tools (format nil "\"tools\": ~A," tools-rendered) "")
                     (if response-format (format nil "\"response_format\": { \"type\": ~S }," response-format) "")
                     max-tokens))
           (headers `(("Content-Type" . "application/json")
                      ("Authorization" . ,(concatenate 'string "Bearer " api-key)))))
      (let ((response (completions-loop provider endpoint headers messages payload-format-string streaming-callback)))
        (values response
                (append messages (list `((:ROLE . "assistant") (:CONTENT . ,response)))))))))


(defmethod get-completion ((provider ollama-completer) messages &key (max-tokens 1024) (streaming-callback nil) (response-format nil))
  ;; Fixme: max-tokens is ignored
  (when (stringp messages)
    (setf messages `(((:role . "user") (:content . ,messages)))))

  (with-slots (endpoint model) provider
    (let ((content
            (format nil "{ \"model\": ~S, \"stream\": ~A, ~A \"messages\": ~A }"
                    model
                    (if streaming-callback "true" "false")
                    (if response-format (format nil "\"format\": ~S," response-format) "")
                    (json:encode-json-to-string (make-array (length messages) :initial-contents messages))))
          (headers `(("Content-Type" . "application/json"))))

      (if streaming-callback

          (let ((response-stream (dex:post endpoint
                                           :read-timeout *read-timeout*
                                           :content content
                                           :headers headers
                                           :want-stream t)))
            (let ((response
                    (unwind-protect
                         (with-output-to-string (sstream)
                           (loop
                             for json-object = (ignore-errors (json:decode-json-from-string (read-line response-stream)))
                             until (or (null json-object) (cdr (assoc :done json-object)))
                             do (progn
                                  (format sstream "~A" (cdr (assoc :CONTENT (cdr (assoc :MESSAGE json-object)))))
                                  (funcall streaming-callback (cdr (assoc :CONTENT (cdr (assoc :MESSAGE json-object))))))))
                      (close response-stream))))
                  (values response
                          (append messages (list `((:ROLE . "assistant") (:CONTENT . ,response)))))))

          (let* ((response (json:decode-json-from-string
                            (dex:post endpoint
                                      :read-timeout *read-timeout*
                                      :content content
                                      :headers headers))))
            (values (cdr (assoc :content (cdr (assoc :message response))))
                (append messages (list (cdr (assoc :message response))))))))))

(defmethod get-completion ((provider anthropic-completer) messages &key (max-tokens 1024) (streaming-callback nil))
  (when (stringp messages)
    (setf messages `(((:role . "user") (:content . ,messages)))))

  (when streaming-callback
    (error "streaming-callback not currently supported with anthropic completer"))

  (with-slots (endpoint api-key model) provider
    (let ((content
            (format nil "{
\"model\": ~S,
\"max_tokens\": ~A,
\"messages\": ~A
}"
                    model
                    max-tokens
                    (json:encode-json-to-string (make-array (length messages) :initial-contents messages))))
          (headers `(("x-api-key" . ,api-key)
                     ("anthropic-version" . "2023-06-01"))))
      (let* ((response (json:decode-json-from-string
                        (dex:post endpoint
                                  :read-timeout *read-timeout*
                                  :content content
                                  :headers headers
                                  :content-type "application/json"))))
        (values (cdr (assoc :text (cadr (assoc :content response))))
                (append messages `(((:role . "assistant") ,(assoc :content response)))))))))
