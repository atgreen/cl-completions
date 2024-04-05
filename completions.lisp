;;; completions.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2024  Anthony Green <green@moxielogic.com>
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

(defvar *tools* (make-hash-table :test 'equalp))

(defclass completer ()
  ())

(defclass openai-completer (completer)
  ((endpoint :initform "https://api.openai.com/v1/chat/completions")
   (api-key :initarg :api-key)
   (prompt-token-count :initform 0)
   (completion-token-count :initform 0)
   (model :initarg :model :initform "gpt-4")
   (tools :initarg :tools :initform (list))))

(defclass ollama-completer (completer)
  ((endpoint :initform "http://localhost:11434/api/chat")
   (model :initarg :model)))

(defclass tool ()
  ())

(defclass function-tool (tool)
  ((description :initarg :description)
   (name :initarg :name)
   (parameters :initarg :parameters)
   (fn :initarg :fn)))

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

(defmacro defun-tool (name args description &rest body)
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
                            :fn (lambda ,arg-names
                                  ,@body))))))

(defun read-streamed-json-objects (stream streaming-callback)
  (loop for line = (read-line stream nil 'eof)
        until (or (eq line 'eof) (string= "data: [DONE]" line))
        when (and (> (length line) 6) (str:starts-with? "data: {" line))
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

(defun completions-loop (provider endpoint headers messages payload-format-string streaming-callback)
  (let ((payload (format nil payload-format-string (json:encode-json-to-string messages)))
        (objs))
    (if streaming-callback
        (let* ((response-stream (drakma:http-request endpoint
                                                     :method :post
                                                    :content payload
                                                    :additional-headers headers
                                                    :content-type "application/json"
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
                   (flexi-streams:octets-to-string
                    (drakma:http-request endpoint
                                        :method :post
                                        :content payload
                                        :additional-headers headers
                                        :content-type "application/json"
                                        :want-stream nil)))))
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

(defmethod get-completion ((provider openai-completer) messages &key (max-tokens 1024) (streaming-callback nil))
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
             (format nil "{ \"model\": ~S, \"stream\": ~A, ~A \"messages\": ~~A, \"max_tokens\": ~A }"
                     model
                     (if streaming-callback "true" "false")
                     (if tools (format nil "\"tools\": ~A," tools-rendered) "")
                     max-tokens))
           (headers `(("Content-Type" . "application/json")
                      ("Authorization" . ,(concatenate 'string "Bearer " api-key)))))
      (let ((response (completions-loop provider endpoint headers messages payload-format-string streaming-callback)))
        (values response
                (append messages (list `((:ROLE . "assistant") (:CONTENT . ,response)))))))))


(defmethod get-completion ((provider ollama-completer) messages &key (max-tokens 1024) (streaming-callback nil))
  ;; Fixme: max-tokens is ignored
  (when (stringp messages)
    (setf messages `(((:role . "user") (:content . ,messages)))))

  (with-slots (endpoint model) provider
    (let ((content
            (format nil "{ \"model\": ~S, \"stream\": ~A, \"messages\": ~A }"
                    model
                    (if streaming-callback "true" "false")
                    (json:encode-json-to-string (make-array (length messages) :initial-contents messages))))
          (headers `(("Content-Type" . "application/json"))))

      (if streaming-callback

          (let ((response-stream (drakma:http-request endpoint
                                                      :method :post
                                                      :content content
                                                      :additional-headers headers
                                                      :content-type "application/json"
                                                      :want-stream t)))
            (unwind-protect
                 (with-output-to-string (sstream)
                   (loop
                     for json-object = (ignore-errors (json:decode-json-from-string (read-line response-stream)))
                     until (or (null json-object) (cdr (assoc :done json-object)))
                     do (progn
                          (format sstream "~A" (cdr (assoc :CONTENT (cdr (assoc :MESSAGE json-object)))))
                          (funcall streaming-callback (cdr (assoc :CONTENT (cdr (assoc :MESSAGE json-object))))))))
              (close response-stream)))

          (let* ((response (json:decode-json-from-string
                            (flexi-streams:octets-to-string
                             (drakma:http-request endpoint
                                                  :method :post
                                                  :content content
                                                  :additional-headers headers
                                                  :content-type "application/json")))))
            (values (cdr (assoc :content (cdr (assoc :message response))))
                (append messages (list (cdr (assoc :message response))))))))))
