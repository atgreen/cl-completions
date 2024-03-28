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
    (let ((type (second arg)))
      (unless (member (second arg) '(string number boolean) :test #'equal)
        (error "Unsupported defun-tool argument type: ~a" (second arg)))))
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

(defun completions-loop (provider endpoint headers payload)
  (let ((response (drakma:http-request endpoint
                                       :method :post
                                       :content (cl-json:encode-json-to-string payload)
                                       :additional-headers headers
                                       :content-type "application/json")))
    (let ((response (json:decode-json-from-string
                     (flexi-streams:octets-to-string response :external-format :utf-8))))
      (let ((first-choice (cdr (assoc :MESSAGE (cadr (assoc :CHOICES response))))))
        (if (assoc :TOOL--CALLS first-choice)
            (progn
              (setf (cdr (assoc :MESSAGES payload))
                    (append (cdr (assoc :MESSAGES payload)) (list first-choice)))
              (setf (cdr (assoc :MESSAGES payload))
                    (append (cdr (assoc :MESSAGES payload))
                            (loop for tool-call in (cdr (assoc :TOOL--CALLS first-choice))
                                  collect (let* ((fn-name (cdr (assoc :NAME (cdr (assoc :FUNCTION tool-call)))))
                                                 (fn-tool (gethash fn-name *tools*)))
                                            `((:role . "tool")
                                              (:tool_call_id . ,(cdr (assoc :ID tool-call)))
                                              (:content . ,(apply (slot-value fn-tool 'fn)
                                                                    (let ((args (json:decode-json-from-string (cdr (assoc :ARGUMENTS (cdr (assoc :FUNCTION tool-call)))))))
                                                                      (loop for arg in args
                                                                            collect (cdr arg))))))))))
              (completions-loop provider endpoint headers payload))
            (cdr (assoc :CONTENT first-choice)))))))

(defmethod get-completion ((provider openai-completions) starter-text max-tokens)
  (with-slots (endpoint api-key model completion-token-count prompt-token-count tools) provider
    (let* ((tools-rendered (loop for tool-symbol in tools
                                 collect (let ((tool (gethash (symbol-name tool-symbol) *tools*)))
                                           (if tool
                                               (render tool)
                                               (error "Undefined tool function: " tool-symbol)))))
           (payload `((:messages . (((:role . "user") (:content . ,starter-text))))
                      (:model . ,model)
                      (:tools . ,tools-rendered)
                      (:max_tokens . ,max-tokens)))
           (headers `(("Content-Type" . "application/json")
                      ("Authorization" . ,(concatenate 'string "Bearer " api-key)))))
      (completions-loop provider endpoint headers payload))))
