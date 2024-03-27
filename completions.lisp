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

(defclass completions ()
  ())

(defclass openai-completions (completions)
  ((endpoint :initform "https://api.openai.com/v1/chat/completions")
   (api-key :initarg :api-key)
   (prompt-token-count :initform 0)
   (completion-token-count :initform 0)
   (model :initarg :model :initform "gpt-4")))

(defmethod get-completion ((provider openai-completions) starter-text max-tokens)
  (with-slots (endpoint api-key model completion-token-count prompt-token-count) provider
      (let* ((data `((:messages . (((:role . "user") (:content . ,starter-text))))
                     (:model . ,model)
                     (:max_tokens . ,max-tokens)))
             (payload (cl-json:encode-json-to-string data))
             (headers `(("Content-Type" . "application/json")
                        ("Authorization" . ,(concatenate 'string "Bearer " api-key)))))
        (let ((response (drakma:http-request endpoint
                                             :method :post
                                             :content payload
                                             :additional-headers headers
                                             :content-type "application/json")))
          (let ((response (json:decode-json-from-string
                           (flexi-streams:octets-to-string response :external-format :utf-8))))
            (let ((usage (cdr (assoc :USAGE response))))
              (incf completion-token-count (cdr (assoc :COMPLETION--TOKENS usage)))
              (incf prompt-token-count (cdr (assoc :PROMPT--TOKENS usage))))
            (cdr (assoc :CONTENT
                        (cdr (assoc :MESSAGE
                                    (car (cdr (assoc :CHOICES response))))))))))))
