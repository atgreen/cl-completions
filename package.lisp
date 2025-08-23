;;; package.lisp
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

(defpackage #:completions
  (:use #:cl)
  (:export completer openai-completer ollama-completer anthropic-completer get-completion defun-tool *read-timeout* *debug-stream*
           ;; Permission and safety system
           *permission-callback* *default-safety-level* *safe-tools*
           ;; Tool classification
           get-tool-safety-level set-tool-safety-level
           ;; Context binding
           *tool-context-vars*
           ;; Event hooks
           *tool-start-hooks* *tool-complete-hooks* *tool-error-hooks*
           ;; Tool discovery and introspection
           list-available-tools get-tool-info get-tools-by-category get-tools-by-safety-level
           ;; Enhanced parameter validation
           validate-tool-parameters
           ;; Tool categories
           *tool-categories*))
