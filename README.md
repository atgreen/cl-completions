# cl-completions
> A Common Lisp LLM completions library

Usage
------

`cl-completions` is available via [ocicl](https://github.com/ocicl/ocicl).  Install it like so:
```
$ ocicl install completions
```

`cl-completions` supports [ollama](https://ollama.com/), [OpenAI](https://openai.com/blog/openai-api), and [Anthropic](https://anthropic.com/api) APIs.

To use the ollama API:

```
(let ((completer (make-instance 'ollama-completer :model "mistral:latest")))
  (get-completion completer "It's a beautiful day for " 100))
```

To use the OpenAI API:

```
(let ((completer (make-instance 'openai-completer :api-key (uiop:getenv "OPENAI_API_KEY"))))
  (get-completion completer "It's a beautiful day for " 100))
```

To use the Anthropic API:

```
(let ((completer (make-instance 'anthropic-completer :api-key (uiop:getenv "ANTHROPIC_API_KEY"))))
  (get-completion completer "It's a beautiful day for " 100))
```

## Tool Functions

The library provides a powerful `defun-tool` macro for creating LLM-callable functions with built-in permission management, safety classification, parameter validation, and event hooks.

### Basic Tool Definition

```lisp
(defun-tool time-of-day ()
  "Useful if you want to know what time it is."
  (let ((decoded-time (multiple-value-list (get-decoded-time))))
    (format nil "Current time: ~2,'0D:~2,'0D:~2,'0D~%"
            (third decoded-time)
            (second decoded-time)
            (first decoded-time))))

(defun-tool get-temperature ((location string "Where to get the temperature for."))
  "Get the temperature for a specific location"
  (:safety-level :safe)
  (:category :information)
  (cond
    ((equal location "Toronto") "cold")
    (t "warm")))
```

### Advanced Tool Features

Tools can include sophisticated permission management and validation:

```lisp
;; Set global permission callback
(setf *permission-callback* (lambda (tool-name args description)
                              (y-or-n-p "Execute ~A: ~A?" tool-name description)))

(defun-tool create-file ((file-path string "Path to the file")
                         (content string "File content"))
  "Create a file with specified content"
  (:safety-level :requires-approval)
  (:category :file-system)
  (:approval-description (lambda (args)
                          (format nil "Create file ~A with ~A bytes"
                                  (cdr (assoc 'file-path args))
                                  (length (cdr (assoc 'content args))))))
  (:parameter-validators (file-path (lambda (f) (not (str:contains? f ".."))))
                         (content (lambda (c) (< (length c) 10000))))
  (:on-start (lambda (args) (log:info "Starting file creation")))
  (:on-complete (lambda (result) (log:info "File created: ~A" result)))
  (with-open-file (stream file-path :direction :output :if-exists :supersede)
    (write-string content stream))
  (format nil "Created file ~A" file-path))
```

### Tool Safety and Classification

Tools are classified by safety level:
- `:safe` - Safe to execute without approval  
- `:requires-approval` - Requires user permission
- `:dangerous` - Should be used with extreme caution

Categories help organize tools by functionality (`:file-system`, `:network`, `:utility`, etc.).

### Context Binding

Tools can capture dynamic variables during execution:

```lisp
(defvar *current-user* nil)

(defun-tool user-operation ((data string "Data to process"))
  "An operation that needs user context"
  (:context-vars *current-user*)
  (format nil "Processing ~A for user ~A" data *current-user*))
```

### Event Hooks

Global and per-tool hooks provide monitoring and logging:

```lisp
;; Global hooks for all tools
(push (lambda (tool-name args)
        (log:info "Tool ~A starting with ~A" tool-name args))
      *tool-start-hooks*)

;; Per-tool hooks
(defun-tool monitored-operation ((input string "Input data"))
  "Operation with custom monitoring"
  (:on-error (lambda (error) (alert:send "Tool failed: ~A" error)))
  (process-input input))
```

### Tool Discovery

The library provides functions for discovering and inspecting available tools:

```lisp
(list-available-tools)                    ; Get all tool names
(get-tools-by-category :file-system)      ; Get tools by category
(get-tools-by-safety-level :safe)         ; Get safe tools
(get-tool-info "CREATE-FILE")             ; Get detailed tool info
```

### Using Tools with Completers

```lisp
(let ((c (make-instance 'openai-completer
                        :api-key (uiop:getenv "OPENAI_API_KEY")
                        :tools '(time-of-day get-temperature create-file))))
  (get-completion c "I'm in Toronto. What's the time and temperature here?" 20))
```

This generates output like:
```
The current time in Toronto is 20:01:22 and it's cold there right now
```

The default read timeout for a response from the completer is 120
seconds.  You can modify this by setting `completions:*read-timeout*`
to a new value.


Related Projects
-----------------

Related projects include:
* [cl-text-splitter](https://github.com/atgreen/cl-text-splitter): a text splitting library
* [cl-embeddings](https://github.com/atgreen/cl-embeddings): an LLM embeddings library
* [cl-chroma](https://github.com/atgreen/cl-chroma): for a Lisp interface to the [Chroma](https://www.trychroma.com/) vector database.
* [cl-chat](https://github.com/atgreen/cl-chat): a wrapper around `completions` to maintain chat history,

Author and License
-------------------

``cl-completions`` was written by [Anthony
Green](https://github.com/atgreen) and is distributed under the terms
of the MIT license.
