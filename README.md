# cl-completions
> A Common Lisp LLM completions library

Usage
------

`cl-completions` is available via [ocicl](https://github.com/ocicl/ocicl).  Install it like so:
```
$ ocicl install completions
```

`cl-completions` supports [ollama](https://ollama.com/), [OpenAI](https://openai.com/blog/openai-api), [Anthropic](https://anthropic.com/api), and [Google Gemini](https://ai.google.dev/) APIs.

To use the ollama API:

```
(let ((completer (make-instance 'ollama-completer :model "mistral:latest")))
  (get-completion completer "It's a beautiful day for " :max-tokens 100))
```

To use the OpenAI API:

```
(let ((completer (make-instance 'openai-completer :api-key (uiop:getenv "OPENAI_API_KEY"))))
  (get-completion completer "It's a beautiful day for " :max-tokens 100))
```

To use the Anthropic API:

```
(let ((completer (make-instance 'anthropic-completer :api-key (uiop:getenv "ANTHROPIC_API_KEY"))))
  (get-completion completer "It's a beautiful day for " :max-tokens 100))
```

To use the Google Gemini API:

```
(let ((completer (make-instance 'gemini-completer :api-key (uiop:getenv "GEMINI_API_KEY") :model "gemini-2.0-flash")))
  (get-completion completer "It's a beautiful day for " :max-tokens 100))
```

### Multi-turn Conversations

`get-completion` returns two values: the completion text and the updated conversation history (including the assistant's response). This makes it easy to maintain context across multiple turns:

```lisp
(let ((completer (make-instance 'openai-completer
                                :api-key (uiop:getenv "OPENAI_API_KEY")))
      (messages '((("role" . "user")
                   ("content" . "What's the capital of France?")))))
  ;; First turn
  (multiple-value-bind (response updated-messages)
      (get-completion completer messages :max-tokens 100)
    (format t "Assistant: ~A~%~%" response)

    ;; Second turn - use updated-messages to maintain context
    (setf messages (append updated-messages
                          '((("role" . "user")
                             ("content" . "What's its population?")))))
    (multiple-value-bind (response2 updated-messages2)
        (get-completion completer messages :max-tokens 100)
      (format t "Assistant: ~A~%" response2))))
```

When tools are invoked, they are automatically included in the returned message history.

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

Tools work with OpenAI, Anthropic, Gemini, and Ollama completers:

```lisp
;; OpenAI
(let ((c (make-instance 'openai-completer
                        :api-key (uiop:getenv "OPENAI_API_KEY")
                        :tools '(time-of-day get-temperature create-file))))
  (get-completion c "I'm in Toronto. What's the time and temperature here?" 20))

;; Anthropic
(let ((c (make-instance 'anthropic-completer
                        :api-key (uiop:getenv "ANTHROPIC_API_KEY")
                        :tools '(time-of-day get-temperature))))
  (get-completion c "I'm in Toronto. What's the time and temperature here?" :max-tokens 200))

;; Gemini
(let ((c (make-instance 'gemini-completer
                        :api-key (uiop:getenv "GEMINI_API_KEY")
                        :tools '(time-of-day get-temperature))))
  (get-completion c "I'm in Toronto. What's the time and temperature here?" :max-tokens 200))

;; Ollama (tools require non-streaming mode)
(let ((c (make-instance 'ollama-completer
                        :model "llama3.1:latest"
                        :tools '(time-of-day get-temperature))))
  (get-completion c "I'm in Toronto. What's the time and temperature here?" :max-tokens 200))
```

This generates output like:
```
The current time in Toronto is 20:01:22 and it's cold there right now
```

### System Messages

System messages (role `"system"`) are automatically handled for all providers:

- **OpenAI / Ollama**: Passed directly in the messages array (native support).
- **Anthropic**: Extracted and sent as the top-level `system` field in the API request.
- **Gemini**: Extracted and sent as the `systemInstruction` field in the API request.

```lisp
(let ((c (make-instance 'anthropic-completer
                        :api-key (uiop:getenv "ANTHROPIC_API_KEY")))
      (messages '(((:role . "system") (:content . "You are a helpful pirate. Speak like a pirate."))
                  ((:role . "user") (:content . "What is Common Lisp?")))))
  (get-completion c messages :max-tokens 200))
```

### Streaming

All four providers support streaming via the `:streaming-callback` keyword. The callback receives text chunks as they arrive:

```lisp
(get-completion completer "Tell me a story"
                :max-tokens 200
                :streaming-callback (lambda (text) (princ text)))
```

**Note:** OpenAI, Anthropic, and Gemini support streaming with tools (tool calls are handled automatically within the streaming loop). Ollama does not currently support combining streaming with tools.

### Structured Output (response-format)

OpenAI, Anthropic, and Gemini support structured output via the `:response-format` keyword:

```lisp
;; OpenAI / Anthropic
(get-completion completer messages :response-format "json_object")

;; Gemini (json_object maps to application/json MIME type)
(get-completion completer messages :response-format "json_object")
```

### Token Tracking

All completers track token usage. After a `get-completion` call, you can inspect how many tokens were used:

```lisp
(let ((c (make-instance 'openai-completer :api-key (uiop:getenv "OPENAI_API_KEY"))))
  (get-completion c "Hello!" :max-tokens 100)
  (format t "Prompt tokens: ~A~%" (prompt-token-count c))
  (format t "Completion tokens: ~A~%" (completion-token-count c))
  (format t "Total tokens: ~A~%" (total-tokens-used c))
  (reset-token-counts c))
```

### Tool Interceptor

The `*tool-interceptor*` variable allows you to intercept tool calls before they are executed. When set, it is called with the tool name and arguments. If it returns a non-nil string, that string is used as the tool result and normal execution is skipped:

```lisp
(setf *tool-interceptor*
      (lambda (tool-name args)
        (when (string= tool-name "DANGEROUS-TOOL")
          "Tool execution blocked by interceptor")))
```

This is useful for client-side tool delegation or sandboxing.

### Prompt Caching (Anthropic)

When using the Anthropic completer, `cache_control` markers are automatically added to system messages and the last tool definition. This enables [Anthropic's prompt caching](https://docs.anthropic.com/en/docs/build-with-claude/prompt-caching) to reduce costs and latency for repeated calls with the same system prompt or tool definitions.

### Error Handling

Tool invocation errors are caught and returned as `"Error: ..."` strings to the LLM, allowing it to see the error and recover rather than crashing the completion loop. This applies to unknown tools and tools that signal errors during execution.

The default read timeout for a response from the completer is 120
seconds.  You can modify this by setting `completions:*read-timeout*`
to a new value.

## Testing

The library includes a test suite using [FiveAM](https://github.com/sionescu/fiveam):

```lisp
(asdf:load-system "completions/tests")
(fiveam:run! 'completions/tests::completions-suite)
```

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
