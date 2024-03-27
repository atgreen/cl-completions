# cl-completions
> A Common Lisp LLM completions library

Usage
------

`cl-completions` is available via [ocicl](https://github.com/ocicl/ocicl).  Install it like so:
```
$ ocicl install completions
```

OpenAI is the only supported provider today.  Use it like so:

```
(let ((completion (make-instance 'openai-completions :api-key OPENAI-API-KEY)))
  (get-completion completion "It's a beautiful day for " 100))
```


Author and License
-------------------

``cl-completions`` was written by [Anthony
Green](https://github.com/atgreen) and is distributed under the terms
of the MIT license.
