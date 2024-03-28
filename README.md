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

You can also define callback functions, like so:

```
(defun-tool time-of-day ()
  "Useful if you want to know what time it is."
  (let ((decoded-time (multiple-value-list (get-decoded-time))))
    (format nil "Current time: ~2,'0D:~2,'0D:~2,'0D~%"
            (third decoded-time)
            (second decoded-time)
            (first decoded-time))))

(defun-tool get-temperature ((location string "Where to get the temperature for."))
  "Get the temperature for a specific location"
  (cond
    ((equal location "Toronto")
     "cold")
    (t "warm")))

(let ((c (make-instance 'openai-completions
                        :api-key OPENAI-API-KEY
                        :tools '(time-of-day get-temperature))))
  (get-completion c "I'm in Toronto. What's the time and temperature here?" 20))
```

This code generates output like:
```
The current time in Toronto is 20:01:22 and it's cold there right now
```


Author and License
-------------------

``cl-completions`` was written by [Anthony
Green](https://github.com/atgreen) and is distributed under the terms
of the MIT license.
