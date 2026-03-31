# cl-completions 1.1.0

**Release date:** 2026-03-31

## Security Fixes

### CLSEC-2026-0001 — Gemini API key in URL query parameter (HIGH)

The Gemini provider appended the API key as a `?key=` query parameter
in the URL, leaking it in HTTP logs, proxy access logs, debug output,
and error messages.

**Fix:** API key moved to the `x-goog-api-key` HTTP header, consistent
with how OpenAI and Anthropic providers already handle credentials.

### CLSEC-2026-0002 — URI with credentials leaked in error messages (MEDIUM)

The `safe-http-request` error handler included the full request URI
(with query parameters) in error messages.

**Fix:** Query parameters are now stripped from URIs in error messages.

## Acknowledgments

Security issues identified by the CLSEC (Common Lisp Security
Initiative) automated audit.
