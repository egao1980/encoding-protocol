# encoding-protocol

RFC 4648 Base16 / Base32 / Base32hex / Base64 / Base64url for [cl-stack](https://github.com/egao1980/cl-stack).

This is a **transfer encoding**, not HTTP `Content-Encoding` (gzip / br / zstd / snappy) and not MIME CTE line-wrapping. Implements [`serdes-protocol`](https://github.com/egao1980/serdes-protocol) `:base64` `:base64url` `:base32` `:base32hex` `:base16` (`:hex` alias).

```lisp
(asdf:load-system "encoding-protocol")   ; nick stack-encoding

(stack-encoding:encode #(102 111 111) :encoding :base64)          ; "Zm9v"
(stack-encoding:decode "Zm9v" :encoding :base64)                  ; #(102 111 111)
(stack-encoding:encode "f" :encoding :base32)                     ; "MY======"
(stack-encoding:encode #(#xff #xef) :encoding :base64url :pad nil) ; "_-8"

(serdes-protocol:encode #(1 2 3) :format :base64)
(serdes-protocol:decode "AQID" :format :base64)
```

`:pad t` (default) is RFC-canonical. Decode ignores `SP` / `HT` / `CR` / `LF` unless `:strict t`. Invalid alphabet characters signal `encoding-decode-error` with `continue` (skip) and `use-value` (alphabet index).

## License

MIT
