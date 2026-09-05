# encoding-protocol

Transfer encodings for [cl-stack](https://github.com/egao1980/cl-stack): RFC 4648 (Base16/32/64), RFC 2045 quoted-printable, and classic byte RLE.

Not HTTP `Content-Encoding` (gzip / br / zstd / snappy). MIME CTE line-wrapping is `:columns 76` on encode. Parquet hybrid RLE/bit-packing stays in `arrow-protocol`.

Implements [`serdes-protocol`](https://github.com/egao1980/serdes-protocol) `:base64` `:base64url` `:base32` `:base32hex` `:base16` (`:hex` alias) `:quoted-printable` `:rle`.

```lisp
(asdf:load-system "encoding-protocol")   ; nick stack-encoding

(stack-encoding:encode #(102 111 111) :encoding :base64)          ; "Zm9v"
(stack-encoding:decode "Zm9v" :encoding :base64)                  ; #(102 111 111)
(stack-encoding:encode "f" :encoding :base32)                     ; "MY======"
(stack-encoding:encode #(#xff #xef) :encoding :base64url :pad nil) ; "_-8"

(serdes-protocol:encode #(1 2 3) :format :base64)
(serdes-protocol:decode "AQID" :format :base64)

(encode #(1 1 1 2) :encoding :rle)            ; #(3 1 1 2)
(rle-runs #(1 1 1 2 2))                       ; ((3 . 1) (2 . 2))
(encode #(61) :encoding :quoted-printable)    ; "=3D"
```

`:pad t` (default) is RFC-canonical. Decode ignores `SP` / `HT` / `CR` / `LF` unless `:strict t`. Invalid alphabet characters signal `encoding-decode-error` with `continue` (skip) and `use-value` (alphabet index).

## License

MIT
