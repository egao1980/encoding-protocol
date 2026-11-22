# encoding-protocol

Text encodings (Babel, default **UTF-8**) and transfer encodings for [cl-stack](https://github.com/egao1980/cl-stack): RFC 4648 (Base16/32/64), RFC 2045 quoted-printable, and classic byte RLE.

One `encode` / `decode` GF pair. Family decides polarity:

- **`:text`** (`:utf-8` default, `:ascii`, `:iso-8859-1`, plus any Babel charset) — string → bytes
- **RFC 4648 / QP** — bytes → string (pass `:encoding` explicitly)
- **`:rle`** — octets → octets

Not HTTP `Content-Encoding` (gzip / br / zstd / snappy). MIME CTE line-wrapping is `:columns 76` on encode. Parquet hybrid RLE/bit-packing stays in `arrow-protocol`.

Load `encoding-protocol/serdes` to register [`serdes-protocol`](https://github.com/egao1980/serdes-protocol) `:base64` `:base64url` `:base32` `:base32hex` `:base16` (`:hex` alias) `:quoted-printable` `:rle`. The core system depends only on Babel (implementation, not a consumer API).

```lisp
(asdf:load-system "encoding-protocol")   ; nick stack-encoding

(stack-encoding:encode "hello")                           ; UTF-8 octets
(stack-encoding:decode #(104 101 108 108 111))            ; "hello"

(stack-encoding:encode #(102 111 111) :encoding :base64)  ; "Zm9v"
(stack-encoding:decode "Zm9v" :encoding :base64)          ; #(102 111 111)
(stack-encoding:encode "f" :encoding :base32)             ; "MY======"
(stack-encoding:encode #(#xff #xef) :encoding :base64url :pad nil) ; "_-8"

(serdes-protocol:encode #(1 2 3) :format :base64)
(serdes-protocol:decode "AQID" :format :base64)

(encode #(1 1 1 2) :encoding :rle)            ; #(3 1 1 2)
(rle-runs #(1 1 1 2 2))                       ; ((3 . 1) (2 . 2))
(encode #(61) :encoding :quoted-printable)    ; "=3D"
```

`:pad t` (default) is RFC-canonical. Decode ignores `SP` / `HT` / `CR` / `LF` unless `:strict t`. Invalid alphabet characters signal `encoding-decode-error` with `continue` (skip) and `use-value` (alphabet index).

## Percent-encoding and strict UTF-8

RFC 3986 §2.1 over UTF-8 octets. The unreserved set is ASCII `A-Z a-z 0-9 - . _ ~`; every other octet becomes upper-case `%XX`.

```lisp
(percent-encode "ж/a b" :safe "/")   ; "%D0%B6/a%20b" (SigV4 path style)
(percent-decode "%C3%A9")             ; "é"
(percent-decode "%C0%AF")             ; encoding-decode-error: overlong "/"
(percent-decode-octets "%FF")         ; #(255), no text decode
(decode-utf-8-strict octets)          ; RFC 3629: no overlong forms, surrogates or > U+10FFFF
(decode octets :strict t)             ; same, through the GF
```

A malformed escape, a non-ASCII input character or invalid UTF-8 signals `encoding-decode-error`.

## License

MIT
