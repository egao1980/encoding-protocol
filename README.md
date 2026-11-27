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

## Charsets

`:encoding` takes a keyword or a MIME / IANA charset name. Matching ignores case and punctuation (`"Shift_JIS"`, `"shift-jis"` and `:sjis` are one name), and `normalize-encoding` returns the Babel encoding:

| Babel encoding | Names (with Python's aliases) |
|----------------|-------------------------------|
| `:utf-8` | `utf-8` `utf8` `u8` `cp65001` (also `:utf-8b`, `:utf-16[le/be]`, `:utf-32[le/be]`, `:ucs-2[le/be]`) |
| `:ascii` | `us-ascii` `ascii` `ansi_x3.4-1968` `iso646-us` `us` `cp367` |
| `:iso-8859-1` … `:iso-8859-16` | `iso-8859-N` `iso_8859-N` `latin1`…`latin10` `l1`… `cyrillic` `arabic` `greek` `hebrew` `thai` (no `-12`) |
| `:cp1251` / `:cp1252` | `windows-1251` / `windows-1252` |
| `:koi8-r` / `:koi8-u` / `:koi8-ru` | `koi8-r` `cskoi8r` / `koi8-u` / `koi8-ru` |
| `:cp932` | `shift_jis` `sjis` `x-sjis` `ms_kanji` `csshiftjis` `windows-31j` `cp932` `ms932` |
| `:eucjp` | `euc-jp` `x-euc-jp` `ujis` `cseucpkdfmtjapanese` |
| `:gbk` | `gb2312` `csgb2312` `euc-cn` `x-euc-cn` `chinese` `gbk` `cp936` `ms936` `windows-936` |
| `:ebcdic-us` / `:ebcdic-international` | `ibm-037` |

`shift_jis` decodes and encodes as Windows-31J (CP932), the variant mail clients send. `gb2312` uses GBK, a superset: GB2312 text gives the same octets. Babel's EUC-JP has no JIS X 0212, so `é` does not encode (Python gives three octets).

Not supported: `big5`, `iso-2022-jp`, `euc-kr`, `gb18030`, `hz`, `utf-7`, `windows-125x` other than 1251/1252, and anything else Babel lacks.

Conditions (all `encoding-error`):

- `encoding-unknown-encoding` — no such charset; `encoding-unknown-encoding-encoding` is the normalized name. A `use-value` restart takes another encoding:

  ```lisp
  (handler-bind ((encoding-unknown-encoding
                   (lambda (c) (use-value :iso-8859-1 c))))
    (decode octets :encoding "big5"))
  ```

- `encoding-transcode-error` — a character the charset cannot encode (`cannot encode '€' (U+20AC) at index 2`), or malformed input octets.

## License

MIT
