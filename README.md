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
| `:cp1250` `:cp1253` … `:cp1258` | `windows-125N` `cp125N` `x-cp125N` |
| `:cp866` | `cp866` `ibm866` `csibm866` |
| `:mac-roman` | `macintosh` `mac-roman` `macroman` `csmacintosh` |
| `:tis-620` / `:cp874` | `tis-620` `tis620` `iso-ir-166` / `windows-874` `ms874` |

`normalize-encoding` interns an unknown name as a keyword, so `encode` / `decode` can name it in `encoding-unknown-encoding`. For a name from untrusted input (a MIME `charset` parameter, an HTTP header) use `find-encoding` (0.6.0): the same canonical keyword, or `nil` when no encoding has that name, and it never interns a symbol. It loads an optional charset's system like `normalize-encoding`.

```lisp
(find-encoding "Shift_JIS")        ; => :CP932
(find-encoding "x-no-such-charset") ; => NIL, nothing interned
```

`shift_jis` decodes and encodes as Windows-31J (CP932), the variant mail clients send. `gb2312` uses GBK, a superset: GB2312 text gives the same octets. Babel's EUC-JP has no JIS X 0212, so `é` does not encode (Python gives three octets).

The multibyte charsets below are optional: each lives in its own secondary system, loaded on first use.

| Babel encoding | Names | System | Fasl (SBCL) |
|----------------|-------|--------|-------------|
| `:iso-2022-jp` | `iso-2022-jp` `csiso2022jp` | `encoding-protocol/iso-2022-jp` | 40 KB |
| `:big5` | `big5` `big5-tw` `csbig5` `cn-big5` `x-x-big5` | `encoding-protocol/big5` | 48 KB |
| `:euc-kr` | `euc-kr` `euckr` `cseuckr` | `encoding-protocol/euc-kr` | 95 KB (with cp949) |
| `:cp949` | `ks_c_5601-1987` `ks_c_5601` `uhc` `windows-949` `korean` `ms949` `csksc56011987` | `encoding-protocol/euc-kr` | |
| `:gb18030` | `gb18030` `gb18030-2000` | `encoding-protocol/gb18030` | 73 KB |

`encoding-protocol/cjk` depends on all four, for code that wants every charset up front.

### Loading on demand

`encode`, `decode` and `normalize-encoding` load a charset's system the first time they meet one of its names: the first big5 message loads `encoding-protocol/big5` and nothing else. No dependency on the charset systems is needed.

```lisp
(asdf:load-system "encoding-protocol")
(encode "中文" :encoding "big5")    ; loads encoding-protocol/big5, then encodes
```

- Each system loads once per image, under a global lock: threads that meet big5 at the same time wait for the one load, and then all succeed. The load runs before any Babel call and writes nothing to `*standard-output*`. Babel's tables are updated all at once when the load succeeds, so other threads never see a half-registered charset, and a failed load leaves them as they were.
- `*auto-load-charsets*` (default `t`) controls it. Bound to `nil`, nothing loads: `normalize-encoding` returns the system as a second value (`"big5"` → `:big5`, `"encoding-protocol/big5"`), and `encode` / `decode` signal `encoding-unknown-encoding` with that system in `encoding-unknown-encoding-system`.
- The `load-charset-system` restart loads the system and retries, whatever `*auto-load-charsets*` says. The function of the same name invokes it, so it works as a handler:

  ```lisp
  (let ((*auto-load-charsets* nil))
    (handler-bind ((encoding-unknown-encoding #'load-charset-system))
      (decode octets :encoding "big5")))
  ```

- When the load fails (no ASDF in the image, the system not found, a compile error), the caller gets `encoding-unknown-encoding` with the system and the failure in `encoding-unknown-encoding-load-error`, never a raw ASDF error. `normalize-encoding` also offers `continue`: return the name and the system, unloaded.

### Delivered images

An image saved with `save-lisp-and-die` may have no ASDF, or no sources, at run time. Load the charsets it needs before saving:

```lisp
(load-charsets "big5" "ks_c_5601-1987")   ; → ("encoding-protocol/big5" "encoding-protocol/euc-kr")
(load-charsets :all)                       ; every optional charset
(sb-ext:save-lisp-and-die "app" :executable t)
```

`load-charsets` takes any names `normalize-encoding` takes and returns the systems that define them. A charset of the core system needs nothing; an unknown name signals `encoding-unknown-encoding`. In the saved image those charsets are plain Babel encodings, and a charset that was not loaded signals `encoding-unknown-encoding` with the load error. Depending on `encoding-protocol/cjk` (or one charset system) in the application's `.asd` does the same at build time.

### Charset details

These charsets are Babel encodings registered by this library, so `babel:string-to-octets` / `octets-to-string` accept them too. Each one matches the Python codec of the same name, octet for octet: the tables are generated from Python's codecs by `tools/gen-charsets.py` (`uv run --no-project tools/gen-charsets.py`), which checks its model of each decoder and encoder against Python over every single octet, every two-octet sequence and every code point before it writes anything. Details:

- `iso-2022-jp` is stateful: ASCII, JIS X 0201 Roman (`¥`, `‾`) and JIS X 0208, switched with `ESC ( B`, `ESC ( J` and `ESC $ B`. Encoding switches mode only when it has to and always ends in ASCII. Decoding also takes `ESC $ @`, `ESC $ ( B` and `ESC & @ ESC $ B`, and passes control characters through in every mode, as Python does. Half-width katakana does not encode (Python's `iso2022_jp` refuses it too).
- `euc-kr` is KS X 1001 and encodes the other Hangul syllables as KS X 1001:1998 eight-octet make-up sequences, like Python's `euc_kr`. Mail labelled `ks_c_5601-1987` is really CP949 (Outlook), so those names map to `:cp949`.
- `gb18030` covers all of Unicode: two-octet GBK forms, four-octet BMP ranges and four-octet supplementary planes.
- `cp1258` maps the Windows table only; it does not compose or decompose Vietnamese tone marks.

Not supported: `big5-hkscs`, `hz`, `utf-7`, `iso-2022-jp-2`, `iso-2022-kr`, and anything else Babel lacks.

### Conditions

All are `encoding-error`s:

- `encoding-unknown-encoding` — no such charset, or its system is not loaded; `encoding-unknown-encoding-encoding` is the normalized name, `encoding-unknown-encoding-system` the system that defines it, `encoding-unknown-encoding-load-error` the condition from a failed load. A `use-value` restart takes another encoding, and `load-charset-system` loads the system and retries:

  ```lisp
  (handler-bind ((encoding-unknown-encoding
                   (lambda (c) (use-value :iso-8859-1 c))))
    (decode octets :encoding "utf-7"))
  ```

- `encoding-transcode-error` — a character the charset cannot encode (`cannot encode '€' (U+20AC) at index 2`), or malformed input octets.

## License

MIT
