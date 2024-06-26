---
title: The ultimate guide to Haskell Strings
subtitle: Haskell Strings are really simple, I promise
author: Julian Ospald
tags: haskell, unicode, string
---

This guide is aimed at Haskellers who seek to improve their understanding of String types,
be it beginners or seasoned developers. It is also meant to be a quick reference/cheat sheet
for deciding which string type to use in a given situation.

## TOC

* [Motivation](#motivation)
* [String in Prelude](#string-in-prelude)
* [Unicode](#unicode)
   * [Unicode Code Point](#unicode-code-point)
   * [UTF-32](#utf-32)
   * [UTF-16](#utf-16)
   * [Unicode Scalar Values](#unicode-scalar-values)
   * [UTF-8](#utf-8)
   * [Unicode summary](#unicode-summary)
* [Back to Haskell String type](#back-to-haskell-string-type)
* [String types](#string-types)
   * [Text](#text)
   * [ShortText](#shorttext)
   * [ByteString](#bytestring)
   * [ShortByteString](#shortbytestring)
   * [Bytes](#bytes)
   * [OsString, PosixString and WindowsString](#osstring-posixstring-and-windowsstring)
   * [OsPath, PosixPath and WindowsPath](#ospath-posixpath-and-windowspath)
   * [CString and CStringLen](#cstring-and-cstringlen)
   * [FilePath](#filepath)
* [Lazy vs Strict](#lazy-vs-strict)
* [Slicable vs non-slicable](#slicable-vs-non-slicable)
* [Pinned vs unpinned](#pinned-vs-unpinned)
* [String Types Cheat Sheet](#string-types-cheat-sheet)
* [Construction](#construction)
   * [String literals](#string-literals)
   * [String Classes](#string-classes)
   * [OverloadedStrings](#overloadedstrings)
   * [QuasiQuoters](#quasiquoters)
* [Conversions](#conversions)
   * [From String to...](#from-string-to)
   * [From Text to...](#from-text-to)
   * [From ByteString to...](#from-bytestring-to)
   * [From ShortByteString to...](#from-shortbytestring-to)
   * [From OsString to...](#from-osstring-to)
   * [To JSON](#to-json)
* [A word on lazy IO](#a-word-on-lazy-io)
* [Streaming](#streaming)
* [A note on FilePaths](#a-note-on-filepaths)
* [Reflection](#reflection)
   * [What we should know](#what-we-should-know)
   * [Too many Strings](#too-many-strings)
   * [What are we missing](#what-are-we-missing)
* [Special thanks to](#special-thanks-to)
* [Links and relevant stuff](#links-and-relevant-stuff)

## Motivation

In 2022 I implemented the [Abstract FilePath proposal](https://hasufell.github.io/posts/2022-06-29-fixing-haskell-filepaths.html),
which lead to several new String types, such as `OsString`.

At the time of writing, I'm also serving on the [Core Libraries Committee](https://github.com/haskell/core-libraries-committee),
which oversees the base API. In the context of base, there have been recurring discussions about String types, e.g.:

- [https://discourse.haskell.org/t/informal-discussion-about-the-progression-of-base/6439/153](https://discourse.haskell.org/t/informal-discussion-about-the-progression-of-base/6439/153)

When discussing this topic with other Haskellers, I realized it can indeed be quite confusing and we don't have
comprehensive, over-arching documentation. After all, there is no equivalent of
[The Rust book](https://doc.rust-lang.org/stable/book/).

I hope this blog post can fill some of the documentation gaps and also explain the newly introduced types and
why I think that **we don't have too many String types**.

## String in Prelude

The most widely used String type in Haskell is defined by the Haskell Standard in
[Chapter 9 Standard Prelude](https://www.haskell.org/onlinereport/haskell2010/haskellch9.html#x16-1710009):

```hs
-- Lists

data  [a]  =  [] | a : [a]
        -- Not legal Haskell; for illustration only

-- Character type

data Char = ... 'a' | 'b' ... -- Unicode values

type  String = [Char]
```

Since lists are one of the most idiomatic data types in Haskell, this allows us to easily pattern match on strings,
because they are just a list of characters. E.g. the following function returns the first character of a string and
its remainder or `Nothing` if the list is empty.

```hs
uncons :: [a] -> Maybe (a, [a])
uncons []     = Nothing
uncons (x:xs) = Just (x, xs)
```

### Char

If we look closely at the pseudo code definition of `Char` from the Haskell standard, we realize the comment saying `-- Unicode values`.
This is a bit vague, in fact. If we look at the documentation in
[Data.Char](https://hackage.haskell.org/package/base-4.19.1.0/docs/Data-Char.html#t:Char) from base, we see that it is actually
implemented as a [Unicode Code Point](https://www.unicode.org/glossary/#code_point).

This can be seen by the [smart constructor](https://wiki.haskell.org/Smart_constructors) `chr` as well:

```hs
chr :: Int -> Char
chr i@(I# i#)
 | isTrue# (int2Word# i# `leWord#` 0x10FFFF##) = C# (chr# i#)
 | otherwise
    = errorWithoutStackTrace ("Prelude.chr: bad argument: " ++ showSignedInt (I# 9#) i "")
```

So `Char` is basically just an `Int` with an upper bound on `0x10FFFF`. In order to understand this,
we actually have to take a short dive into Unicode.

## Unicode

The Unicode Standard is a standard to identify and encode visible "characters" that comprise "text",
supporting all of the world's major writing systems.

The exact terminology can be very confusing. We'll focus only on a couple of core concepts. If you want to read up yourself on
the standard, here are some pointers:

- the actual standard: [https://www.unicode.org/versions/latest/](https://www.unicode.org/versions/latest/)
- Wikipedia Unicode article: [https://en.wikipedia.org/wiki/Unicode](https://en.wikipedia.org/wiki/Unicode)
- brief Unicode introduction for JavaScript developers: [https://exploringjs.com/impatient-js/ch_unicode.html](https://exploringjs.com/impatient-js/ch_unicode.html)

The goal of Unicode is to be universal, efficient and unambiguous. In order to achieve that, it needs:

- a character encoding: translating e.g. `a` or `쟬` to something unambiguous
- a text encoding: translating a sequence of characters into an efficient byte format

The term "character" is quite overloaded and we will go through different definitions along the way.

### Unicode Code Point

Unicode Code Points are a way of encoding a single character through numerical values. It ranges from the hexadecimal values 0 to 10FFFF,
which we saw before in the definition of `chr :: Int -> Char`.
The formal notation of code points is `U+0000` to `U+10FFFF`.

It is essentially a static mapping, e.g.:

| character          | code point       |
|--------------------|------------------|
| a                  | U+0061           |
| b                  | U+0062           |
| 쟬                 | U+C7EC           |
| 🇯                 | U+1F1EF          |
| 🇵                 | U+1F1F5          |
| 🇯🇵               | U+1F1EF, U+1F1F5 |

This allows us a couple of observations:

- the hex values `61` for `a` and `62` for `b` correspond to the [ASCII character set](https://en.wikipedia.org/wiki/ASCII) (cool)
- it can express Chinese and other non-Latin characters
- some "characters" (in this case actually emoji) are expressed by multiple code points, such as 🇯🇵

However, this is just a mapping for a single character. In order to efficiently represent a whole text, several
Unicode Transformation Formats were developed, most notably:

- UTF-32
- UTF-16
- UTF-8

Such transformation formats are necessary to understand code point boundaries in a sequence of bytes and make searching
and splitting feasible. UTF-16 and UTF-8 are also optimized for size.

### UTF-32

The most simple encoding for text would be to just use the code point values. The issue with this is that
the maximum code point value is `U+10FFFF`, which only fits into 21 bits.

UTF-32 is a fixed-length encoding that uses 32 bits (four bytes) and as such can hold all possible Unicode
values without any actual transformation.

The upside of this is that it's simple, the downside is that it's wasting space, because most values don't
need the whole 21 bits (e.g. ASCII just needs 7 bits).

UTF-32 is not ASCII compatible, meaning a program that only understands ASCII won't accidentally work with UTF-32 text,
even if all of the characters used are in the ASCII set (e.g. only Latin characters from `[a-zA-Z]`).

### UTF-16

This is a [variable-width character encoding](https://en.wikipedia.org/wiki/Variable-width_encoding), most notably
[used on Windows](https://learn.microsoft.com/en-us/windows/win32/learnwin32/working-with-strings).

Code points from `U+0000` to `U+FFFF` are expressed "directly" via 2 bytes (16 bits),
with the exception of surrogates, which I will explain later.

Code points from `U+10000` to `U+10FFFF` don't fit into 2 bytes. In order to encode these without
being accidentally ambiguous, **surrogates** were introduced (another option would have been magic bits as used by UTF-8,
but I guess the format wasn't designed with extension in mind). These surrogates must always come in pairs
(so 4 bytes) and are in the following ranges:

- low surrogates: `U+DC00` to `U+DFFF`
- high surrogates: `U+D800` to `U+DBFF`

Through bit shuffling, these 2-byte pairs allow to map to values in the `U+10000` to `U+10FFFF` range.
For the interested reader, the algorithm is as follows
([cited from Wikipedia](https://en.wikipedia.org/wiki/UTF-16#Code_points_from_U+010000_to_U+10FFFF)):

> * 0x10000 is subtracted from the code point (U), leaving a 20-bit number (U') in the hex number range 0x00000–0xFFFFF.
> * The high ten bits (in the range 0x000–0x3FF) are added to 0xD800 to give the first 16-bit code unit or high surrogate (W1), which will be in the range 0xD800–0xDBFF.
> * The low ten bits (also in the range 0x000–0x3FF) are added to 0xDC00 to give the second 16-bit code unit or low surrogate (W2), which will be in the range 0xDC00–0xDFFF

UTF-16 is not ASCII compatible either. It is more space efficient than UTF-32 though. For some languages, it can
even be more space efficient than UTF-8.

### Unicode Scalar Values

It is important to understand that the Haskell `Char` type (which is essentially a Code Point) can represent surrogates
that are used in UTF-16.

The Unicode standard also defines the concept of [Unicode Scalar Values](http://www.unicode.org/versions/Unicode5.2.0/ch03.pdf#page=35):

> Any Unicode code point except high-surrogate and low-surrogate code points. In other words, the ranges of integers 0 to D7FF16 and E00016 to 10FFFF16 inclusive.

So, code point without surrogates. This will become relevant for UTF-8.

### UTF-8

This is similar to UTF-16 a variable-width character encoding. It's often used in web APIs (most notably JSON) and is
often the default on Unix systems.

Here, a Unicode Code Point is represented by a sequence of bytes. The number of bytes required depends on the range of the code point and
varies between 1 and 4 bytes. The whole bit conversion between code point and UTF-8 is illustrated in the following table (adopted
from [Wikipedia](https://en.wikipedia.org/wiki/UTF-8#Encoding)):

| First code point | Last code point | Byte 1   | Byte 2   | Byte 3   | Byte 4   |
|------------------|-----------------|----------|----------|----------|----------|
| U+00<span style="color:red">0</span><span style="color:purple">0</span>           | U+00<span style="color:red">7</span><span style="color:purple">F</span>          | 0<span style="color:red">xxx</span><span style="color:purple">xxxx</span> |          |          |          |
| U+0<span style="color:green">0</span><span style="color:red">8</span><span style="color:purple">0</span>           | U+0<span style="color:green">7</span><span style="color:red">F</span><span style="color:purple">F</span>          | 110<span style="color:green">xxx</span><span style="color:red">xx</span> | 10<span style="color:red">xx</span><span style="color:purple">xxxx</span> |          |          |
| U+<span style="color:blue">0</span><span style="color:green">8</span><span style="color:red">0</span><span style="color:purple">0</span>           | U+<span style="color:blue">F</span><span style="color:green">F</span><span style="color:red">F</span><span style="color:purple">F</span>          | 1110<span style="color:blue">xxxx</span> | 10<span style="color:green">xxxx</span><span style="color:red">xx</span> | 10<span style="color:red">xx</span><span style="color:purple">xxxx</span> |          |
| U+<span style="color:crimson">0</span><span style="color:orange">1</span><span style="color:blue">0</span><span style="color:green">0</span><span style="color:red">0</span><span style="color:purple">0</span>         | U+<span style="color:crimson">1</span><span style="color:orange">0</span><span style="color:blue">F</span><span style="color:green">F</span><span style="color:red">F</span><span style="color:purple">F</span>        | 11110<span style="color:crimson">x</span><span style="color:orange">xx</span> | 10<span style="color:orange">xx</span><span style="color:blue">xxxx</span> | 10<span style="color:green">xxxx</span><span style="color:red">xx</span> | 10<span style="color:red">xx</span><span style="color:purple">xxxx</span> |

Here we see a different technique than surrogates. UTF-8 uses magic bits in the first byte to signal how many bytes in total
must be read for translating to a code point.

Notable properties of UTF-8 are:

- it is ASCII backwards compatible: a program written for UTF-8 will also understand plain ASCII encoding
- Unicode code points in the surrogate range `U+D800` to `U+DFFF` are considered invalid byte sequences
  - as a result: UTF-8 only expresses Unicode Scalar Values

### Unicode summary

Given the above encodings, let's have another look at our table from above:

| character          | code point       | Hex UTF-8 | Hex UTF-16 | Hex UTF-32 |
|--------------------|------------------|-----------|------------|------------|
| a                  | U+0061           | 61        | 0061       | 00000061   |
| b                  | U+0062           | 62        | 0062       | 00000062   |
| 쟬                 | U+C7EC           | ec 9f ac  | c7ec       | 0000c7ec   |
| 🇯                 | U+1F1EF          | f0 9f 87 af | d83c ddef | 0001f1ef  |
| 🇵                 | U+1F1F5          | f0 9f 87 b5 | d83c ddf5 | 0001f1f5  |
| 🇯🇵               | U+1F1EF, U+1F1F5 | f0 9f 87 af, f0 9f 87 b5 | d83c ddef, d83c ddf5 | 0001f1ef, 0001f1f5 |

The interested reader is welcome to verify those values (at least for UTF-8 and UTF-16).

We now understand:

- the character encoding is the mapping of code points to visible characters
- UTF-8, UTF-16 and UTF-32 are text encodings with different trade offs
- surrogates are a special case for UTF-16 (`Unicode Scalar Values = Unicode Code Points - surrotages`)

Going back to the definition of "character", we now see the confusion:

- a surrogate can hardly be a visible character
- the visible character  🇯🇵 needs two code points to be expressed (and there are many others)

This has lead to yet another definition: **"Grapheme Cluster"**. This is specified by the [Unicode Standard Annex #29](https://www.unicode.org/reports/tr29/), which deals with determining boundaries between characters, words and sentences.
It is, again, quite technical, but is much closer to "user visible character".

## Back to Haskell String type

Now that we know what a **Unicode Code Point** is, we also understand that the Haskell String type
has essentially no *text encoding*. It is just a linked list of those code points (a subset of `Int`, in fact).
This can be a nice property, e.g. as an intermediate representation when converting between encodings (say UTF-8 to UTF-16).

However, it is a **questionable default for a String type**, because:

- it is inefficient for large text (carries the overhead of a linked list with thunks for every `Char`);
  the [haddock documentation of Data.String](https://hackage.haskell.org/package/base-4.19.1.0/docs/Data-String.html#t:String)
  goes into more detail
- it is often confusing for users who don't have a good mental model of what a *Unicode Code Point* is
- it causes problems for certain conversions (e.g. `String -> Text`), because of surrogates
  (it should have been Unicode Scalar Values instead or maybe even Grapheme Clusters)

Unfortunately, since it's defined by the Haskell Standard and has been around since the beginning of time, we won't be able to get rid of it ever.

This type should only be used for small little projects, prototypes and hello worlds and maybe
intermediate representations in some algorithms.

The `Show` instance of `Char`/`String` will print the Unicode Code Point value as a decimal for non-ASCII ranges:

```
ghci> "a"
"a"
ghci> "쟬"
"\51180"
```

Show is for debugging, so that seems fine. However this behavior has been challenged before: [Proposal: `showLitChar` (and `show @Char`) shouldn't escape readable Unicode characters](https://github.com/haskell/core-libraries-committee/issues/26).

## String types

In this section, we will examine each string like type and what its properties and use cases are.
`String` was already discussed and we don't recommend it, so it's omitted here.

- proper Unicode text
    - [Text](https://hackage.haskell.org/package/text-2.1.1/docs/Data-Text.html#t:Text) (strict and lazy)
    - [ShortText](https://hackage.haskell.org/package/text-short-0.1.5/docs/Data-Text-Short.html#t:ShortText)
- byte sequences
    - [ByteString](https://hackage.haskell.org/package/bytestring-0.12.1.0/docs/Data-ByteString.html#t:ByteString) (strict and lazy)
    - [ShortByteString](https://hackage.haskell.org/package/bytestring-0.12.1.0/docs/Data-ByteString-Short.html#t:ShortByteString)
    - [Bytes](https://hackage.haskell.org/package/byteslice-0.2.13.2/docs/Data-Bytes.html#t:Bytes)
    - [Chunks](https://hackage.haskell.org/package/byteslice-0.2.13.2/docs/Data-Bytes-Chunks.html#t:Chunks)
- byte sequences dealing with platform API differences
    - [OsString](https://hackage.haskell.org/package/os-string-2.0.2.1/docs/System-OsString.html#t:OsString)
    - [PosixString](https://hackage.haskell.org/package/os-string-2.0.2.1/docs/System-OsString-Posix.html#t:PosixString)
    - [WindowsString](https://hackage.haskell.org/package/os-string-2.0.2.1/docs/System-OsString-Windows.html#t:WindowsString)
- FFI types
    - [CString](https://hackage.haskell.org/package/base-4.19.1.0/docs/Foreign-C-String.html#t:CString)
    - [CStringLen](https://hackage.haskell.org/package/base-4.19.1.0/docs/Foreign-C-String.html#t:CStringLen)
- filepath types (just type synonyms)
    - [FilePath](https://hackage.haskell.org/package/filepath-1.5.2.0/docs/System-FilePath.html#t:FilePath)
    - [OsPath](https://hackage.haskell.org/package/filepath-1.5.2.0/docs/System-OsPath.html#t:OsPath)
    - [PosixPath](https://hackage.haskell.org/package/filepath-1.5.2.0/docs/System-OsPath-Posix.html#t:PosixPath)
    - [WindowsPath](https://hackage.haskell.org/package/filepath-1.5.2.0/docs/System-OsPath-Windows.html#t:WindowsPath)

If we delve more into filepaths, there are actually even more, e.g. strongly typed filepaths. But those are out of scope.

### Text

If you are not sure what you need, you most likely want `Text` from the [text package](https://hackage.haskell.org/package/text),
which is shipped with GHC. This type is meant for human readable Unicode text and has all the primitives you need. The
API is in fact more complete than the one for `String`, containing functions like `stripPrefix` and `toLower`.

Internally, Text uses a UTF-8 encoded byte array since version 2.0 and UTF-16 before version 2.0. So it is always
guaranteed to be valid Unicode.

The current definition for strict `Text` is (as of [2.1.1](https://hackage.haskell.org/package/text-2.1.1)):

```hs
-- | A space efficient, packed, unboxed Unicode text type.
data Text = Text
    {-# UNPACK #-} !A.Array -- ^ bytearray encoded as UTF-8
    {-# UNPACK #-} !Int     -- ^ offset in bytes (not in Char!), pointing to a start of UTF-8 sequence
    {-# UNPACK #-} !Int     -- ^ length in bytes (not in Char!), pointing to an end of UTF-8 sequence
```

As we can see here, this type allows efficient slicing to avoid unnecessary `memcpy` for many operations.
E.g. `init` and `tail` are *O(1)* time and space. `splitAt` is *O(1)* space, but *O(n)* time, because UTF-8 complicates
the offset computation (remember, a Unicode Code Point encoding can be anywhere between 1 and 4 bytes in UTF-8).

We explain more about this later in [Slicable vs non-slicable](#slicable-vs-non-slicable).

The lazy Text variant is as follows:

```hs
data Text = Empty
          | Chunk {-# UNPACK #-} !T.Text Text
```

This has the same structure as a list, and as such can also be potentially streamed in constant space or allow
the GC to clean up unused chunks after splitting/slicing.

Text does not allow to represent surrogates. It is a sequence of [Unicode Scalar Values](http://www.unicode.org/versions/Unicode5.2.0/ch03.pdf#page=35).
Invalid values will be converted to the replacement character `U+FFFD` silently when using e.g. `pack`. You might be thinking that's not
a problem... but I have to disappoint you. There is a reason `String` allows surrogates: [PEP-383](https://peps.python.org/pep-0383/).
This is an abomination and base uses it: On Unix, it uses
[`getFileSystemEncoding`](https://hackage.haskell.org/package/base-4.16.1.0/docs/GHC-IO-Encoding.html#v:getFileSystemEncoding) and
[`mkTextEncoding`](https://hackage.haskell.org/package/base-4.16.1.0/docs/GHC-IO-Encoding.html#v:mkTextEncoding) to
pick a round-trippable encoding for filepaths. E.g. if your locale returns `en_US.UTF-8` you'll get `UTF-8//ROUNDTRIP`
`TextEncoding`, which is based on PEP-383 and invalid bytes get translated to some special representation
(lone surrogates) in order to be roundtripped. This has been described in my blog
[Fixing 'FilePath' in Haskell](https://hasufell.github.io/posts/2022-06-29-fixing-haskell-filepaths.html).

#### Text summary

Invariants:

- is always Unicode
- never encodes surrogates (uses replacement char `U+FFFD`)
- unpinned memory (can be moved by the GC at any time, see the [Pinned vs unpinned](#pinned-vs-unpinned) section)
- strict and lazy variants

Useful for:

- anything that fits ASCII or Unicode
- large human readable text processing that requires efficient formats
- complex Unicode handling via advanced libraries such as [text-icu](https://hackage.haskell.org/package/text-icu)
- quite efficient slicing

Not so useful for:

- dealing with C FFI
- trying to store or deal with non-Unicode encodings
- dealing with filepaths
- lots of small Unicode texts

Lazy variants are useful for streaming and incremental processing, as the strict variant requires the whole content to be in memory.

### ShortText

This is an alternative Unicode text type that is meant for lots of small text sequences. It is part of the
[text-short](https://hackage.haskell.org/package/text-short) package. The definition is as follows:

```hs
newtype ShortText = ShortText ShortByteString
```

So there is no length or offset field.
This means it has all the same properties as an unpinned `ShortByteString`, except that the data is guaranteed
to be valid UTF-8.

#### ShortText summary

Invariants:

- is always Unicode
- never encodes surrogates (uses replacement char `U+FFFD`)
- unpinned memory (can be moved by the GC at any time)
- strict

Useful for:

- anything that fits ASCII or Unicode
- lots of small text sequences

Not so useful for:

- using with `text-icu` package, which expects `Text`
- efficient slicing
- dealing with C FFI
- trying to store or deal with non-Unicode encodings
- dealing with filepaths

### ByteString

This is a low level type from the [bytestring](https://hackage.haskell.org/package/bytestring) package, shipped with GHC.
It is just a sequence of bytes and carries no encoding information. It uses
**pinned memory** (see [Pinned vs unpinned](#pinned-vs-unpinned) section). As such, it doesn't require
copying when dealing with the FFI. It is also often more desirable when interacting with FFI, see the GHC
user guide:

- [GHC differences to the FFI Chapter](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/ffi.html#ghc-differences-to-the-ffi-chapter)
- [GHC extensions to the FFI Chapter](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/ffi.html#ghc-extensions-to-the-ffi-chapter)

ByteString is quite efficient and has a large API, but (obviously) lacks text processing
facilities, because it has no knowledge of Unicode (or other textual formats). Most operations work on `Word8` boundaries.

The definition for strict ByteString is (as of [0.12.1.0](https://hackage.haskell.org/package/bytestring-0.12.1.0)):

```hs
data ByteString = BS {-# UNPACK #-} !(ForeignPtr Word8) -- payload
                     {-# UNPACK #-} !Int                -- length
```

This allows, similar to Text, slicing without copying memory (through pointer arithmetic and the length field).
Since we're not dealing with Unicode, but just `Word8` boundaries, operations like `splitAt` are *O(1)* time and
space. We don't need an offset field, because we can just advance the pointer instead.

And the lazy counterpart, which looks similar to lazy Text:

```hs
data ByteString = Empty
                | Chunk  {-# UNPACK #-} !S.StrictByteString ByteString
```

There is an API variant [Data.ByteString.Char8](https://hackage.haskell.org/package/bytestring-0.12.1.0/docs/Data-ByteString-Char8.html),
which allows operations to work on `Char` boundaries. However, it can be misleading to newcomers, because it actually truncates
all Chars to 8 bits. **You should avoid this, unless you know what you are doing.** It is more likely that you are looking
for decoding libraries, where you can specify which encoding to use, e.g. [bytestring-encoding](https://hackage.haskell.org/package/bytestring-encoding-0.1.2.0/docs/Data-ByteString-Encoding.html#v:decode).

It also has to be noted that pinned memory can cause memory fragmentation for lots of small ByteStrings
(this is also discussed in [Fixing 'FilePath' in Haskell](https://hasufell.github.io/posts/2022-06-29-fixing-haskell-filepaths.html)).
An alternative type is `ShortByteString`, which will be discussed next.

#### ByteString summary

Invariants:

- pinned memory
- strict and lazy variants

Useful for:

- large data
- very efficient slicing
- dealing with raw bytes (e.g. web servers)
- dealing with C FFI
- storing non-Unicode encodings e.g. via a newtype wrapper
- fast parsers, see the excellent blog post from Chris Done on [Fast Haskell: Competing with C at parsing XML](https://chrisdone.com/posts/fast-haskell-c-parsing-xml/)

Not so useful for:

- dealing with Unicode or human readable text
- dealing with lots of small byte sequences

Lazy variants, again, are useful for streaming and incremental processing, as the strict variant requires the whole content to be in memory.

### ShortByteString

This type is from the bytestring package as well and lives under
[Data.ByteString.Short](https://hackage.haskell.org/package/bytestring-0.12.1.0/docs/Data-ByteString-Short.html).

It has the same API as `ByteString` since [0.11.3.0](https://hackage.haskell.org/package/bytestring-0.11.3.0/changelog),
so can be used as a drop-in replacement. The main difference is that it is usually backed by *unpinned memory*, so causes no
heap fragmentation. It is possible to construct it pinned via internal API, but slicing operations like `splitAt`
will return unpinned byte strings.

The definition as of [0.12.1.0](https://hackage.haskell.org/package/bytestring-0.12.1.0) is:

```hs
newtype ShortByteString =
  ShortByteString
  { unShortByteString :: ByteArray
  }
```

This makes it suitable for things like Unix filepaths. But we will explore better filepath types later.

The name is maybe a little bit misleading. It can very well be used for large data as well, if you
don't mind its strictness (the whole content is always in memory). **However, this type does not allow slicing**,
unlike `Text` and `ByteString`, and so a lot of operations cause `memcpy`. This however has the advantage that we save
at least 2 words compared to e.g. `Text`, because we don't need an offset or length field.

If you want a similar type, but with slicing capability, use [Bytes](#bytes).

Interfacing with C FFI triggers memory copy as well, because we need pinned memory.

There is no lazy variant.

#### ShortByteString summary

Invariants:

- unpinned memory (when using the default API)
- always strict

Useful for:

- lots of small to medium sized byte sequences
- large data, if strictness is desired and efficient slicing not needed
- dealing with C FFI (although it incurs `memcpy`)
- storing non-Unicode encodings e.g. via a newtype wrapper

Not so useful for:

- dealing with Unicode or human readable text
- fast parsers, because no lazy variant and no efficient slicing

### Bytes

This type is from the `byteslice` package and lives under
[Data.Bytes](https://hackage.haskell.org/package/byteslice-0.2.13.2/docs/Data-Bytes.html). It is
not shipped by GHC.

It is a essentially a `ShortByteString` with 0-copy slicing (`init`, `splitAt` etc.).
It can be constructed as a pinned or unpinned byte sequence and all the usual operations for
it will maintain that invariant.

The definition as of [0.2.13.2](https://hackage.haskell.org/package/byteslice-0.2.13.2) is:

```hs
data Bytes = Bytes
  { array :: {-# UNPACK #-} !ByteArray
  , offset :: {-# UNPACK #-} !Int
  , length :: {-# UNPACK #-} !Int
  }
```

This is exactly the same definition as the `Text` type. But it does not maintain UTF-8. It uses `ByteArray` like `ShortByteString` does.
Compared to `ShortByteString` however, we have three words more memory overhead.

The API allows to convert to `ByteString` and `ShortByteString`. Depending on whether it was pinned or unpinned, sliced
or unsliced, those may be 0-copy operations as well.

There's another variant called `Chunks` in
[Data.Bytes.Chunks](https://hackage.haskell.org/package/byteslice-0.2.13.2/docs/Data-Bytes-Chunks.html):

```hs
data Chunks
  = ChunksCons {-# UNPACK #-} !Bytes !Chunks
  | ChunksNil
```

Although This is quite similar to how lazy `Text` is defined, this type is not lazy at all.
It has [bang patterns](https://downloads.haskell.org/ghc/9.6.5/docs/users_guide/exts/strict.html)
on both the value and the recursion, so it is spine-strict.

The only real use case the `Chunk` type has is when you want to avoid the overhead of constant appending
of `ByteArray`s, because you're e.g. reading a file incrementally.

#### Bytes summary

Invariants:

- can be both pinned or unpinned
- is always strict

Useful for:

- when you want an unpinned strict ByteString...
- or a slicable ShortByteString
- dealing with C FFI
- parsers, if we don't mind strictness

Not so useful for:

- dealing with Unicode or human readable text

### OsString, PosixString and WindowsString

These are relatively new types, which were first added to
[filepath-1.4.100.0](https://hackage.haskell.org/package/filepath-1.4.100.0/changelog) as part of a user-space
implementation of the [Abstract FilePath Proposal](https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/abstract-file-path).
More details [here](https://hasufell.github.io/posts/2022-06-29-fixing-haskell-filepaths.html).

Starting with filepath-1.5.0.0, the types were moved to a new home in the
[os-string](https://hackage.haskell.org/package/os-string) package.

These types are meant to abstract over platform differences and their encodings when dealing with operating system API.
It is similar to the rust type [OsString](https://doc.rust-lang.org/std/ffi/struct.OsString.html), but the implementation is
quite different.

Simplified, the Haskell definitions are:

```hs
-- | Commonly used Windows string as wide character bytes.
newtype WindowsString = WindowsString ShortByteString

-- | Commonly used Posix string as uninterpreted @char[]@ array.
newtype PosixString = PosixString ShortByteString

-- | Newtype representing short operating system specific strings.
--
-- Internally this is either 'WindowsString' or 'PosixString',
-- depending on the platform. Both use unpinned
-- 'ShortByteString' for efficiency.
newtype OsString = OsString
#if defined(mingw32_HOST_OS)
  WindowsString
#else
  PosixString
#endif
```

As we can see, on Unix, we're basically dealing with `Word8` sequences (`char[]`),
but on Windows, we're dealing with `Word16` sequences (`wchar_t*`).

The constructors are internal and it is impossible to pattern match on the wrong platform in `OsString`, due
to the CPP.

OsString provides a rich API just like ByteString.

This allows packages like `unix` and `Win32` to provide alternatives to `String`, where the bytes that are
received from operating system API is not transformed, decoded or otherwise roundtripped. It is **unchanged**.
E.g.:

- [System.Posix.IO.PosixString.openFd](https://hackage.haskell.org/package/unix-2.8.5.1/docs/System-Posix-IO-PosixString.html#v:openFd)
- [System.Win32.WindowsString.File.createFile](https://github.com/haskell/win32/blob/350ebd43f9a8d9e1ca767b0000f95bdfb42a5471/System/Win32/WindowsString/File.hsc#L139)

And at the same time, we are able to write safe, platform agnostic code utilizing `OsString`. E.g.:

- [System.File.OsPath](https://hackage.haskell.org/package/file-io-0.1.1/docs/System-File-OsPath.html)

This strategy has been used for filepaths, where `unix` package uses `PosixString`, `Win32` package uses `WindowsString`
and the platform agnostic `directory` and `file-io` packages use `OsString`, combining the APIs of Unix and Windows. More
information on this with examples and API explanation can be found
[here](https://hasufell.github.io/posts/2022-06-29-fixing-haskell-filepaths.html).

It is not restricted to filepaths, but may be extended to dealing with environment variables, program arguments and
other bits of operating system API. It is always safer than `String` and more type safe than `ByteString`.

#### OsString, PosixString and WindowsString summary

Invariants:

- unpinned memory
- OsString abstracts over platforms
- PosixString is char array
- WindowsString is wide char array

Useful for:

- writing type safe operating system APIs
   - while maintaining the original bytes without decoding
   - abstracting over Unix and Windows
   - making minimal assumptions on underlying encodings

Not so useful for:

- very large data
- data that is not platform specific or doesn't originate from operating system API
- efficient slicing

### OsPath, PosixPath and WindowsPath

These are equivalent to `OsString`, `PosixString` and `WindowsString` and are part of the
[filepath](https://hackage.haskell.org/package/filepath) package as of
[1.4.100.0](https://hackage.haskell.org/package/filepath-1.4.100.0). They are just type synonyms:

```hs
-- | FilePath for Windows.
type WindowsPath = WindowsString

-- | FilePath for posix systems.
type PosixPath = PosixString

-- | Abstract filepath, depending on current platform.
-- Matching on the wrong constructor is a compile-time error.
type OsPath = OsString
```

Use them whenever you can with the new filepath API. Refer to the
[Fixing Haskell filepaths](https://hasufell.github.io/posts/2022-06-29-fixing-haskell-filepaths.html) blog post for more details.

### CString and CStringLen

These are part of `base` and low-level FFI types.

The definitions are very straight forward:

```hs
-- | A C string is a reference to an array of C characters terminated by NUL.
type CString    = Ptr CChar

-- | A string with explicit length information in bytes instead of a
-- terminating NUL (allowing NUL characters in the middle of the string).
type CStringLen = (Ptr CChar, Int)
```

The haddock also explains the expected properties.

As an interesting edge case: if you're converting
from `ByteString` to `CString` and happen to have a NUL byte in your ByteString, then `useAsCString`
will over-allocate bytes:

```hs
useAsCString :: ByteString -> (CString -> IO a) -> IO a
useAsCString (BS fp l) action =
  allocaBytes (l+1) $ \buf -> do
    unsafeWithForeignPtr fp $ \p -> copyBytes buf p l
    pokeByteOff buf l (0::Word8)
    action (castPtr buf)
```

So it can make sense, in some cases, to check your ByteString for NUL bytes.

We won't dive into the Haskell C FFI, but this is literally the only proper use case.
Refer to the [wikibook article on Haskell FFI](https://en.wikibooks.org/wiki/Haskell/FFI).

### FilePath

This type is a **legacy** filepath type, but is still the most widespread across the ecosystem at the time of writing. It is
part of the [filepath](https://hackage.haskell.org/package/filepath) package, which is also shipped with GHC.

The definition is:

```hs
type FilePath = String
```

This is not a very good choice for filepaths. Use the new [OsPath](#ospath-posixpath-and-windowspath) instead.

## Lazy vs Strict

The properties of lazy vs strict variants for `Text` and `ByteString` might already
be obvious for many Haskellers:

- **Lazy**:
  * can be streamed and incrementally processed, potentially in constant space
  * can allow the GC to clean up unused chunks after slicing/splitting
  * can express infinite data streams
  * slightly less efficient in terms of time complexity, depending on number of chunks
    (compared to their strict counterparts)
  * can work with lazy IO ([more on that later](#a-word-on-lazy-io))
- **Strict**:
  * is the most efficient in terms of time complexity
  * is always forced into memory
  * has less overhead than lazy types

A lot of time, people use lazy types in conjunction with lazy IO. However, another use case is to use
**Builders**. These exist for both Text and ByteString:

- [Data.ByteString.Builder](https://hackage.haskell.org/package/bytestring-0.12.1.0/docs/Data-ByteString-Builder.html)
- [Data.Text.Lazy.Builder](https://hackage.haskell.org/package/text-2.1.1/docs/Data-Text-Lazy-Builder.html)

In general, streaming libraries can be a more elegant and performant alternative to lazy Text/ByteString.
We talk about that later in the chapter [Streaming](#streaming). But since much of the ecosystem uses
lazy types, these are still relevant for practical purposes.

## Slicable vs non-slicable

All strings are slicable, but some strings can slice without copying data. E.g. compare `Text` and `ShortText`:

```hs
data Text = Text
    {-# UNPACK #-} !A.Array -- ^ bytearray encoded as UTF-8
    {-# UNPACK #-} !Int     -- ^ offset in bytes (not in Char!), pointing to a start of UTF-8 sequence
    {-# UNPACK #-} !Int     -- ^ length in bytes (not in Char!), pointing to an end of UTF-8 sequence

newtype ShortText = ShortText ShortByteString
```

E.g. when we call `splitAt` on a `Text` value, we get back two new `Text` values that just differ in the "offset" and
"length" fields, but can point at the same byte array. If we slice a lot, this can
save a lot of `memcpy`, especially on large data.

This means that slicing comes at two costs. First, if we split a text in half, the memory of the original byte array can't
be cleaned up by the GC. We just changed the offset and length fields, nothing else.
This can be alleviated by using explicit copy operations when you don't need the whole data anymore, e.g. via
[Data.Text.copy](https://hackage.haskell.org/package/text-2.1.1/docs/Data-Text.html#v:copy).

Second, we carry two unboxed `Int`s around for the "offset" and "length" fields, which is 2 words "overhead".
For more information on boxed and unboxed types, see the GHC user guide:

- [Unboxed types and primitive operations](https://downloads.haskell.org/ghc/9.6.5/docs/users_guide/exts/primitives.html)
- [`UNPACK` pragma](https://downloads.haskell.org/ghc/9.6.5/docs/users_guide/exts/pragmas.html#unpack-pragma)

`ShortText` in contrast, on e.g. `splitAt`, will create two new byte arrays and copy the data.
Here we're not only saving two words memory overhead (no offset and length field),
but also have a bit less indirection at runtime and a bit less memory pressure (which might be useful to
fit into CPU cache) as explained in [this comment](https://github.com/hasufell/hasufell.github.io/pull/7#issuecomment-2105160701).

As such, as the name of the types suggest, a simplified criteria could be:

- slicable type: if you have large strings or need a lot of slicing
- non-slicable type: if you have relatively short strings or don't need a lot of slicing

In the end, only profiling can really tell which one is better.

## Pinned vs unpinned

Pinned memory means it can't be moved by the GC. This is useful if we want to move the data directly to foreign code (FFI),
without first copying the entire unpinned data to a pinned memory region at the FFI boundary.
But it also means that we get memory fragmentation, exactly because the GC cannot move stuff around. If you have
lots of small pieces of data with pinned memory, that can severely fragment the heap.

This and the problems it can cause is explained in more detail in the Well-Typed blog
[Understanding Memory Fragmentation](https://well-typed.com/blog/2020/08/memory-fragmentation/#pinned-data).

The problem of memory fragmentation was also one of the things that motivated the original
[Abstract FilePath proposal](https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/abstract-file-path)
and later the new `OsPath` type.

## String Types Cheat Sheet

A few notes on the below table:

- Unicode aware means whether we have access to text processing functions (e.g. split by Unicode Code Point etc.)
- memory overhead means: total words required modulo the payload
- the overhead for lazy types is multiplied by the number of chunks
- some types are unpinned by default (e.g. `ShortByteString`) but can manually be constructed as pinned via internal API

The memory overhead measurements are best effort and explained in more detail in
[this gist](https://gist.github.com/hasufell/61ca8ef438cc912e7446bcc7b1f25028).

| Type                | purpose                                      | Unicode aware | internal representation             | memory overhead             | pinned | slicing | FFI suitable | streaming |
|---------------------|----------------------------------------------|---------------|-------------------------------------|-----------------------------|--------|---------|--------------|-----------|
| **String**          | simplicity                                   | yes           | List of Unicode Code Points         | 4 words per char + 1 word   | no     | \-\-    | \-\-         | yes       |
| **Text**            | human readable text                          | yes           | UTF-8 byte array                    | 7 words                     | no     | +       | -            | no        |
| **Lazy Text**       | human readable text                          | yes           | List of chunks of UTF-8 byte arrays | 9 words per chunk + 1 word  | no     | +       | -            | yes       |
| **ShortText**       | short human readable texts                   | yes           | UTF-8 byte array                    | 4 words                     | no     | -       | -            | no        |
| **ByteString**      | large byte sequences                         | no            | Word8 byte array (pointer)          | 10 words                    | yes    | ++      | ++           | no        |
| **Lazy ByteString** | large byte sequences                         | no            | List of chunks of Word8 byte arrays | 12 words per chunk + 1 word | yes    | ++      | ++           | yes       |
| **ShortByteString** | short byte sequences                         | no            | Word8 byte array                    | 4 words                     | no     | -       | +            | no        |
| **Bytes**           | slicable ShortByteString / pinned ByteString | no            | Word8 byte array                    | 7 words                     | both   | ++      | +            | no        |
| **Chunks**          | Like "Bytes", but for incremental building   | no            | List of chunks of Word8 byte arrays | 9 words per chunk + 1 word  | both   | ++      | +            | no        |
| **OsString**        | interfacing with OS API                      | no            | Word8 or Word16 byte array          | 4 words                     | no     | -       | +            | no        |

## Construction

Now that we know about the different types, we will take a quick look about different ways to construct strings.

### String literals

The Haskell report defines [Character and String literals](https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-200002.6)
as part of the language.

Whenever you write `"string"` in a Haskell file, the compiler will convert it to/consider it as `[Char]`.
Likewise, `'c'` will be considered `Char`.

### String Classes

A popular String class is [IsString](https://hackage.haskell.org/package/base-4.19.1.0/docs/Data-String.html#t:IsString),
which is defined as:

```hs
class IsString a where
    fromString :: String -> a
```

So this allows to convert from `String` to some other compatible type. Note how the type signature does not allow
failure. So the conversion must be total.

`Text`, `ByteString` and `ShortByteString` have `IsString` instances. `OsString` does not.
All these instances have problems though:

- **Text**: as explained earlier, surrogate Unicode Code Points in a String cannot be converted to Text,
  so you'll end up with the replacement char `U+FFFD`
- **ByteString**/**ShortByteString**: these instances **truncate** to 8 bits and are as such arguably broken, see
  [Surprising behavior of ByteString literals via IsString](https://github.com/haskell/bytestring/issues/140#issuecomment-2023002164)

My personal recommendation is to stay away from this class and use explicit functions like `pack` instead.
However, we could also use QuasiQuoters (more on that later).

### OverloadedStrings

This language extensions extends the support for string literals to allow all types that have an `IsString`
instance. This can be convenient when dealing with lots of Text literals. However, it poses two problems:

- it can make type inference harder (since literals are not merely "String"), so sometimes, having a type annotation is necessary
- the caveats explained for the `IsString` class apply here as well: ByteString doesn't behave well

Example use:

```hs
{-# LANGUAGE OverloadedStrings  #-}

myText = "hello world" :: Text
```

I personally advise against using it.

### QuasiQuoters

This is yet another method to construct string like types. An alternative to literals. It uses
[Template Haskell](https://serokell.io/blog/introduction-to-template-haskell), which are essentially expressions
that are run at compile time. This allows us to validate literals much more rigorously and have GHC fail at compile
time if we attempt to e.g. construct an invalid UTF-8 sequence as Text.

There are many libraries that support quasiquotation. Lots of them also support interpolation (using Haskell expressions/variables)
inside the string) e.g.:

- [string-interpolate](https://hackage.haskell.org/package/string-interpolate)
- [string-qq](https://hackage.haskell.org/package/string-qq)
- [interpolate](https://hackage.haskell.org/package/interpolate)
- [PyF](https://hackage.haskell.org/package/PyF)
- [raw-strings-qq](https://hackage.haskell.org/package/raw-strings-qq)
- [streamly](https://streamly.composewell.com/haddocks/streamly-core-0.2.2/Streamly-Unicode-String.html#v:str)

I personally prefer `string-interpolate`. The README gives a
[nice comparison](https://gitlab.com/williamyaoh/string-interpolate/blob/master/README.md#comparison-to-other-interpolation-libraries)
to some other libraries (copy-pasted for convenience):

|                                          | string-interpolate | interpolate | formatting | Interpolation | interpolatedstring-perl6 | neat-interpolation |
|------------------------------------------|--------------------|-------------|------------|---------------|--------------------------|--------------------|
| String/Text support                      | ✅                  | ✅           | ✅          | ⚠️             | ✅                        | ⚠️                  |
| ByteString support                       | ✅                  | ✅           | ❌          | ⚠️             | ✅                        | ❌                  |
| Can interpolate arbitrary Show instances | ✅                  | ✅           | ✅          | ✅             | ✅                        | ❌                  |
| Unicode-aware                            | ✅                  | ❌           | ⚠️          | ❌             | ❌                        | ⚠️                  |
| Multiline strings                        | ✅                  | ✅           | ✅          | ✅             | ✅                        | ✅                  |
| Indentation handling                     | ✅                  | ✅           | ❌          | ✅             | ❌                        | ✅                  |
| Whitespace/newline chomping              | ✅                  | ❌           | ❌          | ❌             | ❌                        | ❌                  |

An example use case:

```hs
showWelcomeMessage :: Text -> Integer -> Text
showWelcomeMessage username visits =
  [i|Welcome to my website, #{username}! You are visitor #{visits}!|]
```

It is important to note that having many quasi-quotations in your source files **can slow down compilation time**.
There are also (sometimes) issues with tooling, such as code formatters or
[Haskell Language Server](https://haskell-language-server.readthedocs.io/en/stable/).

The `OsString` type provides its own quasi-quoter
[osstr](https://hackage.haskell.org/package/os-string-2.0.2.1/docs/System-OsString.html#v:osstr).

The main advantage, again, is that quasi-quoters can properly fail and do so at compile-time.

## Conversions

There are many ways to convert from one type to another. I propose here the most safe conversions.
For some cases, we will have to provide the encoding, because it cannot be guessed.

The `Data.ByteString.Encode` module listed further down below is part of the
[bytestring-encoding](https://hackage.haskell.org/package/bytestring-encoding-0.1.2.0/docs/Data-ByteString-Encoding.html)
package, which is not shipped with GHC. There are other similar packages like
[utf8-string](https://hackage.haskell.org/package/utf8-string).

Other than that, we only need the packages that provide the types we're dealing with.

We're omitting `ShortText`, because conversions are similar to `Text`. `Bytes` can
be converted to `ByteString` or `ShortByteString` depending on the pinned/unpinned
nature and from there we can follow the below strategies.

### From String to...

Let's write a neat conversion module:

```hs
module StringConversions where

import Data.ByteString (ByteString)
import Data.ByteString.Encoding (TextEncoding)
import Data.ByteString.Short (ShortByteString)
import Data.Text (Text)
import System.OsString
import System.OsString.Encoding (EncodingException)

import qualified Data.ByteString.Encoding as BE
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as T
import qualified System.OsString as OS

toString :: String -> String
toString = id

toText :: String -> Text
toText = T.pack

toByteString :: TextEncoding -> String -> ByteString
toByteString encoding = BE.encode encoding . T.pack

toShortByteString :: TextEncoding -> String -> ShortByteString
toShortByteString encoding = SBS.toShort . BE.encode encoding . T.pack

toOsString :: (TextEncoding, TextEncoding) -> String -> Either EncodingException OsString
toOsString (unixEncoding, windowsEncoding) = OS.encodeWith unixEncoding windowsEncoding
```

For converting to `ByteString` and `ShortByteString`, we have to explicitly specify
an encoding for the resulting byte sequence.
For `OsString` we have to provide encodings per platform, since this type is platform agnostic.

The caveat wrt. Text's `pack` not dealing well with surrogates applies.

### From Text to...

```hs
module TextConversions where

import Data.ByteString (ByteString)
import Data.ByteString.Encoding (TextEncoding)
import Data.ByteString.Short (ShortByteString)
import Data.Text (Text)
import System.OsString
import System.OsString.Encoding (EncodingException)

import qualified Data.ByteString.Encoding as BE
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as T
import qualified System.OsString as OS

toString :: Text -> String
toString = T.unpack

toText :: Text -> Text
toText = id

toByteString :: TextEncoding -> Text -> ByteString
toByteString encoding = BE.encode encoding

toShortByteString :: TextEncoding -> Text -> ShortByteString
toShortByteString encoding = SBS.toShort . BE.encode encoding

toOsString :: (TextEncoding, TextEncoding) -> Text -> Either EncodingException OsString
toOsString (unixEncoding, windowsEncoding) = OS.encodeWith unixEncoding windowsEncoding . T.unpack
```

When converting from `Text`, we can essentially reuse all the API that deals
with just `String` and vice versa.

### From ByteString to...

```hs
module ByteStringConversions where

import Data.ByteString (ByteString)
import Data.ByteString.Encoding (TextEncoding)
import Data.ByteString.Short (ShortByteString)
import Data.Text (Text)
import System.OsString
import System.OsString.Encoding (EncodingException)

import qualified Data.ByteString.Encoding as BE
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as T
import qualified System.OsString as OS

toString :: TextEncoding -> ByteString -> String
toString encoding = T.unpack . BE.decode encoding

toText :: TextEncoding -> ByteString -> Text
toText encoding = BE.decode encoding

toByteString :: ByteString -> ByteString
toByteString = id

toShortByteString :: ByteString -> ShortByteString
toShortByteString = SBS.toShort

-- | This is hard to write correctly. It depends on where the @ByteString@
-- comes from. It may not be possible to interpret it on both platforms.
-- @OsString@ is meant to interface with operating system API, not to manually
-- construct arbitrary strings. Use the @osstr@ quasi quoter if you need
-- literals. Or look at the internals in 'System.OsString.Internal.Types'.
toOsString :: ByteString -> OsString
toOsString = undefined
```

For converting to `String` and `Text`, we have to provide an encoding for
the ByteString in order to decode it.

Converting from a byte sequence of unknown origin to `OsString` is hard.
The way this usually happens is at the FFI boundaries in `Win32` and `unix`
package. The question is what does the given byte sequence represent...
where does it come from, what is its encoding, if any?
If it comes from operating system API, we can just wrap it into our types,
see [System.OsString.Internal.Types](https://hackage.haskell.org/package/os-string-2.0.2.1/docs/System-OsString-Internal-Types.html).
Otherwise, we may need to decode the bytes first and then pick a target encoding.

### From ShortByteString to...

```hs
module ByteStringConversions where

import Data.ByteString (ByteString)
import Data.ByteString.Encoding (TextEncoding)
import Data.ByteString.Short (ShortByteString)
import Data.Text (Text)
import System.OsString
import System.OsString.Encoding (EncodingException)

import qualified Data.ByteString.Encoding as BE
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as T
import qualified System.OsString as OS

toString :: TextEncoding -> ShortByteString -> String
toString encoding = T.unpack . BE.decode encoding . SBS.fromShort

toText :: TextEncoding -> ShortByteString -> Text
toText encoding = BE.decode encoding . SBS.fromShort

toByteString :: ShortByteString -> ByteString
toByteString = SBS.fromShort

toShortByteString :: ShortByteString -> ShortByteString
toShortByteString = id

-- | This is hard to write correctly. It depends on where the @ShortByteString@
-- comes from. It may not be possible to interpret it on both platforms.
-- @OsString@ is meant to interface with operating system API, not to manually
-- construct arbitrary strings. Use the @osstr@ quasi quoter if you need
-- literals. Or look at the internals in 'System.OsString.Internal.Types'.
toOsString :: ShortByteString -> OsString
toOsString = undefined
```

The same caveats as for ByteString apply.

### From OsString to...

```hs
module OsStringConversions where

import Control.Monad.Catch (MonadThrow)
import Data.ByteString (ByteString)
import Data.ByteString.Encoding (TextEncoding)
import Data.ByteString.Short (ShortByteString)
import Data.Text (Text)
import System.OsString
import System.OsString.Encoding (EncodingException)

import qualified Data.ByteString.Encoding as BE
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as T
import qualified System.OsString as OS

toString :: MonadThrow m => OsString -> m String
toString = OS.decodeUtf

toText :: MonadThrow m => OsString -> m Text
toText = fmap T.pack . OS.decodeUtf

-- | It depends whether we want the original bytes passed unchanged
-- and platform specific or whether we want to convert to a unified
-- representation that is the same on both platforms, but in ByteString
-- format.
toByteString :: OsString -> ByteString
toByteString = undefined

-- | Same as 'toByteString'.
toShortByteString :: OsString -> ShortByteString
toShortByteString = undefined

toOsString :: OsString -> OsString
toOsString = id
```

OsString always comes with 3 families of decoding and encoding functions:

- `encodeUtf`/`decodeUtf`: assumes UTF-8 on Unix and UTF-16 LE on Windows
  * we are using this in the code above for simplicity
- `encodeWith`/`decodeWith`: here we have to pass the encoding for both platforms
  explicitly
- `encodeFS`/`decodeFS`: this mimics the behavior of the base library, using
  PEP-383 style encoding on Unix and permissive UTF-16 on Windows

### To JSON

A lot of times we want to send our strings over the wire, possibly
via JSON. We will examine this via the popular [aeson](https://hackage.haskell.org/package/aeson)
library.

Both `Text` and `String` already have `ToJSON` [instances](https://hackage.haskell.org/package/aeson-2.2.1.0/docs/Data-Aeson.html#t:ToJSON).
These are easy, because they are Unicode and [JSON demands UTF-8](https://www.rfc-editor.org/rfc/rfc8259#section-8.1).

For `ByteString`, `ShortByteString` and `OsString` this gets a bit more
complicated. It depends on the exact use case. What is the byte sequence
used for on the machine receiving the JSON? Also see the discussion
[Add saner ByteString instances](https://github.com/haskell/aeson/issues/187)
on the aeson issue tracker.

From my perspective, there are 3 possibilities:

1. convert to `String` (e.g. by assuming UTF-8 or UTF-16), use the existing
   ToJSON instance and hope the receiver knows how to interpret the data
2. if you're dealing with binary data, you can convert to e.g. base64 String or Text
   and then again use the existing instances
   (there's the [base64-bytestring-type](https://hackage.haskell.org/package/base64-bytestring-type-1.0.1/docs/Data-ByteString-Base64-Type.html) library that does this via a newtype)
3. convert the byte sequence to `[Word8]`, which has a valid instance as well

For the case of `OsString`, keep in mind that the raw bytes depend on the
current platform (`char[]` array on Unix and `wchar_t*` on Windows). So you may
have to attach more information if you choose
methods 2 and 3 (e.g. encoding of the byte sequence and platform). And you
need a strategy to deal with e.g. a Windows machine sending binary data
to a Unix machine. As such, I recommend using [`decodeUtf`](https://hackage.haskell.org/package/os-string-2.0.2.1/docs/System-OsString.html#g:3) to get a String. The target machine can then use [`encodeUtf`](https://hackage.haskell.org/package/os-string-2.0.2.1/docs/System-OsString.html#v:encodeUtf) to get back an OsString.

## A word on lazy IO

Some of the named packages expose API for reading and writing files via their lazy variants:

- [Data.Text.Lazy.IO.readfile](https://hackage.haskell.org/package/text-2.1.1/docs/Data-Text-Lazy-IO.html#v:readFile)
- [Data.ByteString.Lazy.readFile](https://hackage.haskell.org/package/bytestring-0.12.1.0/docs/Data-ByteString-Lazy.html#v:readFile)

Lazy IO is a hack to use incremental reading/processing without the use of a
proper streaming library. The [bytestring documentation](https://hackage.haskell.org/package/bytestring-0.12.1.0/docs/Data-ByteString-Lazy.html#g:25) warns about it:

> * The program reads a file and writes the same file. This means that the file may be locked because the handler has not been released when writeFile is executed.
> * The program reads thousands of files, but due to lazy evaluation, the OS's file descriptor limit is reached before the handlers can be released.

Lazy IO makes it hard to reason about resources, order of execution etc.
It is better to use a proper streaming library.

## Streaming

Streaming can not only solve the lazy IO problem, but may also
solve some of the inefficiency of the `[Char]` type and can be more perfomant than lazy Text/ByteString,
while keeping a similarly simple API.

There are many popular streaming libraries. A few of them are:

- [conduit](https://hackage.haskell.org/package/conduit)
- [streaming](https://hackage.haskell.org/package/streaming)
- [streamly](https://hackage.haskell.org/package/streamly)
- [pipes](https://hackage.haskell.org/package/pipes)

### Via Streamly

A couple of years ago I wrote the blog post
[From conduit to streamly](https://hasufell.github.io/posts/2021-10-22-conduit-to-streamly.html),
which gives an introduction into both streamly and conduit. The streamly API
has diverged quite a bit since then, with multiple major versions. So I won't
go into much detail about it.

However, streamly is one notable example which provides an alternative
to the `[Char]` type in [Streamly.Unicode.Stream](https://hackage.haskell.org/package/streamly-core-0.2.2/docs/Streamly-Unicode-Stream.html):

```hs
decodeUtf8 :: Monad m => Stream m Word8 -> Stream m Char
encodeUtf8 :: Monad m => Stream m Char -> Stream m Word8
```

A very simple program to print the last Unicode char of a file via streamly is:

```hs
import System.Environment (getArgs)
import Streamly.Data.Stream (Stream)
import Streamly.Data.Fold (Fold)
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.FileSystem.File as File
import qualified Streamly.Unicode.Stream as Unicode

main :: IO ()
main = do
  (file:_) <- getArgs
  c <- getLastCharFromFile file
  print c

getLastCharFromFile :: FilePath -> IO (Maybe Char)
getLastCharFromFile file = stream `Fold.drive` fold
 where
  stream :: Stream IO Char
  stream = Unicode.decodeUtf8Chunks $ File.readChunks file

  fold :: Monad m => Fold m a (Maybe a)
  fold = Fold.latest
```

To compile this program you need the `streamly-core` package. As we can see
here we can create streams of Unicode Chars easily while reading from a file...
without lazy IO and without the need for the lazy Text type.

If you want to compare the performance of string vs text vs streamly,
you can check out the code here in my [example repository](https://github.com/hasufell/streamly-string).
My results on a 189MB file are:

- string: 1,152s
- lazy text: 0,654s
- streamly: 0,222s

## A note on FilePaths

Just a quick reminder:

- `String` for filepaths is very wrong
- `Text` for filepaths is wrong
- `ByteString` for filepaths is questionable
- `OsPath` for filepaths is good

For more details, read up on:

- [Fixing Haskell filepaths, by Julian Ospald](https://hasufell.github.io/posts/2022-06-29-fixing-haskell-filepaths.html)
- [System.OsPath haddocks](https://hackage.haskell.org/package/filepath-1.5.2.0/docs/System-OsPath.html)

## Reflection

### What we should know

Almost at the end of the post, we should now have some insights into Unicode and understand:

- what a character encoding is (Unicode Code Point)
- what a text encoding is (UTF-8, UTF-16, UTF-32)
- how the different Unicode Transformation Formats work
   - and their trade offs (word boundaries, searching, spaces)
- the problems with Code Points and Surrogates
   - and how this affects the `Char` type, `Text` and the `IsString` instance
- that grapheme clusters are the closest definition of "visible symbol"
   - and that they can consist of multiple code points
- that only UTF-8 is ASCII compatible

We understand the weird numbers that the `Show` instance of `Char`/`String`
sometimes returns.

We have seen a summary of the different string types:

- Text/ShortText for Unicode
- ByteString/ShortByteString for binary data
- The very flexible Bytes type
- OsString for operating systems API
- String for the bin

We know how to construct strings safely, can utilize QuasiQuoters
to do compile-time validation and know how to convert between different
types and how to deal with JSON.

We know the dangers of lazy IO and how to utilize streaming libraries instead.

### Too many Strings

After all these topics, I want to address the opinion that gets thrown around
on the internet a lot: "Haskell has too many String types",
e.g. on [Hacker News](https://news.ycombinator.com/item?id=14567755).

If we take another look at the [String Types Cheat Sheet](#string-types-cheat-sheet),
we don't really see any type that could be replaced by another. They all have
different properties and trade-offs. `ByteString` vs `ShortByteString` may be
a bit less intuitive, but e.g. `Text` is clearly different. `OsPath`
is a specialized type that exists in Rust too.

Maybe people dislike the Lazy variants and prefer proper streaming libraries,
which is a fair point. But even if the community decides to shift, now you have
another type (it's just a streaming type), have to learn streaming library API
and decide which of those 5+ libraries to use. So while we could technically do
away with them, they're a useful low-entry barrier alternative and are still
widely used.

In the end, once all these properties are well understood, I find it hard to make an argument
for less types. However, it is clear that not everyone thinks so:

- [Haskell base proposal: unifying vector-like types](https://www.snoyman.com/blog/2021/03/haskell-base-proposal/)
- [Discourse thread on vector proposal](https://discourse.haskell.org/t/base-proposal-around-vector-like-types/2112)

I am still unable to see the bigger picture, other than more unification of
*internal representations*, but less so of public APIs.

E.g. if we compare the following 3 types, we see a pattern:

```hs
data Text = Text
    {-# UNPACK #-} !A.Array -- ^ bytearray encoded as UTF-8
    {-# UNPACK #-} !Int     -- ^ offset in bytes (not in Char!), pointing to a start of UTF-8 sequence
    {-# UNPACK #-} !Int     -- ^ length in bytes (not in Char!), pointing to an end of UTF-8 sequence

data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                       {-# UNPACK #-} !(Array a)

data Bytes = Bytes
  { array :: {-# UNPACK #-} !ByteArray
  , offset :: {-# UNPACK #-} !Int
  , length :: {-# UNPACK #-} !Int
  }
```

A unification of internal representations would e.g. allow 0-cost conversions, unless
an invariant needs to be checked (like valid unicode).

`Text` could maybe be a newytpe over `Bytes`. But that won't actually save us a type. We still need
the newtype to write an API that maintains the "valid unicode" invariant, which `Bytes`
does not guarantee.

It is also hard to argue for the removal of the "short" types `ShortText` and `ShortByteString`
as described in the section [Slicable vs non-slicable](#slicable-vs-non-slicable).

Writing a new string type from scratch can be really hard. But with the rich APIs of `ByteString`,
`ShortByteString` and `Bytes`, coming up with newtypes might not be that difficult.

### What are we missing

We don't have types for:

* Unicode Scalar Values (away with those surrogates)
* Grapheme Clusters

Especially the latter is something that seems to be potentially useful. We don't
just want to know the boundaries of Unicode code points, but of the actual
user visible symbols, don't we?
The `text-icu` package seems to have an [API for breaking on grapheme boundaries](https://hackage.haskell.org/package/text-icu-0.8.0.5/docs/Data-Text-ICU.html#v:breakCharacter),
but it doesn't look very straight forward. I must admit I haven't looked
very hard though.

We also don't have a good streaming solution in base. And maybe we
never will. But that, probably, also means we will never get rid of lazy IO,
which is a foot-gun for newcomers and everyone else.

My next project is likely going to be strongly typed filepaths, which
[do](https://hackage.haskell.org/package/hpath) [already](https://hackage.haskell.org/package/path) [exist](https://hackage.haskell.org/package/strong-path), just not in combination with `OsPath`.

## Special thanks to

- Andrew Lelechenko
- Jonathan Knowles
- Mike Pilgrem
- John Ericson
- streamly maintainers for their cutting edge API
- all the text, bytestring, byteslice, short-text etc. maintainers
- Other people I pinged about this topic

## Links and relevant stuff

### String type blog posts

- [Fixing Haskell filepaths, by Julian Ospald](https://hasufell.github.io/posts/2022-06-29-fixing-haskell-filepaths.html)
- [String types, by FPComplete](https://www.fpcomplete.com/haskell/tutorial/string-types/)
- [Eat Haskell String Types for Breakfast, by Ziyang Liu](https://free.cofree.io/2020/05/06/string-types/)
- [Untangling Haskell's Strings](https://mmhaskell.com/blog/2017/5/15/untangling-haskells-strings)
- [Haskell Strings, by Chris Warburton](http://www.chriswarbo.net/blog/2020-06-08-haskell_strings.html)
- [vector: Efficient Packed-Memory Data Representations, by FPComplete](https://www.fpcomplete.com/haskell/library/vector/)

### Other blog posts

- [From conduit to streamly](https://hasufell.github.io/posts/2021-10-22-conduit-to-streamly.html)
- [Fast Haskell: Competing with C at parsing XML](https://chrisdone.com/posts/fast-haskell-c-parsing-xml/)
- [Beating C With 80 Lines Of Haskell: Wc](https://chrispenner.ca/posts/wc)
- [Haskell base proposal: unifying vector-like types](https://www.snoyman.com/blog/2021/03/haskell-base-proposal/)
- [Haskell base proposal, part 2: unifying vector-like types](https://www.snoyman.com/blog/2021/03/haskell-base-proposal-2/)
- [The text package: finally with UTF-8, by Andrew Lelechenko](https://github.com/Bodigrim/my-talks/blob/master/zurihac2022/slides.pdf)

### Interesting issues

- [Quit using ForeignPtr in favor of ByteArray#](https://github.com/haskell/bytestring/issues/193)

### String types not discussed here

- [monoid-subclasses](https://hackage.haskell.org/package/monoid-subclasses)
- [Data.Vector](https://hackage.haskell.org/package/vector-0.13.1.0/docs/Data-Vector.html)
- [Data.ByteString.Builder](https://hackage.haskell.org/package/bytestring-0.12.1.0/docs/Data-ByteString-Builder.html) (not really a string type)
- [GHC.Data.FastString](https://hackage.haskell.org/package/ghc-9.8.2/docs/GHC-Data-FastString.html)
- [Data.JSString](https://hackage.haskell.org/package/jsaddle-0.9.9.0/docs/Data-JSString.html)

