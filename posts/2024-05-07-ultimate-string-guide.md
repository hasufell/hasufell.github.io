---
title: The ultimate guide to Haskell Strings
author: Julian Ospald
tags: haskell, unicode, string
---

This guide is aimed at Haskellers who seek to improve their understanding of String types,
be it beginners or seasoned developers. It is also meant to be a quick reference/cheat sheet
for deciding which string type to use in a given situation.

## TOC

* [Motivation](#motivation)
* [String in Prelude](#string-in-prelude)
   * [Char](#char)
* [Unicode](#unicode)
   * [Unicode Codepoint](#unicode-codepoint)
   * [UTF-32](#utf-32)
   * [UTF-16](#utf-16)
   * [Unicode Scalar Values](#unicode-scalar-values)
   * [UTF-8](#utf-8)
   * [Unicode summary](#unicode-summary)
* [Back to Haskell String type](#back-to-haskell-string-type)
* [String Types overview](#string-types-overview)
* [Which String type to pick](#which-string-type-to-pick)
   * [Text](#text)
      * [Text summary](#text-summary)
   * [ByteString](#bytestring)
      * [ByteString summary](#bytestring-summary)
   * [ShortByteString](#shortbytestring)
      * [ShortByteString summary](#shortbytestring-summary)
   * [OsString, PosixString and WindowsString](#osstring-posixstring-and-windowsstring)
      * [OsString, PosixString and WindowsString summary](#osstring-posixstring-and-windowsstring-summary)
   * [CString and CStringLen](#cstring-and-cstringlen)
   * [FilePath](#filepath)
   * [OsPath, PosixPath and WindowsPath](#ospath-posixpath-and-windowspath)
* [Related blog posts](#related-blog-posts)

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
This is a bit vague, in fact. What it actually means is [Unicode Code Point](https://www.unicode.org/glossary/#code_point),
or at least that's how it's implemented in GHC as a [smart constructor](https://wiki.haskell.org/Smart_constructors):

```hs
chr :: Int -> Char
chr i@(I# i#)
 | isTrue# (int2Word# i# `leWord#` 0x10FFFF##) = C# (chr# i#)
 | otherwise
    = errorWithoutStackTrace ("Prelude.chr: bad argument: " ++ showSignedInt (I# 9#) i "")
```

We see here that `Char` is basically just an `Int` with an upper bound on `0x10FFFF`. In order to understand this,
we actually have to take a short dive into Unicode.

## Unicode

The Unicode Standard is a standard to identify and encode visible "characters" that comprise "text",
supporting all of the world's major writing systems.

The exact terminology can be very confusing. We'll focus only on a couple of core concepts. If you want to read up yourself on
the standard, here are some pointers:

- the actual standard: [https://www.unicode.org/versions/latest/](https://www.unicode.org/versions/latest/)
- Wikipedia Unicode article: [https://en.wikipedia.org/wiki/Unicode](https://en.wikipedia.org/wiki/Unicode)
- brief unicode introduction for JavaScript developers: [https://exploringjs.com/impatient-js/ch_unicode.html](https://exploringjs.com/impatient-js/ch_unicode.html)

The goal of unicode is to be universal, efficient and unambiguous. In order to achieve that, it needs:

- a character encoding: translating e.g. `a` or `쟬` to something unambiguous
- a text encoding: translating a sequence of characters into an efficient byte format

The term "character" is quite overloaded and we will go through different definitions along the way.

### Unicode Codepoint

Unicode Codepoints are a way of encoding a single character through numerical values. It ranges from the hexadecimal values 0 to 10FFFF,
which we saw before in the definition of `chr :: Int -> Char`.
The formal notation of codepoints is `U+0000` to `U+10FFFF`.

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
- it can express chinese and other non-latin characters
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

UTF-32 is a fixed-length encoding that uses 32 bits (four bytes) and as such can hold all possible unicode
values without any actual transformation.

The upside of this is that it's simple, the downside is that it's wasting space, because most values don't
need the whole 21 bits (e.g. ASCII just needs 7 bits).

UTF-32 is not ASCII compatible, meaning a program that only understands ASCII won't accidentially work with UTF-32 text,
even if all of the characters used are in the ASCII set (e.g. only latin characters from `[a-zA-Z]`).

### UTF-16

This is a [variable-width character encoding](https://en.wikipedia.org/wiki/Variable-width_encoding), most notably
[used on windows](https://learn.microsoft.com/en-us/windows/win32/learnwin32/working-with-strings).

Since we only have access to 16 bits per machine word, only the code points from `U+0000` to `U+FFFF`
(with the exception of surrogates, which I will explain later) are encoded "directly".

Code points from `U+10000` to `U+10FFFF` need two 16-bit words. In order to encode these without
being accidentially ambiguous, **surrogates** were introduced (another option would have been magic bits as used by UTF-8,
but I guess the format wasn't designed with extension in mind). These surrogates must always come in pairs and express:

- low surrogates: `U+DC00` to `U+DFFF`
- high surrogates: `U+D800` to `U+DBFF`

Through bit shuffling, these two bytes allows to map to values in the `U+10000` to `U+10FFFF` range.
For the interested reader, the algorithm is as follows
([cited from wikipedia](https://en.wikipedia.org/wiki/UTF-16#Code_points_from_U+010000_to_U+10FFFF)):

> * 0x10000 is subtracted from the code point (U), leaving a 20-bit number (U') in the hex number range 0x00000–0xFFFFF.
> * The high ten bits (in the range 0x000–0x3FF) are added to 0xD800 to give the first 16-bit code unit or high surrogate (W1), which will be in the range 0xD800–0xDBFF.
> * The low ten bits (also in the range 0x000–0x3FF) are added to 0xDC00 to give the second 16-bit code unit or low surrogate (W2), which will be in the range 0xDC00–0xDFFF

UTF-16 is not ASCII compatible either. It is more space efficient than UTF-32 though. For some languages, it can
even be more space efficient than UTF-8.


### Unicode Scalar Values

It is important to understand that the Haskell `Char` type (which is essentially a Code Point) can represent surrogates
that are used in UTF-16.

The unicode standard also defines the concept of [Unicode Scalar Values](http://www.unicode.org/versions/Unicode5.2.0/ch03.pdf#page=35):

> Any Unicode code point except high-surrogate and low-surrogate code points. In other words, the ranges of integers 0 to D7FF16 and E00016 to 10FFFF16 inclusive.

So, code point without surrogates. This will become relevant for UTF-8.

### UTF-8

This is similar to UTF-16 a variable-width character encoding. It's often used in web APIs (most notably JSON) and is
often the default on unix systems.

Here a word is only 8 bit. The number of bytes required depends on the range of the code point and
varies between 1 and 4 bytes. The whole bit conversion between code point and UTF-8 is illustrated in the following table (adopted from [wikipedia](https://en.wikipedia.org/wiki/UTF-8#Encoding)):

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
- unicode code points in the surrogate range `U+D800` to `U+DFFF` are considered invalid byte sequences
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

- it is inefficient for large text (carries the overhead of a linked list with thunks for every `Char`)
- it is often confusing for users who don't have a good mental model of what a *Unicode Code Point* is
  (and is uncommon or non-existent in other languages)
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

## String Types overview

Luckily, we have many more String types:

- proper unicode text
    - [Text](https://hackage.haskell.org/package/text-2.1.1/docs/Data-Text.html#t:Text) (strict and lazy)
- byte sequences
    - [ByteString](https://hackage.haskell.org/package/bytestring-0.12.1.0/docs/Data-ByteString.html#t:ByteString) (strict and lazy)
    - [ShortByteString](https://hackage.haskell.org/package/bytestring-0.12.1.0/docs/Data-ByteString-Short.html#t:ShortByteString)
- bytes sequences dealing with platform API differences
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

## Which String type to pick

In this section, we will examine each string like type and what its properties and use cases are.
`String` was already discussed and we don't recommend it, so it's omitted here.

### Text

If you are not sure what you need, you most likely want `Text` from the [text package](https://hackage.haskell.org/package/text),
which is shipped with GHC. This type is meant for human readable Unicode text and has all the primitives you need. The
API is in fact more complete than the one for `String`, containing functions like `stripPrefix` and `toLower`.

Internally, Text uses a UTF-8 encoded byte array since version 2.0 and UTF-16 before version 2.0. So it is always
guaranteed to be valid unicode.

The current definition for strict `Text` is (as of [2.1.1](https://hackage.haskell.org/package/text-2.1.1)):

```hs
-- | A space efficient, packed, unboxed Unicode text type.
data Text = Text
    {-# UNPACK #-} !A.Array -- ^ bytearray encoded as UTF-8
    {-# UNPACK #-} !Int     -- ^ offset in bytes (not in Char!), pointing to a start of UTF-8 sequence
    {-# UNPACK #-} !Int     -- ^ length in bytes (not in Char!), pointing to an end of UTF-8 sequence
```

As we can see here, this type allows efficient slicing to avoid unnecessary `memcpy` for many operations.

The lazy variant:

```hs
data Text = Empty
          | Chunk {-# UNPACK #-} !T.Text Text
```

Text does not allow to represent surrogates. It is a sequence of [Unicode Scalar Values](http://www.unicode.org/versions/Unicode5.2.0/ch03.pdf#page=35).
Invalid values will be converted to the replacement character `U+FFFD` silently when using e.g. `pack`. You might be thinking that's not
a problem... but I have to disappoint you. There is a reason `String` allows surrogates: [PEP-383](https://peps.python.org/pep-0383/).
This is an abomination and base uses it: On unix, it uses [`getFileSystemEncoding`](https://hackage.haskell.org/package/base-4.16.1.0/docs/GHC-IO-Encoding.html#v:getFileSystemEncoding) and [`mkTextEncoding`](https://hackage.haskell.org/package/base-4.16.1.0/docs/GHC-IO-Encoding.html#v:mkTextEncoding) to pick a round-trippable encoding for filepaths. E.g. if your locale returns `en_US.UTF-8` you'll get `UTF-8//ROUNDTRIP` TextEncoding, which is based on PEP-383 and invalid bytes get translated to some special representation (lone surrogates) in order to be roundtripped. This has been described in my blog
[Fixing 'FilePath' in Haskell](https://hasufell.github.io/posts/2022-06-29-fixing-haskell-filepaths.html).

#### Text summary

Invariants:

- is always unicode
- never encodes surrogates (uses replacement char `U+FFFD`)
- unpinned memory

Useful for:

- anything that fits ASCII or unicode
- large human readable text processing that requires efficient formats
- complex unicode handling via advanced libraries such as [text-icu](https://hackage.haskell.org/package/text-icu)

Not so useful for:

- dealing with C FFI
- trying to store or deal with non-unicode encodings
- dealing with filepaths

Lazy variants are useful for streaming and incremental processing, as the strict variant requires the whole content to be in memory.

### ByteString

This is a low level type from the [bytestring](https://hackage.haskell.org/package/bytestring) package, shipped with GHC.
**It is not a string**. It shouldn't have been called \*String. It is a sequence of bytes,
carries no encoding information and is **pinned memory** (cannot be moved by the GC). As such, it doesn't require
copying when dealing with the FFI. It is quite efficient and has a large API, but (obviously) lacks text processing
facilities, because it has no knowledge of unicode. Most operations work on `Word8` boundaries.

The definition for strict ByteString is (as of [0.12.1.0](https://hackage.haskell.org/package/bytestring-0.12.1.0)):

```hs
data ByteString = BS {-# UNPACK #-} !(ForeignPtr Word8) -- payload
                     {-# UNPACK #-} !Int                -- length
```

This allows, similar to Text, slicing without copying memory (through pointer arithmetic and the length field).

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

Useful for:

- large data
- dealing with raw bytes (e.g. web servers)
- dealing with C FFI
- storing non-unicode encodings e.g. via a newtype wrapper
- fast parsers, see the excellent blog post from Chris Done on [Fast Haskell: Competing with C at parsing XML](https://chrisdone.com/posts/fast-haskell-c-parsing-xml/)

Not so useful for:

- dealing with unicode or human readable text
- dealing with lots of small byte sequences

Lazy variants, again, are useful for streaming and incremental processing, as the strict variant requires the whole content to be in memory.

### ShortByteString

This type is from the bytestring package as well and lives under
[Data.ByteString.Short](https://hackage.haskell.org/package/bytestring-0.12.1.0/docs/Data-ByteString-Short.html).

It has the same API as `ByteString` since [0.11.3.0](https://hackage.haskell.org/package/bytestring-0.11.3.0/changelog),
so can be used as a drop-in replacement. The main difference is that it is *unpinned memory*, so causes no
heap fragmentation. It also has no lazy variant.

The definition as of [0.12.1.0](https://hackage.haskell.org/package/bytestring-0.12.1.0) is:

```hs
newtype ShortByteString =
  ShortByteString
  { unShortByteString :: ByteArray
  }
```

This makes it suitable for things like unix filepaths. But we will explore better filepath types later.

The name is maybe a little bit misleading. It can very well be used for large data as well, if you
don't mind its strictness (the whole content is always in memory). **However, this type does not allow slicing**,
unlike `Text` and `ByteString`, and so a lot of operations cause `memcpy`.

Interfacing with C FFI triggers memory copy as well, because we need pinned memory.

#### ShortByteString summary

Invariants:

- unpinned memory

Useful for:

- lots of small to medium sized byte sequences
- large data, if strictness is desired and efficient slicing not needed
- dealing with C FFI (although it incurs `memcpy`)
- storing non-unicode encodings e.g. via a newtype wrapper

Not so useful for:

- dealing with unicode or human readable text
- fast parsers, because no lazy variant and no efficient slicing

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
-- | Commonly used windows string as wide character bytes.
newtype WindowsString = WindowsString ShortByteString

-- | Commonly used Posix string as uninterpreted @char[]@ array.
newtype PosixString = PosixString ShortByteStrine

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
and the platform agnostic `directory` and `filo-io` packages use `OsString`, combining the APIs of unix and windows. More
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
   - abstracting over unix and windows
   - making minimal assumptions on underlying encodings

Not so useful for:

- very large data
- data that is not platform specific or doesn't originate from operating system API

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

This is not a very good choice for filepaths. Use the new `OsPath` instead.

### OsPath, PosixPath and WindowsPath

These are equivalent to `OsString`, `PosixString` and `WindowsString`. They are just type synonyms:

```hs
-- | FilePath for windows.
type WindowsPath = WindowsString

-- | FilePath for posix systems.
type PosixPath = PosixString

-- | Abstract filepath, depending on current platform.
-- Matching on the wrong constructor is a compile-time error.
type OsPath = OsString
```

Use them whenever you can with the new filepath API. Refer to the
[Fixing Haskell filepaths](https://hasufell.github.io/posts/2022-06-29-fixing-haskell-filepaths.html) blog post for more details.

## Related blog posts

- [Fixing Haskell filepaths, by Julian Ospald](https://hasufell.github.io/posts/2022-06-29-fixing-haskell-filepaths.html)
- [String types, by FPComplete](https://www.fpcomplete.com/haskell/tutorial/string-types/)
- [Eat Haskell String Types for Breakfast, by Ziyang Liu](https://free.cofree.io/2020/05/06/string-types/)
- [Untangling Haskell's Strings](https://mmhaskell.com/blog/2017/5/15/untangling-haskells-strings)

