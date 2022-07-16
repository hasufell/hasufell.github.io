# Fixing 'FilePath' in Haskell

I'm pleased to announce that the Haskell type `type FilePath = String`
has a successor, which was first discussed many years ago as the [Abstract FilePath proposal (AFPP)](https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/abstract-file-path).

The new type shipped with the [filepath-1.4.100.0](https://hackage.haskell.org/package/filepath-1.4.100.0) package is:

```hs
-- * Path types

-- | FilePath for windows.
type WindowsPath = WindowsString

-- | FilePath for posix systems.
type PosixPath = PosixString

-- | Abstract filepath, depending on current platform.
-- Matching on the wrong constructor is a compile-time error.
type OsPath = OsString


-- * String types
-- Constructors are not public API.

newtype WindowsString = WindowsString ShortByteString

newtype PosixString = PosixString ShortByteString

newtype OsString = OsString
#if defined(mingw32_HOST_OS)
  WindowsString
#else
  PosixString
#endif
```

The reason we have two sets of types here is simply to maintain the current weak distinction
in filepath for functions that deal with not-quite-filepaths, e.g.: `splitSearchPath :: String -> [FilePath]`. This also
allows us to provide slightly different API (e.g. QuasiQuoter for `OsString` differs from `OsPath`).
OsPath is not a newtype, because it doesn't provide any additional guarantees over OsString. 'filepath' remains
a low-level library and does not provide strong guarantees for filepaths (such as validity).

Libraries with stronger filepath guarantees are listed in the [README](https://gitlab.haskell.org/haskell/filepath/-/blob/master/README.md#what-is-a-filepath).

Unlike the original proposal, this is **additional API (not part of `base`) and will not break any existing code**.
Core libraries are expected to upgrade their API and provide additional variants that support this new type.
Migration strategies are discussed further down. The ecosystem might need some time to migrate.
This is also a [call for help](#how-to-help)!

But let's look at the reasons why `String` is problematic first.

## TOC

* [What's wrong with String?](#whats-wrong-with-string)
* [The solution](#the-solution)
* [How to use the new API](#how-to-use-the-new-api)
* [Migration for library authors](#migration-for-library-authors)
  * [1. drop String based API and just provide OsPath](#1-drop-string-based-api-and-just-provide-ospath)
  * [2. provide a shim compatibility API for String](#2-provide-a-shim-compatibility-api-for-string)
  * [3. using CPP to export two APIs](#3-using-cpp-to-export-two-apis)
  * [Accessing the raw bytes in a cross-platform manner](#accessing-the-raw-bytes-in-a-cross-platform-manner)
* [History of the proposal](#history-of-the-proposal)
* [Contributors](#contributors)
* [Patch load](#patch-load)
* [How to help](#how-to-help)
* [FAQ](#faq)

## What's wrong with String?

Filepaths are resources on the (users) system. We create, delete, copy them. Any corner case with filepaths can have devastating effects: deleting the wrong file, comparing the wrong files, failing whitelists, security bugs, etc.

To recap, the definition of String is:

```hs
type String = [Char]
```

So a String is a list of `Char`. And `Char` is encoded as UTF-8, right? Unfortunately not, it's a *Unicode code point*.

A unicode code point is an integer in the *Unicode codespace*. The
[standard](https://www.unicode.org/versions/Unicode14.0.0/ch03.pdf#G2212) gets a little
technical here, but let's just say UTF-8 is one of many encodings of `[Char]`.

That out of the way, let's look at how filepaths are actually represented on the system level.

On windows, filepaths are just [*wide character* arrays](https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-dtyp/76f10dd8-699d-45e6-a53c-5aefc586da20) (`wchar_t*`, so basically `[Word16]`).
On unix, filepaths are [*character* arrays](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_170) (`char[]`, so basically `[Word8]`).

In both cases, there's no encoding specified, although on windows we can *mostly* assume UTF-16LE. So... to go from `String` to
`CString`/`CWString` at the outer FFI layer, we need to make a decision.

`base` currently does the following:

1. On unix, it uses [`getFileSystemEncoding`](https://hackage.haskell.org/package/base-4.16.1.0/docs/GHC-IO-Encoding.html#v:getFileSystemEncoding) and [`mkTextEncoding`](https://hackage.haskell.org/package/base-4.16.1.0/docs/GHC-IO-Encoding.html#v:mkTextEncoding) to pick a round-trippable encoding for filepaths. E.g. if your locale returns `en_US.UTF-8` you'll get `UTF-8//ROUNDTRIP` TextEncoding, which is based on [PEP 383](https://peps.python.org/pep-0383/) and invalid bytes get translated to some special representation (lone surrogates) in order to be roundtripped.
2. On windows, it uses a [private permissive UTF-16 encoding](https://gitlab.haskell.org/ghc/ghc/-/blob/0e22f16cda8468256b4c5d04214276be30e23faa/libraries/base/Foreign/C/String.hs#L423-444) that allows to roundtrip coding errors as well.

Windows isn't too problematic here. The encoding is total. However, on unix, the interpretation of filepaths depends on the *currently set locale*. This is wrong for a number of reasons:

1. there's no guarantee that the currently set locale corresponds to the encoding of a specific filepath (the filepath could be on a USB drive that has a japanese encoding, such as `CP932`)
2. as the documentation of [mkTextEncoding](https://hackage.haskell.org/package/base-4.16.1.0/docs/GHC-IO-Encoding.html#v:mkTextEncoding) says, only very specific encodings actually roundtrip properly (`CP932` does not)
3. on conversion to `String`, you "lose" the underlying encoding and may end up with weirdly escaped Unicode codepoints. Roundtripping can break if a call to `setFileSystemEncoding` interleaves the conversions.
4. it's hard to get the original bytes back... this may have security implications for e.g. filepath whitelists

So, how do other languages solve this? Python simply enforces `UTF-8` (with PEP 383 escaping) on unix.
That makes the roundtripping almost sound. But this comes with its own set of problems:

1. if the underlying filepath is not UTF-8, the `[Char]` representation is lossless (from `CString` to `[Char]`), but may be somewhat non-sensical for further interpretation, because you might have excessive escaping or your `Char`s don't correspond to what the user sees on their system
2. this has really bad interoperability, because the roundtrip encoding can in fact produce invalid UTF-8. The unicode consortium itself has [voiced their concerns with this approach](https://unicode.org/L2/L2009/09236-pep383-problems.html)
3. since Haskell `Char` also includes surrogates, the conversion from `String` to e.g. UTF-8 `CString` can in fact fail, so is not total

I have assembled a [list of correctness issues](https://gist.github.com/hasufell/c600d318bdbe010a7841cc351c835f92)
with these approaches for in-depth reading.

## The solution

Just stop converting filepaths!

We can just keep the original bytes from the system API. Many filepath operations actually don't need to know the exact underlying encoding.
E.g. the filepath separator `/` on unix is a pre-defined *byte* (`0x2F`). You can just scan the byte array for this byte.
The position doesn't matter, the encoding doesn't matter. File **names** *cannot* include this byte, period.

However, since unix and windows are different (`[Word8]` vs `[Word16]`), any API that deals with low-level filepaths in a
cross-platform manner needs to understand this and write correct code. More on this in the migration strategy section below.

We decided to use `ShortByteString` as the internal representation of filepaths, because:

1. these are raw, uninterpreted bytes, a wrapper around `ByteArray#`, which has many efficient primops
2. it's unpinned, so doesn't contribute to memory fragmentation ([proof](https://github.com/hasufell/filepath-debug/blob/master/result.txt))
3. provides convenient API via `bytestring`, which has been [greatly enhanced as part of this proposal](https://github.com/haskell/bytestring/pull/471)

So, in general the idea is to avoid dealing with `String` at all. There may still be use cases for String though, e.g.:

1. dealing with legacy APIs
2. reading filepaths from a UTF-8 encoded text file (you probably want `Text` here, but it's trivial to convert to String)
3. a unified representation across platforms (e.g. to send over the wire or to serialize)

## How to use the new API

Many examples are here: [https://github.com/hasufell/filepath-examples](https://github.com/hasufell/filepath-examples)

Note that not all libraries have released support for the new API yet, so have a look at this [cabal.project](https://github.com/hasufell/filepath-examples/blob/master/cabal.project) if you want to start right away. Generally, you should be able to use these packages already:

* **filepath**: provides filepath manipulation and the new `OsPath` type
* **unix**: provides new API variants, e.g. `System.Posix.Files.PosixString` (as an alternative to `System.Posix.Files`)
* **Win32**: similarly, provides new variants, e.g. `System.Win32.WindowsString.File`
* **directory**: provides the new API under `System.Directory.OsPath`
* [**file-io**](https://github.com/hasufell/file-io): companion package that provides base-like file reading/writing/opening operations

Most end-users developing applications should be able to convert to the new API with little effort, given that their favorite libraries
already support this new type.

[System.OsPath](https://hackage.haskell.org/package/filepath-1.4.100.0/docs/System-OsPath.html) exports the same API as `System.FilePath` with some additional helpers to convert from and to `String`. Likewise `System.OsPath.Posix`/`System.OsPath.Windows` are equivalent to `System.FilePath.Posix`/`System.FilePath.Windows`.

So, you can just:

1. update your dependencies lower bounds to the minimum version that supports `OsPath` (might need [source-repository-package](https://github.com/hasufell/filepath-examples/blob/master/cabal.project) stanzas)
2. for `filepath` import `System.OsPath` instead of `System.FilePath`
3. use the specialised API from your dependencies (e.g. for unix `System.Posix.Directory.PosixPath` instead of `System.Posix.Directory`)
4. to write OsPath literals, use the provided [QuasiQuoters](https://hackage.haskell.org/package/filepath-1.4.100.0/docs/System-OsPath.html#v:osp). There's no `IsString` instance, see the [faq](#why-is-there-no-isstring-instance-overloadedstrings).
5. if you're just using an ASCII subset or strict unicode scalar values, you can use `fromJust . encodeUtf` and `fromJust . decodeUtf` to pack/unpack literals
6. since `base` doesn't support this new type, you'll need the already mentioned companion library [file-io](https://github.com/hasufell/file-io) for opening a `Handle` and writing/reading files
7. if you use legacy APIs that still use `FilePath`, there are [examples](https://github.com/hasufell/filepath-examples/blob/master/examples/Process.hs) on how to deal with them (usually `System.OsPath.encodeFS` and `System.OsPath.decodeFS`)

A table for encoding/decoding strategies follows:

| API function        | from     | to     | posix encoding                   | windows encoding               | remarks          |
|---------------------|----------|--------|----------------------------------|--------------------------------|------------------|
| `encodeUtf`   | FilePath   | OsPath | UTF-8 (strict)                   | UTF-16 (strict)                | not total        |
| `encodeWith`      | FilePath   | OsPath | user specified                   | user specified                 | depends on input |
| `encodeFS`    | FilePath   | OsPath | depends on getFileSystemEncoding | UTF-16 (escapes coding errors) | requires IO, used by `base` for roundtripping      |
| `decodeUtf`   | OsPath   | FilePath | UTF-8 (strict)                   | UTF-16 (strict)                | not total        |
| `decodeWith`      | OsPath   | FilePath | user specified                   | user specified                 | depends on input |
| `decodeFS`    | OsPath   | FilePath | depends on getFileSystemEncoding | UTF-16 (escapes coding errors) | requires IO, used by `base` for roundtripping      |

These conversions are particularly useful if you're dealing with legacy API that is still `FilePath` based. An example on
how to do that with the process package is [here](https://github.com/hasufell/filepath-examples/blob/master/examples/Process.hs).

## Migration for library authors

Core libraries or other libraries exporting API that is heavy on filepaths generally have 3 options:

### 1. drop String based API and just provide OsPath

This is feasible, because users can themselves convert via `System.OsPath.encodeFS` and `System.OsPath.decodeFS` to and from `String`.

### 2. provide a shim compatibility API for String

This is what this `directory` PR does: [https://github.com/haskell/directory/pull/136/files](https://github.com/haskell/directory/pull/136/files)... see `System/Directory.hs`.

The idea is to write the core against `OsPath` and then create a `String` based API that wraps the core via
`System.OsPath.encodeFS` and `System.OsPath.decodeFS` to mimic behavior of `base`. This usually requires IO, though.

### 3. using CPP to export two APIs

This is what filepath itself does. It contains an [abstract module](https://gitlab.haskell.org/haskell/filepath/-/blob/master/System/FilePath/Internal.hs), which is then imported while setting specific types and platform information ([PosixPath](https://gitlab.haskell.org/haskell/filepath/-/blob/master/System/OsPath/Posix/Internal.hs), [WindowsPath](https://gitlab.haskell.org/haskell/filepath/-/blob/master/System/OsPath/Windows/Internal.hs), [System.FilePath.Posix](https://gitlab.haskell.org/haskell/filepath/-/blob/master/System/FilePath/Posix.hs) and [System.FilePath.Windows](https://gitlab.haskell.org/haskell/filepath/-/blob/master/System/FilePath/Windows.hs)).

The main trick here is to not use any String based API (e.g. no pattern matching or use of `:`). Instead, we only use `uncons`/`unsnoc`, `head`/`last` etc, so the intersection of String and ShortByteString APIs... and then adjust the imports based on the type.

E.g. the following code:

```hs
splitSearchPath :: String -> [FilePath]
splitSearchPath = f
    where
    f xs = case break isSearchPathSeparator xs of
           (pre, []    ) -> g pre
           (pre, _:post) -> g pre ++ f post

    g "" = ["." | isPosix]
    g ('\"':x@(_:_)) | isWindows && last x == '\"' = [init x]
    g x = [x]
```

became:

```hs
splitSearchPath :: STRING -> [FILEPATH]
splitSearchPath = f
    where
    f xs = let (pre, post) = break isSearchPathSeparator xs
           in case uncons post of
             Nothing     -> g pre
             Just (_, t) -> g pre ++ f t

    g x = case uncons x of
      Nothing -> [singleton _period | isPosix]
      Just (h, t)
        | h == _quotedbl
        , (Just _) <- uncons t -- >= 2
        , isWindows
        , (Just (i, l)) <- unsnoc t
        , l == _quotedbl -> [i]
        | otherwise -> [x]
```

The windows include site is something like:

```hs
-- word16 based bytestring functions
import System.OsPath.Data.ByteString.Short.Word16
-- defining types
#define FILEPATH ShortByteString
#define WINDOWS
-- include the CPP module
#include "Internal.hs"
```

Then we can have a `WindowsPath`/`PosixPath`/`OsPath` wrappers:

```hs
splitPath :: FILEPATH_NAME -> [FILEPATH_NAME]
splitPath (OSSTRING_NAME bs) = OSSTRING_NAME <$> C.splitPath bs
```

And that is included like so:

```hs
import System.OsPath.Types
import System.OsString.Windows
import qualified System.OsPath.Windows.Internal as C

#define FILEPATH_NAME WindowsPath
#define WINDOWS

#include "PathWrapper.hs"
```

Not very pretty, but avoids a lot of repetition and doesn't require a partial wrapper layer that converts between `ShortByteString` and `String`.

### Accessing the raw bytes in a cross-platform manner

Some libraries might need access to the raw bytes of the filepaths, e.g. because the `filepath` API is insufficient.
It's important to understand that on unix, we're basically dealing with `[Word8]` and on windows with `[Word16]`, where
both lists are represented as a compact `ShortByteString`.

E.g. a cross-platform function might look like this:

```hs
module MyModule where

import System.OsPath.Types
import System.OsString.Internal.Types
#if defined(mingw32_HOST_OS)
-- word 16 based windows API
import qualified System.OsPath.Data.ByteString.Short.Word16
       as SBS
import qualified System.OsPath.Windows as PFP
#else
-- word 8 based posix API
import qualified System.OsPath.Data.ByteString.Short as SBS
import qualified System.OsPath.Posix as PFP
#endif

crossPlatformFunction :: OsPath -> IO ()
#if defined(mingw32_HOST_OS)
crossPlatformFunction (OsString pfp@(WindowsString ba)) = do
    -- use filepath functions for windows specific
    -- operating system strings
    let ext = PFP.takeExtension pfp
    -- operate directly on the underlying bytestring
    -- (which is a wide character bytestring, so uses Word16)
    let foo = SBS.takeWhile
    ...
#else
crossPlatformFunction (OsString pfp@(PosixString ba)) = do
    -- use filepath functions for posix specific
    -- operating system strings
    let ext = PFP.takeExtension pfp
    -- operate directly on the underlying bytestring
    -- (which is just Word8 bytestring)
    let foo = SBS.takeWhile
    ...
#endif
```

## History of the proposal

1. first wiki proposal: [https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/abstract-file-path](https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/abstract-file-path)
2. Revival attempts
    - [https://discourse.haskell.org/t/reviving-the-abstract-filepath-proposal-afpp-in-user-space/2344](https://discourse.haskell.org/t/reviving-the-abstract-filepath-proposal-afpp-in-user-space/2344)
    - [https://mail.haskell.org/pipermail/libraries/2021-August/031427.html](https://mail.haskell.org/pipermail/libraries/2021-August/031427.html)
    - [https://groups.google.com/g/haskell-core-libraries/c/WzapcSvvfQM/m/oDhGbf9wCwAJ?pli=1](https://groups.google.com/g/haskell-core-libraries/c/WzapcSvvfQM/m/oDhGbf9wCwAJ?pli=1)
3. PRs:
    - [https://gitlab.haskell.org/haskell/filepath/-/merge_requests/103](https://gitlab.haskell.org/haskell/filepath/-/merge_requests/103)
    - [https://github.com/haskell/bytestring/pull/471](https://github.com/haskell/bytestring/pull/471)
    - [https://github.com/haskell/win32/pull/198](https://github.com/haskell/win32/pull/198)
    - [https://github.com/haskell/unix/pull/202](https://github.com/haskell/unix/pull/202)
    - [https://github.com/haskell/directory/pull/136](https://github.com/haskell/directory/pull/136)
4. Haskell Foundation thread: [https://github.com/haskellfoundation/tech-proposals/issues/35](https://github.com/haskellfoundation/tech-proposals/issues/35)
5. Reddit discussion: [https://www.reddit.com/r/haskell/comments/vivjdo/abstract_filepath_coming_soon/](https://www.reddit.com/r/haskell/comments/vivjdo/abstract_filepath_coming_soon/)

## Contributors

1. Author, filepath maintainer and proposal champion: Julian Ospald (me)
2. Bodigrim providing help and support as CLC chair, giving reviews as bytestring maintainer and providing help with questions about encoding
3. `bytestring` maintainers providing review for the `ShortByteString` PR
4. `unix` maintainers providing PR review
5. Tamar Christina (`Win32` maintainer) providing PR review and further guidance for the `file-io` library
6. `directory` maintainer providing PR review
7. Ericson2314 via various dicussions
8. Koz Ross helping with encoding questions
9. GHC team helping with getting this into 9.6
10. HF encouraging me
11. reddit community giving loads of opinions on function names ;)
12. various people on IRC discussing alternatives like PEP-383/UTF-8b/WTF-8

## Patch load

* filepath: 11126 insertions(+), 3062 deletions(-)
* bytestring: 1795 insertions(+), 145 deletions(-)
* Win32: 2668 insertions(+), 986 deletions(-)
* unix: 8705 insertions(+), 3 deletions(-)
* directory: 2959 insertions(+), 939 deletions(-)
* file-io: 296 insertions(+)

Total: 27549 insertions(+), 5135 deletions(-)

## How to help

* create issues for your favorite libraries to support `OsPath` linking to this blog
* create PRs for existing issues:
    - [https://github.com/haskell/process/issues/252](https://github.com/haskell/process/issues/252)

## FAQ

### Why is there no IsString instance (OverloadedStrings)?

`IsString` has a broken API: [https://github.com/haskell/bytestring/issues/140](https://github.com/haskell/bytestring/issues/140)

It can't express failure. Conversion to `OsPath` can fail. Use the provided QuasiQuoters instead.

### Why is this not in base?

Nothing is stopping this from eventually getting into base. But the barrier of doing so is much higher.
It may happen eventually.

### When will 'FilePath' be dropped?

Probably never. It would break loads of code. We don't want to do that, for now.

### Yet another String type?

Right... I suggest using python if you don't like types ;)
