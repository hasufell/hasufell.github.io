---
title: From conduit to streamly
subtitle: A migration guide
author: Julian Ospald
tags: haskell, conduit, streamly, streaming
---

## Motivation

At GHCup I recently put a lot of effort into [reducing the dependency footprint](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/212)
to improve build times. Since `conduit` was not a direct dependency and only used for yaml parsing and some other things, I replaced
those deps with alternatives or re-implemented them (like logging).

[yaml](https://hackage.haskell.org/package/yaml), which uses conduit under the hood, was replaced with
[HsYAML](https://hackage.haskell.org/package/HsYAML),
but to my despair... that turned out to be [10 times slower](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/270), which also caused
[issues for pandoc](https://github.com/haskell-hvr/HsYAML/issues/40).

[Conduit](https://hackage.haskell.org/package/conduit) is an excellent fully featured streaming library,
but I didn't want to go back to it by re-introducing yaml, since
GHCup previously depended on `streamly` and will likely do so in the future.
So I simply decided to migrate yaml to streamly: [https://hackage.haskell.org/package/yaml-streamly](https://hackage.haskell.org/package/yaml-streamly).

[Streamly](https://github.com/composewell/streamly) is a very general streaming library with a the strong focus on
[performance](https://github.com/composewell/streaming-benchmarks#streamly-vs-conduit)
through inlining and stream fusion optimizations. As such, it may exceed other implementations performance, but also
depends quite heavily on GHC behavior, flags, INLINE pragmas etc. It can also be used as an
[alternative for async](https://github.com/composewell/streamly/blob/master/docs/streamly-vs-async.md),
for reactive programming and much more.

So in this post, I will shortly explain conduit and streamly and provide a simple migration guide.

## Recap on conduit

There are many approaches on streaming. Conduit and streamly diverge quite heavily in terms of paradigm and API.

Conduit expresses streaming by providing a type that captures
`i`nput, `o`utput and a possible final `r`esult, all in one type (and the obligatory effect `m`):

```hs
data ConduitT i o m r
```

As such, it expresses:

* [Producers](https://hackage.haskell.org/package/conduit-1.3.4.2/docs/Data-Conduit-Combinators.html#g:1)
* [Transformers](https://hackage.haskell.org/package/conduit-1.3.4.2/docs/Data-Conduit-Combinators.html#g:10)
* [Consumers](https://hackage.haskell.org/package/conduit-1.3.4.2/docs/Data-Conduit-Combinators.html#g:6)

### Producers

These are generators from a seed value. Conduit defines it generically as such:

```hs
unfold :: Monad m => (b -> Maybe (a, b)) -> b -> ConduitT i a m ()
```

A simple unfold that lets us turn a list into a stream would be:

```hs
-- this is also provided by conduit
sourceList :: Monad m => [a] -> ConduitT i a m ()
sourceList = unfold gen
 where
  gen :: [a] -> Maybe (a, [a])
  gen (x:xs) = Just (x,xs)
  gen _      = Nothing -- stream aborts

-- our own stream of "output" chars with no final result
chars :: Monad m => ConduitT i Char m ()
chars = sourceList "abc"
```

As can be seen, the `o` in `data ConduitT i o m r` gets fixed to `Char`. A Producer can then be
be "piped" into another conduit, e.g. a transformer.

A producer focuses on the `o`utput.

### Transformer

A transformer is like `map :: (a -> b) -> [a] -> [b]`. It transforms the stream and may yield a different type.

```hs
-- provided by conduit, notice how it has only one argument
map :: Monad m => (a -> b) -> ConduitT a b m ()

-- transforms Char to Int
charToInt :: Monad m => ConduitT Char Int m ()
charToInt = map ord

-- applies the transformation to the chars, yielding a Producer
-- we'll explaing '.|' shortly
ints :: Monad m => ConduitM a Int m ()
ints = chars .| charToInt
```

Notable is also that the Functor `fmap` isn't a transformation. It would map on the final value, not the
produced values. That's why we need `Data.Conduit.List.map`. Streamly
is very different here.

A transformer maps the `i`nput to the `o`utput.

To apply a transformation, we use the `(.|)` pipe operator, which reminds us of shell pipes:

```hs
(.|) :: Monad m
     => ConduitM a b m () -- ^ producer of values 'b'
     -> ConduitM b c m r  -- ^ transformer (b -> c), or consumer
     -> ConduitM a c m r
```

It takes a little while to see what's going on. The type variables guide us.

### Consumer

A consumer works on the input stream, much like a transformer, but may also
yield a final result. E.g. If we wanted to return all the Int's we just converted
from the Char stream, we'd do:

```hs
-- provided by conduit
foldl :: Monad m => (a -> b -> a) -> a -> ConduitT b o m a

-- 'a' (the input) gets folded as a list, so the final result is '[a]'
toList :: Monad m => ConduitT a o m [a]
toList = foldl (\a b -> b:a) []

-- applying the fold on the stream of Ints
foldedInts :: Monad m => ConduitM a c m [Int]
foldedInts = ints .| toList
```

The consumer focuses on the `i`nput to produce a final `r`esult (however, consumers may also drop elements
from the stream).

As demonstrated, one has to look closely at the type parameters in `data ConduitT i o m r`
to understand a conduit.

All concepts are unified in one type. Most operations need specific combinators.

### Wrapping up conduit

Finally, we can get our Ints:

```hs
ints :: Monad m => m [Int]
ints = runConduit foldedInts
```

That's basically conduit. A conduit as such doesn't really express streams.
Instead we're dealing with stream processors (functions).

## Streamly

Streamly's approach is very different. It focuses on the simple concept of a *stream* of elements.
It has 4 main types:

* [streams](https://hackage.haskell.org/package/streamly-0.8.0/docs/Streamly-Prelude.html#g:50): `(Monad m, IsStream t) => t m a`
* [unfolds](https://hackage.haskell.org/package/streamly-0.8.0/docs/Streamly-Data-Unfold.html): `data Unfold m a b`
* [folds](https://hackage.haskell.org/package/streamly-0.8.0/docs/Streamly-Data-Fold.html): `data Fold m a b`
* [parsers](https://hackage.haskell.org/package/streamly-0.8.0/docs/Streamly-Internal-Data-Parser.html): `newtype Parser m a b`

As can be seen, this is nothing like `data ConduitT i o m r`. I also note that `IsStream t` is abstract
to allow for different types of streams like `SerialT` or `AsyncT`, which I won't go into detail about here.

We'll now figure out how these concepts translate to conduit.

### Producers

Conduits producers are basically *Unfolds*.

The simplest function to create an `Unfold` is:

```hs
unfoldr :: Applicative m => (a -> Maybe (b, a)) -> Unfold m a b
```

...which actually looks a lot like conduit:

```hs
unfold :: Monad m => (b -> Maybe (a, b)) -> b -> ConduitT i a m ()
```

The difference in streamly is that we provide the initial seed value
when we turn the Unfold into a Stream.

So, let's do the same procedure as above. We'll create a list of Chars:

```hs
-- equivalent to conduits 'sourceList', also provided by streamly
fromList :: Monad m => Unfold m [a] a
fromList = unfoldr gen
 where
  gen :: [a] -> Maybe (a, [a])
  gen (x:xs) = Just (x,xs)
  gen _      = Nothing -- stream aborts

-- provided by streamly
-- given a seed value, turn an Unfold into a stream
unfold :: (IsStream t, Monad m) => Unfold m a b -> a -> t m b

-- we turn the unfold into a stream of chars
chars :: (IsStream t, Monad m) => t m Char
chars = Streamly.Prelude.unfold fromList "abc"
```

This type `t m Char` looks a lot simpler. It's basically a glorified list with possible effects
run for every element.

### Transformers

A transformer doesn't have its own type. It's in my opinion much simpler than conduit.
Here, we can simply reuse the Prelude's `fmap`. The main difference is that we have an
input and an output stream, so:

```hs
-- transforms Char to Int
charToInt :: (IsStream t, Monad m, Functor (t m)) => t m Char -> t m Int
charToInt inputStream = fmap ord inputStream

-- applies the transformation to the chars, yielding a stream of Ints
ints :: (IsStream t, Monad m, Functor (t m)) => t m Int
ints = charToInt chars
```

This feels much more like lists! Compare with `fmap ord "abc"`. Streams can be passed around
and transformed just like lists. If you want to run effects for every item, you just use the
Monad interface:

```hs
charToInt :: (IsStream t, Monad m, Monad (t m)) => t m Char -> t m Int
charToInt inputStream = inputStream >>= pure . ord
```

However, this creates a data dependency (as we're used from Monad).
There's the more general `mapM` that can run effects
in parallel:

```hs
mapM :: (IsStream t, MonadAsync m) => (a -> m b) -> t m a -> t m b
```

Excellent. So Functor, Monad etc. follow our intuition.

### Consumers

Simple consumers in streamly terms are usually **Folds**.

E.g. if we wanted to convert our stream of Ints to an actual list of Ints we would
combine our input stream with a Fold.

Remember the Fold type `data Fold m a b`, where `a` are the values of the input stream
and `b` is the final folded value.

```hs
-- provided by streamly for creating a Fold
foldl' :: Monad m => (b -> a -> b) -> b -> Fold m a b

-- provided by streamly for executing a fold over a stream
fold :: Monad m => Fold m a b -> Stream m a -> m b

-- A Fold that turns any input stream into a list
toList :: Monad m => Fold m a [a]
toList = foldl' (\a b -> b:a) []

-- Applying the Fold to an actual stream already executes it
foldedInts :: Monad m => m [Int]
foldedInts = fold toList ints
```

### Parsers

Folds don't have a monadic interface  (yet). If we want backtracking and a monadic interface
to choose the next step depending on the current element in the stream, we can use a
[Parser](https://hackage.haskell.org/package/streamly-0.8.0/docs/Streamly-Internal-Data-Parser.html).

In conduit, we can use consumers like
[head](https://hackage.haskell.org/package/conduit-1.3.4.2/docs/Data-Conduit-Combinators.html#v:head)
and [peek](https://hackage.haskell.org/package/conduit-1.3.4.2/docs/Data-Conduit-Combinators.html#v:peek)
and utilize the Monad interface of `ConduitT` to make our decisions. Theoretically, we could do the same
in the Stream type of streamly via [uncons](https://hackage.haskell.org/package/streamly-0.8.0/docs/Streamly-Prelude.html#v:uncons), but the parser feels more idiomatic here.

I note that there is a parser-like package [conduit-parse](https://hackage.haskell.org/package/conduit-parse),
but the [yaml conduit code](https://github.com/snoyberg/yaml/blob/c4392f3855002ab0bbf9bc16e1e32034254234a7/yaml/src/Data/Yaml/Internal.hs#L286) doesn't utilize that and this blog was written while I converted yaml to streamly.

The streamly parser type is the same as a Fold: `newtype Parser m a b`.

It parses a streamed value `a` into `b`. Much of the
[API](https://hackage.haskell.org/package/streamly-0.8.0/docs/Streamly-Internal-Data-Parser.html#g:2)
resembles what you're used to of `parsec` or `attoparsec` etc.

Let's look at this conduit code (not tested to compile):

```hs
import qualified Data.Conduit.Combinators as C

chars :: Monad m => ConduitT i Char m ()
chars = sourceList "a1b2c3"

-- We parse '1' from 'a1', '2' from 'b2' and so on, no matter
-- the order the pairs appear in.
parse' :: MonadIO m => ConduitT Char o m [Int]
parse' = do
  mc <- C.head
  case mc of
    Just 'a' -> do
       mcn <- C.head
       case mcn of
         Just '1' -> (1:) <$> parse'
         Just cn  -> liftIO $ throwIO $ "Unexpected char: " ++ [cn]
         Nothing  -> pure []
    Just ... -- and so on
    Nothing -> pure []
```

To translate this to streamly, we would write:

```hs
chars :: (IsStream t, Monad m) => t m Char
chars = Streamly.Prelude.unfold fromList "a1b2c3"

-- we define a helper that acts like conduits C.head
anyChar :: MonadCatch m => Parser m Char (Maybe Char)
anyChar = (Just <$> satisfy (const True)) <|> pure Nothing

-- We parse '1' from 'a1', '2' from 'b2' and so on, no matter
-- the order the pairs appear in.
parse' :: MonadIO m => Parser m Char [Int]
parse' = do
  mc <- anyChar
  case mc of
    Just 'a' -> do
       mcn <- anyChar
       case mcn of
         Just '1' -> (1:) <$> parse'
         Just cn  -> liftIO $ throwIO $ "Unexpected char: " ++ [cn]
         Nothing  -> pure []
    Just ... -- and so on
    Nothing -> pure []
```

This looks *exactly* like the conduit code, except we replaced `head` with `anyChar`.
Although we could likely reduce it further instead of pattern matching on the chars.

Running a parser is like running a fold. We need an input stream:

```hs
parse :: MonadThrow m => Parser m a b -> SerialT m a -> m b
```

### Wrapping up streamly

Running a stream is usually done by applying a `Fold`, as we've done above.
We can also turn a stream into a list directly:

```hs
toList :: Monad m => SerialT m a -> m [a]
```

Or just evaluate the stream and discard the values:

```hs
drain :: Monad m => SerialT m a -> m ()
```

All these functions also exist as Folds, so these are just convenience wrappers.

As can be seen, streamly isn't based on stream processors like conduit.
Instead it composes stream data directly and behaves pretty much like lists.
Usually we don't need special operators. Functor, Monad etc. follow our intuition from lists.

We've also seen that there's an abstract `IsStream` class and specific streaming types like `SerialT`
(for serially processed streams), `AsyncT` (for concurrent streams) and so on. These are explained
in more detail in the streamly documentation.

## Back to yaml

So how does this translate to yaml parsing? Well, the `yaml` package uses the [libyaml C library](https://github.com/yaml/libyaml) for parsing,
which is an event driven parser. So we get a
[stream of events](https://github.com/snoyberg/yaml/blob/c4392f3855002ab0bbf9bc16e1e32034254234a7/libyaml/src/Text/Libyaml.hs#L577)
and then [turn that into a single JSON value](https://github.com/snoyberg/yaml/blob/c4392f3855002ab0bbf9bc16e1e32034254234a7/yaml/src/Data/Yaml/Internal.hs#L187)
and then let `aeson` do its magic.

Finally, for reference, here's the migration patch: [https://github.com/hasufell/streamly-yaml/commit/bfd1da498588af906cbc5d3bb519f1ccdf7ad63e](https://github.com/hasufell/streamly-yaml/commit/bfd1da498588af906cbc5d3bb519f1ccdf7ad63e)

In fact, it didn't require a rewrite at all. Simply applying the concepts from above was enough. Figuring out
that we need a Parser type etc. took a while (I tried with Fold first). Thanks to the helpful streamly developers
for providing guidance. There were some rough edges here and there, since much of the streamly API is still marked as **Internal**.

### Performance

Did it actually improve performance?

On my first attempt, I used the wrong inefficient internal
[ParserD](https://hackage.haskell.org/package/streamly-0.8.0/docs/Streamly-Internal-Data-Parser-ParserD.html#t:Parser)
type, which seemed to cause exponential allocations. After fixing that, I was still slower than conduit. Since streamly
heavily relies on GHCs inliner, this wasn't a surprise. It required [some effort](https://github.com/hasufell/streamly-yaml/commit/640596f344675cc21970a66335b5f1ca2ae4c3c9), but finally the performance was on-par with conduit
(tested informally via the `yaml2json` executable on a 100mb YAML file).

Streamly also provides [some](https://github.com/composewell/streamly/blob/master/docs/optimizing.md)
[guidance](https://github.com/composewell/streamly/blob/master/docs/building.md#compilation-options)
for optimization.

I guess since the actual parsing is done by the C code and the `event->json` conversion is really a slow
element-by-element monadic parsing transformation, there's not much space to improve performance anyway.

If you find ideas about how to improve it further, please let me know.

### Dependency footprint

Did this actually reduce dependency footprint?

Well, no. But the point was to only depend on a single streaming framework.
I also note that streamly is [planning to split up the `streamly` package](https://github.com/composewell/streamly/issues/533)
into `streamly-core` (only depends on boot packages) and separate out further
feature-packages.

## Conclusion

1. migrating conduit code to streamly is easier than I thought
2. performance optimization in streamly requires some time and effort
3. you definitely want performance regression tests with streamly to ensure new GHC versions or refactorings don't cause regressions

## What's next?

Writing a streamly yaml parser in pure Haskell?
