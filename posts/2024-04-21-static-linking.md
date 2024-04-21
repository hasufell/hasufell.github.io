# Getting your Haskell executable statically linked without Nix

## Motivation

Following the excellent post from Tom Sydney
["Getting your Haskell executable statically linked with Nix"](https://cs-syd.eu/posts/2024-04-20-static-linking-haskell-nix),
I want to present an alternative approach.

I believe nix has questionable ergnomics and most Haskell developers don't need it, even if they want
to link their binaries statically.

## Musl and Alpine Linux

GHC/cabal don't really know how to do partial static linking, unless you employ some trickery. So we need
a system where we can link everything statically, including libc. This leads us to the Musl libc, which
has good support for static linking.

Two prominent choices for musl based Linux distributions are:

* Alpine Linux
* Void Linux musl

In this guide, we pick Alpine.

## GHCup and GHC

In order to use Alpine Linux as a build environment, we need proper toolchain support.
GHCup supports Alpine Linux as a first class citizen, so you should be able to install
GHC on Alpine. If you run into issues, [open a bug report](https://github.com/haskell/ghcup-hs/issues/new).

Note that you do not need a statically linked GHC to build a static binary. This is a misconception.

## Build environment

We need a clean build environment that is reproducible (-ish). We can use docker, which
has excellent support for Alpine Linux containers.

## Tying everything together

To tie everything together, we start an interactive shell in a docker container:

```sh
$ docker run --rm -ti alpine:3.19 sh
```

Then we install pre-requisites:

```sh
$ apk update
$ apk add curl gcc g++ git gmp-dev libc-dev libffi-dev make musl-dev ncurses-dev perl tar xz
```

We install GHCup:

```sh
$ curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh
source ~/.ghcup/env
```

Let's create a dummy app:

```sh
$ mkdir test-app
$ cd test-app
$ cabal init --non-interactive
$ cabal build --enable-executable-static
$ mkdir out/
$ cp $(cabal -v0 list-bin exe:test-app) out/
```

We use `cabal build` in combination with `cabal list-bin`, because some versions of cabal are buggy
when combining `--enable-executable-static` with `install`: [https://github.com/haskell/cabal/pull/9697](https://github.com/haskell/cabal/pull/9697)

It is also possible to pass `-ghc-options='-optl-static'` instead of `--enable-executable-static`.

Now we examine the binary:

```sh
$ apk add file
$ file out/test-app
out/test-app: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), statically linked, BuildID[sha1]=ab54deda534ac8065f5e263e84f168fb46eb8227, with debug_info, not stripped
```

That looks good.

## Linking against system libraries

If your binary depends on system C libraries, you will need to install those packages. E.g. if you link against zlib, you
need the `-dev` and sometimes `-static` packages:

```
apk add zlib-dev zlib-static
```

You can search for libraries and installed files at [https://pkgs.alpinelinux.org/packages](https://pkgs.alpinelinux.org/packages)

## Github CI

Examples of Github actions using alpine and building static release binaries can be found here:

- [https://github.com/haskell/ghcup-hs/blob/master/.github/workflows/release.yaml](https://github.com/haskell/ghcup-hs/blob/master/.github/workflows/release.yaml)
- [https://github.com/hasufell/stack2cabal/blob/master/.github/workflows/release.yaml](https://github.com/hasufell/stack2cabal/blob/master/.github/workflows/release.yaml)
- [https://github.com/stable-haskell/cabal/blob/cabal-install-v3.10.3.0/.github/workflows/release.yaml](https://github.com/stable-haskell/cabal/blob/cabal-install-v3.10.3.0/.github/workflows/release.yaml)

## Conclusion

This approach has been used in GHCup since its rewrite in Haskell. It has worked very well.

The only downside is that you rely on Alpine Linux packaging of system C libraries. If you
link to a package that is not in the Alpine repos, you will need more manual work.

In that case it might be worthwhile to check Void Linux as an alternative.

