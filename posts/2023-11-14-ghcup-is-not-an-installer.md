---
title: GHCup is not an installer
subtitle: Or how to use ghcup-metadata channels
author: Julian Ospald
tags: haskell, GHCup, installer
---

## Misunderstandings

Over the past few years, there have been recurring questions or misunderstandings about GHCup.
E.g.:

* GHCup only installs bindists from upstream (e.g. GHC/Cabal/HLS CI)
* GHCup never applies patches to tools it distributes

Both those assumptions do not apply, because GHCup is primarily a **distribution channel**,
not just an installer. The distribution channel is basically the
[`ghcup-0.0.8.yaml` metadata file](https://github.com/haskell/ghcup-metadata/blob/develop/ghcup-0.0.8.yaml).

Users who strictly only want upstream bindists (whether they're broken or not) can use a different
distribution channel and opt out of all unofficial things: [`ghcup-vanilla-0.0.8.yaml`](https://github.com/haskell/ghcup-metadata/blob/develop/ghcup-vanilla-0.0.8.yaml). More information in the [README](https://github.com/haskell/ghcup-metadata#for-end-users).

## Policies and priorities

GHCup follows similar philosophies and policies like Debian or Fedora. Most of them are
outlined [here](https://www.haskell.org/ghcup/about/#distribution-policies) in more details.
The most important points, simplified, are:

1. The end-user experience is our primary concern
2. We strive to collaborate with all maintainers of all the tools we support and maintain a good relationship
3. We may fix build system or other distribution bugs in upstream bindists
4. We may even patch source code of supported tools in very rare cases if that is required to ensure that the end-user experience does not break

As such, we prioritize the end-user over interests of upstream developers. E.g. it frequently happens
that upstream developers want a new release to be 'recommended' (that is: installed by default if you
run GHCup for the first time). However, experience shows that it's usually better to wait.

So far, we have never patched source code. In case that ever happens, it would be communicated
to both upstream and the end user via post-install messages.

## Affects on maintenance

Following these priorities, the smallest part of GHCup maintenance sometimes seems to be the codebase.
The following tasks have come up repeatedly, until
[I decreased my workload considerably to avoid a proper burnout](https://github.com/haskell/ghcup-hs/issues/848):

* building unofficial bindists for missing platforms (e.g. GHC alpine i386, armv7, FreeBSD or stack darwin aarch64)
* patching upstream bindists in case of issues discovered post-release
* tracking releases and bugs of all tools to decide which release is to be 'recommended'
* being involved in CI code and release issues of most tools
* meetings and communication with HF, GHC HQ, other tooling maintainers and especially HLS
* developing and supporting new ideas (dynamic HLS bindists, GHC nightlies, ...)
* advocating and pushing for prioritizing end user experience, e.g. [here](https://github.com/haskellfoundation/tech-proposals/issues/48)
* supporting users having installation issues via IRC, Discord, email, different issue trackers, ...

Most of this has now stalled, until GHCup gets more support (e.g. [from Obsidian](https://discourse.haskell.org/t/haskell-foundation-october-2023-update/8054#ghcup-backup-maintenance-5), which I'm excited about).

## Possible future

GHCup being a distribution channel also means that, theoretically, we might completely stop relying
on upstream bindists and roll our own. For this idea I already have prepared
[a document about Midstream bindists](https://gist.github.com/hasufell/18cb5438ce7c2ba388160588d751b32d)
that could be submitted as a HF tech proposal. As I don't have the capacity, I have not submitted it yet
and maybe I never will.

In a perfect world, we want full control over the bindists, decide on the provided configurations,
distribution support, platform support, etc.

This is what Linux distributions do too. They rarely use upstream bindists, except for bootstrapping
purposes.

## What we want from upstream is not bindists

What distributors really want from upstream is not bindists, but something else:

* feasibility to run test suites on the end-users system (that's where it matters, not just in CI)
  - and have processes and mechanisms to get feedback for failing test suites (send report via cli that gets aggregated and analyzed somewhere)
- awareness that the build system is not just a dev tool for hackers, but an interface for distributors and end users
- mindfulness about platform support (including less common ones)
- not relying on hermetically built binaries: instead make sure a manually compiled binary works on all platforms and have sufficient mechanisms to detect when it wouldn't (through bindist configure, runtime checks, test suite, ...)
- have prereleases as much as possible, including minor releases
- communicate everything that potentially impacts distributors
- longer patch/security maintenance windows for older versions

If the build system interface was stable, we could easily use `ghcup compile ghc` in our own CI,
followed by a `ghcup test ghc` (yes, that exists!) and be done. Then tell end users to utilize `ghcup test ghc`
after installation to make sure it really works with their environment (that's not a given even
with official bindists). However, the test suite is flaky and the test bindists are buggy
and not very portable, so this goal is far out.

## Conclusion

I hope that this clears up some of the confusion and expectations and that end users understand that
they have a choice by utilizing different [metadata files](https://github.com/haskell/ghcup-metadata#metadata-variants-distribution-channels).


