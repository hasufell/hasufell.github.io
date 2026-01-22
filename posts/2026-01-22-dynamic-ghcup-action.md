---
title: Dynamic GHC matrix in GitHub CI
author: Julian Ospald
tags: haskell, ghcup, github
---

## TOC

* [How to](#how-to)
* [Reacting to failures](#reacting-to-failures)
* [All put together](#all-put-together)
* [A few notes](#a-few-notes)
* [Prior Art](#prior-art)

## How to

When new GHC versions are released, I often have to go through all my library CI systems
and update the ghc version matrix in my github action. You could argue a CI generation
system like [haskell-CI](https://github.com/haskell-CI/haskell-ci) makes this somewhat simpler,
but I'm a strong opponent of generating CI configurations, because:

- it makes debugging harder (where does the line in my 800 LOC config come from?)
- it requires frequent manual updates (e.g. when new GHC versions are released)
- it requires to learn the generation script/format and keep up with updates
- the output is often awful to read, verbose and lacks commentary

Instead, you can dynamically get the latest, say, 5 major GHC versions in GHCup via:

```sh
ghcup -s GHCupURL list -r -t ghc -c available | awk '{ print $2 }' | awk -F '.' '{ print $1 "." $2}' | sort -Vu | tail -5
```

That's nice... we could just be done now, but I also want to make sure GHCup's `recommended` and `latest`
versions are always tested. That requires us to resolve the major versions (X.Y) back to the latest full version (X.Y.Z)
and also avoid duplicates due to adding `recommended` and `latest`.

Here's the full code:

```sh
# store all available ghc versions
all="$(ghcup -s GHCupURL list -r -t ghc -c available)"
# get the recommended version
rec=$(echo "${all}"         | grep recommended | awk '{ print $2 }')
# get the latest version
latest=$(echo "${all}"      | grep latest      | awk '{ print $2 }')
# get the last 5 major versions
other_major=$(echo "${all}"                    | awk '{ print $2 }' | awk -F '.' '{ print $1 "." $2}' | sort -Vu | tail -5)
# resolve the major versions back to their respective latest version (e.g. 9.6 -> 9.6.7)
other=$(for v in $other_major ; do point_releases=$(echo "$all" | awk '{ print $2 }' | grep --color=never "^$v.") ; echo "${point_releases}" | tail -n1  ; done)
# sort and deduplicate
selected=$(echo -n ${rec} ${latest} ${other} | tr " " "\n"  | sort -Vu)
```

At the time of writing, we'll get:

```sh
$ echo "$selected"
9.6.7
9.8.4
9.10.3
9.12.2
9.14.1
```

This looks fine. Now the remaining question is how to make this work with GitHub CI matrix.
We can easily utilize the `GITHUB_OUTPUT` feature like so:

```yaml
jobs:
  tool-output:
    runs-on: ubuntu-latest
    outputs:
      ghc_versions: ${{ steps.gen_output.outputs.ghc_versions }}

    steps:

    - uses: haskell/ghcup-setup@v1

    - name: Generate output
      id: gen_output
      run: |
        all="$(ghcup -s GHCupURL list -r -t ghc -c available)"
        rec=$(echo "${all}"         | grep recommended | awk '{ print $2 }')
        latest=$(echo "${all}"      | grep latest      | awk '{ print $2 }')

        other_major=$(echo "${all}" | awk '{ print $2 }' | awk -F '.' '{ print $1 "." $2}' | sort -Vu | tail -5)
        other=$(for v in $other_major ; do point_releases=$(echo "$all" | awk '{ print $2 }' | grep --color=never "^$v.") ; echo "${point_releases}" | tail -n1  ; done)

        selected=$(echo -n ${rec} ${latest} ${other} | tr " " "\n"  | sort -Vu)
        selected_json=$(echo -n $selected | jq -c -r -R 'split(" ") | [ .[] | if length > 0 then . else empty end ]')

        echo "${selected}"
        echo "${selected_json}"

        echo ghc_versions="${selected_json}" >> "$GITHUB_OUTPUT"

  build:
    runs-on: ${{ matrix.os }}
    needs: ["tool-output"]
    strategy:
      fail-fast: false
      matrix:
        os: [ubuntu-latest, ubuntu-24.04-arm, macOS-15-intel, macOS-latest, windows-latest]
        ghc: ${{ fromJSON(needs.tool-output.outputs.ghc_versions) }}
    steps:
    - uses: actions/checkout@v4

    - name: Install dependencies (Ubuntu)
      if: runner.os == 'Linux'
      run: |
        sudo apt-get -y update
        sudo apt-get -y install build-essential curl libffi-dev libgmp-dev libncurses-dev pkg-config

    - uses: haskell/ghcup-setup@v1
      with:
        ghc: ${{ matrix.ghc }}
        cabal: latest

    - name: Build
      run: |
        cabal update
        cabal test --test-show-details=direct all
```

Done, now we never have to update our CI manually, just because a new GHC version arrived.

We can also make use of [repository variables](https://docs.github.com/en/actions/how-tos/write-workflows/choose-what-workflows-do/use-variables)
to change the number of GHC versions tested on the fly without changing CI configuration, e.g.:

```yaml
    - name: Generate output
      id: gen_output
      run: |
        # ... snip ...

        other_major=$(echo "${all}" | awk '{ print $2 }' | awk -F '.' '{ print $1 "." $2}' | sort -Vu | tail -${GHC_TEST_NUM:=5})

        # ... snip ...
      env:
        GHC_TEST_NUM: ${{ vars.GHC_TEST_NUM }}
```

## Reacting to failures

So what does this mean for a regular project? Your CI will likely suddenly fail some day, because a new GHC release dropped.
But that's exactly what we want. It requires our attention.

To make this even more immediate, it's good to also have your CI run every night, like so:

```yaml
on:
  push:
    branches: [ master ]
  pull_request:
    branches: [ master ]
  schedule:
    - cron: '0 0 * * *'
```

But we might not want to constantly have CI failures when we're waiting for some of our dear hackage colleagues to update our dependencies,
until our package starts working again. So we want some sort of exclusion list. We can do this dynamically too via
[repository variables](https://docs.github.com/en/actions/how-tos/write-workflows/choose-what-workflows-do/use-variables) again.
We'll end up with something like:

```yaml
    - name: Generate output
      id: gen_output
      run: |
        all="$(ghcup -s GHCupURL list -r -t ghc -c available)"
        rec=$(echo "${all}"         | grep recommended | awk '{ print $2 }')
        latest=$(echo "${all}"      | grep latest      | awk '{ print $2 }')

        other_major=$(echo "${all}" | awk '{ print $2 }' | awk -F '.' '{ print $1 "." $2}' | sort -Vu | tail -${GHC_TEST_NUM:=5})
        other=$(for v in $other_major ; do point_releases=$(echo "$all" | awk '{ print $2 }' | grep --color=never "^$v.") ; echo "${point_releases}" | tail -n1  ; done)

        selected=$(echo -n ${rec} ${latest} ${other} | tr " " "\n"  | sort -Vu)
        ghc_exclude=( $GHC_EXCLUDE )
        selected_filtered=()
        for ghc in $selected ; do
          printf '%s\0' "${ghc_exclude[@]}" | grep --quiet --color=never -F -x -z -- $ghc || selected_filtered+=( $ghc )
        done
        unset ghc
        selected_json=$(echo -n ${selected_filtered[@]} | jq -c -r -R 'split(" ") | [ .[] | if length > 0 then . else empty end ]')

        echo "${selected}"
        echo "${selected_filtered}"
        echo "${selected_json}"

        echo ghc_versions="${selected_json}" >> "$GITHUB_OUTPUT"
      shell: bash
      env:
        GHC_TEST_NUM: ${{ vars.GHC_TEST_NUM }}
        GHC_EXCLUDE: ${{ vars.GHC_EXCLUDE }}
```

## All put together

At last, all put together, we get:

```yaml
name: Haskell CI

on:
  push:
    branches: [ master ]
  pull_request:
    branches: [ master ]
  schedule:
    - cron: '0 0 * * *'

jobs:
  tool-output:
    runs-on: ubuntu-latest
    outputs:
      ghc_versions: ${{ steps.gen_output.outputs.ghc_versions }}

    steps:

    - uses: haskell/ghcup-setup@v1

    - name: Generate output
      id: gen_output
      run: |
        all="$(ghcup -s GHCupURL list -r -t ghc -c available)"
        rec=$(echo "${all}"         | grep recommended | awk '{ print $2 }')
        latest=$(echo "${all}"      | grep latest      | awk '{ print $2 }')

        other_major=$(echo "${all}" | awk '{ print $2 }' | awk -F '.' '{ print $1 "." $2}' | sort -Vu | tail -${GHC_TEST_NUM:=5})
        other=$(for v in $other_major ; do point_releases=$(echo "$all" | awk '{ print $2 }' | grep --color=never "^$v.") ; echo "${point_releases}" | tail -n1  ; done)

        selected=$(echo -n ${rec} ${latest} ${other} | tr " " "\n"  | sort -Vu)
        ghc_exclude=( $GHC_EXCLUDE )
        selected_filtered=()
        for ghc in $selected ; do
          printf '%s\0' "${ghc_exclude[@]}" | grep --quiet --color=never -F -x -z -- $ghc || selected_filtered+=( $ghc )
        done
        unset ghc
        selected_json=$(echo -n ${selected_filtered[@]} | jq -c -r -R 'split(" ") | [ .[] | if length > 0 then . else empty end ]')

        echo "${selected}"
        echo "${selected_filtered}"
        echo "${selected_json}"

        echo ghc_versions="${selected_json}" >> "$GITHUB_OUTPUT"
      shell: bash
      env:
        GHC_TEST_NUM: ${{ vars.GHC_TEST_NUM }}
        GHC_EXCLUDE: ${{ vars.GHC_EXCLUDE }}

  build:

    runs-on: ${{ matrix.os }}
    needs: ["tool-output"]
    strategy:
      fail-fast: false
      matrix:
        os: [ubuntu-latest, ubuntu-24.04-arm, macOS-15-intel, macOS-latest, windows-latest]
        ghc: ${{ fromJSON(needs.tool-output.outputs.ghc_versions) }}
    steps:
    - uses: actions/checkout@v4

    - name: Install dependencies (Ubuntu)
      if: runner.os == 'Linux'
      run: |
        sudo apt-get -y update
        sudo apt-get -y install build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev pkg-config libtinfo6 libncurses6

    - uses: haskell/ghcup-setup@v1
      with:
        ghc: ${{ matrix.ghc }}
        cabal: latest

    - name: Build
      run: |
        cabal update
        cabal test --test-show-details=direct all
```

You can see these dynamic GHC matrices in action here:

* [workflow file](https://github.com/haskell/filepath/blob/master/.github/workflows/test.yaml)
* [CI run](https://github.com/haskell/filepath/actions/runs/21240352923)

## A few notes

You may also notice that we don't use [haskell-actions/setup](https://github.com/haskell-actions/setup) here, but the [ghcup-setup](https://github.com/haskell/ghcup-setup)
one, which is simpler, more predictable and maintained by GHCup developers.

## Prior Art

GitHub runner-images [do something similar](https://github.com/actions/runner-images/blob/5668be8c2f48215b30abe1f6cc5dd6a827ff411d/images/ubuntu/scripts/build/install-haskell.sh#L26) when preinstalling GHCup.
