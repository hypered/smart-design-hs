# A Haskell implementation of `hypered/smart-design`

[mono.hypered.design](https://mono.hypered.design/) contains a reference
implementation of a design system. This repository is a Haskell implementation,
intended to be used in prototyping.

This repository contains both a library and an executable. The library presents
is written with `blaze-html`. The executable is used to recreate components and
example pages.

A [GitHub
Action](https://github.com/hypered/smart-design-hs/blob/main/.github/workflows/deployment.yml)
is setup to generate pages and serve them at
[monohs.hypered.design](https://monohs.hypered.design/).

## The structure

The code is organized as a Cabal project, with two packages:

- `design-hs-lib`: the library itself.
- `design-hs-exe`: the executable server that provides us with a page like
  https://mono.hypered.design/development/docs/, but using the lib.

## Notes on ease of development.

To keep the compilation and test costs low at this point; I'll take the liberty to put the exe into the library package and we'll split this out as things mature. 

All the packages made available via this package are listed in `contents.nix`.

## Building the static site

```
$ nix-build -A design-hs-html
$ nix-shell -p busybox --run 'httpd -f -p 8000 -h result/html'
```

## Formatting with Brittany

```
$ scripts/brittany.sh
```

## Linting with HLint

```
$ scripts/hlint.sh
```

## Automatic GHCi reloading with ghcid

```
$ scripts/ghcid.sh
```
