# paragraphs with elm

This is an implementation of random paragraph generator using elm


it'll be for a static webpage, i hope


## Installation

you need [ELM 0.19.1](https://guide.elm-lang.org/install/elm)

## Dev

also

```shell
npm install
```

tests run with

```shell
npx elm-test
npx elm-format . --validate
```

## Compiling

The following will create a main.js file which is picked up by the index.html file in the root of the project

```shell
npx elm make src/Main.elm --output=main.js
```