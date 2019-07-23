# AbstractBoard in Elm

## Introduction

This project revisited an old idea to make a web tool that could be used for testing different abstract strategy games. This time it's done in Elm.

## Installation

### Requirements

* Elm for compiling
* Web server (Apache, etc.) to deliver the Html, CSS, and JS

1. Clone the repository.
2. Run `elm make src/Main.elm --output=main.js`
3. Put the `main.js` and `elm.html` in a directory that server will serve.
4. Rename `elm.html` to `index.html`.
