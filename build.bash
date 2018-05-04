#!/usr/bin/env bash
elm-package install
elm-make poll.elm --output=main.js
