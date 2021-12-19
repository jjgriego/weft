#!/usr/bin/env bash

cd "$(dirname "$0")"/build
cabal run weft-build -- "$@"
