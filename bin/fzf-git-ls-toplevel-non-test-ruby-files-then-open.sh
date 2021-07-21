#!/bin/bash

opts=--bind ctrl-k:kill-line --no-extended

cd $(git rev-parse --show-toplevel)

file=$(git ls-files -co --exclude-standard | sed -E '/^test\/|^sorbet\/|\/test\//d' | grep -E '(\.rb|\.rake)$' | fzf $opts)

if [ ! -z "$file" ]; then
    emacsclient -e "(find-file \"$PWD/$file\")"
fi
