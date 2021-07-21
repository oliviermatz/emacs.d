#!/bin/bash

opts=--bind ctrl-k:kill-line --no-extended

cd $(git rev-parse --show-toplevel)

file=$(git ls-files -co --exclude-standard | grep -E '^test\/|\/test\/' | grep -E '\.rb$' | fzf $opts)

if [ ! -z "$file" ]; then
    emacsclient -e "(find-file \"$PWD/$file\")"
fi
