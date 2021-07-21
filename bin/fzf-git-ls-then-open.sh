#!/bin/sh

opts=--bind ctrl-k:kill-line --no-extended

file=$(git ls-files -co --exclude-standard | fzf $opts)

if [ ! -z "$file" ]; then
    emacsclient -e "(find-file \"$PWD/$file\")"
fi
