#!/bin/bash

set -e

mkdir -p ~/.cache

if [[ -d ~/.cache/blade-build ]]; then
    git -C ~/.cache/blade-build pull
else
    git clone https://github.com/blade-build/blade-build ~/.cache/blade-build
fi

cd ~/.cache/blade-build
git switch v2
git checkout 3295898226ee43f01f3b9c7081d6f24d7f4556b7
./install

