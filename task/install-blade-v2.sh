#!/bin/bash

set -e

if [[ -d blade-build ]]; then
    rm -rfv blade-build
fi

git clone https://github.com/blade-build/blade-build
cd blade-build
git switch v2
git checkout -f 3295898226ee43f01f3b9c7081d6f24d7f4556b7
