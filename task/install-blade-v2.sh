#!/bin/bash

set -e

if [[ -d blade-build ]]; then
    git -C blade-build pull
else
    git clone https://github.com/blade-build/blade-build
fi

cd blade-build
git switch v2
git checkout -f 3295898226ee43f01f3b9c7081d6f24d7f4556b7
