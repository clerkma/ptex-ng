#!/bin/bash

set -e

if [[ -d blade-build ]]; then
    rm -rfv blade-build
fi

git clone -b v2 https://github.com/blade-build/blade-build
cd blade-build
git reset --hard 3295898226ee43f01f3b9c7081d6f24d7f4556b7
