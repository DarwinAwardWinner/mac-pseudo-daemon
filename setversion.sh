#!/bin/bash

TARGET_VERSION="$1"
if [ -n "$TARGET_VERSION" ]; then
    echo "Updating version to $TARGET_VERSION";
    perl -i'orig_*' -lap \
         -e "s/;; Version: [0-9.]+/;; Version: $TARGET_VERSION/g;" \
         -e "s/;; Package-Requires: (.*)pseudo-daemon \"[0-9.]+\"/;; Package-Requires: \$1pseudo-daemon \"$TARGET_VERSION\"/g;" \
         *.el
    rm orig_*.el
else
    echo "Usage: $0 VERSION_NUMBER"
fi
