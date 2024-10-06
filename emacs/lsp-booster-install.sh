#!/bin/bash

# Variables
REPO="blahgeek/emacs-lsp-booster"
LATEST_RELEASE=$(curl -s "https://api.github.com/repos/$REPO/releases/latest" | jq -r .tag_name)
ASSET_NAME="emacs-lsp-booster_${LATEST_RELEASE}_x86_64-unknown-linux-musl.zip"
DOWNLOAD_URL="https://github.com/$REPO/releases/download/$LATEST_RELEASE/$ASSET_NAME"
DESTINATION="/usr/local/bin"

# Download the latest release
curl -L -o "$ASSET_NAME" "$DOWNLOAD_URL"

# Unzip to the destination
unzip -o "$ASSET_NAME" -d "$DESTINATION"

# Clean up
rm "$ASSET_NAME"

