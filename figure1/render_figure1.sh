#!/usr/bin/env bash
# Render Figure 1 (paper and talk versions) from the Mermaid files.
# Setup: npm install -g @mermaid-js/mermaid-cli @mermaid-js/layout-elk playwright
#        npx playwright install chromium
# Text is in the .mmd files, font size and spacing in the .config.json files.
# The two numbers after each fix_arrows.js call are arrowhead length and width (px).
set -e
cd "$(dirname "$0")"
mmdc -i Figure1_DEV_Control.mmd      -c Figure1_DEV_Control.config.json      -o _raw.svg -b white
node fix_arrows.js _raw.svg Figure1_DEV_Control 13 10
mmdc -i Figure1_DEV_Control_talk.mmd -c Figure1_DEV_Control_talk.config.json -o _raw.svg -b white
node fix_arrows.js _raw.svg Figure1_DEV_Control_talk 18 14
rm -f _raw.svg
