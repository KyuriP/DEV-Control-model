#!/usr/bin/env bash
# Re-render Figure 1 (manuscript + talk versions) from the Mermaid sources.
# Needs: npm install -g @mermaid-js/mermaid-cli @mermaid-js/layout-elk playwright && npx playwright install chromium
# Edit wording in the .mmd files; font sizes/spacing in the .config.json files;
# arrowhead size is the last two numbers on the fix_arrows.js lines (length, width in px).
set -e
cd "$(dirname "$0")"
mmdc -i Figure1_DEV_Control.mmd      -c Figure1_DEV_Control.config.json      -o _raw.svg -b white
node fix_arrows.js _raw.svg Figure1_DEV_Control 13 10
mmdc -i Figure1_DEV_Control_talk.mmd -c Figure1_DEV_Control_talk.config.json -o _raw.svg -b white
node fix_arrows.js _raw.svg Figure1_DEV_Control_talk 18 14
