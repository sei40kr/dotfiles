#!/usr/bin/env nix-shell
#!nix-shell -i bash -p curl jq xxd
# shellcheck shell=bash
# Bump sources.json to the latest kotlin-lsp release.
set -euo pipefail

sources=$(dirname "$0")/sources.json
tag=$(curl -fsSL https://api.github.com/repos/Kotlin/kotlin-lsp/releases/latest | jq -r .tag_name)
new=${tag#kotlin-lsp/v}
old=$(jq -r .version "$sources")
if [[ $new == "$old" ]]; then
  echo "kotlin-lsp is up to date ($old)"
  exit 0
fi

base=https://download.jetbrains.com/language-server/kotlin-server/$new/kotlin-server-$new
entry() {
  local url=$1
  jq -n --arg url "$url" --arg hash "sha256-$(curl -fsSL "$url.sha256" | cut -d' ' -f1 | xxd -r -p | base64)" \
    '{$url, $hash}'
}

jq -n --arg version "$new" \
  --argjson aarch64 "$(entry "$base-aarch64.sit")" \
  --argjson x86_64 "$(entry "$base.sit")" \
  '{$version, "aarch64-darwin": $aarch64, "x86_64-darwin": $x86_64}' >"$sources"
echo "kotlin-lsp: $old -> $new"
