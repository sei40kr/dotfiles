#!/usr/bin/env nix
#! nix shell nixpkgs#bash nixpkgs#coreutils nixpkgs#curl nixpkgs#gnugrep nixpkgs#gnused nixpkgs#jq --command bash
# Pins default.nix to the newest DMG in Rex.app's preview appcast.
set -euo pipefail
cd "$(dirname "$0")"

base=https://blob.superlogical.com/public/rex-app/preview
dmg=$(curl -fsSL "$base/appcast.xml" | grep -o 'Rex-[0-9.]*-[0-9]*\.dmg' | sort -uV | tail -n1)
[[ $dmg =~ ^Rex-(.+)-([0-9]+)\.dmg$ ]]
version=${BASH_REMATCH[1]}
build=${BASH_REMATCH[2]}
hash=$(nix store prefetch-file --json "$base/$dmg" | jq -r .hash)

sed -i \
  -e "s|^  version = \"[0-9][^\"]*\";|  version = \"$version\";|" \
  -e "s|^  build = \"[0-9]*\";|  build = \"$build\";|" \
  -e "s|^    hash = \"[^\"]*\";|    hash = \"$hash\";|" \
  default.nix
echo "Rex.app: $version ($build)"
