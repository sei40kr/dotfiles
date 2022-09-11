{ gnused, mkShell, rofi, writeShellScriptBin }:

let rofi_dev = writeShellScriptBin "rofi" ''
  export XDG_DATA_DIRS="''${XDG_DATA_DIRS:+$XDG_DATA_DIRS:}$HOME/.nix-profile/share:/etc/profiles/per-user/$USER/share:/run/current-system/sw/share"

  theme=$(mktemp)
  ${gnused}/bin/sed -e 's/@fontName@/Cantarell/g' \
                    -e 's/@fontSize@/11/g' \
                    $PWD/config/rofi/themes/mytheme.rasi >$theme

  ${rofi}/bin/rofi -config $PWD/config/rofi/rofi.rasi \
                   -theme $theme \
                   "$@"

  rm -f $theme
'';
in
mkShell {
  buildInputs = [ rofi_dev ];
}
