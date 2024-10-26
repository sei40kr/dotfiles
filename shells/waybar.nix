{
  gnused,
  material-design-icons,
  mkShell,
  waybar,
  writeShellScriptBin,
}:

let
  waybar_dev = writeShellScriptBin "waybar" ''
    export XDG_DATA_DIRS=${material-design-icons}/share:''${XDG_DATA_DIRS:+:''${XDG_DATA_DIRS}}

    style=$(mktemp)
    ${gnused}/bin/sed -e 's/@sidePadding@/16/g' \
                         $PWD/config/waybar/style.css >$style

    ${waybar}/bin/waybar -c $PWD/config/waybar/config.json -s $style "$@"

    rm -f $style
  '';
in
mkShell { buildInputs = [ waybar_dev ]; }
