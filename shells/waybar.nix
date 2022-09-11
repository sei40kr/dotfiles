{ material-design-icons, mkShell, waybar, writeShellScriptBin }:

let waybar_dev = writeShellScriptBin "waybar" ''
  export XDG_DATA_DIRS=${material-design-icons}/share:''${XDG_DATA_DIRS:+:''${XDG_DATA_DIRS}}

  ${waybar}/bin/waybar -c $PWD/config/waybar/config.json \
                       -s $PWD/config/waybar/style.css \
                       "$@"
'';
in
mkShell {
  buildInputs = [ waybar_dev ];
}
