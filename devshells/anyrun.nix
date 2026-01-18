{
  pkgs,
  perSystem,
  inputs,
  ...
}:

let
  hmConfig = inputs.home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    modules = [
      {
        _module.args = {
          inherit perSystem;
        };
      }
      ../modules/home/anyrun
      {
        home = {
          username = "dummy";
          homeDirectory = "/tmp/dummy";
          stateVersion = "25.11";
        };

        modules.desktop.apps.anyrun.enable = true;
      }
    ];
  };

  inherit (hmConfig.config.modules.desktop.apps.anyrun) package;
  generatedConfigs = "${hmConfig.activationPackage}/home-files/.config/anyrun";

  anyrun-dev = pkgs.writeShellScriptBin "anyrun" ''
    if [ ! -d "$PWD/modules/home/anyrun" ]; then
      echo "Error: modules/home/anyrun directory not found in $PWD"
      echo "Please run this command from the dotfiles root directory"
      exit 1
    fi

    export ANYRUN_CONFIG_DIR
    rm -rf $ANYRUN_CONFIG_DIR/*
    cp ${generatedConfigs}/*.ron $ANYRUN_CONFIG_DIR/
    ln -sf "$PWD/modules/home/anyrun/style.css" "$ANYRUN_CONFIG_DIR/style.css"

    echo "Config synced to $ANYRUN_CONFIG_DIR"

    ${package}/bin/anyrun -c $ANYRUN_CONFIG_DIR "$@"
  '';
in
pkgs.mkShell {
  buildInputs = [ anyrun-dev ];

  shellHook = ''
    export ANYRUN_CONFIG_DIR="$(mktemp -d)"
    mkdir -p $ANYRUN_CONFIG_DIR

    # Cleanup temp directory when shell exits
    trap "rm -rf $ANYRUN_CONFIG_DIR" EXIT

    echo "Anyrun development shell"
    echo "========================"
    echo "Config directory: $ANYRUN_CONFIG_DIR"
    echo ""
    echo "Run 'anyrun' to start with current config."
    echo "Config files are synced on each launch."
    echo "style.css is symlinked, so changes are reflected immediately."
  '';
}
