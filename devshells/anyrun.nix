{ pkgs, perSystem, ... }:

let
  package = perSystem.anyrun.anyrun-with-all-plugins;
  anyrun-dev = pkgs.writeShellScriptBin "anyrun" ''
    # Check if we're in the dotfiles root directory
    if [ ! -d "$PWD/modules/home/anyrun" ]; then
      echo "Error: modules/home/anyrun directory not found in $PWD"
      echo "Please run this command from the dotfiles root directory"
      exit 1
    fi

    # Create temp config directory if it doesn't exist
    export ANYRUN_CONFIG_DIR
    mkdir -p "$ANYRUN_CONFIG_DIR"

    # Symlink style.css
    ln -sf "$PWD/modules/home/anyrun/style.css" "$ANYRUN_CONFIG_DIR/style.css"

    # Process config.ron (replace @package@ with anyrun path)
    ${pkgs.gnused}/bin/sed "s|@package@|${package}|g" \
      "$PWD/modules/home/anyrun/config.ron" > "$ANYRUN_CONFIG_DIR/config.ron"

    # Copy applications.ron
    cp "$PWD/modules/home/anyrun/applications.ron" "$ANYRUN_CONFIG_DIR/applications.ron"

    echo "Config synced to $ANYRUN_CONFIG_DIR"

    # Run anyrun with dev config directory
    exec ${package}/bin/anyrun -c "$ANYRUN_CONFIG_DIR" "$@"
  '';
in
pkgs.mkShell {
  buildInputs = [ anyrun-dev ];

  shellHook = ''
    export ANYRUN_CONFIG_DIR="''${ANYRUN_CONFIG_DIR:-$(mktemp -d)}"
    mkdir -p "$ANYRUN_CONFIG_DIR"

    echo "Anyrun development shell"
    echo "========================"
    echo "Config directory: $ANYRUN_CONFIG_DIR"
    echo ""
    echo "Run 'anyrun' to start with current config."
    echo "Config files are synced on each launch."
    echo "style.css is symlinked, so changes are reflected immediately."
  '';
}
