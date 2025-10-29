{
  anyrun,
  gnused,
  mkShell,
  writeShellScriptBin,
}:

let
  anyrun-dev = writeShellScriptBin "anyrun" ''
    # Check if we're in the dotfiles root directory
    if [ ! -d "$PWD/config/anyrun" ]; then
      echo "Error: config/anyrun directory not found in $PWD"
      echo "Please run this command from the dotfiles root directory"
      exit 1
    fi

    # Create temp config directory if it doesn't exist
    export ANYRUN_CONFIG_DIR
    mkdir -p "$ANYRUN_CONFIG_DIR"

    # Symlink style.css
    ln -sf "$PWD/config/anyrun/style.css" "$ANYRUN_CONFIG_DIR/style.css"

    # Process config.ron (replace @package@ with anyrun path)
    ${gnused}/bin/sed "s|@package@|${anyrun}|g" \
      "$PWD/config/anyrun/config.ron" > "$ANYRUN_CONFIG_DIR/config.ron"

    # Copy applications.ron
    cp "$PWD/config/anyrun/applications.ron" "$ANYRUN_CONFIG_DIR/applications.ron"

    echo "Config synced to $ANYRUN_CONFIG_DIR"

    # Run anyrun with dev config directory
    exec ${anyrun}/bin/anyrun -c "$ANYRUN_CONFIG_DIR" "$@"
  '';
in
mkShell {
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
