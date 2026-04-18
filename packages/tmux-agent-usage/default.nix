{ pkgs }:

let
  src = pkgs.rustPlatform.buildRustPackage rec {
    pname = "agent-usage";
    version = "0.1.4";

    src = pkgs.fetchFromGitHub {
      owner = "raine";
      repo = "tmux-agent-usage";
      rev = "v${version}";
      hash = "sha256-iNXAx+hPjtNoUrAPFQZmZfFbXUgsGbqXsqHIDV4I5Ak=";
    };

    cargoLock.lockFile = "${src}/Cargo.lock";

    meta = with pkgs.lib; {
      description = "Display AI agent rate limit usage in your tmux status bar";
      homepage = "https://github.com/raine/tmux-agent-usage";
      license = licenses.mit;
      mainProgram = "agent-usage";
      platforms = platforms.unix;
    };
  };
in
pkgs.tmuxPlugins.mkTmuxPlugin {
  pluginName = "tmux-agent-usage";
  inherit (src) version src meta;
  rtpFilePath = "tmux-agent-usage.tmux";

  patches = [ ./skip-auto-install.patch ];

  nativeBuildInputs = [ pkgs.makeWrapper ];

  preInstall = ''
    shopt -s extglob dotglob
    rm -rf !(plugin)
    mv plugin/* .
    rm -rf plugin
  '';

  postInstall = ''
    wrapProgram $target/bin/status.sh \
      --prefix PATH : ${pkgs.lib.makeBinPath [ src ]}
  '';
}
