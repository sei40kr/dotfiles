{ fetchFromGitHub, lib, pkgs, stdenv, ... }:

let
  rtpPluginsDir = "/share/tmux-plugins";
  buildTmuxPluginPackage = attrs@{ pluginName, src, ... }:
    let
      rtpPluginDir = "${rtpPluginsDir}/${lib.getName pluginName}";
      rtpPluginPath = "${rtpPluginDir}/${
          builtins.replaceStrings [ "-" ] [ "_" ] pluginName
        }.tmux";
      derivation = stdenv.mkDerivation ({
        pname = "tmuxplugin-${pluginName}";
        dependencies = with pkgs; [ bash ];

        installPhase = ''
          target="$out"${lib.escapeShellArg rtpPluginsDir}
          mkdir -p "$out"${lib.escapeShellArg rtpPluginDir}
          cp -r . "$target"
        '';
      } // attrs);
    in derivation.overrideAttrs
    (oldAttrs: { rtp = "${derivation}${rtpPluginPath}"; });
in (buildTmuxPluginPackage {
  pluginName = "per-project-session";
  version = "unstable-2020-05-26";
  src = fetchFromGitHub {
    owner = "sei40kr";
    repo = "tmux-per-project-session";
    rev = "dabb8845f7df8bfd58047b3588e632d729354942";
    sha256 = "1v34mjjm6a2h8dxn5a50c9xk1zzs0pjdaxgz8g3s1z8fw57xkabl";
  };
})
