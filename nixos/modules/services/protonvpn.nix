{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.services.protonvpn;

  protonvpn-gui = pkgs.protonvpn-gui.overrideAttrs ({ postInstall, ... }: {
    postInstall = ''
      ${postInstall}
      substituteInPlace $out/share/applications/protonvpn.desktop \
        --replace 'Icon=protonvpn' 'Icon=protonvpn-gui' \
        --replace 'StartupWMClass=Protonvpn' 'StartupWMClass=.protonvpn-wrapped'
    '';
  });
in
{
  options.modules.services.protonvpn = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = [ pkgs.protonvpn-cli protonvpn-gui ];

    # systemd.services.protonvpn-autoconnect = {
    #   wantedBy = [ "multi-user.target" ];
    #   description = "ProtonVPN-CLI auto-connect";
    #   wants = [ "network-online.target" ];
    #   serviceConfig = {
    #     Type = "forking";
    #     Environment =
    #       [ "PVPN_WAIT=300" "PVPN_DEBUG=1" "SUDO_USER=${config.user.name}" ];
    #     ExecStart = "${pkgs.protonvpn-cli}/bin/protonvpn c --p2p";
    #   };
    # };

    security.sudo.extraRules = [{
      users = [ config.user.name ];
      commands = [{
        command = "${pkgs.protonvpn-cli}/bin/protonvpn";
        options = [ "NOPASSWD" ];
      }];
    }];

    modules.shell.aliases = {
      protonvpn = "sudo protonvpn";
      pvpn = "sudo protonvpn";
    };
  };
}
