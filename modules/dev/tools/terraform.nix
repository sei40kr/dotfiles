{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
  cfg = config.modules.dev.tools.terraform;
in
{
  options.modules.dev.tools.terraform = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      terraform
      tflint
    ];

    modules.shell.aliases = {
      tf = "terraform";
      tfa = "terraform apply";
      tfd = "terraform destroy";
      tff = "terraform fmt";
      tfi = "terraform init";
      tfp = "terraform plan";
      tfv = "terraform validate";
    };

    modules.ai.mcpServers = {
      terraform = rec {
        transport = "stdio";
        package = pkgs.terraform-mcp-server;
        command = "${package}/bin/terraform-mcp-server";
        args = [ ];
      };
    };

    modules.editors.lspServers.terraformls = rec {
      package = pkgs.terraform-ls;
      command = "${package}/bin/terraform-ls";
      args = [ "serve" ];
      filetypes = [
        "terraform"
        "terraform-vars"
      ];
      rootMarkers = [
        ".terraform"
        ".git"
      ];
    };

    modules.shell.zsh.rcInit = ''
      zinit ice as'completion' id-as'OMZP::terraform'
      zinit snippet ${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/terraform/_terraform
    '';
  };
}
