{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.tools.terraform;
in
{
  options.modules.dev.tools.terraform = {
    enable = mkEnableOption "Terraform development tools";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      terraform
      tflint
    ];

    home.shellAliases = {
      tf = "terraform";
      tfa = "terraform apply";
      tfd = "terraform destroy";
      tff = "terraform fmt";
      tfi = "terraform init";
      tfp = "terraform plan";
      tfv = "terraform validate";
    };

    modules.ai.mcpServers.terraform = rec {
      transport = "stdio";
      package = pkgs.terraform-mcp-server;
      command = "${package}/bin/terraform-mcp-server";
      args = [ ];
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

    programs.zsh.oh-my-zsh.plugins = [ "terraform" ];
  };
}
