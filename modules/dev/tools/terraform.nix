{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.dev.tools.terraform;
in
{
  options.modules.dev.tools.terraform = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      terraform
      terraform-ls
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

    modules.shell.zsh.rcInit = ''
      zinit ice as'completion' id-as'OMZP::terraform'
      zinit snippet ${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/terraform/_terraform
    '';
  };
}
