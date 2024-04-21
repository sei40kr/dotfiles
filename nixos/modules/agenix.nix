{ pkgs, ... }:

{
  user.packages = with pkgs; [ agenix ];
}
