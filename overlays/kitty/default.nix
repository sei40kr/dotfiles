_self: super:
{
  kitty = super.kitty.overrideAttrs (_attrs: {
    # A workaround for NixOS/nixpkgs#177074
    doInstallCheck = false;
  });
}
