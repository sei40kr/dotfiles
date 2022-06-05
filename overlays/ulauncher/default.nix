_self: super:
{
  ulauncher = super.ulauncher.overrideAttrs (
    { patches ? [ ], preFixup ? "", ... }: {
      patches = patches ++ [
        # Fix Ulauncher/Ulauncher#1001
        (super.fetchpatch {
          url = "https://github.com/Ulauncher/Ulauncher/commit/8e2e0b001fc86d068df498a24343760e0558df07.patch";
          sha256 = "sha256-eB0u+2dWmSa5IQC/+Z1jXy6GD15JDM4BhnYz5vo0bko=";
        })
      ];

      preFixup = ''
        ${preFixup}
        makeWrapperArgs+=(
         "''${gappsWrapperArgs[@]}"
         --set WEBKIT_DISABLE_COMPOSITING_MODE 1
        )
      '';
    }
  );
}
