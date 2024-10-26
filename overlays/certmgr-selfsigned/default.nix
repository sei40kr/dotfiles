_self: super: {
  certmgr-selfsigned = super.certmgr-selfsigned.overrideAttrs (
    { patches, ... }:
    {
      patches = [
        (super.fetchpatch {
          # https://github.com/cloudflare/certmgr/pull/51
          name = "cloudflare-certmgr-pull-51.patch";
          url = "https://github.com/cloudflare/certmgr/compare/232e0adf8379db28ab74c46e0dd3eddb3cd8f2ea...55c595a4a2dc871726b3c8337469daf5597718a3.patch";
          hash = "sha256-0TEvUWH+ybtLiNxJAcpq7mThQzU6iQ1ykYVcikAyhVI=";
        })
      ];
    }
  );
}
