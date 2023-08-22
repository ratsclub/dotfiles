{ pkgs, ... }:

let
  inherit (pkgs) buildGoModule fetchFromGitHub;
in
{
  legit-web = buildGoModule {
    name = "legit-web";

    src = fetchFromGitHub {
      repo = "legit";
      owner = "icyphox";
      rev = "8f9e7f14ff7ef0fef4a4e828e49ee347c72b94d2";
      sha256 = "sha256-0N0uatGZWHBetmPeabbwoH7INcEB2HGFe1dAf1Swp5U=";
    };

    vendorSha256 = "sha256-R46Ab2fdUWV8gYpgKdImT2J6ovNmUwZZrYgu7udTP8U=";

    postInstall = ''
      mkdir -p $out/lib/templates
      mkdir -p $out/lib/static

      cp -r $src/templates/* $out/lib/templates
      cp -r $src/static/* $out/lib/static
    '';
  };
}
