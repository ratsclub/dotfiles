{
  description = ".NET project template";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs@{ nixpkgs, ... }:
    inputs.utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      rec {
        # `nix build`
        packages.default = with pkgs; buildDotnetModule {
          name = "Project";
          src = ./.;
          projectFile = "Project.sln";

          # to generate a new dependency file run:
          # `nix build .#default.passthru.fetch-deps && sh ./result`
          # and then copy the printed path to this directory with the name deps.nix
          nugetDeps = ./deps.nix;

          dotnet-sdk = dotnetCorePackages.sdk_6_0;
          dotnet-runtime = dotnet-aspnetcore;
        };

        # `nix run`
        apps.default = inputs.utils.lib.mkApp { drv = packages.default; };

        # `nix develop`
        devShells.default = with pkgs; mkShell {
          buildInputs = [
            dotnet-sdk
            icu
          ];

          shellHook = ''
            export DOTNET_ROOT=${dotnet-sdk}
            export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${lib.makeLibraryPath [ icu  ]}
          '';
        };
      });
}
