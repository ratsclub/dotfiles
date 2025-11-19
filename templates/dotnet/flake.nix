{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devenv.url = "github:cachix/devenv";
  };

  outputs =
    {
      self,
      nixpkgs,
      devenv,
      ...
    }@inputs:
    let
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forAllSystems =
        f:
        builtins.listToAttrs (
          map (name: {
            inherit name;
            value = f name;
          }) systems
        );
    in
    {
      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = [
              (
                { pkgs, ... }:
                let
                  inherit (pkgs) dotnetCorePackages;
                  dotnet = (
                    with dotnetCorePackages;
                    combinePackages [
                      sdk_9_0
                      sdk_10_0
                    ]
                  );
                in
                {
                  languages.dotnet = {
                    enable = true;
                    package = dotnet;
                  };
                }
              )
            ];
          };
        }
      );
    };
}
