{
  description = "Go Project Template";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        pname = "project-name";
        version = "0.0.1";
        pkgs = import nixpkgs {
          inherit system;
        };
        tools = with pkgs; [
          # https://github.com/golang/vscode-go/blob/master/docs/tools.md
          delve
          go-outline
          golangci-lint
          gomodifytags
          gopls
          gopkgs
          gotests
          impl
        ];
      in
      rec {
        # `nix build`
        packages."${pname}" = pkgs.buildGoModule {
          inherit pname version;
          src = ./.;
          vendorSha256 = pkgs.lib.fakeSha256;
        };
        defaultPackage = packages."${pname}";

        # `nix run`
        apps."${pname}" = utils.lib.mkApp {
          drv = packages."${pname}";
        };
        defaultApp = apps."${pname}";

        # `nix develop`
        devShell = with pkgs; mkShell {
          buildInputs = [ go ] ++ tools;
        };
      });
}
