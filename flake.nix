{
  description = "My personal dotfiles";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/331dfc770cef774d07542c284b9a65cd6b0bd98b";
    home-manager.url = "github:nix-community/home-manager";
  };

  outputs = { self, nixpkgs, home-manager }@inputs:
    let
      system = "x86_64-linux";
      username = "victor";
      homeDirectory = "/home/${username}";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
    in
    {
      homeConfigurations.victor = home-manager.lib.homeManagerConfiguration rec {
        inherit system username homeDirectory pkgs;
        configuration = ./home;
        extraSpecialArgs = {
          inherit inputs system username homeDirectory pkgs;
        };
      };
    };
}
