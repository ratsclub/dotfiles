{ pkgs, ... }:

{
  environment.systemPackages = (import ../pkgs/cli.nix { inherit pkgs; });
  programs.git = {
    enable = true;
    package = pkgs.gitFull;
    config = {
      init = {
        defaultBranch = "master";
      };
    };
  };
}
