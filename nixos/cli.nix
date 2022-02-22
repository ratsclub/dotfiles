{ pkgs, ... }:

{
  environment.systemPackages = pkgs.cli-tools;
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
