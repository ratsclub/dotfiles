{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    aria2
    bat
    exa
    fd
    htop
    jq
    ripgrep
    wget
  ];

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
