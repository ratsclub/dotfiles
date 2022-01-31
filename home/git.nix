{ pkgs, ... }:

{
  programs.git = {
    enable = true;
    package = pkgs.gitFull;

    userName = "Victor Freire";
    userEmail = "victor@freire.dev.br";

    aliases = {
      ca = "commit --amend";
      cm = "commit -m";
      co = "checkout";
      df = "diff";
      ri = "rebase --interactive --autosquash";
      st = "status --short --branch";
    };

    delta = {
      enable = true;
      options = {
        features = "side-by-side line-numbers decorations";
        delta = {
          navigate = true;
        };
        line-numbers = {
          line-numbers-minus-style = 124;
          line-numbers-plus-style = 28;
        };
      };
    };
  };
}
