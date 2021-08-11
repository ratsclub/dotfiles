{ pkgs, ... }:

{
  programs.git = {
    enable = true;

    userName = "Victor Freire";
    userEmail = "victor@freire.dev.br";
    package = pkgs.gitFull;

    delta = {
      enable = true;
      options = {
        features = "side-by-side line-numbers decorations";
        syntax-theme = "Dracula";
        plus-style = ''syntax "#003800"'';
        minus-style = ''syntax "#3f0001"'';
        decorations = {
          commit-decoration-style = "bold yellow box ul";
          file-style = "bold yellow ul";
          file-decoration-style = "none";
          hunk-header-decoration-style = "cyan box ul";
        };
        delta = {
          navigate = true;
        };
        line-numbers = {
          line-numbers-left-style = "cyan";
          line-numbers-right-style = "cyan";
          line-numbers-minus-style = 124;
          line-numbers-plus-style = 28;
        };
      };
    };
  };
}
