{ pkgs, ... }:

let
  gt = "${pkgs.graphite-cli}/bin/gt";
in
{
  programs.direnv.enableZshIntegration = true;
  home.shell.enableZshIntegration = true;


  programs.zsh = {
    enable = true;
    history.save = 1000000000;

    autosuggestion.enable = true;
    shellAliases = {
      giss = "npm i && ${gt} sync && ${gt} submit";
      gss = "${gt} sync && ${gt} submit";
      gmss = "${gt} modify -a && ${gt} sync && ${gt} submit";
    };

    initContent = ''
      export NVM_DIR="$HOME/.nvm"
      [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
      [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
    '';
  };
}
