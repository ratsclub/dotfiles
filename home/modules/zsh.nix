 { pkgs, ... } : 
 
 let gt = "${pkgs.graphite-cli}/bin/gt";
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
      gms = "${gt} modify -a && ${gt} sync";
    };
  };
}
