{ inputs, pkgs, ... }:

let
  emacsConfig = pkgs.writeTextDir "config/init.el" ''
    (load "${./init.el}")
  '';

  customEmacs = pkgs.emacs29.override {
    withGTK3 = true;
    withTreeSitter = true;
  };
in
{
  programs.emacs = {
    enable = true;
    package = customEmacs;
    extraPackages = epkgs: with epkgs; [
      consult
      corfu
      corfu-terminal
      citeproc
      direnv
      editorconfig
      eglot
      eglot-fsharp
      forge
      fsharp-mode
      magit
      magit-todos
      marginalia
      nix-mode
      orderless
      sml-mode
      switch-window
      treemacs
      treemacs-projectile
      typescript-mode
      vertico
      which-key

      # org
      org-contrib
      org-drill
      org-super-agenda
      ox-hugo

      # treesitter
      treesit-grammars.with-all-grammars
    ];
  };

  # this is needed because `programs.emacs.extraConfig` is appended to
  # `default.nix`, so some things can't run there
  home.file.".emacs.d/init.el".source = ./init.el;
}
