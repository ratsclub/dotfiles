{ inputs, pkgs, ... }:

let
  customEmacs = pkgs.emacs.override {
    withGTK3 = !pkgs.stdenv.hostPlatform.isDarwin;
    withImageMagick = true;
    withTreeSitter = true;
  };
in
{
  programs.emacs = {
    enable = true;
    package = customEmacs;
    extraPackages =
      epkgs: with epkgs; [
        agent-shell
        avy
        consult
        corfu
        corfu-terminal
        direnv
        eglot
        exec-path-from-shell
        forge
        ghostel
        magit
        magit-todos
        marginalia
        multiple-cursors
        nix-mode
        orderless
        sml-mode
        switch-window
        treemacs
        treemacs-projectile
        vertico

        # org
        org-contrib
        org-roam
        treesit-grammars.with-all-grammars
      ];
  };

  home.packages = [ pkgs.claude-agent-acp ];

  services.emacs.enable = true;

  # this is needed because `programs.emacs.extraConfig` is appended to
  # `default.el`, so some things can't run there
  home.file.".emacs.d/init.el".source = ./init.el;
}
