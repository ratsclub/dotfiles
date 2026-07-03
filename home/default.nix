{ inputs, self, ... }:

let
  mkHome = import ../lib/mk-home.nix { inherit inputs self; };
in
{
  mini = mkHome {
    system = "aarch64-darwin";
    modules = [
      (
        { ... }:
        {
          home = {
            username = "victor";
            homeDirectory = "/Users/victor";

            stateVersion = "24.11";
          };
        }
      )

      ./modules/claude-code.nix
      ./modules/cli.nix
      ./modules/direnv.nix
      ./modules/emacs
      ./modules/vcs.nix
      ./modules/nix.nix
      ./modules/vscodium.nix
      ./modules/zsh.nix
    ];
  };

  air = mkHome {
    system = "aarch64-darwin";
    modules = [
      (
        { ... }:
        {
          home = {
            username = "victor";
            homeDirectory = "/Users/victor";

            stateVersion = "24.11";
          };
        }
      )

      ./modules/claude-code.nix
      ./modules/cli.nix
      ./modules/direnv.nix
      ./modules/emacs
      ./modules/vcs.nix
      ./modules/nix.nix
      ./modules/vscodium.nix
      ./modules/zsh.nix
    ];
  };

  victor = mkHome {
    system = "x86_64-linux";
    modules = [
      inputs.agenix.homeManagerModules.age
    ]
    ++ [
      ./modules
      ./modules/bash.nix
      ./modules/chromium.nix
      ./modules/cli.nix
      ./modules/direnv.nix
      ./modules/emacs
      ./modules/gui.nix
      ./modules/nix.nix
      ./modules/vscodium.nix
      # ./modules/email.nix
      # ./modules/firefox.nix
      ./modules/vcs.nix
    ];
  };
}
