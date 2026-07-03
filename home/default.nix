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
          };
        }
      )

      ./modules/claude-code.nix
      ./modules/cli.nix
      ./modules/direnv.nix
      ./modules/emacs
      ./modules/hm.nix
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
          };
        }
      )

      ./modules/claude-code.nix
      ./modules/cli.nix
      ./modules/direnv.nix
      ./modules/emacs
      ./modules/hm.nix
      ./modules/vcs.nix
      ./modules/nix.nix
      ./modules/vscodium.nix
      ./modules/zsh.nix
    ];
  };
}
