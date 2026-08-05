{ ... }:

{
  default =
    final: prev:
    prev.lib.recursiveUpdate (import ../pkgs { pkgs = final; }) {
      emacsPackagesFor =
        emacs:
        (prev.emacsPackagesFor emacs).overrideScope (
          efinal: _eprev: {
            agent-shell = final.callPackage ../pkgs/emacs-agent-shell {
              emacsPackages = efinal;
            };
            shell-maker = final.callPackage ../pkgs/emacs-shell-maker {
              emacsPackages = efinal;
            };
            tabspaces = final.callPackage ../pkgs/emacs-tabspaces {
              emacsPackages = efinal;
            };
          }
        );
    };
}
