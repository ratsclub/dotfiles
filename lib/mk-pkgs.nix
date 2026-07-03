# Instantiate nixpkgs for a given system with our common defaults.
{
  nixpkgs,
  system,
  overlays ? [ ],
  config ? {
    allowUnfree = true;
  },
}:
import nixpkgs {
  inherit system overlays config;
}
