{ pkgs, pname, ... }:

let
  inherit (pkgs) mkShell;
in
mkShell {
  nativeBuildInputs = with pkgs; [
    "${pname}RustToolchain"
    rust-analyzer
  ];
}
