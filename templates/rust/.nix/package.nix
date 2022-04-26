{ pkgs, pname, version, ... }:

let
  rustPlatform = pkgs.makeRustPlatform {
    rustc = pkgs."${pname}RustToolchain";
    cargo = pkgs."${pname}RustToolchain";
  };
in rustPlatform.buildRustPackage {
  inherit pname version;

  src = pkgs.lib.cleanSource ../.;

  buildInputs = pkgs.lib.optionals pkgs.stdenv.isDarwin
    [ pkgs.darwin.apple_sdk.frameworks.CoreServices ];

  makeFlags = [ "PREFIX=$(out)" ];

  cargoSha256 = pkgs.lib.fakeSha256;
}
