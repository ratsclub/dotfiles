{ pkgs, ... }:

pkgs.buildGoModule (finalAttrs: {
  pname = "reasonix";
  version = "1.9.1";

  src = pkgs.fetchFromGitHub {
    owner = "esengine";
    repo = "DeepSeek-Reasonix";
    rev = "v${finalAttrs.version}";
    hash = "sha256-SeNIu4X30Gn98UXJHIRwYc/6Fr76vx4q8w3wTUT95z8=";
  };

  vendorHash = "sha256-mbHQDoSEj+56kqrPrinuQY3XEw4oBHoOrSO6iW62R7g=";

  subPackages = [ "cmd/reasonix" ];

  env.CGO_ENABLED = 0;

  ldflags = [
    "-s"
    "-w"
    "-X main.version=v${finalAttrs.version}"
  ];

  meta = with pkgs.lib; {
    description = "DeepSeek-native AI coding agent for your terminal";
    homepage = "https://github.com/esengine/DeepSeek-Reasonix";
    license = licenses.mit;
    mainProgram = "reasonix";
    platforms = platforms.unix;
  };
})
