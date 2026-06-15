{ pkgs, ... }:

pkgs.buildGoModule (finalAttrs: {
  pname = "reasonix";
  version = "1.7.0";

  src = pkgs.fetchFromGitHub {
    owner = "esengine";
    repo = "DeepSeek-Reasonix";
    rev = "v${finalAttrs.version}";
    hash = "sha256-1LqDdEX9loA18zdinKRba1DJo6eG8LICnfAwgm4Z7io=";
  };

  vendorHash = "sha256-UML4t+WFagt2yHEvr8ql3/B4pcVJncZc5OdycdY0WCo=";

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
