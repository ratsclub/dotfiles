{ pkgs, ... }:

pkgs.buildGoModule (finalAttrs: {
  pname = "reasonix";
  version = "1.18.0";

  src = pkgs.fetchFromGitHub {
    owner = "esengine";
    repo = "DeepSeek-Reasonix";
    rev = "v${finalAttrs.version}";
    hash = "sha256-DlWO5/YJO5QIxKYqcqXiiwKy83V8knZ0fzGgY2zoJzw=";
  };

  vendorHash = "sha256-Byt7/DbSHZ+PJ8evWARRQHds/kyuydTyYH98pFwAxNY=";

  subPackages = [ "cmd/reasonix" ];

  env.CGO_ENABLED = 0;

  ldflags = [
    "-s"
    "-w"
    "-X main.version=v${finalAttrs.version}"
  ];

  passthru.updateScript = pkgs.nix-update-script {
    extraArgs = [ "--flake" ];
  };

  meta = with pkgs.lib; {
    description = "DeepSeek-native AI coding agent for your terminal";
    homepage = "https://github.com/esengine/DeepSeek-Reasonix";
    license = licenses.mit;
    mainProgram = "reasonix";
    platforms = platforms.unix;
  };
})
