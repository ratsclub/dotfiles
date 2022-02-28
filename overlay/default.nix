final: prev: {
  hut = prev.buildGoModule rec {
    pname = "hut";
    version = "b12d9554fbd9371fc59d00075b1f7159ebb9237f";

    src = builtins.fetchGit {
      url = "https://git.sr.ht/~emersion/hut";
      ref = "master";
      rev = version;
    };

    nativeBuildInputs = with prev; [ installShellFiles scdoc ];

    vendorSha256 = "sha256-zdQvk0M1a+Y90pnhqIpKxLJnlVJqMoSycewTep2Oux4=";

    postBuild = ''
      scdoc < doc/hut.1.scd > hut.1
      installManPage hut.1
    '';
  };

  cli-tools = with final; [
    aria2
    bat
    convco
    entr
    exa
    fd
    fzf
    htop
    hut
    jq
    nixpkgs-fmt
    ripgrep
    wget
  ];
}
