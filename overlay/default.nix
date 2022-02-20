final: prev: rec {
  vscode-extensions = prev.vscode-extensions // {
    # FIXME https://nixpk.gs/pr-tracker.html?pr=160535
    ms-pyright.pyright = prev.vscode-utils.buildVscodeMarketplaceExtension {
      mktplcRef = {
        name = "pyright";
        publisher = "ms-pyright";
        version = "1.1.222";
        sha256 = "sha256-QMX/SawDEnG1xVrug8mvN7EvRrRDkJffcXBUFpQi1XE=";
      };
      meta = with final.lib; {
        description = "VS Code static type checking for Python";
        downloadPage = "https://marketplace.visualstudio.com/items?itemName=ms-pyright.pyright";
        homepage = "https://github.com/Microsoft/pyright#readme";
        changelog = "https://marketplace.visualstudio.com/items/ms-pyright.pyright/changelog";
        license = licenses.mit;
        maintainers = with maintainers; [ ratsclub ];
      };
    };
  };

  hut = prev.buildGoModule rec {
    pname = "hut";
    version = "ca4420d992f2e7653277d466dfcd1341f4c1f916";

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

  # REVIEW https://nixpk.gs/pr-tracker.html?pr=159015
  rbw = with prev; rustPlatform.buildRustPackage rec {
    pname = "rbw";
    version = "1.4.3";

    src = fetchCrate {
      inherit version;
      crateName = pname;
      sha256 = "sha256-teeGKQNf+nuUcF9BcdiTV/ycENTbcGvPZZ34FdOO31k=";
    };

    cargoSha256 = "sha256-Soquc3OuGlDsGSwNCvYOWQeraYpkzX1oJwmM03Rc3Jg=";

    nativeBuildInputs = [
      pkg-config
      makeWrapper
      installShellFiles
    ];

    buildInputs = lib.optionals stdenv.isDarwin [ Security libiconv ];

    postPatch = ''
      patchShebangs bin/git-credential-rbw
      substituteInPlace bin/git-credential-rbw \
          --replace rbw $out/bin/rbw
      patchShebangs bin/rbw-fzf
      substituteInPlace bin/rbw-fzf \
          --replace fzf ${fzf}/bin/fzf \
          --replace perl ${perl}/bin/perl
    '';

    preConfigure = ''
      export OPENSSL_INCLUDE_DIR="${openssl.dev}/include"
      export OPENSSL_LIB_DIR="${openssl.out}/lib"
    '';

    postInstall = ''
      for shell in bash zsh fish; do
        $out/bin/rbw gen-completions $shell > rbw.$shell
        installShellCompletion rbw.$shell
      done
      cp bin/git-credential-rbw $out/bin
      cp bin/rbw-fzf $out/bin
    '';

    meta = with lib; {
      description = "Unofficial command line client for Bitwarden";
      homepage = "https://crates.io/crates/rbw";
      changelog = "https://git.tozt.net/rbw/plain/CHANGELOG.md?id=${version}";
      license = licenses.mit;
      maintainers = with maintainers; [ albakham luc65r marsam ];
    };
  };
}
