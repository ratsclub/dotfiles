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
}
