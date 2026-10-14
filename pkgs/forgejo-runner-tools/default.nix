{
  bashInteractive,
  buildEnv,
  bun,
  bzip2,
  coreutils-full,
  curl,
  diffutils,
  findutils,
  gawk,
  git-pages-cli,
  git,
  gnugrep,
  gnused,
  gnutar,
  gzip,
  jq,
  nix,
  nodejs_24,
  openssh,
  sqlite,
  unzip,
  wget,
  forgejo-cli,
  which,
  xz,
  zip,
  zstd,
}:

# Merge every tool into a single /bin. nixpkgs binaries reference their own
# store paths for libexec/libraries, so linking just /bin is enough.
buildEnv {
  name = "forgejo-runner-tools";

  paths = [
    bashInteractive
    bun
    coreutils-full
    findutils
    diffutils
    gnugrep
    gnused
    gawk
    git-pages-cli
    gnutar
    gzip
    bzip2
    xz
    zstd
    forgejo-cli
    zip
    unzip
    which
    curl
    wget
    git
    jq
    openssh
    nodejs_24
    nix
    sqlite
  ];

  pathsToLink = [ "/bin" ];

  meta = {
    description = "Toolchain bind-mounted as /bin into Forgejo runner job containers";
    platforms = [ "x86_64-linux" ];
  };
}
