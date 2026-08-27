{
  buildEnv,
  bashInteractive,
  coreutils-full,
  findutils,
  diffutils,
  gnugrep,
  gnused,
  gawk,
  git-pages-cli,
  gnutar,
  gzip,
  bzip2,
  xz,
  zstd,
  zip,
  unzip,
  which,
  curl,
  wget,
  git,
  jq,
  openssh,
  nodejs_24,
  nix,
  sqlite,
}:

# Merge every tool into a single /bin. nixpkgs binaries reference their own
# store paths for libexec/libraries, so linking just /bin is enough.
buildEnv {
  name = "forgejo-runner-tools";

  paths = [
    bashInteractive
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
