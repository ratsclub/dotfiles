{
  lib,
  dockerTools,
  buildEnv,
  runCommand,
  writeText,
  bashInteractive,
  coreutils-full,
  findutils,
  diffutils,
  gnugrep,
  gnused,
  gawk,
  gnutar,
  gzip,
  bzip2,
  xz,
  zstd,
  zip,
  jq,
  unzip,
  which,
  curl,
  wget,
  git,
  openssh,
  nodejs_24,
  nix,
  sudo,
  pam,
  cacert,
  iana-etc,
  sqlite,
}:

let
  # Merge every tool into a single /bin. nixpkgs binaries reference their own
  # store paths for libexec/libraries, so linking just /bin is enough.
  runnerEnv = buildEnv {
    name = "forgejo-runner-env";
    paths = [
      sudo
      bashInteractive
      coreutils-full
      findutils
      diffutils
      gnugrep
      gnused
      gawk
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
  };

  # Standard nix build users, so the default `build-users-group = nixbld` works
  # and builds drop out of root instead of running as it.
  nixbldUids = lib.range 1 32;
  nixbldPasswd = lib.concatMapStrings (
    i:
    "nixbld${toString i}:x:${toString (30000 + i)}:30000:Nix build user ${toString i}:/var/empty:/bin/false\n"
  ) nixbldUids;
  nixbldGroupMembers = lib.concatMapStringsSep "," (i: "nixbld${toString i}") nixbldUids;

  passwd = writeText "passwd" ''
    root:x:0:0:System administrator:/root:/bin/bash
    ${nixbldPasswd}runner:x:1001:1001:Forgejo runner:/home/runner:/bin/bash
    nobody:x:65534:65534:Nobody:/var/empty:/bin/false
  '';

  group = writeText "group" ''
    root:x:0:
    nixbld:x:30000:${nixbldGroupMembers}
    runner:x:1001:
    nogroup:x:65534:
  '';

  nsswitch = writeText "nsswitch.conf" ''
    passwd: files
    group: files
    hosts: files dns
  '';

  # Passwordless sudo for everyone in the image: it runs as root already, so
  # `sudo` is only there to satisfy actions that shell out to it, not to gate
  # access. Installed to /etc/sudoers (sudo's compiled sysconfdir is /etc).
  sudoers = writeText "sudoers" ''
    Defaults lecture = never
    root ALL=(ALL:ALL) NOPASSWD: ALL
    runner ALL=(ALL:ALL) NOPASSWD: ALL
  '';

  # sudo is built against PAM, so it needs a policy at /etc/pam.d/sudo even with
  # NOPASSWD (account/session stages still run). pam_permit lets every stage
  # pass; modules are referenced by absolute store path so no securedir search
  # or /lib is required in the image.
  pamSudo = writeText "pam-sudo" ''
    account  required ${pam}/lib/security/pam_permit.so
    auth     required ${pam}/lib/security/pam_permit.so
    password required ${pam}/lib/security/pam_permit.so
    session  required ${pam}/lib/security/pam_permit.so
  '';

  # Flakes for the update workflow; sandbox off because the job runs inside an
  # unprivileged container that can't set up the build sandbox's namespaces.
  nixConf = writeText "nix.conf" ''
    experimental-features = nix-command flakes
    sandbox = false
  '';

  etc = runCommand "forgejo-runner-etc" { } ''
    mkdir -p $out/etc/nix $out/etc/pam.d
    cp ${passwd} $out/etc/passwd
    cp ${group} $out/etc/group
    cp ${nsswitch} $out/etc/nsswitch.conf
    cp ${nixConf} $out/etc/nix/nix.conf
    # sudo refuses to run unless sudoers is root-owned and not group/world
    # writable; ownership is 0:0 in the image, mode must be 0440.
    cp ${sudoers} $out/etc/sudoers
    chmod 0440 $out/etc/sudoers
    cp ${pamSudo} $out/etc/pam.d/sudo
  '';
in
dockerTools.buildImageWithNixDb {
  # localhost/ prefix so podman resolves the loaded image locally instead of
  # trying docker.io/library/forgejo-runner.
  name = "localhost/forgejo-runner";
  tag = "latest";

  copyToRoot = [
    runnerEnv
    etc
    cacert # /etc/ssl/certs/ca-bundle.crt
    iana-etc # /etc/services, /etc/protocols
  ];

  extraCommands = ''
    # /usr/bin/env and general FHS compatibility.
    mkdir -p usr
    ln -s ../bin usr/bin

    # Writable scratch + home directories.
    mkdir -m 1777 -p tmp
    mkdir -p root home/runner
  '';

  config = {
    # No User set -> the container runs as root, which owns the nix store.
    Cmd = [ "/bin/bash" ];
    WorkingDir = "/root";
    Env = [
      "PATH=/usr/bin:/bin"
      "HOME=/root"
      "USER=root"
      "LANG=C.UTF-8"
      "PAGER=cat"
      "SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt"
      "GIT_SSL_CAINFO=${cacert}/etc/ssl/certs/ca-bundle.crt"
      "NODE_OPTIONS=--use-openssl-ca"
    ];
  };

  meta = {
    description = "Nix-built OCI image for the Forgejo Runner docker:// label (nix + node, runs as root, ships a `runner` user)";
    platforms = [ "x86_64-linux" ];
  };
}
