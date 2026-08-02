{
  dockerTools,
  runCommand,
  writeText,
  cacert,
  iana-etc,
}:

let
  user = "nixuser";
  uid = 1101;
  gid = 1101;

  passwd = writeText "passwd" ''
    root:x:0:0:System administrator:/root:/bin/bash
    ${user}:x:${toString uid}:${toString gid}:Forgejo runner jobs:/tmp:/bin/bash
    nobody:x:65534:65534:Nobody:/var/empty:/bin/false
  '';

  group = writeText "group" ''
    root:x:0:
    ${user}:x:${toString gid}:
    nogroup:x:65534:
  '';

  nsswitch = writeText "nsswitch.conf" ''
    passwd: files
    group: files
    hosts: files dns
  '';

  nixConf = writeText "nix.conf" ''
    experimental-features = nix-command flakes
    accept-flake-config = true
  '';

  etc = runCommand "forgejo-runner-etc" { } ''
    mkdir -p $out/etc/nix $out/etc/ssl/certs

    cp ${passwd} $out/etc/passwd
    cp ${group} $out/etc/group
    cp ${nsswitch} $out/etc/nsswitch.conf
    cp ${nixConf} $out/etc/nix/nix.conf

    cp ${cacert}/etc/ssl/certs/ca-bundle.crt $out/etc/ssl/certs/ca-bundle.crt
    cp ${iana-etc}/etc/services $out/etc/services
    cp ${iana-etc}/etc/protocols $out/etc/protocols
  '';
in
dockerTools.buildImage {
  # localhost/ prefix so podman resolves the loaded image locally instead of
  # trying docker.io/library/forgejo-runner.
  name = "localhost/forgejo-runner";
  tag = "latest";

  copyToRoot = [ etc ];

  extraCommands = ''
    # /usr/bin/env and general FHS compatibility.
    mkdir -p usr
    ln -s ../bin usr/bin

    mkdir -p bin

    # Writable scratch + home directories.
    mkdir -m 1777 -p tmp
    mkdir -p root
  '';

  config = {
    User = "${toString uid}:${toString gid}";
    Cmd = [ "/bin/bash" ];
    WorkingDir = "/tmp";
    Env = [
      "PATH=/usr/bin:/bin"
      "HOME=/tmp"
      "USER=${user}"
      "LANG=C.UTF-8"
      "PAGER=cat"
      "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
      "GIT_SSL_CAINFO=/etc/ssl/certs/ca-bundle.crt"
      "NODE_OPTIONS=--use-openssl-ca"
      "NIX_BUILD_SHELL=/bin/bash"
      "NIX_REMOTE=daemon"
    ];
  };

  meta = {
    description = "Minimal OCI image for the Forgejo Runner docker:// label (toolchain and nix store come from bind mounts)";
    platforms = [ "x86_64-linux" ];
  };
}
// {
  inherit user uid gid;
}
