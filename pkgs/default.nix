{ pkgs, ... }:

let
  inherit (pkgs)
    lib
    rustPlatform
    cmake
    copyDesktopItems
    curl
    perl
    pkg-config
    protobuf
    fontconfig
    freetype
    libgit2
    openssl
    sqlite
    zlib
    zstd
    alsa-lib
    libxkbcommon
    wayland
    libxcb
    stdenv
    makeFontsConf
    envsubst
    nix-update-script
    cargo-about
    versionCheckHook
    buildFHSEnv
    cargo-bundle
    git
    apple-sdk_15
    darwinMinVersionHook
    makeBinaryWrapper
    nodejs
    libGL
    libx11
    libxext
    livekit-libwebrtc
    testers
    writableTmpDirAsHomeHook
    fetchFromCodeberg

    ;

  buildRemoteServer = true;

  channel = "stable";
  executableName = "grameditor";
  # Based on vscode.fhs
  # Zed allows for users to download and use extensions
  # which often include the usage of pre-built binaries.
  # See #309662
  #
  # buildFHSEnv allows for users to use the existing Zed
  # extension tooling without significant pain.
  fhs =
    {
      gram-editor,
      additionalPkgs ? pkgs: [ ],
    }:
    buildFHSEnv {
      # also determines the name of the wrapped command
      name = executableName;

      # additional libraries which are commonly needed for extensions
      targetPkgs =
        pkgs:
        (with pkgs; [
          # ld-linux-x86-64-linux.so.2 and others
          glibc
          # required by at least https://github.com/zed-industries/package-version-server
          openssl
        ])
        ++ additionalPkgs pkgs;

      extraBwrapArgs = [
        "--bind-try /etc/nixos/ /etc/nixos/"
        "--ro-bind-try /etc/xdg/ /etc/xdg/"
      ];

      # symlink shared assets, including icons and desktop entries
      extraInstallCommands = ''
        ln -s "${gram-editor}/share" "$out/"
      '';

      runScript = "${gram-editor}/bin/${executableName}";

      passthru = {
        inherit executableName;
        inherit (gram-editor) pname version;
      };

      meta = gram-editor.meta // {
        description = ''
          Wrapped variant of ${gram-editor.pname} which launches in a FHS compatible environment.
          Should allow for easy usage of extensions without nix-specific modifications.
        '';
      };
    };

  gram-editor = rustPlatform.buildRustPackage (finalAttrs: {
    pname = "gram-editor";
    version = "1cc898fbc4db8a98d6f8217dc12d58d6054df076";

    outputs = [
      "out"
    ]
    ++ lib.optionals buildRemoteServer [
      "remote_server"
    ];

    src = fetchFromCodeberg {
      owner = "GramEditor";
      repo = "gram";
      rev = "${finalAttrs.version}";
      hash = "sha256-sszyYpMFuWPsQC7mDiVXBP/lEEZhl66N3r1YDNkfWiE=";
    };

    postPatch = ''
      # The generate-licenses script wants a specific version of cargo-about eventhough
      # newer versions work just as well.
      substituteInPlace script/generate-licenses \
        --replace-fail '$CARGO_ABOUT_VERSION' '${cargo-about.version}'
    '';

    cargoHash = "sha256-8lbneMHwxfnCJOqi8jH1zxAyEk3dTqORgdpBkzza34M=";

    nativeBuildInputs = [
      cmake
      copyDesktopItems
      curl
      perl
      pkg-config
      protobuf
      rustPlatform.bindgenHook
      cargo-about
    ]
    ++ lib.optionals stdenv.hostPlatform.isLinux [ makeBinaryWrapper ]
    ++ lib.optionals stdenv.hostPlatform.isDarwin [ cargo-bundle ];

    dontUseCmakeConfigure = true;

    buildInputs = [
      curl
      fontconfig
      freetype
      libgit2
      openssl
      sqlite
      zlib
      zstd
    ]
    ++ lib.optionals stdenv.hostPlatform.isLinux [
      alsa-lib
      libxkbcommon
      wayland
      libxcb
      # required by livekit:
      libGL
      libx11
      libxext
    ]
    ++ lib.optionals stdenv.hostPlatform.isDarwin [
      apple-sdk_15
      # ScreenCaptureKit, required by livekit, is only available on 12.3 and up:
      # https://developer.apple.com/documentation/screencapturekit
      (darwinMinVersionHook "12.3")
    ];

    cargoBuildFlags = [
      "--package=gram"
      "--package=cli"
    ]
    ++ lib.optionals buildRemoteServer [ "--package=remote_server" ];

    # Required on darwin because we don't have access to the
    # proprietary Metal shader compiler.
    buildFeatures = lib.optionals stdenv.hostPlatform.isDarwin [ "gpui/runtime_shaders" ];

    env = {
      ALLOW_MISSING_LICENSES = true;
      ZSTD_SYS_USE_PKG_CONFIG = true;
      FONTCONFIG_FILE = makeFontsConf {
        fontDirectories = [
          "${finalAttrs.src}/assets/fonts/plex-mono"
          "${finalAttrs.src}/assets/fonts/plex-sans"
        ];
      };
      # Setting this environment variable allows to disable auto-updates
      # https://zed.dev/docs/development/linux#notes-for-packaging-zed
      ZED_UPDATE_EXPLANATION = "Zed has been installed using Nix. Auto-updates have thus been disabled.";
      # Used by `zed --version`
      RELEASE_VERSION = finalAttrs.version;
      LK_CUSTOM_WEBRTC = livekit-libwebrtc;
    };

    preBuild = ''
      bash script/generate-licenses
    '';

    postFixup = lib.optionalString stdenv.hostPlatform.isLinux ''
      patchelf --add-rpath ${wayland}/lib $out/libexec/*
      wrapProgram $out/libexec/gram-editor --suffix PATH : ${lib.makeBinPath [ nodejs ]}
    '';

    nativeCheckInputs = [
      writableTmpDirAsHomeHook
    ];

    useNextest = true;

    remoteServerExecutableName = "zed-remote-server-${channel}-${finalAttrs.version}+${channel}";
    installPhase = ''
      runHook preInstall

      release_target="target/${stdenv.hostPlatform.rust.cargoShortTarget}/release"
    ''
    + lib.optionalString stdenv.hostPlatform.isDarwin ''
      # cargo-bundle expects the binary in target/release
      mv $release_target/gram target/release/gram

      pushd crates/gram

      # Note that this is GNU sed, while Zed's bundle-mac uses BSD sed
      sed -i "s/package.metadata.bundle-stable/package.metadata.bundle/" Cargo.toml
      export CARGO_BUNDLE_SKIP_BUILD=true
      app_path=$(cargo bundle --release | xargs)

      # We're not using Zed's fork of cargo-bundle, so we must manually append their plist extensions
      # Remove closing tags from Info.plist (last two lines)
      head -n -2 $app_path/Contents/Info.plist > Info.plist
      # Append extensions
      cat resources/info/*.plist >> Info.plist
      # Add closing tags
      printf "</dict>\n</plist>\n" >> Info.plist
      mv Info.plist $app_path/Contents/Info.plist

      popd

      mkdir -p $out/Applications $out/bin
      # Zed expects git next to its own binary
      ln -s ${lib.getExe git} $app_path/Contents/MacOS/git
      mv $release_target/cli $app_path/Contents/MacOS/cli
      mv $app_path $out/Applications/

      # Physical location of the CLI must be inside the app bundle as this is used
      # to determine which app to start
      ln -s $out/Applications/Gram.app/Contents/MacOS/cli $out/bin/grameditor
    ''
    + lib.optionalString stdenv.hostPlatform.isLinux ''
      install -Dm755 $release_target/gram $out/libexec/gram-editor
      install -Dm755 $release_target/cli $out/bin/grameditor

      install -Dm644 $src/crates/gram/resources/app-icon@2x.png $out/share/icons/hicolor/1024x1024@2x/apps/gram.png
      install -Dm644 $src/crates/gram/resources/app-icon.png $out/share/icons/hicolor/512x512/apps/gram.png

      # extracted from https://github.com/zed-industries/zed/blob/v0.141.2/script/bundle-linux (envsubst)
      # and https://github.com/zed-industries/zed/blob/v0.141.2/script/install.sh (final desktop file name)
      (
        export DO_STARTUP_NOTIFY="true"
        export APP_CLI="grameditor"
        export APP_ICON="gram"
        export APP_NAME="gram"
        export APP_ARGS="%U"
        mkdir -p "$out/share/applications"
        ${lib.getExe envsubst} < "crates/gram/resources/gram.desktop.in" > "$out/share/applications/dev.gram.Gram.desktop"
      )
    ''
    + lib.optionalString buildRemoteServer ''
      install -Dm755 $release_target/remote_server $remote_server/bin/${finalAttrs.remoteServerExecutableName}
    ''
    + ''
      runHook postInstall
    '';

    nativeInstallCheckInputs = [
      versionCheckHook
    ];
    versionCheckProgram = "${placeholder "out"}/bin/grameditor";
    doInstallCheck = true;

    passthru = {
      updateScript = nix-update-script {
        extraArgs = [
          "--version-regex"
          "^v(?!.*(?:-pre|0\\.999999\\.0|0\\.9999-temporary)$)(.+)$"

          # use github releases instead of git tags
          # zed sometimes moves git tags, making them unreliable
          # see: https://github.com/NixOS/nixpkgs/pull/439893#issuecomment-3250497178
          "--use-github-releases"
        ];
      };
      fhs = fhs { gram-editor = finalAttrs.finalPackage; };
      fhsWithPackages =
        f:
        fhs {
          gram-editor = finalAttrs.finalPackage;
          additionalPkgs = f;
        };
      tests = {
        remoteServerVersion = testers.testVersion {
          package = finalAttrs.finalPackage.remote_server;
          command = "${finalAttrs.remoteServerExecutableName} version";
        };
      }
      // lib.optionalAttrs stdenv.hostPlatform.isLinux {
        withGles = gram-editor.override { withGLES = true; };
      };
    };
  });
in
{
  gram = gram-editor;
}
