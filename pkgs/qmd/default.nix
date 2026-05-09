{
  lib,
  stdenv,
  fetchFromGitHub,
  makeWrapper,
  nodejs,
  pnpm_10,
  pnpmConfigHook,
  fetchPnpmDeps,
  python3,
  pkg-config,
  cctools,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "qmd";
  version = "2.1.0";

  src = fetchFromGitHub {
    owner = "tobi";
    repo = "qmd";
    rev = "v${finalAttrs.version}";
    hash = "sha256-bqIVaNRTa8H5vrw3RwsD7QdtTa0xNvRuEVzlzE1hIBQ=";
  };

  nativeBuildInputs = [
    nodejs
    pnpm_10
    pnpmConfigHook
    makeWrapper
    python3
    pkg-config
  ]
  ++ lib.optionals stdenv.hostPlatform.isDarwin [ cctools ];

  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    fetcherVersion = 2;
    hash = "sha256-OT/V67S4U1/wpjXABb6lavv+5xdn8p3E5eIrDsfpqDk=";
  };

  buildPhase = ''
    runHook preBuild

    # pnpmConfigHook installs with --ignore-scripts, so native modules
    # listed in package.json's pnpm.onlyBuiltDependencies were never compiled.
    # node-gyp would otherwise try to download node headers from nodejs.org;
    # point it at the local nixpkgs node tree so the build stays offline.
    export npm_config_nodedir=${nodejs}
    pnpm --reporter=append-only rebuild better-sqlite3

    pnpm run build
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/lib/qmd
    cp -r dist node_modules package.json $out/lib/qmd/

    makeWrapper ${lib.getExe nodejs} $out/bin/qmd \
      --add-flags "$out/lib/qmd/dist/cli/qmd.js"

    runHook postInstall
  '';

  meta = {
    description = "Mini CLI search engine for markdown docs, knowledge bases, and notes — local hybrid BM25 + vector + LLM rerank";
    homepage = "https://github.com/tobi/qmd";
    license = lib.licenses.mit;
    mainProgram = "qmd";
    platforms = lib.platforms.unix;
  };
})
