{ curl
, dotnet-runtime
, fetchurl
, icu
, lib
, libmediainfo
, makeWrapper
, mono
, openssl
, sqlite
, stdenv
, zlib
}:

let
  os = if stdenv.isDarwin then "osx" else "linux";
  arch = {
    x86_64-linux = "x64";
    aarch64-linux = "arm64";
    x86_64-darwin = "x64";
  }."${stdenv.hostPlatform.system}" or (throw "Unsupported system: ${stdenv.hostPlatform.system}");

  hash = {
    x64-linux_hash = "sha256-ufyHzCjqvbhz7EY/tsUgCFofGKQcsFp9wMkYqFAqXnE=";
    arm64-linux_hash = "";
    x64-osx_hash = "";
  }."${arch}-${os}_hash";
in
stdenv.mkDerivation rec {
  pname = "readarr";
  version = "0.1.1.1320";

  src = fetchurl {
    url = "https://readarr.servarr.com/v1/update/develop/updatefile?version=${version}&os=${os}&runtime=netcore&arch=${arch}";
    name = "Readarr.develop.${version}.${os}-core-${arch}.tar";
    sha256 = hash;
  };

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/{bin,share/${pname}-${version}}
    cp -r * $out/share/${pname}-${version}/.

    makeWrapper "${dotnet-runtime}/bin/dotnet" $out/bin/Readarr \
      --add-flags "$out/share/${pname}-${version}/Readarr.dll" \
      --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath [
        curl sqlite libmediainfo mono openssl icu zlib ]}

    runHook postInstall
  '';

  meta = with lib; {
    description = "A Usenet/BitTorrent book downloader";
    homepage = "https://radarr.video/";
    license = licenses.gpl3Only;
    maintainers = with maintainers; [ ratsclub ];
    platforms = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" ];
  };
}
