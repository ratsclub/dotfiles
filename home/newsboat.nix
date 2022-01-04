{ pkgs, ... }:

{
  programs.newsboat = {
    enable = true;
    urls = [
      {
        url = "https://drewdevault.com/blog/index.xml";
        tags = [ "tech" ];
        title = "Drew DeVault's blog";
      }
      {
        url = "https://go.dev/blog/feed.atom?format=xml";
        tags = [ "tech" "go" ];
        title = "The Go Programming Language Blog";
      }
    ];
    extraConfig = ''
      html-renderer      "${pkgs.w3m}/bin/w3m -dump -T text/html"
      refresh-on-startup yes
      text-width         72
    '';
  };
}
