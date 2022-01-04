{ pkgs, ... }:

let
  w3m = "${pkgs.w3m}/bin/w3m";
  mpv = "${pkgs.mpv}/bin/mpv";
in
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
      {
        url = "https://www.youtube.com/feeds/videos.xml?channel_id=UC9-y-6csu5WGm29I7JiwpnA";
        tags = [ "tech" "youtube" ];
        title = "Computerphile";
      }
    ];
    extraConfig = ''
      html-renderer      "${w3m} -dump -T text/html"
      refresh-on-startup yes
      text-width         72

      # open video on mpv
      macro v set browser "${mpv} %u" ; open-in-browser ; set browser "${w3m} %u"
    '';
  };
}
