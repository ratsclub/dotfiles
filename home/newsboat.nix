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
        title = "Drew DeVault's";
        url = "https://drewdevault.com/blog/index.xml";
        tags = [ "tech" ];
      }
      {
        title = "The Go Programming Language Blog";
        url = "https://go.dev/blog/feed.atom?format=xml";
        tags = [ "tech" "go" ];
      }

      # Aggregators
      {
        title = "Lobste.rs - Nix/Go";
        url = "https://lobste.rs/t/nix,go.rss";
        tags = [ "tech" "go" "nix" ];
      }
      {
        title = "Lobste.rs - Frontpage";
        url = "https://lobste.rs/rss";
        tags = [ "tech" ];
      }
      {
        title = "Hacker News - Frontpage";
        url = "https://news.ycombinator.com/rss";
        tags = [ "tech" ];
      }

      # Youtube
      {
        title = "Computerphile";
        url = "https://www.youtube.com/feeds/videos.xml?channel_id=UC9-y-6csu5WGm29I7JiwpnA";
        tags = [ "tech" "youtube" ];
      }
      {
        title = "Channel 5 with Andrew Callaghan";
        url = "https://www.youtube.com/feeds/videos.xml?channel_id=UC-AQKm7HUNMmxjdS371MSwg";
        tags = [ "entertainment" "youtube" ];
      }
      {
        title = "Luke Smith";
        url = "https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA";
        tags = [ "entertainment" "tech" "youtube" ];
      }
    ];
    extraConfig = ''
      # misc
      refresh-on-startup yes

      # display
      feed-sort-order unreadarticlecount-asc
      text-width         72

      # navigation
      bind-key j down feedlist
      bind-key k up feedlist
      bind-key j next articlelist
      bind-key k prev articlelist
      bind-key J next-feed articlelist
      bind-key K prev-feed articlelist
      bind-key j down article
      bind-key k up article

      # macros
      macro v set browser "${mpv} %u" ; open-in-browser ; set browser "${w3m} %u" -- "Open video on mpv"
    '';
  };
}