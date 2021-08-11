{
  programs.newsboat = {
    enable = true;
    urls = [
      {
        title = "Lobsters";
        url = "https://lobste.rs/rss";
        tags = [ "tech" ];
      }
      {
        title = "Drew DeVault's blog";
        url = "https://drewdevault.com/blog/index.xml";
        tags = [ "tech" ];
      }
      {
        title = "Andrea Della Corte";
        url = "https://www.dellacorte.me/feed/newsletters.xml";
        tags = [ "personal" ];
      }
      {
        title = "Hundred Rabbits";
        url = "https://100r.co/links/rss.xml";
        tags = [ "personal" ];
      }
    ];

    extraConfig = builtins.readFile ./config;
  };
}
