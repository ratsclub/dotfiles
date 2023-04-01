{ osConfig, config, pkgs, ... }:

let
  notmuch = "${pkgs.notmuch}/bin/notmuch";
in
{
  programs = {
    mbsync.enable = true;
    msmtp.enable = true;
    notmuch = {
      enable = true;
      new.tags = [ "unread" "inbox" ];
      hooks = {
        preNew = "mbsync --all";
        postNew = ''
          ${notmuch} tag +uber      -- tag:unread and from:noreply@uber.com
          ${notmuch} tag +nixos     -- tag:unread and from:discourse@discourse.nixos.org
          ${notmuch} tag +github    -- tag:unread and from:notifications@github.com
          ${notmuch} tag +sourcehut -- tag:unread and from:*@sr.ht or to:*@lists.sr.ht
        '';
      };
    };
  };

  accounts.email = {
    accounts.personal = rec {
      realName = "Victor Freire";
      address = "victor@freire.dev.br";
      userName = address;
      primary = true;

      imap.host = "imap.mailbox.org";
      smtp.host = "smtp.mailbox.org";
      passwordCommand = "";

      notmuch.enable = true;
      msmtp.enable = true;
      mbsync = {
        enable = true;
        create = "maildir";
      };

      signature = {
        text = ''
          ${realName}
        '';
        showSignature = "append";
      };
    };
  };
}
