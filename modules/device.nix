{ config, lib, pkgs, ... }:

with lib; {
  options.device = {
    type = mkOption {
      type = types.enum [ "graphical" "textual" ];
      description = "Type of device's interface";
      default = "graphical";
    };
  };
}
