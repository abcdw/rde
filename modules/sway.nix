{ config, lib, pkgs, ... }:

{
  wayland.windowManager.sway = {
    enable = true;
    config = {
      terminal = "alacritty";
      input = {
          "type:keyboard" = {
            xkb_layout = "us,ru";
            xkb_variant = "dvorak,";
            xkb_options = "ctrl:nocaps,grp:win_space_toggle,grp:rctrl_switch";
          };
          "type:touchpad" = {
            accel_profile = "adaptive";
            click_method = "clickfinger";
            dwt = "enabled";
            natural_scroll = "enabled";
            pointer_accel = "0.5";
            tap = "enabled";
          };
      };
      output = {
        eDP-1 = {scale = "2";};
      };
    };
  };
}
