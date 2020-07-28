My current setup
=======

| Tool type            | Tool                                                                   |
| ---:                 | :---                                                                   |
| Operating system:    | [NixOS](https://nixos.org/)                                            |
| Window manager:      | [i3-gaps](https://github.com/Airblader/i3)/[sway](https://swaywm.org/) |
| Editor/secondary OS: | [emacs](https://www.gnu.org/s/emacs/)                                  |
| Terminal:            | [alacritty](https://github.com/jwilm/alacritty)                        |
| Shell:               | [zsh](https://wiki.archlinux.org/index.php/zsh)                        |
| Layouts:             | us,ru ([dvorak](http://www.dvzine.org/zine/01-toc.html),)              |
 
## Hardware
My current hardware setup:
- Ixy - ThinkPad [X1 Yoga](./etc/nixos/configuration.ixy.nix) 4th generation
- Xenia - evga sr-2 + 2x xeon x5650
- [atreus keyboard](https://atreus.technomancy.us/)

My previous harware:
- ThinkPad [X1 Carbon](./x1carbon5.org) 5th generation

Also, some legacy configs (vimrc/awesome/etc) can be found in
[old-configs directory](./stale/old-configs).

## Screenshots

### bspwm
![2020-04-04-12:01:29](https://user-images.githubusercontent.com/1218615/78423008-06bfcc80-766c-11ea-8a79-ec63f1237126.png)

### i3wm
![2018-05-15-101737_1600x900_scrot](https://user-images.githubusercontent.com/1218615/40052255-27201c3c-5846-11e8-97a5-e308b61fddc2.png)

### awesomewm
![2018-04-03-194232_1600x900_scrot](https://user-images.githubusercontent.com/1218615/38268733-87d842d2-3787-11e8-8379-e7bc6fa4be2c.png)

Usage
=======

Packages are managed by [nix](https://nixos.org/nix). This setup is
NixOS focused, but nix works on GNU/Linux and mac.

Get:
```
cd ~
git clone https://github.com/abcdw/configs.git
```

Install
```
cd configs
nix-shell # or direnv allow
make dotfiles-install
```

## Layout
- files
- `modules/` description
  - `hosts/`
  
## Flakes
## Modules
## Overlays
