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
 
 
Lightweight environment for work and fun. Some neat properties:

- Reproducible
- Modular
- Minimalistic
- Manages your dotfiles

Environments are managed by [nix](https://nixos.org/nix). This setup is NixOS
focused.

The documentation is WIP. Some or all commands may not work.

Usage
=======

## Get nix first
The simpliest approach is to [install nix](https://nixos.org/download.html) in
your current system with:

``` sh
curl -L https://nixos.org/nix/install | sh
```

## Get flakes
Currently flakes are experimental and not shipped with `nix`. That's why you
need to open a shell with `flakes` installed.

``` sh
nix-shell -p nixFlakes
```

More information about flake installation:
https://nixos.wiki/wiki/Flakes#Installing_nix_flakes

## Using templates

```
nix flake new -t "github:abcdw/rde#python.poetry" ~/work/python-poetry-project 
```

## Get and configure rde

Create a directory with configuration:
```
nix flake new -t "github:abcdw/rde/configs" configs 
cd configs
git init .
```

Edit `hosts/<your-hostname>`.

Install
```
make switch
```

## Layout
- `files/`
- `src/` source code for different features
  - `hosts/`
  - `profiles/`
  - `devices/`
  - `modules/`

Additional info
=======

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
