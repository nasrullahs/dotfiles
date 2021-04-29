#!/usr/bin/env bash

sudo apt-get install i3 i3blocks dmenu feh xautolock redshift gtk-redshift autokey autokey-gtk zsh git ripgrep tree xclip xsel htop ipython3 scrot colordiff emacs global imagemagick, i3lock-color-git, scrot

mv ~/.config/i3/config ~/.config/i3/config.bak
ln -s ./config/i3/config ~/.config/i3/config

mv ~/.config/i3blocks/config ~/.config/i3blocks/config.bak
ln -s ./config/i3blocks/config ~/.config/i3blocks/config

mv ~/.zshrc ~/.zshrc.bak
ln -s ./zshrc ~/.zshrc

mv ~/.emacs ~/.emacs.bak
ln -s ./emacs ~/.emacs

mv ~/.gitconfig ~/.gitconfig.bak
ln -s ./gitconfig ~/.gitconfig

mv ~/.gitignore_global ~/.gitignore_global.bak
ln -s ./gitignore_global ~/.gitignore_global

mkdir -p ~/.local/bin/
ln -s ./local/bin/diff-highlight ~/.local/bin/diff-highlight
ln -s ./local/bin/lock.png ~/.local/bin/lock.png
ln -s ./local/bin/screen-lock ~/.local/bin/screen-lock
ln -s ./local/bin/setup-multiple-monitors ~/.local/bin/setup-multiple-monitors
ln -s ./local/bin/suspend ~/.local/bin/suspend
ln -s ./local/bin/weather ~/.local/bin/weather
