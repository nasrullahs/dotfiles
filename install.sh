#!/usr/bin/env bash

sudo apt install i3 i3blocks dmenu feh xautolock redshift redshift-gtk zsh git ripgrep tree xclip xsel htop ipython3 scrot colordiff emacs global imagemagick scrot brightnessctl debian-goodies python3-pip foliate fonts-font-awesome


cd ~

mv ~/.config/i3/config ~/.config/i3/config.bak
ln -s ~/Projects/dotfiles/config/i3/config ~/.config/i3/config

mv ~/.config/i3blocks/config ~/.config/i3blocks/config.bak
mkdir -p ~/.config/i3blocks/
ln -s ~/Projects/dotfiles/config/i3blocks/config ~/.config/i3blocks/config

mv ~/.zshrc ~/.zshrc.bak
ln -s ~/Projects/dotfiles/zshrc ~/.zshrc

mv ~/.emacs ~/.emacs.bak
ln -s ~/Projects/dotfiles/emacs ~/.emacs

mv ~/.gitconfig ~/.gitconfig.bak
ln -s ~/Projects/dotfiles/gitconfig ~/.gitconfig

mv ~/.gitignore_global ~/.gitignore_global.bak
ln -s ~/Projects/dotfiles/gitignore_global ~/.gitignore_global

mkdir -p ~/.local/bin/
ln -s ~/Projects/dotfiles/local/bin/i3blocks ~/.local/bin/i3blocks
ln -s ~/Projects/dotfiles/local/bin/lock.png ~/.local/bin/lock.png
ln -s ~/Projects/dotfiles/local/bin/make-backup ~/.local/bin/make-backup
ln -s ~/Projects/dotfiles/local/bin/screen-lock ~/.local/bin/screen-lock
ln -s ~/Projects/dotfiles/local/bin/setup-multiple-monitors ~/.local/bin/setup-multiple-monitors
ln -s ~/Projects/dotfiles/local/bin/set-wallpaper ~/.local/bin/set-wallpaper
ln -s ~/Projects/dotfiles/local/bin/suspend ~/.local/bin/suspend
ln -s ~/Projects/dotfiles/local/bin/xrandr-watcher ~/.local/bin/xrandr-watcher

ln -s ~/Projects/dotfiles/config/keymapper.conf ~/.config/keymapper.conf

# ln -s ~/Projects/dotfiles/Xresources ~/.Xresources

# make brightnessctl not require sudo
sudo usermod -aG video ${USER}
sudo chmod +s $(which brightnessctl)

# manually install keymapper from https://github.com/houmain/keymapper
