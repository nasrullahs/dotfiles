#!/usr/bin/env bash

sudo apt-get install i3 i3blocks dmenu feh xautolock redshift gtk-redshift zsh git ripgrep tree xclip xsel htop ipython3 scrot colordiff emacs global imagemagick scrot brightnessctl

cd ~

mv ~/.config/i3/config ~/.config/i3/config.bak
ln -s ~/Projects/dotfiles/config/i3/config ~/.config/i3/config

mv ~/.config/i3blocks/config ~/.config/i3blocks/config.bak
ln -s ~/Projects/dotfiles/config/i3blocks/config ~/.config/i3blocks/config

mv ~/.zshrc ~/.zshrc.bak
ln -s ~/Projects/dotfiles/zshrc ~/.zshrc

mv ~/.emacs ~/.emacs.bak
ln -s ~/Projects/dotfiles/emacs ~/.emacs

mv ~/.gitconfig ~/.gitconfig.bak
ln -s ~/Projects/dotfiles/gitconfig ~/.gitconfig

mv ~/.gitignore_global ~/.gitignore_global.bak
ln -s ~/Projects/dotfiles/gitignore_global ~/.gitignore_global

mkdir -p ~/.local/
ln -s ~/Projects/dotfiles/local/bin ~/.local/
