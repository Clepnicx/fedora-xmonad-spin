#!/bin/bash

cd ~

# updates the system
sudo dnf update

# install all required packages for xmonad and basic usability
sudo dnf install xmonad xmonad-config xmobar picom redhat-rpm-config sddm fontawesome5-fonts-all feh rofi arandr ark okular dolphin ranger firefox kitty micro

# enable sddm
sudo systemctl enable sddm.service

# set the default target to graphical
sudo systemctl set-default graphical.target

# creates all required directorys and user directorys
mkdir Downloads
mkdir Pictures
mkdir ~/Pictures/Wallpaper
mkdir Documents
mkdir Music
mkdir Videos
mkdir Desktop
mkdir ~/.xmonad/
mkdir ~/.config/

# moves all config-files to the correct directorys
mv ~/fedora-xmonad-spin/.bashrc ~/
mv ~/fedora-xmonad-spin/.xmobarrc ~/
mv ~/fedora-xmonad-spin/wallpaper.png ~/Pictures/Wallpaper/
mv ~/fedora-xmonad-spin/xmonad.hs ~/.xmonad/
mv -r ~/fedora-xmonad-spin/kitty ~/.config/

# tests if xmonad recompiles correctly
xmonad --recompile

# everything is finished and it is time to reboot
printf "Everything is set up. You can rebbot the system now."