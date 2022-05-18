#!/bin/bash

cd ~

# updates the system
sudo dnf update

# install all required packages for xmonad and basic usability
sudo dnf install xmonad xmonad-config xmobar picom redhat-rpm-config sddm fontawesome5-fonts-all feh rofi arandr ark okular dolphin ranger firefox kitty micro kvantum neofetch

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
cp ~/fedora-xmonad-spin/.bashrc ~/
cp ~/fedora-xmonad-spin/.xmobarrc ~/
cp ~/fedora-xmonad-spin/.profile ~/
cp ~/fedora-xmonad-spin/wallpaper.png ~/Pictures/Wallpaper/
cp ~/fedora-xmonad-spin/xmonad.hs ~/.xmonad/
cp -r ~/fedora-xmonad-spin/kitty ~/.config/

# tests if xmonad recompiles correctly
xmonad --recompile

# everything is finished and it is time to reboot
printf "Everything is set up. You can rebbot the system now. \n"
