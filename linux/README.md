# Get the dotfiles
git clone https://github.com/Atrus7/dotfiles ~/dotfiles

# Install packages
sudo apt-get install xorg i3 rofi git emacs vim stow rxvt-unicode google-chrome zsh pkg-config feh xbacklight maim libnotify-bin libnotify-dev

# Setup home directory
mkdir ~/downloads ~/pictures ~/pictures/screenshots
ln -s ~/Dropbox/org ~/org

## Stow everything
cd ~/dotfiles ; stow -S linux i3 spacemacs vim X misc zsh
git submodule init
git submodule update

# Emacs install (spacemacs)
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
emacs  # install packages

## zsh
chsh -s $(which zsh)
