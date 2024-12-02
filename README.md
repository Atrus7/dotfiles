# dotfiles
Amalgamation of dots and files.

## Bootstrapping

I use Stow to manage the symlinks.

First time usage looks like this:
  stow -S linux

If you're adding a new file, use restow:
  stow -R linux

## Emacs Todos

- Get register saving and getting bound to the leader key

## Spacemacs
Emacs Requires a base installation of emacs as well as a base installation of spacemacs.
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d


## For laptop, start the i3lock.service
sudo ln -s i3lock.service /etc/systemd/system/i3lock.service
systemctl enable i3lock

## For keyboard
sudo ln  X/00-keyboard.conf /etc/X11/xorg.conf.d/00-keyboard.conf

## Animated wallpapers
you need libsdl2-dev, follow paperview README.md
