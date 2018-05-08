# dotfiles
Amalgamation of dots and files.

## Bootstrapping

I use Stow to manage the symlinks.
Emacs Requires a base installation of emacs as well as a base installation of spacemacs.

First time usage looks like this:
  stow -S linux

If you're adding a new file, use restow:
  stow -R linux

## Emacs Todos

- Get register saving and getting bound to the leader key
