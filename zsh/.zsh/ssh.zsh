export SSH_KEY_PATH="~/.ssh/"
# no keychain on mac
export GPG_TTY="$(tty)"
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpgconf --launch gpg-agent
# if type keychain; then
#  eval `keychain --eval id_rsa`
# fi
