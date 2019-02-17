export SSH_KEY_PATH="~/.ssh/"
unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
    export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi
# gpgconf --launch gpg-agent
# no keychain on mac
export GPG_TTY="$(tty)"
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpgconf --launch gpg-agent
function load_rsa {
    if type keychain; then
        load id_rsa
        eval `keychain --eval id_rsa`
    fi
}
