export SSH_KEY_PATH="~/.ssh/"
# no keychain on mac
if type keychain; then
 eval `keychain --eval id_rsa`
fi
