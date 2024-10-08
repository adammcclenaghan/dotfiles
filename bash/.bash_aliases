alias dev='cd ~/Development'
alias game-dev='cd ~/Development/Games/'
alias gfp='git fetch -ap && git pull'
alias ec='emacsclient -n'
alias unreal='~/Development/UnrealEngine/Engine/Binaries/Linux/UE4Editor'
alias mci='mvn clean install'
alias awsp='export AWS_PROFILE=$(sed -n "s/\[profile \(.*\)\]/\1/gp" ~/.aws/config | fzf)'
alias awsl='aws sso login'
