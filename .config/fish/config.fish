if status is-interactive
    # Commands to run in interactive sessions can go here
    starship init fish | source

# aliases

# apt
alias inst='sudo apt install'
alias pu='sudo apt purge'
alias autopu='sudo apt autopurge'
alias up='sudo apt update && sudo apt upgrade'
alias upd='sudo apt update'
alias upg='sudo apt upgrade'
alias ainstalled='apt list --installed'


alias ll='ls -la'
alias clr='clear'

end
