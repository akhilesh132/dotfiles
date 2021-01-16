pfetch | lolcat

# https://github.com/starship/starship
# startship prompt
eval "$(starship init zsh)"

# https://github.com/sharkdp/vivid/tree/master/themes
# Vivid LS_COLORS generator
export LS_COLORS="$(vivid generate molokai)"

# Restore colorscheme
wal -Rq

# Zplug - next gen zsh plugin manager
source ~/.zplug/init.zsh

zplug "plugins/fasd", from:oh-my-zsh
zplug "plugins/zsh-interactive-cd", from:oh-my-zsh
zplug "plugins/emoji", from:oh-my-zsh
zplug load

# manage dotfiles/config files with git
alias dotfiles='git --git-dir=$HOME/.dotfiles --work-tree=$HOME'
alias mynixos='git --git-dir=$HOME/.myNixOs --work-tree=/'

# fzf integration
# https://github.com/junegunn/fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
