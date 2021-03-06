# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/home/nasrullah/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  git
)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
backlight() {
    sudo tee /sys/class/backlight/intel_backlight/brightness <<< $(( $1 * $(</sys/class/backlight/intel_backlight/max_brightness) / 100 ))

}

alias diff='colordiff'

alias weather="curl 'wttr.in?m' | less -S"
alias idunno="echo '¯\_(ツ)_/¯' | pbcopy"

alias pbcopy='xsel --clipboard --input'
alias pbpaste='xsel --clipboard --output'

alias cdp="cd ~/Projects/rapidplan/ && source install/setup.zsh"
alias cdp2="cd ~/Projects/rapidplan2/ && source install/setup.zsh"
alias cdurcap="cd ~/Projects/com.rtr.realtimemove"

alias g='gst'
alias gf='git fetch --all --prune'
alias gdw='git diff --color-words'
alias gdww="git diff --color-words='\w+|.' --ignore-space-change "
alias gsww="git show --color-words='\w+|.' --ignore-space-change "
alias gfm='git fetch origin master:master'
alias glgg='git log --graph --decorate'
alias gstu='gsta -u'
alias todos='git diff --unified=0 HEAD | grep -i todo'

alias sag="sudo apt-get"
alias sagi="sudo apt-get install"
alias sagu="sudo apt-get update && sudo apt-get upgrade"
alias sacs="sudo apt-cache search"
alias pbcopy"xclip -selection clipboard"
alias pbpaste="xclip -selection clipboard -o"

alias cm="colcon build --base-paths src -DCMAKE_BUILD_TYPE=RELWITHDEBINFO -DCMAKE_EXPORT_COMPILE_COMMANDS=1"
alias cmd="colcon build --base-paths src -DCMAKE_BUILD_TYPE=Debug"
alias rtrapp="./build/rtr_appliance_app/rtr_appliance_app"
alias rtrwb="./build/rtr_rp_create/rtr_rp_create"
alias cma="cm --packages-up-to rtr_appliance_app && gdb ./build/rtr_appliance_app/rtr_appliance_app"
alias cmw="cm --packages-up-to rtr_rp_create && gdb ./build/rtr_rp_create/rtr_rp_create"
alias webapp="(cd src/apps/rtr_appliance_webapp/client && npm ci && npm run build) && (cd src/apps/rtr_appliance_webapp/server && npm ci && ./node_modules/typescript/bin/tsc && node start.js)"
alias cmtest="colcon test --base-paths src --event-handlers console_direct+"
alias rptest="python ./scripts/run_test.py"
alias format="./scripts/format-code.sh -d"

source /opt/ros/foxy/setup.zsh

# because create react app by default opens a tab when the server starts
export BROWSER=none
export RTR_ROBOT_MODELS_MASTER_TOKEN=58812e58a6d6174d0ca766e065ade8b288516f4ced1f2f53
export RTR_DEV_MODE=true
export DEVTOOL_BUILD=ON

export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

#Virtualenvwrapper settings:
export WORKON_HOME=$HOME/.virtualenvs
VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
. /usr/local/bin/virtualenvwrapper.sh
