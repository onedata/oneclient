# Oneclient shell completion scripts

These are Bash and Zsh completion scripts for oneclient.

# Installation

## Bash
The `oneclient.bash-completion` script can be either directly loaded to the current Bash session using:

```shell
source oneclient.bash-completion
```

Alternatively, the script can be copied to the `/etc/bash_completion.d` (or on OSX with Homebrew to `/usr/local/etc/bash_completion.d`) under `oneclient` name:

```shell
sudo cp oneclient.bash-completion /etc/bash_completion.d/oneclient
```

Make sure to the following lines are uncommented in the `~/.bashrc`:

```shell
if [ -f /etc/bash_completion ]; then
  . /etc/bash_completion
fi
```

#### OS X
On OSX you might need to install bash-completion using Homebrew:
```shell
brew install bash-completion
``` 
Make sure to the following lines are uncommented in the `~/.bashrc`:

```shell
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi
```

## Zsh 
In Zsh, the `_oneclient` Zsh completion file must be copied to one of the folders under `$FPATH` variable, and the Zsh terminal should be reopened.

For testing, _oneclient completion function can be updated without restarting zsh:
```shell
# First copy _oneclient somewhere under $FPATH
$ unfunction _oneclient
$ autoload -U _oneclient
```

