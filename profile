# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi


# SSH-Agent config
# If no SSH agent is already running, start one now. Re-use sockets so we never
# have to start more than one session.
export SSH_AUTH_SOCK=/home/ross/.ssh-socket

ssh-add -l >/dev/null 2>&1
if [ $? = 2 ]; then
   # No ssh-agent running
   rm -rf $SSH_AUTH_SOCK
   ssh-agent -a $SSH_AUTH_SOCK -t 3300 >/tmp/.ssh-script
   chmod +x /tmp/.ssh-script
   source /tmp/.ssh-script > /dev/null
   echo $SSH_AGENT_PID > /home/ross/.ssh-agent-pid
   rm /tmp/.ssh-script
   echo "Agent started"
fi
