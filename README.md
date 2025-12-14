# My personal UNIX/Linux settings repository.

Some automation scripts for my own tool installation. Sometimes, you have to use decades old OS on a state-of-art machine due to 'reasons'... Thus, in this case, building your code is the only solution.

## Usage
Before running the curl script, we have to make sure some prerequisites installed. For example on Ubuntu...

```shell
sudo apt install curl git ruby buil-essential
```

Just run this line on your home directory. Hopefully, server admin is not a crazy person to omit <code>curl</code> from installation.

```shell
curl -s 'https://raw.githubusercontent.com/Taris9047/taris-personal-settings/master/install_conf.sh' | /bin/bash
```

This will download this repository and make sure everything is laid out correctly. 

Fill up <code>~/.gitconfig.local</code> to set up your main GitHub setting.
