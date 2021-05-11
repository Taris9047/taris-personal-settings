#!/bin/bash -e

RB_VER='2.7.3'
if [ ! -z "$1" ]; then
  printf 'Selecting ruby version as %s\n' "$1"
  RB_VER="$1"
else
  printf 'Selecting %s\n' "$RB_VER"
fi

die() {
  printf '%s\n' "$1"
  exit 1
}

GEMS=(
  "rsense",
  "rails",
  "rake",
  "bundler",
  "open3",
  "json",
  "hjson",
  "ruby-progressbar",
  "tty-spinner",
)

COMPILE_OPTS=(
  "--enable-shared"
)

C_FLAGS="-O3 -fomit-frame-pointer -fno-semantic-interposition -march=native -pipe"

COMP_OPTS_STR=$(
  IFS=' '
  echo "${COMPILE_OPTS[*]}"
)

# Checking out their git repository to my own rbenv dir...
#
# Their suggestion was ~/.rbenv
#
RBENV_DIR="$HOME/.rbenv"
[ -d "$RBENV_DIR" ] && rm -rf "$RBENV_DIR"
[ ! -x "$(command -v git)" ] && die 'git not found!! Exiting!!'
[ ! -x "$(command -v curl)" ] && die 'curl not found!! Exiting!!'

# Finally installing it!
if [ ! -d "$RBENV_DIR" ]; then
  printf 'Cloning rbenv into %s ...\n' "$RBENV_DIR"
  git clone 'https://github.com/rbenv/rbenv.git' "$RBENV_DIR" || die "Failed to clone rbenv!! Exiting!!"
fi

# Let's install some default ruby installations!!
export RBENV_ROOT="$RBENV_DIR"
export PATH="$RBENV_DIR/bin:$PATH"
eval "$(rbenv init -)"
INSTALL_SUCCESS='true'
if [ ! -f "$RBENV_ROOT/shims/ruby" ]; then
  git clone 'https://github.com/rbenv/ruby-build.git' "$RBENV_ROOT/plugins/ruby-build" || die "ruby-build cloning failed!!"
  env RUBY_CONFIGURE_OPTS="$COMP_OPTS_STR" RUBY_CFLAGS="$C_FLAGS" rbenv install "$RB_VER" || INSTALL_SUCCESS='false'
  # Select recently installed ruby as main.
  [ "$INSTALL_SUCCESS" = 'true' ] && rbenv global "$RB_VER"
fi
RB_RBENV="$RBENV_DIR/shims/ruby"
GEM="$RBENV_DIR/shims/gem"
if [ "$INSTALL_SUCCESS" = 'true' ]; then
  for gem_i in "${GEMS[@]}"; do
    "$GEM" install "$gem_i" || true
  done
fi

# Closing up comments...
printf '\n\nMake sure those messages added into your shell init files.\n'
printf 'RBENV_ROOT=%s\n' "$RBENV_DIR"
printf 'PATH=%s/bin:$PATH\n' "$RBENV_DIR"
printf 'eval $(rbenv init - bash) # if bash, replace bash to zsh if zsh\n'
# printf 'eval $(rbenv init --path) # To add pyenv to path!\n'
printf '\nThen you are good to go after next session!\n\n'
