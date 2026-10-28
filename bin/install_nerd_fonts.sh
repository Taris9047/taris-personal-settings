#!/bin/bash

CWD="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
GITHUB_URL='git@github.com:officialrajdeepsingh/nerd-fonts-installer.git'
WORK_DIR="${HOME}/.tmp/.inst_nerd_fonts_workspace"
FONT_INST_DIR="${HOME}/.local/share/fonts/"

# ALL_FONTS:
# 3270
# Agave
# AnonymousPro
# Arimo
# AurulentSansMono
# BigBlueTerminal
# BitstreamVeraSansMono
# CascadiaCode
# CodeNewRoman
# Cousine
# DaddyTimeMono
# DejaVuSansMono
# DroidSansMono
# FantasqueSansMono
# FiraCode
# FiraMono
# Go-Mono
# Gohu
# Hack
# Hasklig
# HeavyData
# Hermit
# IBMPlexMono
# Inconsolata
# InconsolataGo
# InconsolataLGC
# Iosevka
# JetBrainsMono
# Lekton
# LiberationMono
# Lilex
# MPlus
# Meslo
# Monofur
# Monoid
# Mononoki
# Noto
# OpenDyslexic
# Overpass
# ProFont
# ProggyClean
# RobotoMono
# ShareTechMono
# SourceCodePro
# SpaceMono
# Terminus
# Tinos
# Ubuntu
# UbuntuMono
# VictorMono
# iA-Writer

FONTS_TO_INSTALL=( \
  "BitstreamVeraSansMono" \
  "FiraMono" \
  "FiraCode" \
  "Hack" \
  "Noto" \
  "SourceCodePro" \
  "RobotoMono" \
  "Mononoki" \
  "JetBrainsMono" \
)

echo "Nerd Font installer! - From github repo."

if [ ! -d "$WORK_DIR" ]; then
  mkdir -pv "$WORK_DIR"
fi

# Checking out from git.
if [ -d "$WORK_DIR" ]; then
  rm -rf "$WORK_DIR"
fi
mkdir -pv "$WORK_DIR"

cd "$WORK_DIR" && \
git clone --depth 1 $GITHUB_URL "$WORK_DIR/nerd-fonts"
for fnt in "${FONTS_TO_INSTALL[@]}"
do
  echo "Installing font: ${fnt}"
  $WORK_DIR/nerd-fonts/install.sh --quiet "$fnt"
done

# Cleaning up
echo "Cleaning up everything!!"
rm -rf "$WORK_DIR"
