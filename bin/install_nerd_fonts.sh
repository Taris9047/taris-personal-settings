#!/bin/bash

CWD="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
GITHUB_URL='https://github.com/ryanoasis/nerd-fonts.git'
WORK_DIR="/tmp/.inst_nerd_fonts_workspace"

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
  "SourceCodePro" \
  "SpaceMono" \
  "Terminus" \
  "RobotoMono" \
  "Mononoki" \
  "OpenDyslexic" \
  "3270" \
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
  echo "Installing $fnt ... "
  $WORK_DIR/nerd-fonts/install.sh -U -T -q "$fnt"
done

# Cleaning up
echo "Cleaning up everything!!"
rm -rf "$WORK_DIR"
