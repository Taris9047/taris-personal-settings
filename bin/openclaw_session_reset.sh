#!/bin/bash

rm -rf $HOME/.openclaw/agents/*/sessions/*
openclaw stop
openclaw sessions cleanup --all-agents
openclaw start
