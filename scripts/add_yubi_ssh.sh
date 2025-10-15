#!/bin/bash
if [ -z "$SSH_AGENT_PID" ]; then
  eval "$(ssh-agent -s)"
  ssh-add ~/.ssh/git_id_ecdsa_sk  # Add your SSH key path here
  ssh-add ~/.ssh/git2_id_ecdsa_sk # Add your SSH key path here
fi
