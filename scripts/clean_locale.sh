#!/bin/bash

# Get the list of active locales from /etc/locale.gen
active_locales=$(grep -v '^#' /etc/locale.gen | awk '{print $1}')

# Function to check if a locale is in the active list
is_active_locale() {
  local locale=$1
  for active_locale in $active_locales; do
    if [[ "$locale" == "$active_locale" ]]; then
      return 0
    fi
  done
  return 1
}

# Remove locale files that are not in the active list
for locale_dir in /usr/share/locale/*; do
  locale=$(basename "$locale_dir")
  if ! is_active_locale "$locale"; then
    sudo rm -rf "$locale_dir"
    echo "Removed unused locale: $locale"
  fi
done

echo "Cleanup of unused locales complete."
