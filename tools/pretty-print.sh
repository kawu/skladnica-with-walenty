while read -r line ; do xmllint --encode UTF-8 --format - <<< $line ; done | grep -v "<?xml version"
