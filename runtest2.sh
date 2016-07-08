unbuffer ./tmp/test2 | while read -r line ; do xmllint --format - <<< $line ; done | grep -v "<?xml version=\"1.0\"?>"
