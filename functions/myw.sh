#!/bin/bash
# myw: like `w`, but the USER column is replaced by each user's
# `getent passwd` entry (uid, gid, gecos, home, shell).
#
# Safe to `source` as well as execute directly: everything below runs in a
# subshell, so `set -e`/`exit`/an unbound variable in here can only ever end
# that subshell -- never the shell (and node session) you sourced this from.
(
set -u

# Use '|' as the column -t delimiter (not space) so that spaces inside w's
# own fields (e.g. "claude --resume" in WHAT, or a multi-word GECOS name)
# don't get split into extra columns -- each row becomes exactly two
# fields: the replacement first column, and everything after it untouched.
{
    first_header_seen=false
    while IFS= read -r line; do
        # Line 1: the "HH:MM:SS up ..., load average: ..." summary -- print as-is.
        if [[ "$first_header_seen" == false ]]; then
            echo "$line"
            first_header_seen=true
            continue
        fi

        first_field=$(awk '{print $1}' <<< "$line")
        rest=${line#"$first_field"}

        # Line 2: the "USER TTY FROM LOGIN@ IDLE JCPU PCPU WHAT" column header.
        if [[ "$first_field" == USER ]]; then
            echo "PASSWD|${rest}"
            continue
        fi

        # Data rows: replace the username with its passwd entry.
        passwd_entry=$(getent passwd "$first_field" 2>/dev/null || true)
        echo "${passwd_entry:-$first_field}|${rest}"
    done < <(w)
} | column -t -s '|'
)
