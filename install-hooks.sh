#!/bin/bash

cat > .git/hooks/pre-push << "EOF"
stack test
EOF

chmod a+x .git/hooks/pre-push

cat > .git/hooks/pre-commit << "EOF"
# Taken from https://github.com/HIPERFIT/L0Language/blob/master/git-hooks/pre-commit

fail() {
    echo "Aborting commit due to verification errors."
    echo "If you disagree, use git commit --no-verify."
    exit 1
}

hlintable() {
    ! egrep -q '{-# LANGUAGE.*QuasiQuotes' $1
}

l0_hlint() {
    # Some hlint-suggestions are terrible, so ignore them here.
    hlint -i "Reduce duplication" -i "Use import/export shortcut" "$@"
}

# Run hlint on changed files.
for file in $(git diff-index --cached --name-only HEAD | egrep '\.l?hsc?$'); do
    if [ -f $file ]; then
        if hlintable $file; then
            echo "Checking $file with hlint:"
            if ! l0_hlint "$file"; then
                fail
            fi
        fi
    fi
done
EOF

chmod a+x .git/hooks/pre-commit
