#!/bin/bash

cat > .git/hooks/pre-push.sh << EOF
stack test
EOF

chmod a+x .git/hooks/pre-push.sh
