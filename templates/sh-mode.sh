#!/usr/bin/env `(if (equal (file-name-extension buffer-file-name) "fish") "fish" "bash")`
# ----------------------------------------------------------------------
#  `(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`: $0
# ----------------------------------------------------------------------

set -euo pipefail

$0
