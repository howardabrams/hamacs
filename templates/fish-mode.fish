#!/usr/bin/env fish
# ----------------------------------------------------------------------
#  `(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`: $1
# ----------------------------------------------------------------------

function `(downcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))` --description "$1"
    $0
end
