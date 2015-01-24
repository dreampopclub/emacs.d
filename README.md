# Emacs configuration. #

Very helpful: Running OS X GUI Emacs.app from login shell with correct env vars:

(see Using Emacs.app from the doc)

<http://www.emacswiki.org/emacs/EmacsForMacOS>

# Make it work sort of ok-ish in iTerm #

iTerm > Preferences > Keys

Left Option key acts as +Esc (ideally left command would be Meta, but I can't figure that out.)

add to .bashrc:

alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"

export EDITOR="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
