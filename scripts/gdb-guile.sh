#!/bin/sh
# GDB wrapper for debugging Guile programs
# Based on advice from Ramin Honary via Mastodon/Emacs.ch
#
# This script helps debug Guile programs at the C level using GDB.
# Particularly useful for tracking down segfaults, infinite loops,
# or understanding the low-level behavior of Guile's VM.
#
# Prerequisites:
# - Install guile development packages (e.g., guile-3.0-dev on Debian/Ubuntu)
# - Ensure GDB is installed
#
# Usage:
#   ./gdb-guile.sh my-program.scm --args=to --my=program
#
# The script automatically:
# - Ignores SIGXCPU (CPU time limit) signals
# - Ignores SIGPWR (power failure) signals  
# - Starts the program running immediately
#
# Once in GDB, useful commands:
#   bt                    - Show C backtrace
#   info threads         - Show all threads
#   break scm_error      - Break on Scheme errors
#   break scm_throw      - Break on exceptions
#   print $1             - Print Scheme value (SCM type)
#   call scm_display($1, scm_current_output_port()) - Display Scheme object
#
# For more advanced debugging:
#   break eval.c:581     - Break at specific line in Guile source
#   watch <variable>     - Break when variable changes
#   info registers       - Show CPU registers
#   disassemble         - Show assembly code

gdb \
  -ex 'handle SIGXCPU nostop' \
  -ex 'handle SIGPWR nostop' \
  -ex 'run' \
  --args \
  guile "${@}"