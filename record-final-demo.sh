#!/bin/bash
# Final demo recording instructions

echo "===================== MANUAL DEMO RECORDING SCRIPT ====================="
echo
echo "Follow these steps to record the demo:"
echo
echo "1. Start recording:"
echo "   asciinema rec --title 'Guile Monkey Interpreter' --idle-time-limit 2.0 monkey-demo.cast"
echo
echo "2. In the recording session, run these commands:"
echo
echo "   clear"
echo "   gmake repl"
echo
echo "3. In the REPL, type these examples:"
echo
echo "   // Basic arithmetic"
echo "   5 + 5 * 2"
echo "   "
echo "   // Variables and functions"
echo "   let add = fn(a, b) { a + b };"
echo "   add(15, 27)"
echo "   "
echo "   // Arrays"
echo "   let arr = [1, 2, 3, 4, 5];"
echo "   len(arr)"
echo "   "
echo "   // Chapter 4 extensions"
echo "   type(42)"
echo "   str(100)"
echo "   keys({\"a\": 1, \"b\": 2})"
echo "   split(\"hello,world\", \",\")"
echo "   join([\"hello\", \"world\"], \" \")"
echo
echo "4. Exit with Ctrl-C"
echo
echo "5. Upload:"
echo "   asciinema upload monkey-demo.cast"
echo
echo "========================================================================="