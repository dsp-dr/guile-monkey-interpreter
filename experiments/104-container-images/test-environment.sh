#!/bin/bash
# Test if all required tools are available for Monkey interpreter development

echo "=== Environment Test for Monkey Interpreter ==="
echo

# Color codes
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Track if all requirements are met
ALL_GOOD=true

# Check Guile
echo -n "Guile: "
if command -v guile >/dev/null 2>&1; then
    VERSION=$(guile --version | head -1)
    echo -e "${GREEN}✓${NC} $VERSION"
else
    echo -e "${RED}✗ NOT FOUND${NC}"
    ALL_GOOD=false
fi

# Check GCC
echo -n "GCC: "
if command -v gcc >/dev/null 2>&1; then
    VERSION=$(gcc --version | head -1)
    echo -e "${GREEN}✓${NC} $VERSION"
else
    echo -e "${RED}✗ NOT FOUND${NC}"
    ALL_GOOD=false
fi

# Check GDB
echo -n "GDB: "
if command -v gdb >/dev/null 2>&1; then
    VERSION=$(gdb --version | head -1)
    echo -e "${GREEN}✓${NC} $VERSION"
else
    echo -e "${RED}✗ NOT FOUND${NC}"
    ALL_GOOD=false
fi

# Check Make
echo -n "Make: "
if command -v make >/dev/null 2>&1; then
    VERSION=$(make --version | head -1)
    echo -e "${GREEN}✓${NC} $VERSION"
else
    echo -e "${RED}✗ NOT FOUND${NC}"
    ALL_GOOD=false
fi

# Check pkg-config
echo -n "pkg-config: "
if command -v pkg-config >/dev/null 2>&1; then
    VERSION=$(pkg-config --version)
    echo -e "${GREEN}✓${NC} version $VERSION"
else
    echo -e "${RED}✗ NOT FOUND${NC}"
    ALL_GOOD=false
fi

# Check Guile development files
echo -n "Guile dev: "
if pkg-config --exists guile-3.0 2>/dev/null; then
    VERSION=$(pkg-config --modversion guile-3.0)
    echo -e "${GREEN}✓${NC} version $VERSION"
else
    echo -e "${RED}✗ NOT FOUND${NC}"
    ALL_GOOD=false
fi

# Test FFI compilation
echo
echo "=== FFI Compilation Test ==="
cat > /tmp/test_ffi.c << 'EOF'
#include <libguile.h>

SCM test_hello(void) {
    return scm_from_utf8_string("Hello from C!");
}

void init_test(void) {
    scm_c_define_gsubr("test-hello", 0, 0, 0, test_hello);
}
EOF

if gcc -shared -fPIC -o /tmp/test_ffi.so /tmp/test_ffi.c $(pkg-config --cflags --libs guile-3.0) 2>/dev/null; then
    echo -e "${GREEN}✓${NC} FFI compilation successful"
    rm -f /tmp/test_ffi.c /tmp/test_ffi.so
else
    echo -e "${RED}✗ FFI compilation failed${NC}"
    ALL_GOOD=false
    rm -f /tmp/test_ffi.c
fi

echo
echo "=== Optional Tools ==="
for tool in tmux vim git; do
    echo -n "$tool: "
    if command -v $tool >/dev/null 2>&1; then
        echo -e "${GREEN}✓ Available${NC}"
    else
        echo -e "  Not installed"
    fi
done

echo
if [ "$ALL_GOOD" = true ]; then
    echo -e "${GREEN}✅ All requirements met! Environment ready for Monkey interpreter development.${NC}"
    exit 0
else
    echo -e "${RED}❌ Some requirements missing. Please install missing tools.${NC}"
    exit 1
fi