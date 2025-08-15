# Experiment 104: Container Images for Development

## Objective

Find and document container images that provide a complete development environment for the Monkey interpreter, including:
- GNU Guile 3.0+
- GCC for FFI compilation
- GDB for debugging
- Build tools (make, pkg-config)
- All required dependencies

## Requirements

### Essential Tools
- [ ] GNU Guile 3.0 or later
- [ ] GCC compiler
- [ ] GDB debugger
- [ ] GNU Make
- [ ] pkg-config
- [ ] Git

### FFI Support
- [ ] Guile development headers (guile-3.0-dev)
- [ ] C standard library headers
- [ ] Ability to compile shared libraries (.so)

### Nice to Have
- [ ] tmux for debugging sessions
- [ ] Vim/Emacs for editing
- [ ] asciinema for recording demos
- [ ] Node.js for Mermaid diagrams

## Candidate Images

### 1. Debian/Ubuntu Based

```dockerfile
FROM debian:bookworm
RUN apt-get update && apt-get install -y \
    guile-3.0 \
    guile-3.0-dev \
    gcc \
    gdb \
    make \
    pkg-config \
    git \
    tmux \
    vim
```

### 2. Ubuntu 22.04 LTS

```dockerfile
FROM ubuntu:22.04
RUN apt-get update && apt-get install -y \
    guile-3.0 \
    guile-3.0-dev \
    build-essential \
    gdb \
    pkg-config \
    git \
    tmux
```

### 3. Alpine Linux (Minimal)

```dockerfile
FROM alpine:latest
RUN apk add --no-cache \
    guile \
    guile-dev \
    gcc \
    musl-dev \
    gdb \
    make \
    pkgconfig \
    git
```

### 4. Fedora Based

```dockerfile
FROM fedora:39
RUN dnf install -y \
    guile30 \
    guile30-devel \
    gcc \
    gdb \
    make \
    pkgconfig \
    git \
    tmux
```

## Testing Script

Create `test-environment.sh`:

```bash
#!/bin/bash
# Test if all required tools are available

echo "=== Environment Test for Monkey Interpreter ==="
echo

# Check Guile
echo -n "Guile: "
if command -v guile >/dev/null 2>&1; then
    guile --version | head -1
else
    echo "NOT FOUND"
fi

# Check GCC
echo -n "GCC: "
if command -v gcc >/dev/null 2>&1; then
    gcc --version | head -1
else
    echo "NOT FOUND"
fi

# Check GDB
echo -n "GDB: "
if command -v gdb >/dev/null 2>&1; then
    gdb --version | head -1
else
    echo "NOT FOUND"
fi

# Check Make
echo -n "Make: "
if command -v make >/dev/null 2>&1; then
    make --version | head -1
else
    echo "NOT FOUND"
fi

# Check pkg-config
echo -n "pkg-config: "
if command -v pkg-config >/dev/null 2>&1; then
    pkg-config --version
else
    echo "NOT FOUND"
fi

# Check Guile development files
echo -n "Guile dev: "
if pkg-config --exists guile-3.0 2>/dev/null; then
    echo "OK ($(pkg-config --modversion guile-3.0))"
else
    echo "NOT FOUND"
fi

# Test FFI compilation
echo
echo "=== FFI Compilation Test ==="
cat > test_ffi.c << 'EOF'
#include <libguile.h>

SCM test_hello(void) {
    return scm_from_utf8_string("Hello from C!");
}

void init_test(void) {
    scm_c_define_gsubr("test-hello", 0, 0, 0, test_hello);
}
EOF

if gcc -shared -fPIC -o test_ffi.so test_ffi.c $(pkg-config --cflags --libs guile-3.0) 2>/dev/null; then
    echo "FFI compilation: SUCCESS"
    rm -f test_ffi.c test_ffi.so
else
    echo "FFI compilation: FAILED"
    rm -f test_ffi.c
fi

echo
echo "=== Optional Tools ==="
for tool in tmux vim emacs git node; do
    echo -n "$tool: "
    if command -v $tool >/dev/null 2>&1; then
        echo "Available"
    else
        echo "Not installed"
    fi
done
```

## Docker Compose Configuration

Create `docker-compose.yml`:

```yaml
version: '3.8'

services:
  monkey-dev:
    build:
      context: .
      dockerfile: Dockerfile.dev
    volumes:
      - ../..:/workspace
    working_dir: /workspace
    command: /bin/bash
    stdin_open: true
    tty: true
    environment:
      - GUILE_AUTO_COMPILE=0
      - GUILE_LOAD_PATH=/workspace/src

  monkey-test:
    build:
      context: .
      dockerfile: Dockerfile.test
    volumes:
      - ../..:/workspace
    working_dir: /workspace
    command: make test
```

## Dockerfile Templates

### Development Image (Dockerfile.dev)

```dockerfile
FROM debian:bookworm

# Install required packages
RUN apt-get update && apt-get install -y \
    guile-3.0 \
    guile-3.0-dev \
    gcc \
    g++ \
    gdb \
    make \
    pkg-config \
    git \
    tmux \
    vim \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Optional: Install Node.js for Mermaid
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
    && apt-get install -y nodejs \
    && npm install -g @mermaid-js/mermaid-cli \
    && rm -rf /var/lib/apt/lists/*

# Set up working directory
WORKDIR /workspace

# Copy test script
COPY test-environment.sh /usr/local/bin/
RUN chmod +x /usr/local/bin/test-environment.sh

# Default command
CMD ["/bin/bash"]
```

### Minimal Test Image (Dockerfile.test)

```dockerfile
FROM alpine:latest

# Install required packages
RUN apk add --no-cache \
    guile \
    guile-dev \
    gcc \
    musl-dev \
    make \
    pkgconfig \
    git

WORKDIR /workspace

# Run tests by default
CMD ["make", "test"]
```

## GitHub Actions Integration

`.github/workflows/test.yml`:

```yaml
name: Test

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    container:
      image: debian:bookworm
    
    steps:
    - uses: actions/checkout@v3
    
    - name: Install dependencies
      run: |
        apt-get update
        apt-get install -y guile-3.0 guile-3.0-dev gcc make pkg-config
    
    - name: Build FFI extensions
      run: make ffi-build
    
    - name: Run tests
      run: make test
```

## Testing Process

1. **Build the image**:
```bash
cd experiments/104-container-images
docker build -f Dockerfile.dev -t monkey-dev .
```

2. **Test the environment**:
```bash
docker run --rm monkey-dev test-environment.sh
```

3. **Run the interpreter**:
```bash
docker run --rm -v $(pwd):/workspace -w /workspace monkey-dev \
    guile -L src src/monkey/main.scm
```

4. **Debug with GDB**:
```bash
docker run --rm -it -v $(pwd):/workspace -w /workspace \
    --cap-add=SYS_PTRACE --security-opt seccomp=unconfined \
    monkey-dev gdb guile
```

## Results

### Recommended Images

| Image | Size | Pros | Cons |
|-------|------|------|------|
| debian:bookworm | ~120MB | Full toolchain, stable | Larger size |
| ubuntu:22.04 | ~80MB | Good package availability | Some overhead |
| alpine:latest | ~15MB | Minimal size | musl vs glibc |
| fedora:39 | ~180MB | Latest packages | Largest size |

### Best Choice: Debian Bookworm

**Reasons**:
- Stable Guile 3.0 packages
- Excellent GDB support
- Compatible with book's assumptions
- Good FFI support with glibc
- Wide package availability

## DevContainer Configuration

Create `.devcontainer/devcontainer.json`:

```json
{
  "name": "Monkey Interpreter Dev",
  "dockerFile": "Dockerfile",
  "forwardPorts": [],
  "postCreateCommand": "make test",
  "customizations": {
    "vscode": {
      "extensions": [
        "suragch.scheme",
        "mhutchie.git-graph"
      ],
      "settings": {
        "terminal.integrated.defaultProfile.linux": "bash"
      }
    }
  },
  "remoteUser": "root"
}
```

## Conclusion

The Debian Bookworm-based image provides the most complete and compatible environment for developing, testing, and debugging the Monkey interpreter including FFI extensions. It balances size, compatibility, and tool availability effectively.

For CI/CD, a minimal Alpine image can be used for testing, while development should use the full Debian image for the best debugging experience.