CC = gcc
CFLAGS = -fPIC -Wall -O2
LDFLAGS = -shared

# OS detection for FreeBSD/macOS compatibility
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),FreeBSD)
    CFLAGS += -I/usr/local/include
    LDFLAGS += -L/usr/local/lib
endif
ifeq ($(UNAME_S),Darwin)
    LDFLAGS = -dynamiclib
endif

# Targets
all: libmonkey_fs.so libmonkey_http.so

# Filesystem library
libmonkey_fs.so: monkey_fs.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

# HTTP library (requires libcurl)
libmonkey_http.so: monkey_http.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $< -lcurl

# Combined library
libmonkey_ext.so: monkey_fs.c monkey_http.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^ -lcurl

# Test programs
test_fs: test_fs.c libmonkey_fs.so
	$(CC) -o $@ test_fs.c -L. -lmonkey_fs -Wl,-rpath,.

test_http: test_http.c libmonkey_http.so
	$(CC) -o $@ test_http.c -L. -lmonkey_http -lcurl -Wl,-rpath,.

# Installation (optional)
install: all
	install -m 755 *.so /usr/local/lib/
	ldconfig || true

# Cleanup
clean:
	rm -f *.so *.o test_fs test_http

# Test target
test: test_fs test_http
	./test_fs
	./test_http

.PHONY: all clean install test