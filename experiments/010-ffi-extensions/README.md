# FFI Extensions for Monkey - Low-Level Primitives

## Goal
Create HTTP and filesystem primitives using C extensions that:
1. Don't rely on Guile internals
2. Use only system libraries (POSIX, libcurl, etc.)
3. Can be called from Monkey via FFI
4. Are portable across Unix-like systems

## Architecture

```
Monkey Code
     ↓
Monkey Built-ins (Scheme)
     ↓
FFI Bridge (Guile's dynamic-link/dynamic-func)
     ↓
C Shared Library (.so)
     ↓
System Libraries (libc, libcurl)
```

## Approach 1: Pure C Shared Libraries

### Filesystem Extension (`libmonkey_fs.so`)

```c
// monkey_fs.c - Filesystem primitives
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <dirent.h>

// Read entire file into buffer
char* monkey_read_file(const char* path) {
    FILE* file = fopen(path, "rb");
    if (!file) return NULL;
    
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    char* buffer = malloc(size + 1);
    if (!buffer) {
        fclose(file);
        return NULL;
    }
    
    fread(buffer, 1, size, file);
    buffer[size] = '\0';
    fclose(file);
    
    return buffer;
}

// Write string to file
int monkey_write_file(const char* path, const char* content) {
    FILE* file = fopen(path, "w");
    if (!file) return -1;
    
    int result = fputs(content, file);
    fclose(file);
    return result >= 0 ? 0 : -1;
}

// Check if file exists
int monkey_file_exists(const char* path) {
    return access(path, F_OK) == 0 ? 1 : 0;
}

// Delete file
int monkey_delete_file(const char* path) {
    return unlink(path);
}

// List directory
char** monkey_list_dir(const char* path, int* count) {
    DIR* dir = opendir(path);
    if (!dir) {
        *count = 0;
        return NULL;
    }
    
    // Count entries first
    int n = 0;
    struct dirent* entry;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") != 0 && 
            strcmp(entry->d_name, "..") != 0) {
            n++;
        }
    }
    
    // Allocate array
    char** files = malloc(n * sizeof(char*));
    rewinddir(dir);
    
    // Fill array
    int i = 0;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") != 0 && 
            strcmp(entry->d_name, "..") != 0) {
            files[i] = strdup(entry->d_name);
            i++;
        }
    }
    
    closedir(dir);
    *count = n;
    return files;
}

// Free memory allocated by read_file
void monkey_free(void* ptr) {
    free(ptr);
}

// Free string array
void monkey_free_array(char** arr, int count) {
    for (int i = 0; i < count; i++) {
        free(arr[i]);
    }
    free(arr);
}
```

### HTTP Extension (`libmonkey_http.so`)

```c
// monkey_http.c - HTTP primitives using libcurl
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <curl/curl.h>

typedef struct {
    char* data;
    size_t size;
} Response;

// Callback for curl
static size_t write_callback(void* contents, size_t size, size_t nmemb, void* userp) {
    size_t total_size = size * nmemb;
    Response* resp = (Response*)userp;
    
    char* ptr = realloc(resp->data, resp->size + total_size + 1);
    if (!ptr) return 0;
    
    resp->data = ptr;
    memcpy(&(resp->data[resp->size]), contents, total_size);
    resp->size += total_size;
    resp->data[resp->size] = '\0';
    
    return total_size;
}

// Initialize curl (call once)
int monkey_http_init() {
    return curl_global_init(CURL_GLOBAL_DEFAULT) == CURLE_OK ? 0 : -1;
}

// Cleanup curl
void monkey_http_cleanup() {
    curl_global_cleanup();
}

// HTTP GET request
char* monkey_http_get(const char* url) {
    CURL* curl = curl_easy_init();
    if (!curl) return NULL;
    
    Response resp = { .data = malloc(1), .size = 0 };
    
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &resp);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);
    
    CURLcode res = curl_easy_perform(curl);
    curl_easy_cleanup(curl);
    
    if (res != CURLE_OK) {
        free(resp.data);
        return NULL;
    }
    
    return resp.data;
}

// HTTP POST request
char* monkey_http_post(const char* url, const char* data, const char* content_type) {
    CURL* curl = curl_easy_init();
    if (!curl) return NULL;
    
    Response resp = { .data = malloc(1), .size = 0 };
    struct curl_slist* headers = NULL;
    
    if (content_type) {
        char header[256];
        snprintf(header, sizeof(header), "Content-Type: %s", content_type);
        headers = curl_slist_append(headers, header);
    }
    
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, data);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &resp);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);
    
    if (headers) {
        curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
    }
    
    CURLcode res = curl_easy_perform(curl);
    
    if (headers) {
        curl_slist_free_all(headers);
    }
    curl_easy_cleanup(curl);
    
    if (res != CURLE_OK) {
        free(resp.data);
        return NULL;
    }
    
    return resp.data;
}

// Get HTTP status code from last request
int monkey_http_status(const char* url) {
    CURL* curl = curl_easy_init();
    if (!curl) return -1;
    
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_NOBODY, 1L);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    
    CURLcode res = curl_easy_perform(curl);
    
    long status = 0;
    if (res == CURLE_OK) {
        curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &status);
    }
    
    curl_easy_cleanup(curl);
    return (int)status;
}
```

## Approach 2: Minimal Wrapper Using dlopen

### Build System (`Makefile`)

```makefile
CC = gcc
CFLAGS = -fPIC -Wall -O2
LDFLAGS = -shared

# Filesystem library
libmonkey_fs.so: monkey_fs.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

# HTTP library (requires libcurl)
libmonkey_http.so: monkey_http.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $< -lcurl

# Combined library
libmonkey_ext.so: monkey_fs.c monkey_http.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^ -lcurl

all: libmonkey_fs.so libmonkey_http.so

install: all
	cp *.so /usr/local/lib/
	ldconfig

clean:
	rm -f *.so *.o

test: all
	./test_extensions
```

## Scheme FFI Bridge

```scheme
;;; ffi-bridge.scm - Bridge between C extensions and Monkey
(define-module (monkey extensions ffi)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (read-file
            write-file
            file-exists?
            delete-file
            list-directory
            http-get
            http-post))

;;; Load shared libraries
(define fs-lib (dynamic-link "./libmonkey_fs.so"))
(define http-lib (dynamic-link "./libmonkey_http.so"))

;;; Filesystem functions
(define c-read-file
  (pointer->procedure '* 
                      (dynamic-func "monkey_read_file" fs-lib)
                      (list '*)))

(define c-write-file
  (pointer->procedure int
                      (dynamic-func "monkey_write_file" fs-lib)
                      (list '* '*)))

(define c-file-exists
  (pointer->procedure int
                      (dynamic-func "monkey_file_exists" fs-lib)
                      (list '*)))

(define c-delete-file
  (pointer->procedure int
                      (dynamic-func "monkey_delete_file" fs-lib)
                      (list '*)))

(define c-free
  (pointer->procedure void
                      (dynamic-func "monkey_free" fs-lib)
                      (list '*)))

;;; HTTP functions
(define c-http-init
  (pointer->procedure int
                      (dynamic-func "monkey_http_init" http-lib)
                      '()))

(define c-http-get
  (pointer->procedure '*
                      (dynamic-func "monkey_http_get" http-lib)
                      (list '*)))

(define c-http-post
  (pointer->procedure '*
                      (dynamic-func "monkey_http_post" http-lib)
                      (list '* '* '*)))

;;; Initialize HTTP
(c-http-init)

;;; Scheme wrappers
(define (read-file path)
  "Read file contents as string"
  (let* ((path-ptr (string->pointer path))
         (result-ptr (c-read-file path-ptr)))
    (if (null-pointer? result-ptr)
        #f
        (let ((content (pointer->string result-ptr)))
          (c-free result-ptr)
          content))))

(define (write-file path content)
  "Write string to file"
  (let ((path-ptr (string->pointer path))
        (content-ptr (string->pointer content)))
    (= 0 (c-write-file path-ptr content-ptr))))

(define (file-exists? path)
  "Check if file exists"
  (let ((path-ptr (string->pointer path)))
    (= 1 (c-file-exists path-ptr))))

(define (delete-file path)
  "Delete file"
  (let ((path-ptr (string->pointer path)))
    (= 0 (c-delete-file path-ptr))))

(define (http-get url)
  "HTTP GET request"
  (let* ((url-ptr (string->pointer url))
         (result-ptr (c-http-get url-ptr)))
    (if (null-pointer? result-ptr)
        #f
        (let ((content (pointer->string result-ptr)))
          (c-free result-ptr)
          content))))

(define (http-post url data #:content-type (content-type "application/x-www-form-urlencoded"))
  "HTTP POST request"
  (let* ((url-ptr (string->pointer url))
         (data-ptr (string->pointer data))
         (type-ptr (string->pointer content-type))
         (result-ptr (c-http-post url-ptr data-ptr type-ptr)))
    (if (null-pointer? result-ptr)
        #f
        (let ((content (pointer->string result-ptr)))
          (c-free result-ptr)
          content))))
```

## Monkey Integration

```scheme
;;; Add to evaluator.scm
(define (builtin-read-file args)
  "Read file contents"
  (if (not (= (length args) 1))
      (new-error "wrong number of arguments. got=~a, want=1" (length args))
      (let ((path-obj (car args)))
        (if (not (string-object? path-obj))
            (new-error "argument to 'read_file' must be STRING")
            (let ((content (read-file (string-object-value path-obj))))
              (if content
                  (make-string-object content)
                  *null*))))))

(define (builtin-write-file args)
  "Write to file"
  (if (not (= (length args) 2))
      (new-error "wrong number of arguments. got=~a, want=2" (length args))
      (let ((path-obj (car args))
            (content-obj (cadr args)))
        (if (not (and (string-object? path-obj)
                      (string-object? content-obj)))
            (new-error "arguments to 'write_file' must be STRING")
            (let ((success (write-file 
                           (string-object-value path-obj)
                           (string-object-value content-obj))))
              (if success *true* *false*))))))

(define (builtin-http-get args)
  "HTTP GET request"
  (if (not (= (length args) 1))
      (new-error "wrong number of arguments. got=~a, want=1" (length args))
      (let ((url-obj (car args)))
        (if (not (string-object? url-obj))
            (new-error "argument to 'http_get' must be STRING")
            (let ((response (http-get (string-object-value url-obj))))
              (if response
                  (make-string-object response)
                  *null*))))))
```

## Benefits of This Approach

1. **No Guile Internals**: Uses only POSIX and standard C libraries
2. **Portable**: Works on any Unix-like system with libcurl
3. **Fast**: Direct C implementation, no overhead
4. **Safe**: Memory management handled properly
5. **Extensible**: Easy to add more functions
6. **Independent**: Can be used by any FFI-capable language

## Security Considerations

```c
// Add sandboxing support
typedef struct {
    char** allowed_paths;
    int path_count;
    int allow_network;
    int read_only;
} SecurityPolicy;

static SecurityPolicy* policy = NULL;

void monkey_set_policy(SecurityPolicy* p) {
    policy = p;
}

int is_path_allowed(const char* path) {
    if (!policy) return 1; // No policy = allow all
    
    for (int i = 0; i < policy->path_count; i++) {
        if (strncmp(path, policy->allowed_paths[i], 
                   strlen(policy->allowed_paths[i])) == 0) {
            return 1;
        }
    }
    return 0;
}

// Modified read_file with security check
char* monkey_read_file_secure(const char* path) {
    if (!is_path_allowed(path)) {
        return NULL; // Access denied
    }
    return monkey_read_file(path);
}
```

## Alternative: WebAssembly Approach

Instead of native libraries, compile to WASM:

```bash
# Compile C to WASM
emcc monkey_fs.c -o monkey_fs.wasm \
     -s EXPORTED_FUNCTIONS='["_monkey_read_file","_monkey_write_file"]' \
     -s MODULARIZE=1 \
     -s EXPORT_NAME="MonkeyFS"

# Use in Scheme via WASM runtime
# Requires a WASM runtime for Guile
```

## Testing

```c
// test_extensions.c
#include <stdio.h>
#include <assert.h>
#include <string.h>

// Declare functions
extern char* monkey_read_file(const char*);
extern int monkey_write_file(const char*, const char*);
extern void monkey_free(void*);

int main() {
    // Test write
    assert(monkey_write_file("test.txt", "Hello, Monkey!") == 0);
    
    // Test read
    char* content = monkey_read_file("test.txt");
    assert(content != NULL);
    assert(strcmp(content, "Hello, Monkey!") == 0);
    monkey_free(content);
    
    // Test HTTP
    char* response = monkey_http_get("http://httpbin.org/get");
    assert(response != NULL);
    printf("HTTP Response: %s\n", response);
    monkey_free(response);
    
    printf("All tests passed!\n");
    return 0;
}
```

## Next Steps

1. Implement the C libraries
2. Create comprehensive test suite
3. Add async/callback support for HTTP
4. Implement streaming for large files
5. Add more protocols (WebSocket, gRPC)
6. Create cross-platform build system
7. Package as standalone library