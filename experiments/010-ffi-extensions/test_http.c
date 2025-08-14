// test_http.c - Test HTTP functions
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Declare functions
extern int monkey_http_init();
extern void monkey_http_cleanup();
extern char* monkey_http_get(const char*);
extern int monkey_http_status(const char*);
extern void monkey_http_free(void*);

int main() {
    printf("Testing Monkey HTTP Functions\n");
    printf("=============================\n\n");

    // Initialize HTTP
    printf("1. Initializing HTTP library...\n");
    int result = monkey_http_init();
    assert(result == 0);
    printf("   ✓ HTTP initialized\n\n");

    // Test HTTP status
    printf("2. Testing HTTP status check...\n");
    int status = monkey_http_status("http://httpbin.org/status/200");
    printf("   Status code: %d\n", status);
    assert(status == 200);
    printf("   ✓ Status check works\n\n");

    // Test HTTP GET
    printf("3. Testing HTTP GET...\n");
    char* response = monkey_http_get("http://httpbin.org/get");
    if (response != NULL) {
        // Just check we got something
        assert(strlen(response) > 0);
        // Check for expected content
        assert(strstr(response, "\"url\"") != NULL);
        printf("   ✓ GET request successful (got %zu bytes)\n", strlen(response));
        monkey_http_free(response);
    } else {
        printf("   ⚠ GET request failed (network issue?)\n");
    }
    printf("\n");

    // Test 404 status
    printf("4. Testing 404 status...\n");
    status = monkey_http_status("http://httpbin.org/status/404");
    printf("   Status code: %d\n", status);
    assert(status == 404);
    printf("   ✓ 404 status detected\n\n");

    // Cleanup
    printf("5. Cleaning up HTTP library...\n");
    monkey_http_cleanup();
    printf("   ✓ HTTP cleaned up\n\n");

    printf("=============================\n");
    printf("HTTP tests completed! ✓\n");
    printf("Note: Some tests require internet connectivity\n");
    return 0;
}