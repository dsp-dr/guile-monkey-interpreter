// test_fs.c - Test filesystem functions
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Declare functions
extern char* monkey_read_file(const char*);
extern int monkey_write_file(const char*, const char*);
extern int monkey_file_exists(const char*);
extern int monkey_delete_file(const char*);
extern long monkey_file_size(const char*);
extern int monkey_is_directory(const char*);
extern char* monkey_getcwd();
extern void monkey_free(void*);

int main() {
    printf("Testing Monkey Filesystem Functions\n");
    printf("====================================\n\n");

    // Test getcwd
    printf("1. Testing getcwd...\n");
    char* cwd = monkey_getcwd();
    assert(cwd != NULL);
    printf("   Current directory: %s\n", cwd);
    monkey_free(cwd);
    printf("   ✓ getcwd works\n\n");

    // Test write_file
    printf("2. Testing write_file...\n");
    const char* test_file = "test_monkey.txt";
    const char* test_content = "Hello from Monkey FFI!";
    int result = monkey_write_file(test_file, test_content);
    assert(result == 0);
    printf("   ✓ File written successfully\n\n");

    // Test file_exists
    printf("3. Testing file_exists...\n");
    int exists = monkey_file_exists(test_file);
    assert(exists == 1);
    printf("   ✓ File exists check works\n\n");

    // Test file_size
    printf("4. Testing file_size...\n");
    long size = monkey_file_size(test_file);
    assert(size == strlen(test_content));
    printf("   File size: %ld bytes\n", size);
    printf("   ✓ File size correct\n\n");

    // Test read_file
    printf("5. Testing read_file...\n");
    char* content = monkey_read_file(test_file);
    assert(content != NULL);
    assert(strcmp(content, test_content) == 0);
    printf("   Read content: '%s'\n", content);
    monkey_free(content);
    printf("   ✓ File read successfully\n\n");

    // Test is_directory
    printf("6. Testing is_directory...\n");
    int is_dir = monkey_is_directory(".");
    assert(is_dir == 1);
    is_dir = monkey_is_directory(test_file);
    assert(is_dir == 0);
    printf("   ✓ Directory check works\n\n");

    // Test delete_file
    printf("7. Testing delete_file...\n");
    result = monkey_delete_file(test_file);
    assert(result == 0);
    exists = monkey_file_exists(test_file);
    assert(exists == 0);
    printf("   ✓ File deleted successfully\n\n");

    printf("====================================\n");
    printf("All filesystem tests passed! ✓\n");
    return 0;
}