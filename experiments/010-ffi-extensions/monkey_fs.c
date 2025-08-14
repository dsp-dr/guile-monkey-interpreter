// monkey_fs.c - Filesystem primitives for Monkey
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
    
    size_t read_size = fread(buffer, 1, size, file);
    buffer[read_size] = '\0';
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

// Get file size
long monkey_file_size(const char* path) {
    struct stat st;
    if (stat(path, &st) != 0) return -1;
    return st.st_size;
}

// Check if path is directory
int monkey_is_directory(const char* path) {
    struct stat st;
    if (stat(path, &st) != 0) return 0;
    return S_ISDIR(st.st_mode) ? 1 : 0;
}

// Create directory
int monkey_mkdir(const char* path, int mode) {
    return mkdir(path, mode);
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
    if (!files) {
        closedir(dir);
        *count = 0;
        return NULL;
    }
    
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

// Get current working directory
char* monkey_getcwd() {
    char* buffer = malloc(1024);
    if (!buffer) return NULL;
    
    if (getcwd(buffer, 1024) == NULL) {
        free(buffer);
        return NULL;
    }
    
    return buffer;
}

// Change directory
int monkey_chdir(const char* path) {
    return chdir(path);
}