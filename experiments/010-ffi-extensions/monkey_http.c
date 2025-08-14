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
    resp.data[0] = '\0';
    
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &resp);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "MonkeyHTTP/1.0");
    
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
    resp.data[0] = '\0';
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
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "MonkeyHTTP/1.0");
    
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

// HTTP PUT request
char* monkey_http_put(const char* url, const char* data, const char* content_type) {
    CURL* curl = curl_easy_init();
    if (!curl) return NULL;
    
    Response resp = { .data = malloc(1), .size = 0 };
    resp.data[0] = '\0';
    struct curl_slist* headers = NULL;
    
    if (content_type) {
        char header[256];
        snprintf(header, sizeof(header), "Content-Type: %s", content_type);
        headers = curl_slist_append(headers, header);
    }
    
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "PUT");
    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, data);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &resp);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "MonkeyHTTP/1.0");
    
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

// HTTP DELETE request
char* monkey_http_delete(const char* url) {
    CURL* curl = curl_easy_init();
    if (!curl) return NULL;
    
    Response resp = { .data = malloc(1), .size = 0 };
    resp.data[0] = '\0';
    
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "DELETE");
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &resp);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 30L);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "MonkeyHTTP/1.0");
    
    CURLcode res = curl_easy_perform(curl);
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
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 10L);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "MonkeyHTTP/1.0");
    
    CURLcode res = curl_easy_perform(curl);
    
    long status = 0;
    if (res == CURLE_OK) {
        curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &status);
    }
    
    curl_easy_cleanup(curl);
    return (int)status;
}

// Download file
int monkey_http_download(const char* url, const char* output_path) {
    CURL* curl = curl_easy_init();
    if (!curl) return -1;
    
    FILE* fp = fopen(output_path, "wb");
    if (!fp) return -1;
    
    curl_easy_setopt(curl, CURLOPT_URL, url);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 60L);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "MonkeyHTTP/1.0");
    
    CURLcode res = curl_easy_perform(curl);
    
    fclose(fp);
    curl_easy_cleanup(curl);
    
    return res == CURLE_OK ? 0 : -1;
}

// Free memory allocated by HTTP functions
void monkey_http_free(void* ptr) {
    free(ptr);
}