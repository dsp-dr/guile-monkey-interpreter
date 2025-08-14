;;; ffi-bridge.scm - Bridge between C extensions and Monkey
(define-module (monkey extensions ffi)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:export (;; Filesystem functions
            read-file
            write-file
            file-exists?
            delete-file
            file-size
            is-directory?
            make-directory
            list-directory
            get-cwd
            change-directory
            ;; HTTP functions
            http-init
            http-cleanup
            http-get
            http-post
            http-put
            http-delete
            http-status
            http-download))

;;; Load shared libraries
;;; Try to load from current directory first, then system paths
(define fs-lib 
  (catch #t
    (lambda () (dynamic-link "./libmonkey_fs"))
    (lambda _ 
      (catch #t
        (lambda () (dynamic-link "libmonkey_fs"))
        (lambda _ #f)))))

(define http-lib
  (catch #t
    (lambda () (dynamic-link "./libmonkey_http"))
    (lambda _
      (catch #t
        (lambda () (dynamic-link "libmonkey_http"))
        (lambda _ #f)))))

;;; Filesystem C function bindings
(define c-read-file
  (and fs-lib
       (pointer->procedure '* 
                          (dynamic-func "monkey_read_file" fs-lib)
                          (list '*))))

(define c-write-file
  (and fs-lib
       (pointer->procedure int
                          (dynamic-func "monkey_write_file" fs-lib)
                          (list '* '*))))

(define c-file-exists
  (and fs-lib
       (pointer->procedure int
                          (dynamic-func "monkey_file_exists" fs-lib)
                          (list '*))))

(define c-delete-file
  (and fs-lib
       (pointer->procedure int
                          (dynamic-func "monkey_delete_file" fs-lib)
                          (list '*))))

(define c-file-size
  (and fs-lib
       (pointer->procedure long
                          (dynamic-func "monkey_file_size" fs-lib)
                          (list '*))))

(define c-is-directory
  (and fs-lib
       (pointer->procedure int
                          (dynamic-func "monkey_is_directory" fs-lib)
                          (list '*))))

(define c-mkdir
  (and fs-lib
       (pointer->procedure int
                          (dynamic-func "monkey_mkdir" fs-lib)
                          (list '* int))))

(define c-list-dir
  (and fs-lib
       (pointer->procedure '*
                          (dynamic-func "monkey_list_dir" fs-lib)
                          (list '* '*))))

(define c-getcwd
  (and fs-lib
       (pointer->procedure '*
                          (dynamic-func "monkey_getcwd" fs-lib)
                          '())))

(define c-chdir
  (and fs-lib
       (pointer->procedure int
                          (dynamic-func "monkey_chdir" fs-lib)
                          (list '*))))

(define c-free
  (and fs-lib
       (pointer->procedure void
                          (dynamic-func "monkey_free" fs-lib)
                          (list '*))))

(define c-free-array
  (and fs-lib
       (pointer->procedure void
                          (dynamic-func "monkey_free_array" fs-lib)
                          (list '* int))))

;;; HTTP C function bindings
(define c-http-init
  (and http-lib
       (pointer->procedure int
                          (dynamic-func "monkey_http_init" http-lib)
                          '())))

(define c-http-cleanup
  (and http-lib
       (pointer->procedure void
                          (dynamic-func "monkey_http_cleanup" http-lib)
                          '())))

(define c-http-get
  (and http-lib
       (pointer->procedure '*
                          (dynamic-func "monkey_http_get" http-lib)
                          (list '*))))

(define c-http-post
  (and http-lib
       (pointer->procedure '*
                          (dynamic-func "monkey_http_post" http-lib)
                          (list '* '* '*))))

(define c-http-put
  (and http-lib
       (pointer->procedure '*
                          (dynamic-func "monkey_http_put" http-lib)
                          (list '* '* '*))))

(define c-http-delete
  (and http-lib
       (pointer->procedure '*
                          (dynamic-func "monkey_http_delete" http-lib)
                          (list '*))))

(define c-http-status
  (and http-lib
       (pointer->procedure int
                          (dynamic-func "monkey_http_status" http-lib)
                          (list '*))))

(define c-http-download
  (and http-lib
       (pointer->procedure int
                          (dynamic-func "monkey_http_download" http-lib)
                          (list '* '*))))

(define c-http-free
  (and http-lib
       (pointer->procedure void
                          (dynamic-func "monkey_http_free" http-lib)
                          (list '*))))

;;; Initialize HTTP if available
(when c-http-init
  (c-http-init))

;;; Scheme wrapper functions - Filesystem

(define (read-file path)
  "Read file contents as string"
  (if (not c-read-file)
      (error "Filesystem extension not available")
      (let* ((path-ptr (string->pointer path))
             (result-ptr (c-read-file path-ptr)))
        (if (null-pointer? result-ptr)
            #f
            (let ((content (pointer->string result-ptr)))
              (c-free result-ptr)
              content)))))

(define (write-file path content)
  "Write string to file"
  (if (not c-write-file)
      (error "Filesystem extension not available")
      (let ((path-ptr (string->pointer path))
            (content-ptr (string->pointer content)))
        (= 0 (c-write-file path-ptr content-ptr)))))

(define (file-exists? path)
  "Check if file exists"
  (if (not c-file-exists)
      (error "Filesystem extension not available")
      (let ((path-ptr (string->pointer path)))
        (= 1 (c-file-exists path-ptr)))))

(define (delete-file path)
  "Delete file"
  (if (not c-delete-file)
      (error "Filesystem extension not available")
      (let ((path-ptr (string->pointer path)))
        (= 0 (c-delete-file path-ptr)))))

(define (file-size path)
  "Get file size in bytes"
  (if (not c-file-size)
      (error "Filesystem extension not available")
      (let ((path-ptr (string->pointer path)))
        (c-file-size path-ptr))))

(define (is-directory? path)
  "Check if path is a directory"
  (if (not c-is-directory)
      (error "Filesystem extension not available")
      (let ((path-ptr (string->pointer path)))
        (= 1 (c-is-directory path-ptr)))))

(define* (make-directory path #:optional (mode #o755))
  "Create a directory"
  (if (not c-mkdir)
      (error "Filesystem extension not available")
      (let ((path-ptr (string->pointer path)))
        (= 0 (c-mkdir path-ptr mode)))))

(define (list-directory path)
  "List directory contents"
  (if (not c-list-dir)
      (error "Filesystem extension not available")
      (let* ((path-ptr (string->pointer path))
             (count-bv (make-bytevector 4 0))
             (count-ptr (bytevector->pointer count-bv))
             (result-ptr (c-list-dir path-ptr count-ptr))
             (count (bytevector-s32-native-ref count-bv 0)))
        (if (null-pointer? result-ptr)
            '()
            (let loop ((i 0) (files '()))
              (if (>= i count)
                  (begin
                    (c-free-array result-ptr count)
                    (reverse files))
                  (let* ((ptr-addr (pointer-address result-ptr))
                         (file-ptr-addr (pointer->bytevector 
                                        (make-pointer (+ ptr-addr (* i 8)))
                                        8))
                         (file-ptr (make-pointer 
                                   (bytevector-u64-native-ref file-ptr-addr 0)))
                         (filename (pointer->string file-ptr)))
                    (loop (+ i 1) (cons filename files)))))))))

(define (get-cwd)
  "Get current working directory"
  (if (not c-getcwd)
      (error "Filesystem extension not available")
      (let ((result-ptr (c-getcwd)))
        (if (null-pointer? result-ptr)
            #f
            (let ((path (pointer->string result-ptr)))
              (c-free result-ptr)
              path)))))

(define (change-directory path)
  "Change current directory"
  (if (not c-chdir)
      (error "Filesystem extension not available")
      (let ((path-ptr (string->pointer path)))
        (= 0 (c-chdir path-ptr)))))

;;; Scheme wrapper functions - HTTP

(define (http-init)
  "Initialize HTTP library"
  (if (not c-http-init)
      (error "HTTP extension not available")
      (= 0 (c-http-init))))

(define (http-cleanup)
  "Cleanup HTTP library"
  (if (not c-http-cleanup)
      (error "HTTP extension not available")
      (c-http-cleanup)))

(define (http-get url)
  "HTTP GET request"
  (if (not c-http-get)
      (error "HTTP extension not available")
      (let* ((url-ptr (string->pointer url))
             (result-ptr (c-http-get url-ptr)))
        (if (null-pointer? result-ptr)
            #f
            (let ((content (pointer->string result-ptr)))
              (c-http-free result-ptr)
              content)))))

(define* (http-post url data #:optional (content-type "application/x-www-form-urlencoded"))
  "HTTP POST request"
  (if (not c-http-post)
      (error "HTTP extension not available")
      (let* ((url-ptr (string->pointer url))
             (data-ptr (string->pointer data))
             (type-ptr (string->pointer content-type))
             (result-ptr (c-http-post url-ptr data-ptr type-ptr)))
        (if (null-pointer? result-ptr)
            #f
            (let ((content (pointer->string result-ptr)))
              (c-http-free result-ptr)
              content)))))

(define* (http-put url data #:optional (content-type "application/json"))
  "HTTP PUT request"
  (if (not c-http-put)
      (error "HTTP extension not available")
      (let* ((url-ptr (string->pointer url))
             (data-ptr (string->pointer data))
             (type-ptr (string->pointer content-type))
             (result-ptr (c-http-put url-ptr data-ptr type-ptr)))
        (if (null-pointer? result-ptr)
            #f
            (let ((content (pointer->string result-ptr)))
              (c-http-free result-ptr)
              content)))))

(define (http-delete url)
  "HTTP DELETE request"
  (if (not c-http-delete)
      (error "HTTP extension not available")
      (let* ((url-ptr (string->pointer url))
             (result-ptr (c-http-delete url-ptr)))
        (if (null-pointer? result-ptr)
            #f
            (let ((content (pointer->string result-ptr)))
              (c-http-free result-ptr)
              content)))))

(define (http-status url)
  "Get HTTP status code"
  (if (not c-http-status)
      (error "HTTP extension not available")
      (let ((url-ptr (string->pointer url)))
        (c-http-status url-ptr))))

(define (http-download url output-path)
  "Download file from URL"
  (if (not c-http-download)
      (error "HTTP extension not available")
      (let ((url-ptr (string->pointer url))
            (path-ptr (string->pointer output-path)))
        (= 0 (c-http-download url-ptr path-ptr)))))