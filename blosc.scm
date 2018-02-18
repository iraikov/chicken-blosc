#|

Chicken Scheme bindings for the Blosc compression library.
Written for Chicken Scheme by Ivan Raikov.

|#

(module blosc
        
        (
         initialize!
         compress
         compress!
         decompress
         decompress!
         sizes
         set-nthreads!
         set-compressor!
         free-resources!
         version-format
         max-threads
         max-overhead
         )

(import scheme chicken foreign)
(require-extension srfi-4)        
(import (only extras printf))
;        (only srfi-4 make-u32vector u32vector->list subu8vector blob->u8vector/shared u8vector->blob/shared))

#>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <blosc.h>

#define C_bytevector_length(x)      (C_header_size(x))
<#


(define version-format (foreign-value "BLOSC_VERSION_FORMAT" int))
(define max-overhead (foreign-value "BLOSC_MAX_OVERHEAD" int))
(define do-shuffle (foreign-value "BLOSC_DOSHUFFLE" int))
(define mem-cpyed (foreign-value "BLOSC_MEMCPYED" int))
(define max-threads (foreign-value "BLOSC_MAX_THREADS" int))
        
(define max-typesize 255)
(define max-buffersize 4294967295)

;; Initializes the BLOSC compressor
(define (initialize!)
    ((foreign-lambda* void ()
#<<END
blosc_init();
END
)))

(define blosc-compress
(foreign-safe-lambda* int ((int level) (int shuffle) (unsigned-int itemsize) 
                              (scheme-object dest) (scheme-object src))
#<<END
     int result=0; size_t srcsize=0, destsize=0;
     void *srcdata=NULL, *destdata=NULL;

     C_i_check_bytevector (src);
     C_i_check_bytevector (dest);
     
     srcsize   = C_bytevector_length(src);
     srcdata   = C_c_bytevector (src);
     destsize  = C_bytevector_length(dest);
     destdata  = C_c_bytevector (dest);

     
     result = blosc_compress(level, shuffle, itemsize, srcsize, srcdata, destdata, destsize);
     if (result < 0)
     {
        printf("Blosc compression error.  Error code: %d\n", result);
     }
     assert(result >= 0);
     
     C_return (result);
END
))


(define (compress! dest src #!key (level 5) (shuffle #t) (itemsize 1))
  (if (< itemsize 0)
      (error 'compress! "item size must be positive"))
  (if (or (< level 0) (> level 9))
      (error 'compress! "level must be between 0 and 9, inclusive"))
  (blosc-compress level (if shuffle 1 0) itemsize dest src))


(define (compress src #!key (level 5) (shuffle #t) (itemsize 1))
  (assert (< 0 (blob-size src)))
  (let* ((dest (make-blob (+ (blob-size src) max-overhead)))
         (sz   (compress! dest src level: level shuffle: shuffle itemsize: itemsize)))
    (u8vector->blob/shared (subu8vector (blob->u8vector/shared dest) 0 sz))))


;; Given a compressed buffer, return the (uncompressed, compressed, block) size
(define cbuffer-sizes
  (foreign-safe-lambda* void ((scheme-object buffer) (u32vector sizes))
#<<END
size_t nbytes=0, cbytes=0, blocksize=0;
void *cbuffer = NULL;

C_i_check_bytevector (buffer);
cbuffer  = C_c_bytevector (buffer);

blosc_cbuffer_sizes(cbuffer, &nbytes, &cbytes, &blocksize);
sizes[0] = nbytes;
sizes[1] = cbytes;
sizes[2] = blocksize;
END
))

;; Given a compressed buffer `buf`, return a tuple
;; of the `(uncompressed, compressed, block)` sizes in bytes.

(define (sizes buf)
  (let ((res (make-u32vector 3 0)))
    (cbuffer-sizes buf res)
    (u32vector->list res)
    ))
    
  
(define blosc-decompress
  (foreign-safe-lambda* int ((scheme-object dest) (scheme-object src))
#<<END
     int result=0;  size_t destsize=0;
     void *srcdata=NULL, *destdata=NULL;

     C_i_check_bytevector (src);
     C_i_check_bytevector (dest);
     
     srcdata   = C_c_bytevector (src);
     destdata  = C_c_bytevector (dest);
     destsize  = C_bytevector_length(dest);
     
     result = blosc_decompress(srcdata, destdata, destsize);
     if (result < 0)
     {
        printf("Blosc decompression error.  Error code: %d\n", result);
     }
     assert(result >= 0);
     
     C_return (result);
END
))

(define (decompress! dest src)
  (let* ((uncompressed-sz (car (sizes src)))
         (len (blob-size dest)))
    (if (not (<= uncompressed-sz len))
        (error 'decompress! "destination buffer is too small"))
    (blosc-decompress dest src)
    dest))


(define (decompress src)
  (let* ((uncompressed-sz (car (sizes src))))
    (decompress! (make-blob uncompressed-sz) src)))
  
;; Initialize a pool of threads for compression / decompression.
;; If `nthreads` is 1, the the serial version is chosen and a possible previous existing pool is ended.
;; If this function is not callled, `nthreads` is set to 1 internally.

(define (set-nthreads! n)
    ((foreign-lambda* void ((int n))
#<<END
blosc_set_nthreads(n);
END
) n))


;; Set the current compression algorithm to `s`.  The currently supported
;; algorithms in the default Blosc module build are `"blosclz"`, `"lz4"`,
;; and `"l4hc"`.   Throws an error if `s` is not the name
;; of a supported algorithm.  Returns a nonnegative integer code used
;; internally by Blosc to identify the compressor.
(define (set-compressor! s)
  (case s (("blosclz" "lz4" "l4hc")
           ((foreign-lambda* void ((c-string s))
#<<END
blosc_set_compressor(s);
END
) s))
        (else (error 'set-compressor! "unrecognized compression algorithm" s))
        ))
           

;; Free possible memory temporaries and thread resources.
;; Use this when you are not going to use Blosc for a long while.
;; In case of problems releasing resources, it returns `false`,
;; whereas it returns `true` on success.

(define (free-resources!)
    ((foreign-lambda* int ()
#<<END
int result = blosc_free_resources();
C_return(result);
END
)))

)
