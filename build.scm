
(import (chicken base) (chicken format) (chicken process) (chicken process-context) srfi-13 compile-file)
(define args (command-line-arguments))

(define (blosc-try-compile header ldflags cppflags)
  (print "header: " header " cppflags: " cppflags " ldflags: " ldflags)
  (and (try-compile 
	(string-append header "\n" 
		       "int main(int argc, char **argv) { blosc_init(); return 0; }\n")

	ldflags: ldflags
	cflags: cppflags
        verbose: #t
	)
       (cons ldflags cppflags)))

(define-syntax blosc-test 
  (syntax-rules ()
    ((_ (flags ...))
     (condition-case (blosc-try-compile flags ...)
		     (t ()    #f)))))

(define blosc-dir (get-environment-variable "BLOSC_DIR"))

(define ld+cpp-options
  (or 
   (and blosc-dir (blosc-test ("#include <blosc.h>" 
                               (sprintf "-lblosc -L~S" (make-pathname blosc-dir "lib") )
                               (sprintf "-I~S -L~S" 
                                        (make-pathname blosc-dir "include") 
                                        (make-pathname blosc-dir "lib") ))
                              ))
   (blosc-test ("#include <blosc.h>" "-lblosc" ""))
   (blosc-test ("#include <blosc.h>" "-lblosc" "-I/usr/include/blosc"))
   (error "unable to figure out location of Blosc library; try setting environment variable BLOSC_DIR to the proper location")))

(define cmd (intersperse (append args (list (sprintf "-L \"~A\"" (car ld+cpp-options)) 
                                            (if (string-null? (cdr ld+cpp-options)) ""
                                                (sprintf "\"~A\"" (cdr ld+cpp-options)))))
                         " "))
(print "cmd = " cmd)
(system (string-concatenate cmd))
