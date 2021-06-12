(define-module (gnu home-services ssh)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home-services files)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages ssh)
  #:use-module (guix packages)
  #:use-module (guix import utils)
  #:use-module (guix i18n)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix records)

  #:export (home-ssh-service-type
            home-ssh-configuration
	    home-ssh-configuration?
            ssh-host
            ssh-match))

;;; Commentary:
;;;
;;; Service to install and configure SSH.
;;;
;;; Code:

;; symbol-name => SymbolName
(define (uglify-field-name field-name)
  "Convert symbol FIELD-NAME to an upper camel case string.
@code{symbol-name} => \"@code{SymbolName}\"."
  (let ((str (symbol->string field-name)))
    (apply string-append (map string-capitalize (string-split str #\-)))))

(define* (serialize-field field-name val #:key (toplevel? #f))
  (cond
   ((boolean? val) (serialize-boolean field-name val))
   ((list? val) (serialize-list field-name val))
   (else 
    (let ((field-name (uglify-field-name field-name)))
      (if (or (member field-name '("Host" "Match"))
              toplevel?)
          (format #f "~a ~a\n" field-name val)
          (format #f "  ~a ~a\n" field-name val))))))

;; '(first second) => first second
(define (serialize-list field-name val)
  (if (null? val) "" (serialize-field field-name (string-join val " "))))

(define serialize-listof-strings serialize-list)

(define (serialize-alist field-name val)
  (generic-serialize-alist string-append serialize-field val))

(define (serialize-toplevel-alist field-name val)
  (generic-serialize-alist
   string-append
   (lambda (field-name val)
     (serialize-field field-name val #:toplevel? #t))
   val))

(define (serialize-extra-config field-name val)
  (define serialize-extra-config-entry
    (match-lambda
      ((host name alist)
       (list
        (serialize-field host name)
        (serialize-alist #f alist)))))
  (apply string-append (append-map serialize-extra-config-entry val)))

;; #t => yes, #f => no
(define (serialize-boolean field-name val)
  (serialize-field field-name (boolean->yes-or-no val)))

(define serialize-string serialize-field)
(define ssh-host-or-ssh-match? list?)

;; TODO: Add docs for these records
(define-record-type <ssh-host>
  (ssh-host host options)
  ssh-host?
  (host ssh-host-host)
  (options ssh-host-options))

(define-record-type <ssh-match>
  (ssh-match match options)
  ssh-match?
  (match ssh-match-match)
  (options ssh-match-options))

(define %ssh-match-keywords
  '(all canonical final exec host originalhost user localuser))

(define %ssh-standalone-keywords
  '(all canonical final))

(define validate-match-block
  (match-lambda
    ((keyword rest ...)
     (if (member keyword %ssh-match-keywords)
         #t
         (raise (formatted-message
                 (G_ "Match keyword must be one of the following ~a")
                 %ssh-match-keywords))))))

(define serialize-ssh-host
  (match-lambda
    (($ <ssh-host> host options)
     (string-append
      (serialize-field 'host host)
      (serialize-alist #f options)))))

(define serialize-ssh-match
  (match-lambda
    (($ <ssh-match> match options)
     (when (validate-match-block match)
       (string-append
        (serialize-field 'match
                         (if (member (car match) %ssh-standalone-keywords)
                             (format #f "~a" (car match))
                             (format #f "~a \"~a\"" (car match) (cadr match))))
        (serialize-alist #f options))))))

(define (serialize-ssh-host-or-ssh-match field-name val)
  (apply string-append
         (map (lambda (entry)
                (if (ssh-host? entry)
                    (serialize-ssh-host entry)
                    (serialize-ssh-match entry)))
              val)))

(define-configuration home-ssh-configuration
  (package
    (package openssh)
    "The SSH package to use.")
  (host
   (string "*")
   "Name of the default host.")
  (user-known-hosts-file
   (listof-strings '("~/.ssh/known_hosts"))
   "One or more files to use for the user host key database.")
  (forward-agent
   (boolean #f)
   "Whether the connection to the authentication agent will be forwarded
to the remote machine.")
  ;; TODO: Maybe we could add fields for some enums?
  ;; (AddressFamily, FingerprintHash)
  (default-options
    (alist '())
    "Configuration options for the default host.  This should be an
associative list representing a key-value pair.  A configuration like this:

@lisp
(home-ssh-configuration
  (host \"*\")
  (default-options
    '((add-keys-to-agent . #t)
      (address-family . \"inet\"))))
@end lisp

would turn into this:

@example
Host *
  AddKeysToAgent yes
  AddressFamily inet
@end example")
  (toplevel-options
   (alist '())
   "Association list of toplevel configuration options.  The configuration below:

@lisp
(home-ssh-configuration
  (toplevel-options
    '((include . \"/some/path/to/file\"))))
@end lisp

would this:

@example
Include /some/path/to/file
@end example"
   serialize-toplevel-alist)
  (extra-config
   (ssh-host-or-ssh-match '())
   "List of configurations for other hosts.  Something like this:

@lisp
(home-ssh-configuration
 (extra-config
  (list (ssh-host \"savannah\"
                  '((compression . #f)
                    (ciphers . (\"3des-cbc\" \"aes256-ctr\"))
                    (identity-file . \"~/.ssh/keys.d/id_rsa\")
                    (server-alive-count-max . 3)))
        (ssh-match '(exec \"grep key secret.txt\")
                   '((compression . #t))))))
@end lisp

will turn into this:

@example
Host savannah
  Compression no
  Ciphers 3des-cbc aes256-ctr
  IdentityFile ~/.ssh/keys.d/id_rsa
  ServerAliveCountMax 3
Match exec \"grep key secret.txt\"
  Compression yes
@end example"))

(define (add-ssh-configuration config)
  `(("ssh/config"
     ,(mixed-text-file "ssh-config"
                       (serialize-configuration
                        config
                        home-ssh-configuration-fields)))))

(define (add-ssh-packages config)
  (list (home-ssh-configuration-package config)))

(define home-ssh-service-type
  (service-type (name 'home-ssh)
                (extensions
                 (list (service-extension
                        home-files-service-type
                        add-ssh-configuration)
                       (service-extension
                        home-profile-service-type
                        add-ssh-packages)))
                (default-value (home-ssh-configuration))
                (description "Install and configure SSH")))

(define (generate-home-ssh-documentation)
  (generate-documentation
   `((home-ssh-configuration
      ,home-ssh-configuration-fields))
   'home-ssh-configuration))
