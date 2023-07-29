(define-module (rde features python)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages python)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:export (feature-python))

(define* (feature-python
          #:key
          (python python-wrapper)
          (emacs-python-black emacs-python-black)
          (black? #f))
  "Configure python for emacs. If black? is #t, configure the
emacs-python-black package, which provides useful functions for formatting
python files."
  (ensure-pred file-like? python)
  (ensure-pred file-like? emacs-python-black)
  (ensure-pred boolean? black?)

  (define f-name 'python)

  (define (get-home-services config)
    (list
     (simple-service
      'add-python-home-package
      home-profile-service-type
      (list python))
     (rde-elisp-configuration-service
      f-name
      config
      `(,@(if black?
              '((eval-when-compile (require 'python-black))
                (add-hook 'python-mode 'python-black-on-save-mode-enable-dwim))
              '())

        ,@(if (get-value 'emacs-org config)
              `((with-eval-after-load 'org
                  (add-to-list 'org-structure-template-alist
                               '("py" . "src python")))
                (with-eval-after-load 'ob-core
                  (require 'ob-python))
                (with-eval-after-load 'ob-python
                  (setq org-babel-python-command
                        ,(file-append python "/bin/python"))))
              '()))
      #:elisp-packages
      (if black? (list emacs-python-black) '()))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
