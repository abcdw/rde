(define-module (rde-configs users abcdw emacs)
  #:export (telega-config))

(define telega-config
  `((with-eval-after-load 'telega
      ;; (setopt telega-proxies
      ;;         '((:type (:@type "proxyTypeSocks5")
      ;;            :server "localhost" :port "8123" :enable t)))
      )))
