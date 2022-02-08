(define-module (rde features bluetooth)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  ;#:use-module (gnu home-services bluetooth) ;; TODO implement as 'fork' (in rde), then upstream to guix home proper
  #:use-module (gnu services)
  #:use-module (gnu services desktop)

  #:export (feature-bluetooth)

  ;;#:re-export (home-bluetooth-configuration) ;; ^^ as above
  )

;; TODO ensure group "lp" exists and is applicable for USER

;; TODO debug faulty config
;; /var/log/messages:Oct 20 18:12:16 localhost shepherd[1]: Service bluetooth has been started.
;; /var/log/messages:Oct 20 18:12:16 localhost bluetoothd[18846]: Bluetooth daemon 5.55
;; /var/log/messages:Oct 20 18:12:16 localhost bluetoothd[18846]: src/main.c:parse_controller_config() Key file does not have group “Controller”
;; /var/log/messages:Oct 20 18:12:16 localhost bluetoothd[18846]: src/main.c:main() Unable to get on D-Bus
(define* (feature-bluetooth
	  #:key
	  ;;(bluetooth-configuration (home-bluetooth-configuration))
          (dual-mode #f)
          (auto-enable? #t)) ;; XXX should this stick to guix defaults, or tailor to ease for users?
  "Setup and configure Bluetooth."
  ;;(ensure-pred home-bluetooth-configuration? bluetooth-configuration)

  (define (bluetooth-home-services config)
    "Returns home services related to bluetooth."
    (list ;;(service bluetooth-service-type bluetooth-configuration)
     (bluetooth-service #:auto-enable? auto-enable?)))

  (feature
   (name 'bluetooth)
   (values '((bluetooth . #t)))
   ;; TODO port etc-service reference to make home-service > system-service
   (system-services-getter bluetooth-home-services)))

