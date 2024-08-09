;;; rde --- Reproducible development environment.
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later
;;;
;;; Copyright © 2021, 2022, 2023, 2024 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2021 Demis Balbach <db@minikn.xyz>
;;; Copyright © 2022 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Miguel Ángel Moreno <me@mianmoreno.com>

(define-module (rde features mail providers)
  #:use-module (guix gexp)
  #:export (default-providers-settings
            generic-folder-mapping
            add-default-values-to-provider-settings))

;; Directory names has lowercased spelling to match notmuch tags
(define generic-folder-mapping
  ;; https://www.rfc-editor.org/rfc/rfc6154#section-2
  '(("inbox"   . "INBOX")
    ("sent"    . "Sent")
    ("drafts"  . "Drafts")
    ("archive" . "Archive")
    ("trash"   . "Trash")
    ("spam"    . "Junk")))

(define generic+spam-folder-mapping
  '(("inbox"   . "INBOX")
    ("sent"    . "Sent")
    ("drafts"  . "Drafts")
    ("archive" . "Archive")
    ("trash"   . "Trash")
    ("spam"    . "Spam")))

(define gmail-folder-mapping
  '(("inbox"   . "INBOX")
    ("sent"    . "[Gmail]/Sent Mail")
    ("drafts"  . "[Gmail]/Drafts")
    ("archive" . "[Gmail]/All Mail")
    ("trash"   . "[Gmail]/Trash")
    ("spam"    . "[Gmail]/Spam")))

(define gmx-fr-folder-mapping
  '(("inbox"   . "INBOX")
    ("sent"    . "Envoy&AOk-s")
    ("drafts"  . "Brouillons")
    ("archive" . "Archive")
    ("trash"   . "Corbeille")
    ("spam"    . "Junk")))

(define hosteurope-de-folder-mapping
  '(("inbox"   . "INBOX")
    ("sent"    . "Sent")
    ("drafts"  . "Entwurf")
    ("archive" . "All")
    ("trash"   . "Trash")
    ("spam"    . "Spam")))

(define outlook-fr-folder-mapping
  '(("inbox"   . "INBOX")
    ("sent"    . "&AMk-l&AOk-ments envoy&AOk-s") ;"Éléments envoyés"
    ("drafts"  . "Brouillons")
    ("archive" . "Notes")
    ("trash"   . "&AMk-l&AOk-ments supprim&AOk-s") ;"Éléments supprimés"
    ("spam"    . "Courrier ind&AOk-sirable"))) ;"Courrier indésirable"

;; TODO: [Andrew Tropin, 2024-08-08] Change junk to spam
(define mailbox-folder-mapping
  '(("inbox"   . "INBOX")
    ("sent"    . "Sent")
    ("drafts"  . "Drafts")
    ("archive" . "Archive")
    ("trash"   . "Trash")
    ("junk"    . "Junk")))

(define default-providers-settings
  ;; Keep alphabet order.
  `((dismail
     (smtp (host . "smtp.dismail.de")
           (starttls? . #t))
     (imap (host . "imap.dismail.de")))

    (fastmail
     (smtp (host . "smtp.fastmail.com"))
     (imap (host . "imap.fastmail.com")
           (folder-mapping . ,generic+spam-folder-mapping)))

    (gandi (smtp (host . "mail.gandi.net")
                 (starttls? . #t))
           (imap (host . "mail.gandi.net")))

    (gmail (smtp (host . "smtp.gmail.com")
                 (starttls? . #t))
           (imap (host . "imap.gmail.com")
                 (folder-mapping . ,gmail-folder-mapping)))

    (gmx-fr
     (smtp (host . "mail.gmx.net")
           (starttls . #t))
     (imap (host . "imap.gmx.net")
           (folder-mapping . ,gmx-fr-folder-mapping)))

    (hosteurope-de
     (smtp (host . "smtp.hosteurope.de")
           (starttls? . #t))
     (imap (host . "imap.hosteurope.de")
           (folder-mapping . ,hosteurope-de-folder-mapping)))

    (mailbox
     (smtp (host . "smtp.mailbox.org")
           (starttls? . #t))
     (imap (host . "imap.mailbox.org")
           (folder-mapping . ,mailbox-folder-mapping)))

    (migadu (smtp (host . "smtp.migadu.com"))
            (imap (host . "imap.migadu.com")))

    (ovh (smtp (host . "ssl0.ovh.net"))
         (imap (host . "ssl0.ovh.net")
               (extra-options (subfolders . Legacy)
                              (auth-mechs . LOGIN))))

    (ovh-pro2-fr
     (smtp (host . "pro2.mail.ovh.net")
           (starttls? . #t))
     (imap (host . "pro2.mail.ovh.net")
           (folder-mapping . ,outlook-fr-folder-mapping)
           (extra-options (subfolders . Legacy)
                          (auth-mechs . LOGIN))))

    (posteo
     (smtp (host . "posteo.de")
           (starttls? . #t))
     (imap (host . "posteo.de")))

    (runbox
     (smtp (host . "mail.runbox.com")
           (starttls? . #t))
     (imap (host . "mail.runbox.com")
           (folder-mapping . ,generic+spam-folder-mapping)))))

(define (add-default-values-to-provider-settings provider-mail-settings)
  "Add default values to mail-providers-settings.

TLDR: Use 465 SMTP port, where possible.

Usually, the default SMTP port is 587, it supports both unencrypted and
encrypted with starttls mode of operation.  It potentially can be spuffed to
established unencrypted connection, thus 465 is preffered by default as it
always encrypted.  If port 587 is needed it must be specified explicitly via
@code{port} option or just by setting @code{starttls} option to @code{#t}.

Default IMAP port is 993 and it's encrypted by default.

If folder-mapping is not specified the generic-folder-mapping one is used."
  (define (ensure-folder-mapping imap)
    (if (assoc-ref imap 'folder-mapping)
        imap
        (acons 'folder-mapping generic-folder-mapping imap)))

  (define (ensure-starttls smtp)
    (if (assoc-ref smtp 'starttls?)
        smtp
        (acons 'starttls? #f smtp)))

  (define (enrich-settings settings)
    (let ((smtp (assoc-ref settings 'smtp))
          (imap (assoc-ref settings 'imap)))
      `((smtp . ,(ensure-starttls smtp))
        (imap . ,(ensure-folder-mapping imap)))))

  (let ((provider (car provider-mail-settings))
               (settings (cdr provider-mail-settings)))
           (if settings
               `(,provider . ,(enrich-settings settings))
               provider-mail-settings)))
