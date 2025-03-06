;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2023 Miguel Ángel Moreno <me@mianmoreno.com>
;;;
;;; This file is part of rde.
;;;
;;; rde is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; rde is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rde.  If not, see <http://www.gnu.org/licenses/>.

(define-module (rde features web)
  #:use-module (rde features)
  #:use-module (rde predicates)
  #:export (feature-alternative-frontends))

(define* (feature-alternative-frontends
          #:key
          (youtube-frontend "https://invidious.snopyta.org")
          (reddit-frontend "https://teddit.net")
          (instagram-frontend "https://picuki.com")
          (quora-frontend "https://quora.vern.cc")
          (google-frontend "https://search.sethforprivacy.com")
          (imgur-frontend "https://rimgo.vern.cc")
          (medium-frontend "https://scribe.rip")
          (twitter-frontend "https://nitter.namazso.eu")
          (tiktok-frontend "https://tok.artemislena.eu")
          (fandom-frontend "https://bw.vern.cc")
          (github-frontend "https://gh.odyssey346.dev"))
  (ensure-pred maybe-string? youtube-frontend)
  (ensure-pred maybe-string? reddit-frontend)
  (ensure-pred maybe-string? instagram-frontend)
  (ensure-pred maybe-string? quora-frontend)
  (ensure-pred maybe-string? google-frontend)
  (ensure-pred maybe-string? imgur-frontend)
  (ensure-pred maybe-string? medium-frontend)
  (ensure-pred maybe-string? twitter-frontend)
  (ensure-pred maybe-string? tiktok-frontend)
  (ensure-pred maybe-string? fandom-frontend)
  (ensure-pred maybe-string? github-frontend)

  (feature
   (name 'alternative-frontends)
   (values (append
            `((alternative-frontends . #t))
            (make-feature-values
             youtube-frontend reddit-frontend instagram-frontend
             quora-frontend google-frontend imgur-frontend
             medium-frontend twitter-frontend tiktok-frontend
             fandom-frontend github-frontend)))))
