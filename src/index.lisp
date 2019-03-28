;; Copyright © 2018 fiv0

;; This file is part of Eclastic

;; Eclastic is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; Eclastic is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with Eclastic.  If not, see
;; <http://www.gnu.org/licenses/>.

(in-package :cl-user)

(defpackage :eclastic.index
  (:use :cl
        :eclastic.generic
        :eclastic.server
        :eclastic.settings
        :eclastic.util)
  (:import-from :yason
                :encode-object
                :encode-slots
                :with-object-element
                :with-output-to-string*)
  (:export :<index-settings>
           :create-index-settings))

(in-package :eclastic.index)

(defclass <index-settings> ()
  ((settings :initarg :settings
             :accessor :<index-settings>-settings)
   (mappings :initarg :mappings
             :accessor :<index-settings>-mappings)))

(defmethod encode-slots progn ((this <index-settings>))
  (with-object-element ("settings")
    (encode-object (:<index-settings>-settings this)))
  (with-object-element ("mappings")
    (encode-object (:<index-settings>-mappings this))))

(defun create-index-settings (settings mappings)
  (make-instance '<index-settings>
                 :settings settings
                 :mappings mappings))

(defmethod create ((place <server>) (settings <index-settings>))
  (let* ((result
          (send-request (format nil "~A"
                                (get-uri place))
                        :put
                        :data (with-output-to-string* ()
                                (encode-object settings)))))
    result))

(defmethod delete* ((place <server>) (settings <index-settings>))
  (let* ((result
          (send-request (format nil "~A"
                                (get-uri place))
                        :delete)))
    result))
