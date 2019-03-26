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

(defpackage :eclastic.settings
  (:use :cl)
  (:import-from :yason
                :with-array
                :with-object
                :with-object-element
                :encode-array-element
                :encode-object
                :encode-object-element
                :encode-slots)
  (:export :<field>
           :create-field
           :<mappings>
           :create-mappings
           :<ngram-tokenizer>
           :create-ngram-tokenizer
           :<property>
           :create-property
           :<settings>
           :create-settings
           :<standard-tokenizer>
           :create-standard-tokenizer))

(in-package :eclastic.settings)

(defclass <tokenizer> ()
  ((type :initarg :type
         :initform "standard"
         :accessor :<tokenizer>-type)))

(defclass <standard-tokenizer> (<tokenizer>)
  ((max-token-length :initarg :max_token_length
                     :accessor :<standard-tokenizer>-max-token-length)))

(defmethod encode-slots progn ((this <standard-tokenizer>))
  (encode-object-element "type" (:<tokenizer>-type this))
  (encode-object-element
    "max_token_length" (:<standard-tokenizer>-max-token-length this)))

(defun create-standard-tokenizer (max-token-length)
  (make-instance '<standard-tokenizer>
                 :max-token-length max-token-length))

(defclass <ngram-tokenizer> (<tokenizer>)
  ((type :initform "ngram")
   (min-gram :initarg :min-gram
             :accessor :<ngram-tokenizer>-min-gram)
   (max-gram :initarg :max-gram
             :accessor :<ngram-tokenizer>-max-gram)
   (token-chars :initarg :token-chars
                :accessor :<ngram-tokenizer>-token-chars)))

(defmethod encode-slots progn ((this <ngram-tokenizer>))
  (encode-object-element "type" (:<tokenizer>-type this))
  (encode-object-element "min_gram" (:<ngram-tokenizer>-min-gram this))
  (encode-object-element "max_gram" (:<ngram-tokenizer>-max-gram this))
  (with-object-element ("token_chars")
    (with-array ()
      (dolist (token-char (:<ngram-tokenizer>-token-chars this))
        (encode-array-element token-char)))))

(defun create-ngram-tokenizer (min-gram max-gram token-chars)
  (make-instance '<ngram-tokenizer>
                 :min-gram min-gram
                 :max-gram max-gram
                 :token-chars token-chars))

(defclass <settings> ()
  ((tokenizer :initarg :tokenizer
              :accessor :<settings>-tokenizer)
   (tokenizer-name :initarg :tokenizer-name
                   :accessor :<settings>-tokenizer-name)
   (analyzer-name :initarg :analyzer-name
                  :accessor :<settings>-analyzer-name)))

(defmethod encode-slots progn ((this <settings>))
  (with-object-element ("analysis")
    (with-object ()
     (with-object-element ("analyzer")
       (with-object ()
         (with-object-element ((:<settings>-analyzer-name this))
           (with-object ()
             (encode-object-element "tokenizer" (:<settings>-tokenizer-name this))))))
     (with-object-element ("tokenizer")
       (with-object ()
         (with-object-element ((:<settings>-tokenizer-name this))
           (encode-object (:<settings>-tokenizer this))))))))

(defun create-settings (tokenizer tokenizer-name analyzer-name)
  (make-instance '<settings>
                 :tokenizer tokenizer
                 :tokenizer-name tokenizer-name
                 :analyzer-name analyzer-name))

(defclass <field> ()
  ((name :initarg :name
         :accessor :<field>-name)
   (type :initarg :type
         :accessor :<field>-type)
   (analyzer :initarg :analyzer
             :accessor :<field>-analyzer)))

(defmethod encode-slots progn ((this <field>))
  (with-object-element ((:<field>-name this))
    (with-object ()
     (encode-object-element "type" (:<field>-type this))
     (encode-object-element "analyzer" (:<field>-analyzer this)))))

(defun create-field (name type analyzer)
  (make-instance '<field>
                 :name name
                 :type type
                 :analyzer analyzer))

(defclass <property> ()
  ((name :initarg :name
         :accessor :<property>-name)
   (type :initarg :type
         :accessor :<propterty>-type)
   (fields :initarg :fields
           :accessor :<property>-fields)))

(defmethod encode-slots progn ((this <property>))
  (encode-object-element "type" (:<propterty>-type this))
  (with-object-element ("fields")
    (dolist (field (:<property>-fields this))
      (encode-object field))))

(defun create-property (name type fields)
  (make-instance '<property>
                 :name name
                 :type type
                 :fields fields))

(defclass <mappings> ()
  ((type :initarg :type
         :accessor :<mappings>-type)
   (properties :initarg :properties
               :accessor :<mappings>-properties)))

(defmethod encode-slots progn ((this <mappings>))
  (with-object-element ((:<mappings>-type this))
    (with-object ()
      (with-object-element ("properties")
        (with-object ()
          (dolist (property (:<mappings>-properties this))
            (with-object-element ((:<property>-name property))
              (encode-object property))))))))

(defun create-mappings (type properties)
  (make-instance '<mappings>
                 :type type
                 :properties properties))
