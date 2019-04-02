;; Copyright © 2014 FMAP SVERIGE AB

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

(defpackage :eclastic
  (:documentation
   "Import and re-export the symbols that make up the public interface
   of Eclastic.")
  (:use :eclastic.bulk
        :eclastic.document
        :eclastic.generic
        :eclastic.index
        :eclastic.query
        :eclastic.search
        :eclastic.settings
        :eclastic.server
        :eclastic.script)
           ;;generic
  (:export :get*
           :index
           :create
           :update
           :delete*

           ;;server
           :<server>
           :host
           :port
           :<index>
           :index-name
           :<type>
           :type-name

           ;;document
           :<document>
           :document-id
           :document-source
           :document-highlight
           :version
           :routing
           :parent-of
           :document-not-found
           :document-with-id
           :document-by-id

           ;;search
           :<simple-search>
           :simple-search
           :<search>
           :new-search
           :sort-by

           ;;script
           :<script>
           :define-script
           :encode-script

           ;;bulk
           :with-bulk-documents

           ;;index
           :<index-settings>
           :create-index-settings

           ;;query
           :<match>
           :match
           :<multi-match>
           :multi-match
           :<highlight>
           :highlight

           ;;settings
           :<field>
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
