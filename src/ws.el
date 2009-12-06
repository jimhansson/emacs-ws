(require 'nxml-parse)
(require 'nxml-util)
(require 'cl)
(require 'ido)
(require 'url)

(defun create-soap-request (location)
  (let* ((wsdl-nxml-tree (get-nxml-tree-for location)))
    (create-soap-request-for-tree wsdl-nxml-tree location)))

(defun create-soap-request-for-tree (wsdl-nxml-tree location)
  (let* ((wsdl-ns (parse-wsdl-tree wsdl-nxml-tree location))
         (services (get-service-names wsdl-ns)))
    (cond ((equal (length services) 0)
           (error "No services found"))
          ((equal (length services) 1)
           (create-soap-request-for-service wsdl-ns (car services)))
          (t (create-soap-request-for-service wsdl-ns (select-with-ido services "Select service: "))))))

(defun create-soap-request-for-service (wsdl-ns service-name)
  (let* ((service (get-service wsdl-ns service-name))
         (ports (funcall service 'get-port-names)))
    (cond ((equal (length ports) 0)
           (error (concat "No ports found in service " service-name)))
          ((equal (length ports) 1)
           (create-soap-request-for-port wsdl-ns service (car ports)))
          (t (create-soap-request-for-port wsdl-ns service (select-with-ido ports "Select port: "))))))

(defun create-soap-request-for-port (wsdl-ns service port-name)
  (let* ((location (funcall service 'get-port-location port-name))
         (binding (funcall service 'get-binding port-name))
         (operations (funcall binding 'get-operation-names)))
    (cond ((equal (length operations) 0)
           (error (concat "No operations found for port " port-name)))
          ((equal (length operations) 1)
           (create-soap-request-for-binding wsdl-ns binding (car operations) location))
          (t (create-soap-request-for-binding 
              wsdl-ns
              binding 
              (select-with-ido operations "Select operation: ")
              location)))))

(defun create-soap-request-for-binding (wsdl-ns binding operation-name location)
  (list location 
        (funcall binding 'get-soap-action operation-name)
        (funcall binding 'get-request operation-name)))

(defun parse-wsdl (path) (parse-wsdl-tree (nxml-parse-file path) (expand-file-name path)))

(defun parse-wsdl-tree (nxml-tree location)
  (let ((ns         (create-namespace (target-namespace nxml-tree) location))
        (msgs       (wsdl-get-messages nxml-tree))
        (aliases    (node-aliases nxml-tree))
        (port-types (wsdl-get-port-types nxml-tree))
        (bindings   (wsdl-get-bindings nxml-tree))
        (services   (wsdl-get-services nxml-tree))
        (schemas    (wsdl-get-embedded-schemes nxml-tree))
        (imports    (wsdl-get-imports nxml-tree)))
    (mapc (lambda (import-node) (define-import ns import-node location)) imports)
    (mapc (lambda (msg-node) (define-message ns msg-node)) msgs)
    (mapc (lambda (port-type-node) (define-port-type ns port-type-node)) port-types)
    (mapc (lambda (binding-node) (define-binding ns binding-node)) bindings)
    (mapc (lambda (service-node) (define-service ns service-node)) services)
    (mapc (lambda (alias) (add-alias! ns (car alias) (cdr alias))) aliases)
    (mapc (lambda (schema) (add-import! ns (parse-xsd-tree schema location aliases))) schemas)
    ns))

(defun parse-xsd (path) (parse-xsd-tree (nxml-parse-file path)))

(defun parse-xsd-tree (nxml-tree location &optional additional-aliases)
  (let ((ns            (create-namespace (target-namespace nxml-tree) location))
        (aliases       (node-aliases          nxml-tree))
        (simple-types  (xsd-get-simple-types  nxml-tree))
        (complex-types (xsd-get-complex-types nxml-tree))
        (elements      (xsd-get-elements      nxml-tree))
        (attributes    (xsd-get-attributes    nxml-tree))
        (imports       (xsd-get-imports       nxml-tree)))
    (mapc (lambda (import-node) (define-import ns import-node location)) imports)
    (mapc (lambda (alias) (add-alias! ns (car alias) (cdr alias))) (append additional-aliases aliases))
    (mapc (lambda (simple-type) (define-simple-type ns simple-type)) simple-types)
    (mapc (lambda (complex-type) (define-complex-type ns complex-type)) complex-types)
    (mapc (lambda (element) (define-element ns element)) elements)
    (mapc (lambda (attribute) (define-attribute ns attribute)) attributes)
    ns))


;; namespaces
;;-----------
(defun create-namespace (name location) 
  (let ((ns (create-empty-namespace name location)))
    (add-import! ns xsd-ns)
    ns))

(defun create-empty-namespace (ns-name location) 
  (list (cons 'name       ns-name)
        (cons 'location   location)
        (cons 'imports    '())
        (cons 'aliases    '())
        (cons 'services   '())
        (cons 'bindings   '())
        (cons 'port-types '())
        (cons 'types      '())
        (cons 'elements   '())
        (cons 'attributes '())
        (cons 'messages   '())))

(defun namespace-name (namespace) (cdr (assoc 'name namespace)))
(defun namespace-location (namespace) (cdr (assoc 'location namespace)))

(defun add-namespace-entry! (namespace entries-key new-entry)
  (let ((entries (assoc entries-key namespace)))
    (setcdr entries (cons new-entry (cdr entries)))))

(defun add-import! (namespace imported-namespace)
  (add-namespace-entry! namespace 'imports (cons (namespace-name imported-namespace) imported-namespace)))

(defun add-alias! (namespace alias namespace-name)
  (if (equal alias "xmlns")
      (add-namespace-entry! namespace 'aliases (cons "" namespace-name))
    (add-namespace-entry! namespace 'aliases (cons alias namespace-name))))

(defun add-service! (namespace name service)
  (add-namespace-entry! namespace 'services (cons name service)))

(defun add-binding! (namespace name binding)
  (add-namespace-entry! namespace 'bindings (cons name binding)))

(defun add-port-type! (namespace name port-type)
  (add-namespace-entry! namespace 'port-types (cons name port-type)))

(defun add-message! (namespace name message)
  (add-namespace-entry! namespace 'messages (cons name message)))

(defun add-type! (namespace name type)
  (add-namespace-entry! namespace 'types (cons name type)))

(defun add-element! (namespace name element)
  (add-namespace-entry! namespace 'elements (cons name element)))

(defun add-attribute! (namespace name attribute)
  (add-namespace-entry! namespace 'attributes (cons name attribute)))

(defun expand-alias (namespace alias-name)
  (let ((aliases (cdr (assoc 'aliases namespace))))
    (let ((alias-entry (assoc alias-name aliases)))
      (if (null alias-entry)
          nil
        (cdr alias-entry)))))

(defun get-imported-namespace-by-name (namespace imported-namespace-name)
  (let ((imports (cdr (assoc 'imports namespace))))
    (let ((import-entry (assoc imported-namespace-name imports)))
      (if (null import-entry)
          nil
        (cdr import-entry)))))

(defun get-imported-namespace-by-alias (namespace alias)
  (let ((imported-namespace-name (expand-alias namespace alias)))
    (get-imported-namespace-by-name namespace imported-namespace-name)))

(defun lookup (namespace entries-key name)
  "Lookup entry in this namespace definitions only"
  (let* ((entries (cdr (assoc entries-key namespace)))
         (entry (assoc (get-local-name name) entries)))
    (if (null entry)
        nil
      (apply-partially (cdr entry) namespace))))

(defun lookup-with-imports (namespace entries-key name)
  "Lookup entry in this namespace definitions and in imported
  namespaces" 
  (let ((this-namespace-entry (lookup namespace entries-key name)))
    (if (null this-namespace-entry)
        (let* ((prefix (get-prefix name))
               (imported-ns (get-imported-namespace-by-alias namespace prefix)))
          (if (null imported-ns)
              nil
            (lookup imported-ns entries-key name)))
      this-namespace-entry)))

(defun get-message (namespace name)
  (lookup-with-imports namespace 'messages name))

(defun get-type (namespace name)
  (lookup-with-imports namespace 'types name))

(defun get-element (namespace name)
  (lookup-with-imports namespace 'elements name))

(defun get-attribute (namespace name)
  (lookup-with-imports namespace 'attributes name))

(defun get-port-type (namespace name)
  (lookup-with-imports namespace 'port-types name))

(defun get-binding (namespace name)
  (lookup-with-imports namespace 'bindings name))

(defun get-service (namespace name)
  (lookup-with-imports namespace 'services name))

(defun get-service-names (namespace)
  (let ((services (cdr (assoc 'services namespace))))
    (mapcar 'car services)))

;;imports
;;-------
(defun define-import (namespace import-node base-url)
  (let* ((import-ns (node-attribute-value import-node "namespace"))
         (import-location (node-attribute-value import-node "location"))
         (resolved-url (resolve-url import-location base-url)))
    (add-import! namespace (parse-wsdl-tree 
                            (get-nxml-tree-for resolved-url)
                            resolved-url))))

;;portTypes
;;---------
(defun define-port-type (namespace port-type-node)
  (let ((port-type (create-port-type-function namespace port-type-node))
        (name (node-attribute-value port-type-node "name")))
    (add-port-type! namespace name port-type)))

(defun create-port-type-function (namespace port-type-node)
  "Create port-type function"
  (let ((port-type-name (node-attribute-value port-type-node "name")))
    `(lambda (my-ns operation-name style)
       (cond ,@(mapcar 'create-operation-handler (wsdl-get-port-type-operations port-type-node))
             (t (error (concat "Operation " operation-name " not found in port-type " ,port-type-name)))))))

(defun create-operation-handler (operation-node)
  (let ((op-name (node-attribute-value operation-node "name"))
        (input-message-name (wsdl-operation-get-input-message operation-node)))
    (if (null input-message-name)
        `((equal operation-name ,op-name) 
          (error (concat "Operation " ,op-name " has no input")))
      `((equal operation-name ,op-name)
        (if (equal style "rpc")
            (funcall (get-message my-ns ,input-message-name) ,op-name)
          (funcall (get-message my-ns ,input-message-name) nil))))))
      
;;bindings
;;--------
(defun define-binding (namespace binding-node)
  (let ((binding (create-binding-function namespace binding-node))
        (name (node-attribute-value binding-node "name")))
    (add-binding! namespace name binding)))

(defun create-binding-function (namespace binding-node)
  (let ((default-style     (wsdl-get-default-binding-style binding-node))
        (soap-action-alist (wsdl-get-soap-actions-alist binding-node))
        (port-type-name    (node-attribute-value binding-node "type"))
        (operation-names   (wsdl-get-binding-operation-names binding-node)))
    `(lambda (my-ns message &optional operation-name)
       (cond ((equal message 'get-operation-names) ',operation-names)
             ((equal message 'get-soap-action) 
              (and (assoc operation-name ',soap-action-alist) (cdr (assoc operation-name ',soap-action-alist))))
             ((equal message 'get-request)
              (concat "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\">\n" 
                      "<soapenv:Body>\n"
                      (funcall (get-port-type my-ns ,port-type-name) operation-name
                               ,default-style)
                      "</soapenv:Body>\n"
                      "</soapenv:Envelope>"))))))

;;services
;;--------
(defun define-service (namespace service-node)
  (let ((service (create-service-function namespace service-node))
        (name (node-attribute-value service-node "name")))
    (add-service! namespace name service)))

(defun create-service-function (namespace service-node)
  (let ((port-names (wsdl-get-service-port-names service-node))
        (port-locations (wsdl-get-service-port-locations-alist service-node))
        (port->binding (wsdl-get-service-port-binding-alist service-node)))
    `(lambda (my-ns message &optional port-name)
       (cond ((equal message 'get-port-names)
              ',port-names)
             ((equal message 'get-port-location)
              (and (assoc port-name ',port-locations) (cdr (assoc port-name ',port-locations))))
             ((equal message 'get-binding)
              (and (assoc port-name ',port->binding) (get-binding my-ns (cdr (assoc port-name ',port->binding)))))))))

;; messages
;;---------
(defun define-message (namespace message-node)
  (let ((message (create-message-print-function namespace message-node))
        (name (node-attribute-value message-node "name")))
    (add-message! namespace name message)))

(defun create-message-print-function (namespace message-node)
  "Create function that print message"
  `(lambda (my-ns tag-name)
     (if tag-name
         (concat "<"  tag-name ">\n"
                 ,@(mapcar 'print-message-part (wsdl-get-message-parts message-node))
                 "</" tag-name ">\n")
       (concat ,@(mapcar 'print-message-part (wsdl-get-message-parts message-node))))))

(defun print-message-part (message-part-node)
  (let ((part-name (node-attribute-value message-part-node "name"))
        (type? (not (null (node-attribute-value message-part-node "type")))))
    (if type?        
        `(funcall (get-type  my-ns ,(node-attribute-value message-part-node "type")) 
                  (if tag-name ,part-name nil))
      `(funcall (get-element my-ns ,(node-attribute-value message-part-node "element")) 
                (if tag-name ,part-name nil)))))


;; types
;;------
(defun define-build-in-type (namespace type-name type-sample)
  (let ((type (create-build-in-type-print-function type-sample)))
    (add-type! namespace type-name type)))

(defun create-build-in-type-print-function (type-sample)
  `(lambda (my-ns tag-name) 
     (if tag-name
         (concat "<" tag-name ">" ,type-sample "</"tag-name ">\n")
       ,type-sample)))

(defun define-simple-type (namespace type-node)
  (let ((simple-type (create-simple-type-print-function type-node))
        (name (node-attribute-value type-node "name")))
    (add-type! namespace name simple-type)))

(defun create-simple-type-print-function (type-node)
  (cond ((xsd-simple-type-by-restriction? type-node)
         `(lambda (my-ns tag-name) 
            (funcall (get-type my-ns ,(xsd-get-restriction-base-type type-node)) tag-name)))
        (t `(lambda (my-ns tag-name) 
              (cons tag-name "Simple type not by restriction, unsupported yet")))))

(defun define-complex-type (namespace type-node)
  (let ((complex-type (create-complex-type-print-function type-node))
        (name (node-attribute-value type-node "name")))
    (add-type! namespace name complex-type)))

(defun create-complex-type-print-function (type-node)
  (cond ((xsd-complex-type-with-sequence? type-node)
         `(lambda (my-ns tag-name)
            (concat
             "<" tag-name ,@(mapcar 'invoke-attribute-print-function (xsd-get-attributes type-node)) ">\n"
             ,@(mapcar 'create-local-element-print-function (xsd-get-sequence-elements type-node))
             "</" tag-name ">\n")))
        ((xsd-complex-type-with-all? type-node)
         `(lambda (my-ns tag-name)
            (concat
             "<" tag-name ,@(mapcar 'invoke-attribute-print-function (xsd-get-attributes type-node)) ">\n"
             ,@(mapcar 'create-local-element-print-function (reverse (xsd-get-all-elements type-node)))
             "</" tag-name ">\n")))
        (t `(lambda (my-ns tag-name) "Unknown complex type"))))


;; elements
;;---------
(defun define-element (namespace element-node)
  (let ((element (create-element-print-function element-node))
        (name (node-attribute-value element-node "name")))
    (add-element! namespace name element)))

(defun create-element-print-function (element-node)
  (let ((element-name (node-attribute-value element-node "name")))
    (cond ((not (null (node-attribute-value element-node "type")))
           `(lambda (my-ns tag-name) 
              (funcall (get-type my-ns ,(node-attribute-value element-node "type")) 
                       (or tag-name ,element-name))))
          ((not (null (node-attribute-value element-node "ref")))
           `(lambda (my-ns tag-name)
              (funcall (get-element my-ns ,(node-attribute-value element-node "ref")) 
                       (or tag-name ,element-name))))
          ((xsd-element-with-inner-complex-type? element-node)
           `(lambda (my-ns tag-name)
              (funcall (create-complex-type-print-function ',(car (node-childs element-node)))
                     my-ns
                     (or tag-name ,element-name))))
          ((xsd-element-with-inner-simple-type? element-node)
           `(lambda (my-ns tag-name)
              (funcall (create-simple-type-print-function ',(car (node-childs element-node)))
                     my-ns
                     (or tag-name ,element-name))))
          (t `(lambda (my-ns tag-name) "Unknown element\n")))))

(defun create-local-element-print-function (element-node)
  (cond ((not (null (node-attribute-value element-node "type")))
         `(funcall (get-type my-ns ,(node-attribute-value element-node "type")) 
                   ,(node-attribute-value element-node "name")))
        ((not (null (node-attribute-value element-node "ref")))
         `(funcall (get-element my-ns ,(node-attribute-value element-node "ref")) nil))
        ((xsd-element-with-inner-complex-type? element-node)
         `(funcall (create-complex-type-print-function ',(car (node-childs element-node)))
                   my-ns
                   ,(node-attribute-value element-node "name")))
        ((xsd-element-with-inner-simple-type? element-node)
         `(funcall (create-simple-type-print-function ',(car (node-childs element-node)))
                   my-ns
                   ,(node-attribute-value element-node "name")))
        (t "Unknown element\n")))


;;attributes
;;----------
(defun define-attribute (namespace attribute-node)
  (let ((attribute (create-attribute-print-function attribute-node))
        (name (node-attribute-value attribute-node "name")))
    (add-attribute! namespace name attribute)))

(defun create-attribute-print-function (attribute-node)
  (let ((attribute-name (node-attribute-value attribute-node "name"))
        (attribute-type (node-attribute-value attribute-node "type")))
    `(lambda (my-ns) 
       (concat " " ,attribute-name "=\""
               (funcall (get-type my-ns ,attribute-type) nil)
               "\""))))

(defun invoke-attribute-print-function (attribute-node)
  (cond ((not (null (node-attribute-value attribute-node "type")))
         `(concat " " ,(node-attribute-value attribute-node "name") "=\"" 
                 (funcall (get-type my-ns ,(node-attribute-value attribute-node "type")) nil)
                 "\""))
        ((not (null (node-attribute-value attribute-node "ref")))
         `(funcall (get-attribute my-ns ,(node-attribute-value attribute-node "ref"))))
        (t "Unknown attribute\n")))


;;build-in xsd namespace
(defconst xsd-ns 
  (let ((ns (create-empty-namespace "http://www.w3.org/2001/XMLSchema" nil)))
    (define-build-in-type ns "string"          "string")
    (define-build-in-type ns "int"             "1")
    (define-build-in-type ns "positiveInteger" "1")
    (define-build-in-type ns "decimal"         "1.0")
    (define-build-in-type ns "date"            "2009-10-25")
    (define-build-in-type ns "NMTOKEN"         "US")
    (define-build-in-type ns "integer"         "1")
    (define-build-in-type ns "dateTime"        "2009-10-25T23:11:00.000-05:00")
    (define-build-in-type ns "boolean"         "true")
    ns))


;;--------------------------------------
;; abstraction over nxml-tree structure:
;;--------------------------------------
(defun attribute-name (attribute) (car attribute))
(defun attribute-value (attribute) (cdr attribute))
(defun node-attributes (node) (cadr node))
(defun node-attribute-value (node attribute-name)
  (let ((attributes (filter (lambda (a) (equal (attribute-name a) attribute-name))
                            (node-attributes node))))
    (if attributes 
        (attribute-value (car attributes))
      nil)))

(defun parse-xml-from-location (location)
  (let ((buf (url-retrieve-synchronously location))
        (tmp-filename (make-temp-file "soap-request-builder")))
    (set-buffer buf)
    (xml/delete-http-header)
    (set-visited-file-name tmp-filename t)
    (save-buffer)
    (kill-buffer buf)
    (let ((result (nxml-parse-file tmp-filename)))
      (delete-file tmp-filename)
      result)))

(defun target-namespace (node) (node-attribute-value node "targetNamespace"))

(defun node-aliases (node)
  (defun alias? (attr)
    (let ((attr-name (attribute-name attr)))
      (and (consp attr-name) 
           (equal (car attr-name) :http://www\.w3\.org/2000/xmlns/))))
  (mapcar (lambda (attr) (cons (cdr (attribute-name attr)) (attribute-value attr)))
          (filter 'alias? (node-attributes node))))

(defun node-name (node) (car node))
(defun node-childs (node) (filter (lambda (n) (listp n)) (cddr node)))

(defun get-prefix(qname) 
  (let ((split (split-string qname ":")))
    (if (equal (length split) 2)
        (car (split-string qname ":"))
    "")))
(defun get-local-name (qname) 
  (let ((split (split-string qname ":")))
    (if (equal (length split) 2)
        (cadr (split-string qname ":"))
      (car (split-string qname ":")))))

(defun get-child-nodes-with-name (parent-node name) 
  (filter (lambda (n) (equal (node-name n) name))
          (node-childs parent-node)))

(defun get-nxml-tree-for (location)
  (let ((url (url-generic-parse-url location)))
    (if (or (equal (elt url 1) "http")
            (equal (elt url 1) "https")
            (equal (elt url 1) "ftp")
            (equal (elt url 1) "file"))
        ;;this is valid url
        (get-nxml-tree-for-url location)
      ;;this is local file path 
      (nxml-parse-file (expand-file-name location)))))

(defun get-nxml-tree-for-url (url)
  (let ((buf (url-retrieve-synchronously url))
        (tmp-filename (make-temp-file "soap-request-builder")))
    (set-buffer buf)
    (delete-http-headers)
    (set-visited-file-name tmp-filename t)
    (save-buffer)
    (kill-buffer buf)
    (let ((nxml-tree (nxml-parse-file tmp-filename)))
      (delete-file tmp-filename)
      nxml-tree)))

(defun delete-http-headers ()
  "Delete http headers from current buffer"
  (goto-char (point-min))
  (if (looking-at "^<\\?xml")
      'done
    (kill-line)
    (delete-http-headers)))

(defun resolve-url (url base-url)
  "Expand location from import"
  (cond ((elt (url-generic-parse-url url) 1) url) ;;correct url - simply return it
        ((and (elt (url-generic-parse-url base-url) 1) ;; base-url is correct url
              (not (elt (url-generic-parse-url url) 1))) ;; and imported location is not
         (error (concat "can not resolve " url " relative to " base-url)))
        ((and (not (elt (url-generic-parse-url base-url) 1)) ;; base-url is file
              (not (elt (url-generic-parse-url url) 1))) ;; and url is file
         (if (file-name-absolute-p url)
             url
           (concat (file-name-directory (expand-file-name base-url)) url)))))


;; functions to get wsdl-specific nodes:
(defun wsdl-get-messages (definitions-node) 
  (get-child-nodes-with-name definitions-node (cons :http://schemas\.xmlsoap\.org/wsdl/ "message")))

(defun wsdl-get-message-parts (message-node)
  (get-child-nodes-with-name message-node (cons :http://schemas\.xmlsoap\.org/wsdl/ "part")))

(defun wsdl-get-port-types (definitions-node) 
  (get-child-nodes-with-name definitions-node (cons :http://schemas\.xmlsoap\.org/wsdl/ "portType")))

(defun wsdl-get-bindings (definitions-node) 
  (get-child-nodes-with-name definitions-node (cons :http://schemas\.xmlsoap\.org/wsdl/ "binding")))

(defun wsdl-get-services (definitions-node) 
  (get-child-nodes-with-name definitions-node (cons :http://schemas\.xmlsoap\.org/wsdl/ "service")))

(defun wsdl-get-ports (service-node) 
  (get-child-nodes-with-name service-node (cons :http://schemas\.xmlsoap\.org/wsdl/ "port")))

(defun wsdl-get-port-type-operations (port-type-node) 
  (get-child-nodes-with-name port-type-node (cons :http://schemas\.xmlsoap\.org/wsdl/ "operation")))

(defun wsdl-get-imports (definitions-node) 
  (get-child-nodes-with-name definitions-node (cons :http://schemas\.xmlsoap\.org/wsdl/ "import")))

(defun wsdl-get-default-binding-style (binding-node)
  (let ((soap:binding-list (get-child-nodes-with-name 
                            binding-node 
                            (cons :http://schemas\.xmlsoap\.org/wsdl/soap/ "binding"))))
    (if soap:binding-list
        (node-attribute-value (car soap:binding-list) "style")
      "document")))

(defun wsdl-get-soap-actions-alist (binding-node)
  (filter (lambda (n) (not (null n)))
          (mapcar '(lambda (operation-node) 
                     (let ((soap:operation-list (get-child-nodes-with-name
                                                 operation-node
                                                 (cons :http://schemas\.xmlsoap\.org/wsdl/soap/ "operation"))))
                       (if soap:operation-list
                           (cons (node-attribute-value operation-node "name")
                                 (node-attribute-value (car soap:operation-list) "soapAction"))
                         nil)))
                  (get-child-nodes-with-name
                   binding-node
                   (cons :http://schemas\.xmlsoap\.org/wsdl/ "operation")))))

(defun wsdl-get-binding-operation-names (binding-node) 
  (mapcar '(lambda (operation-node) (node-attribute-value operation-node "name"))
          (get-child-nodes-with-name
           binding-node
           (cons :http://schemas\.xmlsoap\.org/wsdl/ "operation"))))

(defun wsdl-get-service-port-names (service-node) 
  (mapcar '(lambda (port-node) (node-attribute-value port-node "name"))
          (get-child-nodes-with-name
           service-node
           (cons :http://schemas\.xmlsoap\.org/wsdl/ "port"))))

(defun wsdl-get-service-port-locations-alist (service-node) 
  (filter (lambda (n) (not (null n)))
          (mapcar '(lambda (port-node) 
                     (let ((soap:address-list (get-child-nodes-with-name
                                               port-node
                                               (cons :http://schemas\.xmlsoap\.org/wsdl/soap/ "address"))))
                       (if soap:address-list
                           (cons (node-attribute-value port-node "name")
                                 (node-attribute-value (car soap:address-list) "location"))
                         nil)))
                  (get-child-nodes-with-name
                   service-node
                   (cons :http://schemas\.xmlsoap\.org/wsdl/ "port")))))

(defun wsdl-get-service-port-binding-alist (service-node) 
  (filter (lambda (n) (not (null n)))
          (mapcar '(lambda (port-node) 
                     (cons (node-attribute-value port-node "name")
                           (node-attribute-value port-node "binding")))
                  (get-child-nodes-with-name
                   service-node
                   (cons :http://schemas\.xmlsoap\.org/wsdl/ "port")))))

(defun wsdl-operation-get-input-message (operation-node)
  (let ((input-node-list (get-child-nodes-with-name 
                          operation-node 
                          (cons :http://schemas\.xmlsoap\.org/wsdl/ "input"))))
    (if input-node-list
        (node-attribute-value (car input-node-list) "message")
      nil)))

(defun wsdl-get-embedded-schemes(definitions-node)
  (get-child-nodes-with-name 
   (car (get-child-nodes-with-name 
         definitions-node 
         (cons :http://schemas\.xmlsoap\.org/wsdl/ "types")))
   (cons :http://www\.w3\.org/2001/XMLSchema "schema")))


;; functions to get xsd-specific nodes:
(defun xsd-get-simple-types (schema-node) 
  (get-child-nodes-with-name schema-node (cons :http://www\.w3\.org/2001/XMLSchema "simpleType")))

(defun xsd-get-complex-types (schema-node) 
  (get-child-nodes-with-name schema-node (cons :http://www\.w3\.org/2001/XMLSchema "complexType")))

(defun xsd-get-elements (schema-node) 
  (get-child-nodes-with-name schema-node (cons :http://www\.w3\.org/2001/XMLSchema "element")))

(defun xsd-get-attributes (some-node) 
  (get-child-nodes-with-name some-node (cons :http://www\.w3\.org/2001/XMLSchema "attribute")))

(defun xsd-get-imports (schema-node) 
  (get-child-nodes-with-name schema-node (cons :http://www\.w3\.org/2001/XMLSchema "import")))

(defun xsd-simple-type-by-restriction? (simple-type-node)
  (let ((childs (node-childs simple-type-node)))
    (and (not (null childs))
         (equal (node-name (car childs)) (cons :http://www\.w3\.org/2001/XMLSchema "restriction")))))

(defun xsd-get-restriction-base-type (simple-type-node)
  (let ((restriction (car (node-childs simple-type-node))))
    (node-attribute-value restriction "base")))

(defun xsd-complex-type-with-sequence? (complex-type-node)
  (let ((childs (node-childs complex-type-node)))
    (and (not (null childs))
         (equal (node-name (car childs)) (cons :http://www\.w3\.org/2001/XMLSchema "sequence")))))

(defun xsd-complex-type-with-all? (complex-type-node)
  (let ((childs (node-childs complex-type-node)))
    (and (not (null childs))
         (equal (node-name (car childs)) (cons :http://www\.w3\.org/2001/XMLSchema "all")))))

(defun xsd-element-with-inner-complex-type? (element-node)
  (let ((childs (node-childs element-node)))
    (and (not (null childs))
         (equal (node-name (car childs)) (cons :http://www\.w3\.org/2001/XMLSchema "complexType")))))

(defun xsd-element-with-inner-simple-type? (element-node)
  (let ((childs (node-childs element-node)))
    (and (not (null childs))
         (equal (node-name (car childs)) (cons :http://www\.w3\.org/2001/XMLSchema "simpleType")))))

(defun xsd-get-sequence-elements (complex-type-node)
  (let ((sequence (car (node-childs complex-type-node))))
    (node-childs sequence)))

(defun xsd-get-all-elements (complex-type-node)
  (let ((sequence (car (node-childs complex-type-node))))
    (node-childs sequence)))    

(defun filter (predicat items)
  (defun iter (its)
    (if (null its)
        '()
      (if (funcall predicat (car its))
          (cons (car its) (iter (cdr its)))
        (iter (cdr its)))))
  (iter items))

(defun select-with-ido (choises prompt)
  "Select item from object-list presented at minibuffer using ido."
  (let* ((ido-decorations (list "\n"  ""
                                "\n"  " | ..." 
                                "\n[" "]" 
                                " [No match]"
                                " [Matched]"
                                " [Not readable]"
                                " [Too big]")))
    (ido-completing-read prompt choises nil t)))

(provide 'ws)