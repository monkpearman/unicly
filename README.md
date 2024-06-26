# Unicly Description

Unicly is a Common Lisp library for generation of UUIDs (Universally Unique
Identifiers) as described by RFC 4122.


As of 05-16-2024 Unicly is known to compile on SBCL 2.4.1 (on Darwin Arm64).

In past, Unicly has successfully compiled on Clisp, and Lispworks without
complication. Porting or translations should be trivial. Patches/Pulls welcome.

Please do let us know if you can confirm whether Unicly is known to build on
your platform and implementation. Again, Patches Welcome :)


# Unicly installation

```
SHELL> cd /parent/dir/where/you/put/cl-repos/

SHELL> git clone git://github.com/mon-key/unicly.git 

SHELL> cd unicly
```

Unicly is loadable with Quicklisp assuming a directory containing unicly.asd is
present as an element of ```ASDF:*CENTRAL-REGISTRY*```.

```Common Lisp
CL-USER> (push #P"/parent/dir/where/you/put/cl-repos/unicly/" asdf:*central-registry*)

CL-USER> (quicklisp:quickload 'unicly)
```

Or for the perverse who'd like to see what their lisp can't optimize away:

```Common Lisp
CL-USER> (quicklisp:quickload 'unicly :verbose t :explain t)
```

# Unicly Usage

```Common Lisp
(in-package #:unicly)
;=> #<PACKAGE "UNICLY">
```

## UUID Namespaces: 

As per RFC 4122 "Appendix C - Some Name Space IDs" the Unicly package exports
four "namespaces", these are:

```Common Lisp
UNICLY> *uuid-namespace-dns*
;=> 6ba7b810-9dad-11d1-80b4-00c04fd430c8

UNICLY> *uuid-namespace-url* 
;=> 6ba7b811-9dad-11d1-80b4-00c04fd430c8

UNICLY> *uuid-namespace-oid*
;=> 6ba7b812-9dad-11d1-80b4-00c04fd430c8

UNICLY> *uuid-namespace-x500*
;=> 6ba7b814-9dad-11d1-80b4-00c04fd430c8
```

## v4 UUID Namespaces:

The above "namespaces" are "canonical" in so much as they are presented by the RFC.

Indeed, a common misconception is that the above four namespaces are the <ins>only</ins>
namespaces one can/should use when working with v3 and v5 uuids.

However, one is free to define any number of other "namespaces" to "contain" UUID
objects, and one is free to utilize these namespaces as need dictates.

A problem that arises though, is how does one create a UUID "namespace"?
This represents a sort of bootstrapping issue.

The RFC 4122 describes a mechanism for creating two types of UUIDs which may
serve as initial "namespaces" from which one can then generate other types of
UUIDs with reasonable certainty that the UUIDs so generated will remain unique.

The first of these is a v1 or time based UUID. Unicly does not currently provide
an interface for creation of v1 UUIDs.

The second of these is a v4 or random UUID. This is Unicly's preferred interface
for creation of an initial namespace. 

Once one has a handle on an initial UUID namespace that namespace may then be
leveraged to create any number of new UUID namespaces using ```MAKE-V3-UUID``` OR
```MAKE-V5-UUID```. This is a commonly overlooked/misunderstood aspect of RFC 4122!

So, to be clear, the real utility of UUIDs isn't by way of the more pedestrian
v1 and v4 UUIDs, but rather by utilization of v3 and v5 UUIDs in a manner
similar to that of RDF, whereby a namespace is treated as a URI, and a name in
that namespace is treated as a URIref.

We can't overstate this point enough - don't retstrict use of UUIDs to only the
v1 or v4 forms. Doing so is (by analogy with RDF) equivalent to providing a URI
for every object gererated by a system and then never bothering to reference any
qualified names within that namespace! Or (by analogy with Common Lisp) this is
not unlike defining a package object which doesn't use any symbols just to get
at its identity, e.g.:

```Common Lisp
 (let ((hex (make-array 16
                        :element-type 'base-char
                        :initial-contents (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                                                #\A #\B #\C #\D #\E #\F)))
       (pkg-name '()))
   (loop 
      repeat 32
      for rand = (random 16)
      for rand-char = (aref hex rand)
      collect rand-char into rand-bag
      finally (setf pkg-name (coerce  rand-bag 'string)))
   (setf pkg-name
         (package-name  (eval `(defpackage ,pkg-name)))))
```

In any event, the easiest way to create a <ins>reasonably random</ins> new "namespace" is with ```MAKE-V4-UUID```.

```Common Lisp
UNICLY> (make-v4-uuid)
;=> 2f20bdfa-cd67-4150-8500-80c94821bbda
```

> [!Note]
> Because the v4 uuid is the "random" UUID when evaluating the following
> form your return value will obv. be somewhat different :)

Evaluating the above form returns the printed representation of UUID object.

To cache a namespace we need to bind it to a special variable.

Below we use a defparameter form to do this but, longer running applications
would probably use ```CL:DEFVAR``` or ```CL:DEFCONSTANT```.

The easiest way to persist a UUID object is to bind its printed string
representation.
 To get the string representation of a UUID we use ```UUID-PRINC-TO-STRING```.  

```Common Lisp
UNICLY> (defparameter *unique-random-namespace* 
	   (uuid-princ-to-string  (unicly:make-v4-uuid))) 
;=> *UNIQUE-RANDOM-NAMESPACE*

UNICLY> *UNIQUE-RANDOM-NAMESPACE*
;=> "77b84745-ab13-49c6-8fdc-9afaabc51c52"
```
> [!Note]
> Per RFC 4122 case is significant when the string representation of a UUID is
> output. IOW, sticklers should be careful when using CL printing function which
> depend on dynamic value of ```CL*PRINT-CASE*```!

To convert this string back to a UUID use ```MAKE-UUID-FROM-STRING```:

```Common Lisp
UNICLY> (setf *unique-random-namespace* 
	      (make-uuid-from-string *unique-random-namespace*))
;=> 77b84745-ab13-49c6-8fdc-9afaabc51c52

UNICLY> *unique-random-namespace*
;=> 77b84745-ab13-49c6-8fdc-9afaabc51c52
```

To print a UUID with a URN quailifier use ```UUID-AS-URN-STRING```:

```Common Lisp
UNICLY> (uuid-as-urn-string nil *unique-random-namespace*)
;=> "urn:uuid:77b84745-ab13-49c6-8fdc-9afaabc51c52"
```

v4 UUIDs are fine so long as you don't need to persist an objects
identity and simply need a throw away or single session UUID.
Indeed, one could serialize/deserialize v4 UUIDs from a string to object
representation with each session if desired.

However, as indicated above a v4 UUID is best used as a "seed-value" for
generating a namespace which is unique to your application.

## v4 UUID Namespaces:

For persistent UUID solutions it is recommended to use ```MAKE-V5-UUID``` by
providing a persisted UUID namespace for an object to reside in.

You can make your own fabulous namespace like this:

```Common Lisp
UNICLY> (defparameter *my-fabulous-namespace* 
	 (make-v5-uuid *unique-random-namespace* "com.bubba.namespace"))
;=> *MY-FABULOUS-NAMESPACE*

UNICLY> *MY-FABULOUS-NAMESPACE*
;=> e5c2a048-863f-5c7d-a894-607070d2d299
```

Create some objects in the namespace ```*my-fabulous-namespace*```:

```Common Lisp
UNICLY> (make-v5-uuid *my-fabulous-namespace* (namestring (user-homedir-pathname)))
;=> c0f2a167-dae7-55c0-ad57-1d8bad0444d3

UNICLY> (make-v5-uuid *my-fabulous-namespace* (namestring *default-pathname-defaults*))
;=> a5ace91c-d657-5f5c-abef-81bbef52d27c

UNICLY> (setf *default-pathname-defaults* (user-homedir-pathname))
;=> #P"/home/you/"
```

You should now find that the UUID for the ```CL:NAMESTRING``` of ```CL:*DEFAULT-PATHNAME-DEFAULTS*```
is the same as that of the namestring of ```CL:USER-HOMEDIR-PATHNAME```:

```Common Lisp
UNICLY> (make-v5-uuid *my-fabulous-namespace* (namestring *default-pathname-defaults*))
;=> c0f2a167-dae7-55c0-ad57-1d8bad0444d3
```

Note, that each object returned by MAKE-V5-UUID has unique identity under ```CL:EQUALP```:

```Common Lisp
UNICLY> (equalp (make-v5-uuid *my-fabulous-namespace* (namestring (user-homedir-pathname)))
                (make-v5-uuid *my-fabulous-namespace* (namestring *default-pathname-defaults*)))
;=> NIL
```

To test equality among two UUIDs (even where their ```CL:PRINT-OBJECT``` is
identical), one must first convert the UUID to an intermediary format and compare
the identity of the intermediate formats instead.

One way to do this is test ```CL:EQUAL``` for two UUIDs using their string representation:

```Common Lisp
UNICLY> (equal (uuid-princ-to-string 
		 (make-v5-uuid *my-fabulous-namespace* (namestring (user-homedir-pathname))))
		(uuid-princ-to-string 
		 (make-v5-uuid *my-fabulous-namespace* (namestring *default-pathname-defaults*))))
;=> T
```

```CL:EQUAL``` finds the two UUIDs above as having identical string representations.
However, checking string values for object identity is ugly because internally UUID
objects are represented as unsigned integer values.

Unicly provides features for comparing UUID representations in various
intermediary formats other than as strings, and further below we present some
examples of Unicly's representations of UUIDs in other representations which
illustrate some alterhative (and cleaner) ways to interrogate UUID equality.

## Persisting and serialzing/deserializing a UUID to file:

So, now that you've got a handle on a fabulous UUID namespace how do you persist it?
The quick and dirty way is to write the UUID string representation of
```*my-fabulous-namespace*``` to a file somewhere.

```Common Lisp
UNICLY> (with-open-file (persist (make-pathname :directory '(:absolute "tmp") 
                                                :name "persisted-fabulous-namespace"
                                                :type "uuid")
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
          ;; Here we CL:PRIN1 the UUID string representation.
	  ;; This is for illustrative purposes, there are other ways.
          (prin1 *MY-FABULOUS-NAMESPACE*  persist))
;=> "e5c2a048-863f-5c7d-a894-607070d2d299"

UNICLY> (setf *my-fabulous-namespace* nil)
;=> NIL
```

To restore the string representation of the persisted UUID into the
```*my-fabulous-namespace*``` variable, read in the contents of the file:

```Common Lisp
UNICLY> (with-open-file (persist (make-pathname :directory
                                                '(:absolute "tmp")
                                                :name "persisted-fabulous-namespace"
                                                :type "uuid")
                                 :direction :input
                                 :if-does-not-exist :error)
          (setf *my-fabulous-namespace* 
                (make-uuid-from-string (read-line  persist))))
;=> e5c2a048-863f-5c7d-a894-607070d2d299
```

When serialzing/deserializing large numbers of UUIDs it may be more expedient to
use other intermediary representations of your UUIDs. Unicly provides interfaces
for reading, writing, and converting UUIDs across various representations
including bit-vectors, byte-arrays, 128-bit integers, strings, etc.

### Write a UUID byte-array to file with ```UUID-SERIALIZE-BYTE-ARRAY-BYTES```:

```Common Lisp
UNICLY> (let ((file (make-pathname :directory '(:absolute "tmp")
                                   :name "temp-bytes"))
              (w-uuid (make-v5-uuid *uuid-namespace-dns* "bubba"))
              (gthr '()))
          (with-open-file (s file
                             :if-exists :supersede
                             :if-does-not-exist :create
                             :direction :output
                             :element-type 'uuid-ub8)
            (uuid-serialize-byte-array-bytes  w-uuid s))
          ;; read the byte-array back in
          (with-open-file (stream file :element-type 'uuid-ub8)
            (do ((code (read-byte stream nil :eof) (read-byte stream nil :eof)))
                ((eql code :eof))
              (push code gthr)))
          (and gthr
               (setf gthr (uuid-from-byte-array (make-array 16
                                                            :element-type 'uuid-ub8
                                                            :initial-contents (nreverse gthr)))))
          (unwind-protect
               (list (uuid-eql w-uuid gthr)
                     gthr
                     w-uuid)
            (delete-file file)))
;=> (T eea1105e-3681-5117-99b6-7b2b5fe1f3c7 eea1105e-3681-5117-99b6-7b2b5fe1f3c7)
```
### Read a UUID byte-array to file with ```UUID-SERIALIZE-BIT-VECTOR-BITS```:

```Common Lisp
UNICLY> (let* ((tmp (make-pathname :directory '(:absolute  "tmp")
                                   :name "bitstream-test"))
               (v4     (uuid-get-namespace-bytes (make-v4-uuid)))
               (v4-out (with-open-file (strm tmp
                                             :direction :output
                                             :if-does-not-exist :create
                                             :if-exists :supersede
                                             :element-type 'uuid-ub8)
                         (uuid-serialize-byte-array-bytes v4 strm)))
               (v4-in   (with-open-file (in-strm tmp
                                                 :direction :input
                                                 :if-does-not-exist :error
                                                 :element-type 'uuid-ub8)
                          (uuid-deserialize-byte-array-bytes in-strm))))
          (progn (delete-file tmp)
                 (values (equalp v4-in v4-out)
                         v4-in)))
;=> T
;   #(79 53 137 227 91 111 66 133 148 52 126 41 125 175 137 144)
```

### Write a UUID bit-vector to file with ```UUID-SERIALIZE-BIT-VECTOR-BITS```:

```Common Lisp
UNICLY> (let* ((file (make-pathname :directory '(:absolute "tmp")
                                    :name "temp-bytes"))
               (w-uuid    (make-v5-uuid *uuid-namespace-dns* "bubba"))
               (w-uuid-ba (uuid-to-bit-vector w-uuid)))
          (unwind-protect
               (progn
                 (with-open-file (s file
                                    :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create
                                    :element-type 'uuid-ub8)
                   (uuid-serialize-bit-vector-bits w-uuid-ba s))
                 ;; read the bit-vector back in
                 (with-open-file (s file
                                    :direction :input
                                    :if-does-not-exist :error
                                    :element-type 'uuid-ub8)
                   (let ((ba-read (uuid-deserialize-bit-vector-bits s)))
                     (values (equalp ba-read w-uuid-ba)
                             ba-read
                             w-uuid))))
            (delete-file file)))
;=> T
   #*11101110101000010001000001011110001101101000000101010001000101111001100110110110011110110010101101011111111000011111001111000111
   eea1105e-3681-5117-99b6-7b2b5fe1f3c7
```
### Read a UUID bit-vector from a stream  with ```UUID-DESERIALIZE-BIT-VECTOR-BITS```:

Note, unlike ```UUID-READ-BIT-VECTOR-BITS```, is not wrapped in a ```CL:WITH-OPEN-FILE``` form.

```Common Lisp
UNICLY> (let* ((tmp (make-pathname :directory '(:absolute  "tmp")
                                   :name "bitstream-test"))
               (v4     (uuid-to-bit-vector (make-v4-uuid)))
               (v4-out (with-open-file (strm tmp
                                             :direction :output
                                             :if-does-not-exist :create
                                             :if-exists :supersede
                                             :element-type 'uuid-ub8)
                         (uuid-serialize-bit-vector-bits v4 strm)))
               (v4-in   (with-open-file (in-strm tmp
                                                 :direction :input
                                                 :if-does-not-exist :error
                                                 :element-type 'uuid-ub8)
                          (uuid-deserialize-bit-vector-bits in-strm))))
          (progn (delete-file tmp)
                 (values (uuid-bit-vector-eql v4-in v4-out)
                         v4-in)))
;=> T
;   #*00101101001110100100001110101011000000101110010001000001101011101000010001100010101100011111101000001101100110110010111000001011
```
### Read a UUID bit-vector from a file with ```UUID-READ-BIT-VECTOR-BITS```:

```Common Lisp       
UNICLY> (let* ((tmp (make-pathname :directory '(:absolute  "tmp")
                                   :name "bitstream-test"))
               (v4     (uuid-to-bit-vector (make-v4-uuid)))
               (v4-out (with-open-file (strm tmp
                                             :direction :output
                                             :if-does-not-exist :create
                                             :if-exists :supersede
                                             :element-type 'uuid-ub8
                                             :external-format   :UTF-8)
                         (uuid-serialize-bit-vector-bits v4 strm)))
               (v4-in  (uuid-read-bit-vector-bits *tt--temp-path*)))
          (prog1 (uuid-bit-vector-eql v4-in v4-out)
            (delete-file tmp)))
;=> T
```

## Unicly's UUID Equality Interface:

Following examples illustrate some more of the Unicly interface.

We use the value of the v4-uuid in the variable ```*unique-random-namespace*```
defined earlier above, but feel free to substitute ```*my-fabulous-namespace*``` (or
equivalent).

## Testing the equivalence of two UUID objects with ```UUID-EQL```:

```Common Lisp
UNICLY> (uuid-eql 
         (make-v5-uuid *unique-random-namespace* "bubba")
         (make-v5-uuid *unique-random-namespace* "bubba"))
;=> T
```

## Printing a UUID object in hex-string-36 format with ```UUID-PRINC-TO-STRING```:

```Common Lisp
UNICLY> (uuid-princ-to-string (make-v5-uuid *unique-random-namespace* "bubba"))
;=> "065944a4-7566-53b2-811b-11a20e0bfed2"
```

Testing equivalence of two UUID objects where the first is generated using
```MAKE-V5-UUID``` and the second is generated from an equivelent hex-string-36
representation as per ```MAKE-UUID-FROM-STRING```:

```Common Lisp
UNICLY> (uuid-eql 
         (make-v5-uuid *unique-random-namespace* "bubba")
         (make-uuid-from-string "065944a4-7566-53b2-811b-11a20e0bfed2"))
;=> T
```

Binding a variable *another-unique-random-namespace* for use as a namespace.  We
initally bind it to the hex-string-36 representation of a v4 UUID per the return
value of ```UUID-PRINC-TO-STRING```:

```Common Lisp
UNICLY> (defparameter *another-unique-random-namespace* 
          (uuid-princ-to-string (unicly:make-v4-uuid)))
;=> *ANOTHER-UNIQUE-RANDOM-NAMESPACE*
```

Binding the ```*another-unique-random-namespace*``` variable to a UUID object:

```Common Lisp
UNICLY> (setf *another-unique-random-namespace*
              (make-uuid-from-string *another-unique-random-namespace*))
;=> f65c8371-0c41-4913-96e6-8a917666aa51
```

Creating a container to hold 32 v5 UUIDs for 16 names each of which will occupy
two distinct namespaces:

```Common Lisp
UNICLY> (defparameter *v5-uuids-in-distinct-unique-random-namespaces* '()) 
;=> *V5-UUIDS-IN-DISTINCT-UNIQUE-RANDOM-NAMESPACES*
```

Adding 32 v5 UUIDs to the container where each is a cons with the head of each
cons a UUID object and the tail the name of some object in a namespace.
For each name we create two UUIDs, one will occupy the namespace
```*unique-random-namespace*```, the other will occupy the namepsace
```*another-unique-random-namespace*```:

```Common Lisp
UNICLY> (loop
           initially (setf *v5-uuids-in-distinct-unique-random-namespaces* nil)
           for bubba in (loop
                           for cnt from 0 below 16  
                           collect (format nil "bubba-~D" cnt))
           do (push (cons (make-v5-uuid *unique-random-namespace*  bubba) bubba)
                    *v5-uuids-in-distinct-unique-random-namespaces*)
           (push (cons (make-v5-uuid *another-unique-random-namespace* bubba) bubba)
                 *v5-uuids-in-distinct-unique-random-namespaces*)
           finally (return *v5-uuids-in-distinct-unique-random-namespaces* ))

;=> ((7c34b05e-d7a0-573e-baa2-7cc407532609 . "bubba-15")
;     (f7922a16-0b67-5329-87c9-71fdaa52c6c1 . "bubba-15")
;     { ... }
;     (7af9b747-e1f4-59b1-8f05-0acb70220817 . "bubba-0")
;     (f3228291-0a24-5a46-a9e2-7963d4671069 . "bubba-0"))
```

Retrieving the UUID for the name "bubba-8" in the namespace ```*unique-random-namespace*```:

## Retrieving a UUID  using ```CL:ASSOC```'s with ```UUID-EQL``` as ```:TEST``` keyword: 

```Common Lisp
UNICLY> (assoc 
         (make-v5-uuid *unique-random-namespace* "bubba-8")
         *v5-uuids-in-distinct-unique-random-namespaces*
         :test #'uuid-eql)
;=> (8e64c855-70fd-5d53-82ce-67e545f724a1 . "bubba-8")
```

Retrieving the UUID for the name "bubba-8" in the namespace```*another-unique-random-namespace*```:

```Common Lisp
UNICLY> (assoc 
         (make-v5-uuid *another-unique-random-namespace* "bubba-8")
         *v5-uuids-in-distinct-unique-random-namespaces*
         :test #'uuid-eql)
;=> (ef74e326-4ecc-5edc-9b55-e69e6069610a . "bubba-8")
```

## Testing if two identical names can be ```UUID-EQL``` when each occupies a different namespace:

```Common Lisp
UNICLY> (uuid-eql 
         (make-v5-uuid *unique-random-namespace* "bubba-8")
         (make-v5-uuid *another-unique-random-namespace* "bubba-8"))
;=> NIL
```

## Testing if two identical names can be ```UUID-EQL``` when each occupies the same namespace:

```Common Lisp
UNICLY> (uuid-eql 
         (make-v5-uuid *unique-random-namespace* "bubba-8")
         (car (assoc 
               (make-v5-uuid *unique-random-namespace* "bubba-8")
               *v5-uuids-in-distinct-unique-random-namespaces*
               :test #'uuid-eql)))
;=> T
```

## UUID's as Bit Vector:

Examining the bit-vector representation of the ```*unique-random-namespace*``` UUID:

```Common Lisp
UNICLY> (uuid-to-bit-vector *unique-random-namespace*)
;=> #*01110111101110000100011101000101101010110001001101001001110001101000111111011100100110101111101010101011110001010001110001010010
```
## UUID Predicate:

Testing with ```UNIQUE-UNIVERSAL-IDENTIFIER-P``` whether the value of
```*unique-random-namespace*``` is an instance of class ```UNIQUE-UNIVERSAL-IDENTIFIER```:

```Common Lisp
UNICLY> (unique-universal-identifier-p *unique-random-namespace*)
;=> T
```

When testing an object with ```UNIQUE-UNIVERSAL-IDENTIFIER-P```, if the object is a
bit-vector, and the form of that bit-vector satisfies ```UUID-BIT-VECTOR-128-P```, and
the appropriate version bit of the bit-vector is set, indication is given that
the bit-vector may be coerceable to an object which would satisfy
```UNIQUE-UNIVERSAL-IDENTIFIER-P```. This indication is provided as the ```CL:NTH-VALUE``` 1
as illustrated by the following return value:

```Common Lisp
UNICLY> (unique-universal-identifier-p (uuid-to-bit-vector *unique-random-namespace*))
;=> NIL 
    (UUID-BIT-VECTOR-128 4)
```

Testing whether the null-uuid satisfies ```UNIQUE-UNIVERSAL-IDENTIFIER-P```:

```Common Lisp
UNICLY> (unique-universal-identifier-p (make-null-uuid))
;=> T
```
## UUID Byte Array

Converting a UUID to bit-vector representation with ```UUID-TO-BIT-VECTOR``` then
converting that to an integer value with ```UUID-BIT-VECTOR-TO-INTEGER```:

```Common Lisp
UNICLY> (uuid-bit-vector-to-integer (uuid-to-bit-vector *unique-random-namespace*))
;=> 159134959691145724577639637335874542674
```

### Converting a UUID to byte-array reresentation with ```UNICLY::UUID-TO-BYTE-ARRAY```:

```Common Lisp
UNICLY> (unicly::uuid-to-byte-array *unique-random-namespace*)
;=> #(119 184 71 69 171 19 73 198 143 220 154 250 171 197 28 82)
```

Converting a UUID to byte-array reresentation with ```UNICLY::UUID-TO-BYTE-ARRAY```
then converting that to a bit-vector:

```Common Lisp
UNICLY> (uuid-byte-array-to-bit-vector (unicly::uuid-to-byte-array *unique-random-namespace*))
;=> #*01110111101110000100011101000101101010110001001101001001110001101000111111011100100110101111101010101011110001010001110001010010
```
> [!NOTE]
> Above, when converting the UUID object to a byte-array, we used the internal
> symbol ```UNICLY::UUID-TO-BYTE-ARRAY```. However, the preferred interface for retrieving
> the byte-array representation of a UUID object is ```UUID:GET-NAMESPACE-BYTES```. 
>
> The symbol ```UNICLY::UUID-TO-BYTE-ARRAY``` is not exported by Unicly, because it's
> implementation conflicts with ```UUID:UUID-TO-BYTE-ARRAY``` (the two functions access
> differently named slot values of their respective base classes
> ```UNICLY:UNIQUE-UNIVERSAL-IDENTIFIER``` vs. ```UUID:UUID```).

## More UUID equality interrogations with ```UUID-EQL```:

### Testing if a UUID object is ```UUID-EQL``` to itself:

```Common Lisp
UNICLY> (uuid-eql *unique-random-namespace* *unique-random-namespace*)
;=> T
```

### Testing if a UUID object is UUID-EQL to its bit-vector representation:

```Common Lisp
UNICLY> (uuid-eql *unique-random-namespace* (uuid-to-bit-vector *unique-random-namespace*))
;=> T
```

### Testing if a UUID object is UUID-EQL to a copy of itself as per ```UUID-COPY-UUID```:

```Common Lisp
UNICLY> (let ((copy (uuid-copy-uuid *unique-random-namespace*)))
          (uuid-eql copy *unique-random-namespace*))
;=> T
```

### Testing if a UUID object is ```UUID-EQL``` to its byte-array representation:

> [!NOTE]
> ```UNICLY::UUID-TO-BYTE-ARRAY``` is not exported, and it's use is nominally
>  deprecated (see note above). As such, the underlying implementation of this
>  aspect of the UUID-EQL interface may to change in future versions of UNICLY!

```Common Lisp
UNICLY> (uuid-eql (uuid-to-byte-array *unique-random-namespace*)
                  *unique-random-namespace*)
;=> NIL
```

### Testing if two UUID bit-vector representations are ```UUID-BIT-VECTOR-EQL```:

```Common Lisp
UNICLY> (uuid-bit-vector-eql 
         (uuid-to-bit-vector *unique-random-namespace*)
         (uuid-byte-array-to-bit-vector (unicly::uuid-to-byte-array *unique-random-namespace*)))
;=>T
```

### Testing if two UUID bit-vector representations are ```UUID-EQL```:

```Common Lisp
UNICLY> (uuid-eql 
         (uuid-to-bit-vector *unique-random-namespace*)
         (uuid-byte-array-to-bit-vector (unicly::uuid-to-byte-array *unique-random-namespace*)))
;=> T
```
### Testing UUID bit vector equivalence with ```CL:EQUALP```:

Althogh Unicly provides the function ```UUID-BIT-VECTOR-EQL``` for bit vector
equivalence, we can also test if two UUID bit-vector representations are
```CL:EQUAL```:

```Common Lisp
UNICLY> (equal
         (uuid-to-bit-vector (make-v5-uuid *unique-random-namespace* "bubba"))
         (uuid-to-bit-vector (make-v5-uuid *unique-random-namespace* "bubba"))) 
;=> T

UNICLY> (equal
         (uuid-to-bit-vector (make-v5-uuid *unique-random-namespace* "bubba"))
         (uuid-to-bit-vector (make-v5-uuid *unique-random-namespace* "NOT-A-bubba")))
;=> NIL
```

Note, however, we can <ins>NOT</ins> do the same for two UUID byte-array
representations, instead we must use ```CL:EQUALP``` for that:

```Common Lisp
UNICLY> (equal
         (uuid-get-namespace-bytes (make-v5-uuid *unique-random-namespace* "bubba"))
         (uuid-get-namespace-bytes (make-v5-uuid *unique-random-namespace* "bubba")))
;=> NIL

UNICLY> (equalp
         (uuid-get-namespace-bytes (make-v5-uuid *unique-random-namespace* "bubba"))
         (uuid-get-namespace-bytes (make-v5-uuid *unique-random-namespace* "bubba")))
;=> T

UNICLY> (equalp
         (uuid-get-namespace-bytes (make-v5-uuid *unique-random-namespace* "bubba"))
         (uuid-get-namespace-bytes (make-v5-uuid *unique-random-namespace* "NOT-A-BUBBA")))
;=> NIL
```

To circumvent using ```CL:EQUALP``` we can ```UUID-BYTE-ARRAY-TO-BIT-VECTOR```
and then test byte-array equivalence with ```UUID-BIT-VECTOR-EQL```. See below
for additional examples.

### Comparing return value of ```UUID-EQL``` with CL native equality operators:

Following form illustrates UUID equality as compared with the CL native equality tests, namel:
```CL:EQ```, ```CL:EQL```, ```CL:EQUAL```, ```CL:EQUALP```, and ```CL:SXHASH```:

```Common Lisp
UNICLY> (let* ((uuid-1    (make-v5-uuid *uuid-namespace-dns* "bubba"))
               (uuid-1-bv (uuid-to-bit-vector (make-v5-uuid *uuid-namespace-dns* "bubba")))
               (uuid-2    (uuid-from-bit-vector uuid-1-bv)))
          (list :uuid-eql (uuid-eql uuid-1 uuid-2)
                :eq       (eq uuid-1 uuid-2)
                :eql      (eql uuid-1 uuid-2) 
                :equal    (equal uuid-1 uuid-2)
                :equalp   (equalp uuid-1 uuid-2)
                :sxhash   (list (sxhash uuid-1) (sxhash uuid-2))))
;=> (:UUID-EQL T :EQ NIL :EQL NIL :EQUAL NIL :EQUALP NIL :SXHASH (121011444 363948070))
```

## Round-tripping UUID representations:

Below is an example of round-tripping a UUID as follows:

 > uuid -> bit-vector -> uuid -> byte-array -> bit-vector -> uuid 
 >  -> byte-array -> uuid -> uuid-string-36 -> uuid

First, we verify the identity of the name "bubba" in the ```*UUID-NAMESPACE-DNS*``` namespace:

```Common Lisp
UNICLY> (make-v5-uuid *uuid-namespace-dns* "bubba")
;=> eea1105e-3681-5117-99b6-7b2b5fe1f3c7
```

Does the round-tripping return an equivalent object?

```Common Lisp
UNICLY> (make-uuid-from-string
         (uuid-princ-to-string
          (uuid-from-byte-array
           (uuid-to-byte-array
            (uuid-from-bit-vector
             (uuid-byte-array-to-bit-vector
              (uuid-to-byte-array 
               (uuid-from-bit-vector 
                (uuid-to-bit-vector 
                 (make-v5-uuid *uuid-namespace-dns* "bubba"))))))))))
;=> eea1105e-3681-5117-99b6-7b2b5fe1f3c7
```

Is previously evaluated result ```UUID-EQL``` to itself?

```Common Lisp
UNICLY> (uuid-eql * (make-v5-uuid *uuid-namespace-dns* "bubba"))
;=> T
```

## UUID Version Number Integer Representation:

We can interrogate a UUID object to find it's version.

### Get the integer version of a UUID object with ```UUID-VERSION-UUID```:

```Common Lisp
UNICLY> (uuid-version-uuid *unique-random-namespace*)
;=> 4

UNICLY> (uuid-version-uuid (make-v5-uuid *unique-random-namespace* "bubba-8"))
;=> 5
```
### Using the ```UUID-BIT-VECTOR-V4-P``` predicate to test the version of a UUID object:

```Common Lisp
UNICLY> (uuid-bit-vector-v4-p (uuid-to-bit-vector *unique-random-namespace*))
;=> T

UNICLY> (uuid-bit-vector-v4-p (uuid-to-bit-vector (make-v5-uuid *uuid-namespace-dns* "bubba")))
;-> nil
```

### Using the ```UUID-BIT-VECTOR-V5-P``` predicate to test the version of a UUID object:

```Common Lisp
UNICLY> (uuid-bit-vector-v5-p (uuid-to-bit-vector (make-v5-uuid *uuid-namespace-dns* "bubba")))
;-> T

UNICLY> (uuid-bit-vector-v5-p (uuid-to-bit-vector (make-v4-uuid)))
;=> NIL
```

## The UUID null-uuid:

The null-uuid is a special case, as such we use a dedicated inteface when frobbing it.

> [!NOTE]
> Some special mojo occurs behind the curtains to ensure unique identity for the
> null-uuid because the ```CL:SXHASH``` of the null-uuid is an intransient value.

```MAKE-NULL-UUID``` is the preferred interface for accessing the null-uuid. Use It!

### Generating an instance of the null-uuid with ```MAKE-NULL-UUID```:

```Common Lisp
UNICLY> (make-null-uuid)
;=> 00000000-0000-0000-0000-000000000000
```

### The predicate```UNIQUE-UNIVERSAL-IDENTIFIER-NULL-P```:

WE can test if return-value of ```MAKE-NULL-UUID``` is an instance of class
```UNIQUE-UNIVERSAL-IDENTIFIER-NULL``` with the predicate```UNIQUE-UNIVERSAL-IDENTIFIER-NULL-P```:

```Common Lisp
UNICLY> (unique-universal-identifier-null-p (make-null-uuid))
;=> T
```

### Get the version of the null-uuid with ```UUID-VERSION-UUID```. 

The null-uuid is special in that it's version is 0.

```Common Lisp
UNICLY> (uuid-version-uuid (make-null-uuid))
;=>  0
     UNICLY::NULL-UUID
```

> [!NOTE] 
> The ```CL:NTH-VALUE``` 1 can be checked to verify that every bit of the UUID
> object is 0 (as opposed to an object with a partial bit signature at bits
> 48-51 mimicing that of the null-uuid). Some care should be taken to avoid
> inadverdently misidentifying the null-uuid. Again, the null-uuid is special.

### Testing if the null-uuid is ```UUID-EQL``` to itself:

```Common Lisp
UNICLY> (uuid-eql (make-null-uuid) (make-null-uuid))
;=> T
```

## UUID as Bit Field:

The UUID is sometimes referenced as having an 8:4:4:4:12 hex string representation. 
We refer to this representation as a UUID object with type ```UUID-HEX-STRING-36```.

However, such references imply a string-centric view-point of the UUID when
really it is much saner to see the UUID as a sequence of bits or bytes.

Following table illustrates the components of a UUID as a bit/byte field. 
Note, it will not display correctly in a text-editor word/line wrapping is
enabled and/or your display is unable to lines of render text out to 140 columns :{

### The UUID as bit field:

```
 WEIGHT   INDEX      OCTETS                     BIT-FIELD-PER-OCTET
    4  | (0  31)  | 255 255 255 255         | #*11111111 #*11111111 #*11111111 #*11111111  | %uuid_time-low               | uuid-ub32
    2  | (32 47)  | 255 255                 | #*11111111 #*11111111                        | %uuid_time-mid               | uuid-ub16
    2  | (48 63)  | 255 255                 | #*11111111 #*11111111                        | %uuid_time-high-and-version  | uuid-ub16
    1  | (64 71)  | 255                     | #*11111111                                   | %uuid_clock-seq-and-reserved | uuid-ub8
    1  | (72 79)  | 255                     | #*11111111                                   | %uuid_clock-seq-low          | uuid-ub8
    6  | (80 127) | 255 255 255 255 255 255 | #*11111111 #*11111111 #*11111111 #*11111111 #*11111111 #*11111111 | %uuid_node | uuid-ub48
```

### The UUID bit-vector representation:

```Common Lisp
UNICLY> (uuid-to-bit-vector (make-v5-uuid *uuid-namespace-dns* "bubba"))
```

```
;=> #*11101110101000010001000001011110001101101000000101010001000101111001100110110110011110110010101101011111111000011111001111000111
;     !      !       !       !       !       !       !       !        !      !       !       !       !       !       !       !       !  
;     0      7       15      23      31      39      47      55       63     71      79      87      95      103     111     119     127
;      --1--   --2--   --3--   --4--   --5--   --6--   --7--    --8--   --9--   -10-   -11-     -12-    -13-    -14-    -15-    -16-  
;     |  time-low slot               | time-mid slot | time-high slot | rsvd |  low  |                node slot                      |
```

### The UUIDs binary integer representation:

```Common Lisp
UNICLY> #b11101110101000010001000001011110001101101000000101010001000101111001100110110110011110110010101101011111111000011111001111000111
;=> 317192554773903544674993329975922389959
```

## Other UUID representations:

### Get the byte-array reresentation of a UUIDs integer representation with ```UUID-INTEGER-128-TO-BYTE-ARRAY```:

```Common Lisp
UNICLY> (uuid-integer-128-to-byte-array 317192554773903544674993329975922389959)
;=> #(238 161 16 94 54 129 81 23 153 182 123 43 95 225 243 199)
 
UNICLY> (uuid-to-byte-array (make-v5-uuid *uuid-namespace-dns* "bubba"))
;=> #(238 161 16 94 54 129 81 23 153 182 123 43 95 225 243 199)
```

### The component octet bit-vector reresentation of a UUID:

```Common Lisp
UNICLY> (map 'list #'uuid-octet-to-bit-vector-8
             (uuid-to-byte-array (make-v5-uuid *uuid-namespace-dns* "bubba")))
;=> (#*11101110 #*10100001 #*00010000 #*01011110 #*00110110 #*10000001 #*01010001 #*00010111 
;    #*10011001 #*10110110 #*01111011 #*00101011 #*01011111 #*11100001 #*11110011 #*11000111)
```

### Converting from UUID -> byte-array -> bit-vector with ```UUID-BYTE-ARRAY-TO-BIT-VECTOR```:

```Common Lisp
UNICLY> (uuid-byte-array-to-bit-vector (uuid-to-byte-array (make-v5-uuid *uuid-namespace-dns* "bubba")))
;=> #*11101110101000010001000001011110001101101000000101010001000101111001100110110110011110110010101101011111111000011111001111000111
```
### The upper bounds of a UUID in binary integer representation:

```Common Lisp
UNICLY> #b11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
;=> 340282366920938463463374607431768211455
``` 

### Finding the number of unsigned bits used to represent the upper bounds of a UUID:

We can interrogate the  unsigned bits of a UUIDs integer representation with ```CL:INTEGER-LENGTH```:

```Common Lisp
UNICLY> (integer-length 340282366920938463463374607431768211455) 
;=> 128
```
### Finding the octet count of the upper bounds of a UUIDs integer representation:

```Common Lisp
UNICLY> (truncate (integer-length 340282366920938463463374607431768211455) 8)
;=> 16
```
### Getting the upper bounds of UUID in decimal integer representation (longform):

With ```CL:FORMAT```'s ```"~R"``` flag we can get a long form printed
representation of a UUID's decimal integer representation as follows:

```Common Lisp
UNICLY> (format t "~R" 340282366920938463463374607431768211455)
;=> three hundred forty undecillion two hundred eighty-two decillion three hundred
;   sixty-six nonillion nine hundred twenty octillion nine hundred thirty-eight
;   septillion four hundred sixty-three sextillion four hundred sixty-three
;   quintillion three hundred seventy-four quadrillion six hundred seven trillion
;   four hundred thirty-one billion seven hundred sixty-eight million two hundred
;   eleven thousand four hundred fifty-five
```

### Converting from a UUID bit-vector representation to an integer with ```UUID-BIT-VECTOR-TO-INTEGER```:

```Common Lisp
UNICLY> (uuid-bit-vector-to-integer (uuid-to-bit-vector (make-v5-uuid *uuid-namespace-dns* "bubba")))
;=> 317192554773903544674993329975922389959
```
### Converting from a UUID byte-array representation to an integer:

```Common Lisp
UNICLY> (uuid-integer-128-to-byte-array 317192554773903544674993329975922389959)
;=> #(238 161 16 94 54 129 81 23 153 182 123 43 95 225 243 199)
```
### Converting from a UUID byte-array representation to a UUID integer representation with ```UUID-BYTE-ARRAY-16-TO-INTEGER```:

```Common Lisp
UNICLY> (uuid-byte-array-16-to-integer 
         (uuid-integer-128-to-byte-array 317192554773903544674993329975922389959))
;=> 317192554773903544674993329975922389959
```

### Converting from a UUID integer representation to a UUID bit-vector representation:

```Common Lisp
UNICLY> (uuid-integer-128-to-bit-vector 317192554773903544674993329975922389959)
;=> #*11101110101000010001000001011110001101101000000101010001000101111001100110110110011110110010101101011111111000011111001111000111
```

### Testing if two UUIDs are ```UUID-BIT-VECTOR-EQL``` 

Here, we test here the first is coerced to a bit-vector from a UUID object, and the second is
coerced to a bit-vector from a UUID integer representation:

```Common Lisp
UNICLY> (uuid-bit-vector-eql (uuid-to-bit-vector (make-v5-uuid *uuid-namespace-dns* "bubba")) 
                             (uuid-integer-128-to-bit-vector 317192554773903544674993329975922389959))
;=> T
```

Testing if two UUIDs are ```UUID-BIT-VECTOR-EQL```, where the first is coerced to a
bit-vector from UUID integer representation and the second is coerced to a
bit-vector from a UUID byte-array representation:

```Common Lisp
UNICLY> (uuid-bit-vector-eql (uuid-integer-128-to-bit-vector 317192554773903544674993329975922389959)
                             (uuid-byte-array-to-bit-vector (uuid-integer-128-to-byte-array 317192554773903544674993329975922389959)))
;=> T
```

## Comparing Common Lisp UUID libraries Unicly vs Boian Tzonev's ```UUID``` CL library:

Unicly has a similar interface to Boian Tzonev's Common Lisp library ```UUID```: 
:SEE [Boian Tzonev's CL ```UUID``` Library](URL `https://github.com/dardoria/uuid')

Indeed, portions of the core of Unicly API are derived from Tzonev's ```UUID``` codebase.

However, Unicly deviates in some not insignificant ways from Tzonev's ```UUID```
library, and while we have made some attempt to create a compatibility layer
between the two libraries, the UUID objects generated with Unicly can not be used
interchangeably with those of Tzonev's ```UUID``` library.

Some notable differences between Unicly and Tzonev's ```UUID``` library:

* Unicly is developed on SBCL and with SBCL as it's primary targeted CL compiler.

   * Many routines are targeted towards making use of SBCL specific features.

   * It is highly declaration bound and inlined.

   * I do not test on implementations other than SBCL, but code for generating
     v3, v4, and v5 UUIDs *should* run portably on other Common Lisps ;}

*  Unicly is developed primarily for speedy minting of v3 and v5 UUIDs.
   On an x86-32 SBCL we have found Unicly's minting of v3 and v5 UUIDs to be
   significantly faster (at least 3-5x) than equivalent code from UUID.
   On 64bit ARM architecture the timing differences aren't as significant.
   :SEE [unicly/unicly-timings.lisp](https://github.com/monkpearman/unicly/blob/master/unicly-timings.lisp) for some timing comparisons.
   
   * Unicly is not particlulary faster than uuid when minting v4 UUIDS. 
   This is to be expected as both systems depend on frobbing *random-state*
   and there is little room for optimization beyond some internal declarations.
   
   * Unicly does however have different performace characteristcs when comparing
   timings of ```UNICLY:MAKE-V5-UUID``` with ```UUID:MAKE-V5-UUID```. 

   Following timings were made using functionally identical namespaces for 1mil
   invocations on an 64bit ARM SBCL.

   Name components were taken from an array of 1mil elements where each element was
   a randomly generated string and where each string was between 1-36 characters long
   and where each character of the string was a randomly chosen UTF-8 characater
   (pulled from a constrained set of 360). With each invocation having the basic form:

```Common Lisp
    (unicly:make-v5-uuid <NAMESPACE> <RANDOM-NAME>)
    (uuid:make-v5-uuid   <NAMESPACE> <RANDOM-NAME>)

UNICLY-TIMINGS> (generic-gc)
(time
 (loop 
    for x across *timing-random-array*
    do (unicly::make-v5-uuid unicly::*uuid-namespace-dns* x)))
Evaluation took:

  3.258 seconds of real time
  3.264746 seconds of total run time (3.222223 user, 0.042523 system)
  [ Real times consist of 0.073 seconds GC time, and 3.185 seconds non-GC time. ]
  [ Run times consist of 0.073 seconds GC time, and 3.192 seconds non-GC time. ]
  100.21% CPU
  1,583,015,056 bytes consed

UNICLY-TIMINGS> (generic-gc)
(time
 (loop 
    for x across *timing-random-array*
    do (uuid:make-v5-uuid  uuid:+namespace-dns+ x)))

Evaluation took:
  3.465 seconds of real time
  3.471200 seconds of total run time (3.449735 user, 0.021465 system)
  [ Real times consist of 0.060 seconds GC time, and 3.405 seconds non-GC time. ]
  [ Run times consist of 0.060 seconds GC time, and 3.412 seconds non-GC time. ]
  100.17% CPU
  1,359,595,008 bytes consed
```
   
   The above ratios are similar for the equivalent ```MAKE-V3-UUID``` functions.
   
   Other significant performace differences can be seen between Unicly and ```UUID```
   around the respective system's ```UUID-TO-BYTE-ARRAY```, ```UUID-FROM-BYTE-ARRAY```,
   ```MAKE-UUID-FROM-STRING``` functions.
   
   However, Unicly's biggest performance gains don't become apparent until using
   Unicly for equivalence tests and hash-table lookups using Unicly's extended
   interface as provided by ```UUID-EQL```, ```UUID-BIT-VECTOR-EQL```, etc.

* Unicly is extended with support for creating/storing/(de)serializing UUID
  objects as bit vectors.

   * For persistence libraries which make use of hash-tables to store their keys
     as UUID objects in hex-string-36 representation there are some potentially big
     gains to be had by moving to a bit-vector base UUID representation.
   
     For example, on SBCL it is possible to ```SB-EXT:DEFINE-HASH-TABLE-TEST``` which
     tests for UUID bit-vector equivalence using UUID-BIT-VECTOR-EQL instead of
     ```CL:EQUAL``` and ```CL:EQUALP```.
     (Underneath the covers ```UUID-BIT-VECTOR-EQL``` invokes ```SB-INT:BIT-VECTOR-=```.
      A nearly equivalent routine is provided for other CLs)

*  Unicly is extended with support for preserving identity of the null-uuid.
   This feature is specified in RFC 4122.

*  Unicly prints UUID string representations in case-significant form.
   This feature is specified in RFC 4122 Section 3. "Namespace Registration Template"
   as follows:

    > The hexadecimal values "a" through "f" are output as lower case characters
    > and are case insensitive on input.
    

* Unicly defines its base UUID class as ```UNIQUE-UNIVERSAL-IDENTIFIER```, instead of
  as the class UUID.

* Unicly does not expose accessors for the slots of the UUID class
  ```UNIQUE-UNIVERSAL-IDENTIFIER```.

* Unicly slot-names for the base class ```UNIQUE-UNIVERSAL-IDENTIFIER``` are strongly
  namespaced with "%uuid_". This intent here is twofold:

   * Our opinion is that UUID identity should be considered immutable once minted.
     There should be no need for user code to directly modify a UUIDs slot
     values. Obfuscating easy access to the class slots of
     ```UNIQUE-UNIVERSAL-IDENTIFIER``` helps prevent this.

    * Because the Unicly interface is similar to that of the ```UUID``` library we've
      attempted to prevent trivial visual namespace collision with the slots of
      the ```UUID``` library.  Projects using both Unicly and the ```UUID``` library may
      benefit from being able to easily distinguish among the two.

* Unicly's printing of a UUIDs string representation is not always conformant
  with ANSI spec.

    * The UUID ```CL:PRINT-OBJECT``` method for the class ```UNIQUE-UNIVERSAL-IDENTIFIER``` is
      not wrapped around ```CL:PRINT-UNREADABLE-OBJECT```.

* Unicly's interface is extensively documented.

* Unicly's source-code is commented with references to the relevant portions of
  RFC 4122.

* Unicly does not have a dependency on ```TRIVIAL-UTF-8```

   * SBCL users can use internal features (assuming a Unicode enabled SBCL)

   * non-SBCL code can (and should) use ```FLEXI-STREAMS``` instead.

* Unicly is not released under an LLGPL licenses.

   * If licensing issues are a concern in your project please take a moment to
     investigate unicly/LICENSE.txt

    > [!Note]
    > Although Unicly is initially derived from Tzonev's uuid library we note that
    > significant portions of that library were in turn strongly derived from the
    > non-normative reference implementation source code included of RFC4122 Appendix
    > C as a wholly functional C language source code implementation of RFC4122.

    > We believe the original RFC reference implementation and license have clear
    > precedent in lieu of the later LLGPL and believe it reasonable to revert to the
    > spirit of the original permissive and non-LLGPL'd license included of RFC4122.
   
*  Unicly is not targeted for generation of version 1 UUIDs (e.g. time based).

   The general implementation strategy for minting v1 UUID is reliant on
   interrogation of the system's underlying hardware and clock setting [1].
   When this is the strategy taken, we have found that:

    * It requires platform and implementation specific code;

    * Minting version 1 UUIDs requires interrogating the MAC address of an
      Ethernet device;

    * Minting version 1 UUIDs requires interrogating the system clock -- there
      are in general some notoriously nasty bugs which spring from reliance on
      the value of the system clock e.g. cross-platform multi-boot systems...

    * Minting version 1 UUIDs is slow;

    * There is no portably universal mechanism for generation of version 1 UUIDs
      Some implementations use the hardware for seed value others use a random-number.

    * The uniqueness of version 1 UUIDs is not nearly as robust as the v3, v4,
      v5 variants. There are numerous mechanisms by which a v1 UUID can
      become corrupted which simply do not affect the others.

   [^1]  RFC 4122 Section 4.5 "Node IDs that Do Not Identify the Host"
   Suggests that a v1 UUID may also be minted from a "47-bit cryptographic
   quality random number" by using it as the bottom 47 bits of the UUID Node id
   and setting the LSB of the first octet of the UUID node id to 1.
   Unfortunately, when attempting to implement this alternative strategy we found that
   Tzonev's uuid library has what we believe to be a bug in ```UUID:GET-NODE-ID``` in
   that it sets bit 0 of the the LS-Byte of a 48bit integer with:

   ```Common Lisp
     (setf node (dpb #b01 (byte 8 0) (random #xffffffffffff *random-state-uuid*)))
   ```
   
   Apparently, there is some confusion around the RFC's reference to the
   unicast/multicast bit, instead of the arguably more correct local/global bit.
   
   As it is now, when using Tzonev's uuid one can not reliably inspect a v1 UUID
   for its version because the bits are in the wrong sequence and disambiguation
   of of the various v1, v2, v3, v4, and v5 UUIDs is impossible.
   
  Ror additional details/discussion see [unicly/unicly-compat.lisp](https://github.com/monkpearman/unicly/blob/master/unicly-compat.lisp)

   We could attempt to accommodate this and propagate the error onward or do the
   prudent thing and simply rely on v3, v4, v5 UUIDs instead.

## Examples of Common Lisp libraries which make use of UUIDs:

[Vivace Graph v2](https://raw.github.com/kraison/vivace-graph-v2/master/triples.lisp)
[de.setf.resource](https://raw.github.com/lisp/de.setf.resource/master/resource-object.lisp)
[Blocky](https://raw.github.com/dto/blocky/master/prototypes.lisp)
[CL-mongo](https://raw.github.com/fons/cl-mongo/master/src/bson-oid.lisp)
[Kyoto Persistence](git://github.com/kraison/kyoto-persistence.git)

An Emacs lisp implementation of RFC 4122 UUID generation:

[uuid-el](https://github.com/kanru/uuid-el)

# RFC 4122 for Reference:

[RFC 4122 text](http://www.ietf.org/rfc/rfc4122.txt)
[RFC 4122 pdf](http://tools.ietf.org/pdf/rfc4122)

