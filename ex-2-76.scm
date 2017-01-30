;; Exercise 2.76
;;
;; Description:
;;
;; For each of the following generic operation strategies:
;;
;;    1. Explicit Dispatch
;;    2. Data-Directed Computation
;;    3. Message-Passing
;;
;; Describe the changes that must be made to add new types or operations. Which
;; strategy is best for a system which frequently adds new types? Which strategy
;; is best for a system which frequently adds new operations?
;;



;; Solution:
;;
;; 1. Explicit Dispatch
;;
;; Explicit dispatch is the mechanism in which type-tags are added to data. These
;; tags are then parsed in procedures operating on the compound data, which conditionally
;; route computation on the type tag. This strategy is weak in type addition since it
;; requires explicit support for an arbitrary amount of types, and requires prior knowledge
;; into all possible types when implementing a procedure for that data type. The strength in
;; Explicity Dispatch is the ability to group all implementations for a given operation
;; together, facilitating the additions of new operations using spatial locality.

(define (angle z)
  (cond ((rectangular? z)
	 (angle-rectangular (contents z)))
	((polar? z)
	 (angle-polar (contents z)))
	(else (error "Unkown type -- ANGLE" z))))

;; 2. Data-Directed Computation
;;
;; Data-Directed Computation gives a generic system the partial type-additivity, in that
;; new types selectors may be added to the system without modification to the existing types.
;; The core of the Data-Directed strategy is the implementation lookup table, which
;; allows implementations for new type operations to be inserted and executed when a data object
;; with the same type-tag is evaluated. The generic interface for the data object specifies the
;; procedure names acting upon the object, therefore the programmer need only provide an
;; implementation and type-tag for a given abstract procedure. This method fares slightly worse
;; than the Message-Passing strategy in type addition, since constructors for newly added
;; types will need to be added to existing types in order to provide compatibility between
;; the various type constructors and representations. The additions of operations requires the
;; modification of the generic interface and all packages implementing the interface, but utilizes
;; lexical scoping to allow the packages to be developed independently.

(define (install-rectangular-package)
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  ;; Interface
  (put 'angle '(rectangular) angle))

(define (angle z) (apply-generic 'angle z))

;; 3. Message-Passing
;;
;; Message-Passing is the notion of implementing data objects as procedures who perform
;; an internal dispatch on the message (procedure) being sent (evaluated). This strategy has
;; full type-additivity and allows new operations to be added with a clause in the procedural
;; representation of the type. This system, like the Data-Directed system requires the
;; addition of operations to be implementated in all existing types and is therefore not
;; operation-additive. However in contrast to the Data-Directed strategy, intelligent data
;; objects dipatch on the operation name and encapsulate internal implementations, requiring
;; only one independent constructor and eliminating the need to tag data and index
;; implementations in a global lookup table. Therefore it seems Message-Passing is ideal in
;; systems which feature continual type additions. Like Data-Directed Computation,
;; Message-Passing suffers from the need to modify both the generic interface, and type
;; implementations when new operations are to be added.

(define (make-from-real-imag x y)
  (define (dispatch op)
    (if (eq? op 'angle)
	(atan y x)
        (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))

(define (angle complex-number)
  (apply-generic 'angle complex-number))



;; Notes:
;;
;; This is known as the Expression Problem. The problem is neither fully adressed by
;; Functional Programming (similar to Explicit Dispatch) or Object-Oriented Programming
;; (similar to Data-Directed & Message-Passing).
;;
;; http://wiki.c2.com/?ExpressionProblem

