(ns timetable.core2
  "This is a small timetable scheduler written in core.logic intended to demo some core.logic functionality.  The place to start is the (run ...) and then trace back the symbols.  It implements some basic stuff, but is not a complete timetabler."
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]
        [clojure.tools.macro]))

;;  --------------------------------------------------------
;;   Useful goals
(defne rembero
  "Succeeds if out is the list l with the *first* (not all) instance of element x removed"
  [x l out]
  ([_ [] []])
  ([x [x . d] d])
  ([_ [a . d] [a . r]]
     (!= a x)
     (rembero x d r)))

(defne permo
  "Succeeds if x1 is a permutation of x2."
  [v1 v2]
  ([[] []])
  ([[h . t] v2]
     (fresh [rv2]
            (membero h v2)
            (rembero h v2 rv2)
            (permo t rv2))))

(defne subseto
  "Succeeds if x1 is a subset of x2.  x1 is distinct with elements of x2 but is shorter than or equal to x2"
  [x1 x2]
  ([[] _])
  ([[a . r] x2]
     (fresh [rx2]
            (membero a x2)
            (rembero a x2 rx2)
            (subseto r rx2))))

(defne distincto
  "Succeeds if every element of x is unique"
  [x]
  ([[f . nil]])
  ([[f . r]]
     (rembero f r r)
     (distincto r)))

(defne can-lectureo
  "Succeeds if whenever course c appears in the period, the associated lecturer, l1,  is an element of l, the allowable lecturers for that course."
  [c l tt]
  ([c l []])                 ;; Base case
  ([c l [[c l1 _ _] . r]]    ;; Period course is input course
     (membero l1 l)
     (can-lectureo c l r))
  ([c l [[c1 _ _ _] . r]]    ;; Period course is not input course
     (!= c1 c)
     (can-lectureo c l r)))

(defne attendso
  "Succeeds if the vector of attends is EXACTLY (not a perm) a whenever the content is c"
  [c a q]
  ([c a []])
  ([c a [[c _ _ a] . t]]
     (attendso c a t))
  ([c a [[c1 _ _ _] . t]]
     (!= c1 c)
     (attendso c a t)))

(defne no-concurrent-lecturero
  "Succeed if during a concurrent the same lecturer does not appear twice."
  [conc tmp]
  ([[] tmp] (distincto tmp))
  ([[[_ l _ _] . t] tmp]
     (fresh [x]
            (conso l tmp x)
            (no-concurrent-lecturero t x))))

;; =============================================================================
;; So lets start by defining the atoms.

(def courses   [:rhetoric :dialectics :mathematics :astronomy])
(def lecturers [:plato :aristotle :archimedes :herodetes-of-larissa])
(def venues    [:parthenon :senate :prison])
(def students  [:incanters :rhetors :sophists])

;; -----------------------------------------------------------------------------
;; Now the structure of the solution
;;
;; Timetable format [[period1] ... [periodN]]
;;
;;  Period format
;;     [course lecturer venue students]
;;  courses is what is taught
;;  lecturer is just who's giving it
;;  venue is the venue
;;  student is the student attending the lecture
;;
;;  ie [[:logic :plato :parthenon :incanter] [:rhetoric :artistotle :senate :rhetors] .....]

(run 1 [timetable]
     (fresh [c1 l1 v1 s1
             c2 l2 v2 s2
             c3 l3 v3 s3
             c4 l4 v4 s4
             conc1 conc2]

            ;; ------------------------------------------------

            ;;  Put some structure over the lvars

            ;; Collect the lvars into associated periods,  This structure
            ;; is how we will 'think' about the problem
            (== timetable [[c1 l1 v1 s1]
                           [c2 l2 v2 s2]
                           [c3 l3 v3 s3]
                           [c4 l4 v4 s4]])

            ;; Concurrents are periods that occur concurrently
            (== conc1 [[c1 l1 v1 s1]
                       [c2 l2 v2 s2]])
            (== conc2 [[c3 l3 v3 s3]
                       [c4 l4 v4 s4]])

            ;; ------------------------------------------------
            ;; Start constraning things

            ;; Everything must be taught once
            (permo [c1 c2 c3 c4] courses)

            ;; Who can teach what
            (can-lectureo :dialectics  [:aristotle] timetable)
            (can-lectureo :mathematics [:archimedes :artistotle] timetable)
            (can-lectureo :rhetoric    [:aristotle :herodetes-of-larissa]   timetable)  ;; RH
            (can-lectureo :astronomy   [:plato :herodetes-of-larissa]       timetable)

            ;; Who attends what
            (attendso :dialectics  :incanters timetable)
            (attendso :rhetoric    :rhetors   timetable)
            (attendso :mathematics :sophists  timetable)

            ;; Deal with the concurrency.
            (no-concurrent-lecturero conc1 [])

              ;;; etc, blah, add new constraints for no concurrent attendees etc.
            ))
