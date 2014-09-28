(ns timet.core
  (:gen-class)
  (:require [clj-time.core :as t]
            [clj-time.predicates :as tpr]
            [clojure.string :as s]))

(defn tracker [d m y & entries]
  {:start (t/local-date y m d)
   :entries entries
   :minutes-per-day (* 60 (/ 20 5))})

(defn record
  ([type h m]         {:type type :hour h :minute m})
  ([type h m comment] (assoc (record type h m) :comment comment)))

(def come (partial record :come))
(def go   (partial record :go))

(defn day [matcher checker]
  {:matcher matcher
   :checker checker})

(defn work-day [matcher & come-go-comments]
  (defn time-of-date [d h m] (t/local-date-time (t/year d) (t/month d) (t/day d) h m))
  (let [come-go  (filter map? come-go-comments)
        comments (filter string? come-go-comments)
        comment  (when (seq comments) (s/join "; " comments))]
    {:matcher matcher
     :checker (fn [_] {:come-go come-go-comments})}))

(def mo (partial work-day tpr/monday?))
(def tu (partial work-day tpr/tuesday?))
(def we (partial work-day tpr/wednesday?))
(def th (partial work-day tpr/thursday?))
(def fr (partial work-day tpr/friday?))
(def sa (partial work-day tpr/saturday?))
(def su (partial work-day tpr/sunday?))

(defn fill-blanks [cal]
  (defn assoc-date [entry date] (assoc entry :date date))
  (defn empty-entry [date] (assoc-date {} date))
  (let [today (t/today)]
    (loop [date                 (:start cal)
           [e & es :as entries] (:entries cal)
           new-entries          []]
      (let [next-date     (t/plus date (t/days 1))]
        (cond
         (and
          (empty? entries)
          (t/after? date today)) (assoc cal :entries new-entries)
         (nil? e)                (recur next-date nil (conj new-entries (empty-entry date)))
         ((:matcher e) date)     (recur next-date es (conj new-entries (assoc-date ((:checker e) date) date)))
         :else                   (recur next-date entries (conj new-entries (empty-entry date))))))))

(comment
  (fill-blanks (tracker 1 6 2014))
  )

(defn time-of-date [d h m] (t/local-date-time (t/year d) (t/month d) (t/day d) h m))

(defn date-time-come-go-of-entry [e] (map #(assoc % :date-time (time-of-date (:date e) (:hour %) (:minute %))) (:come-go e)))

(comment
  (map date-time-come-go-of-entry (:entries (fill-blanks (tracker 10 6 2014 (mo (come 12 0) (go 13 0))))))
  )


(defn all-records [cal] (mapcat date-time-come-go-of-entry (:entries cal)))

(defn check-start-with-come [cal]
  (when-not (= :come (:type (first (all-records cal)))) (throw (Exception. "Please start your tracker with a come record"))))

(comment
  (check-start-with-come (tracker 1 1 2000 (mo (go 12  0))))
  ((:checker (work-day nil
                     (come  5  0 "bla")
                     (go   14 30)
                     "lol"
                     (come 15 00)
                     (go   16 45 "blub")
                     "hu"
                     )) (t/local-date 2014 4 7))
)

(defn check-and-fill-alternating [cal]
  (assoc cal :entries (loop [[e & es :as entries] (:entries cal)
                             first-c-iter?        true
                             [c & cs]             nil
                             pre-type             :dont-care
                             new-come-go          []
                             new-entries          []]
                        (cond
                         (nil? e)      new-entries
                         first-c-iter? (recur entries false (:come-go e) pre-type (if (= pre-type :come) ; prev day ended in come, we then inserted go@24:00
                                                                                    [(come 0 0)] ; and now insert come@00:00
                                                                                    []) new-entries)
                         (nil? c)      (recur es true nil pre-type nil (conj new-entries (assoc e :come-go (if (= pre-type :come) ; last record was come
                                                                                                             (conj new-come-go (go 24 0)) ; we insert go@24:00, the next day we add come@00:00
                                                                                                             new-come-go))))
                         :else         (if (= pre-type (:type c))
                                              (throw (Exception. (str "consecutive " (name pre-type) " records on " (:date e))))
                                              (recur entries false cs (:type c) (conj new-come-go c) new-entries))))))

(comment
  (check-and-fill-alternating (fill-blanks (tracker 10 6 2014 (mo (come 12 0) (go 13 0)))))
  (check-and-fill-alternating (fill-blanks (tracker 10 6 2014 (mo (come 12 0) (come 13 0)))))
  (check-and-fill-alternating (fill-blanks (tracker 10 6 2014 (mo (come 12 0)) (tu (go 13 0)))))
  (check-and-fill-alternating (fill-blanks (tracker 10 6 2014 (mo (come 12 0)) (we (go 13 0)))))
  )

(defn check-order [cal]
  (reduce (fn [l r] (when-not (nil? l)
                      (when-not (t/before? l r)
                        (throw (Exception. (str "Record out of order: " r)))))
            r) nil (map :date-time (all-records cal)))
  cal)

(comment
  (all-records (fill-blanks (tracker 10 6 2014 (mo (come 12 0) (go 13 0)))))
  (check-order (fill-blanks (tracker 10 6 2014 (mo (come 12 0) (go 13 0)))))
  (check-order (fill-blanks (tracker 10 6 2014 (mo (come 12 0) (go 11 0)))))
  )


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
