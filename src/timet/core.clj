(ns timet.core
  (:gen-class)
  (:require [clj-time.core :as t]
            [clj-time.predicates :as tpr]
            [clojure.string :as s]))

(defn tracker [d m y & entries]
  "define a tracker by its start date and time tracking entries"
  {:start (t/local-date y m d)
   :entries entries
   :minutes-per-day (* 60 (/ 20 5))}) ; for now fixed to 20h

(defn record
  "define a record by its type and its duration in hours and minutes and optionally a comment"
  ([type h m]         {:type type :hour h :minute m})
  ([type h m comment] (assoc (record type h m) :comment comment)))

(def come (partial record :come))
(def go   (partial record :go))

(defn day [matcher checker]
  {:matcher matcher
   :checker checker})

(defn- time-of-date
  "combine date and hours/minutes data into datetime instance"
  [d h m] (t/local-date-time (t/year d) (t/month d) (t/day d) h m))

(defn work-day [matcher & come-go-comments]
  (let [come-go  (filter map? come-go-comments)
        comments (filter string? come-go-comments)
        comment  (when (seq comments) (s/join "; " comments))
        type     (cond
                   (= come-go-comments [:free]) :free
                   (= come-go-comments [:sick]) :sick
                   :else :work)]
    {:matcher matcher
     :checker (fn [_] {:type type
                       :come-go (if (= type :work)
                                  (vec come-go-comments)
                                  [(come 8 0) (go 12 0)])})})) ; fixed for 20h

(def mo (partial work-day tpr/monday?))
(def tu (partial work-day tpr/tuesday?))
(def we (partial work-day tpr/wednesday?))
(def th (partial work-day tpr/thursday?))
(def fr (partial work-day tpr/friday?))
(def sa (partial work-day tpr/saturday?))
(def su (partial work-day tpr/sunday?))

(defn enumerate-days
  "enumerates days from the tracker start"
  [cal]
  (iterate #(t/plus % (t/days 1)) (:start cal)))

(defn fill-blanks [cal]
  (defn assoc-date [entry date] (assoc entry :date date))
  (defn empty-entry [date] (assoc-date {} date))
  (let [today (t/today)]
    (loop [[d & ds :as days]    (enumerate-days cal)
           [e & es :as entries] (:entries cal)
           new-entries          []]
      (cond
       (and
        (empty? entries)
        (t/after? d today))    (assoc cal :entries new-entries)
       (empty? entries)        (recur ds nil (conj new-entries (empty-entry d)))
       ((:matcher e) d)        (recur ds es (conj new-entries (assoc-date ((:checker e) d) d)))
       :else                   (recur ds entries (conj new-entries (empty-entry d)))))))

(comment
  (fill-blanks (tracker 1 6 2014))
  )

(defn date-time-come-go-of-entry [e] (map #(assoc % :date-time (time-of-date (:date e) (:hour %) (:minute %)) :entry-type (:type e)) (:come-go e)))

(comment
  (map date-time-come-go-of-entry (:entries (fill-blanks (tracker 10 6 2014 (mo (come 12 0) (go 13 0))))))
  )

(def types [:work :sick :free])

(defn all-records
  ([cal] (mapcat date-time-come-go-of-entry (:entries cal)))
  ([cal type] (filter #(= type (:entry-type %)) (all-records cal))))

(defn check-start-with-come [cal]
  (doseq [t types]
    (println t)
    (let [recs (all-records cal t)]
    (println recs)
      (when (and
             (seq recs)
             (not= :come (:type (first recs))) (throw (Exception. "Please start your tracker with a come record"))))))) ; todo care for empty

(comment
  (mapcat date-time-come-go-of-entry (:entries (fill-blanks (tracker 1 1 2000 (mo (go 12  0))))))
  (mapcat date-time-come-go-of-entry (:entries (fill-blanks (tracker 1 1 2000 (mo :free)))))
  (check-start-with-come (fill-blanks (tracker 1 1 2000 (mo (go 12  0)))))
  (check-start-with-come (fill-blanks (tracker 1 1 2000 (mo :free))))
  (check-start-with-come (fill-blanks (tracker 1 1 2000 (mo :sick))))
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
  (defn add-0  [prev cgs] (if (= prev :come)
                            (into [(come 0 0)] cgs)
                            cgs))
  (defn add-24 [last cgs] (if (= last :come)
                            (conj cgs (go 24 0))
                            cgs))
  (assoc cal
    :entries (loop [[e & es :as entries] (:entries cal)
                    prev-type            :dont-care
                    prev-entry-type      :dont-care
                    new-entries          []]
               (if (nil? e)
                 new-entries
                 (let [last-type (reduce (fn [prev curr]
                                           (when (= prev curr) (throw (Exception. (str "consecutive " (name prev) " records on " (:date e)))))
                                           curr)
                                         prev-type
                                         (map :type (:come-go e)))]
                   (when (and (= prev-type :come) (not= prev-entry-type (:type e))) (throw (Exception. (str "type mismatch between two censecutive days on " (:date e)))))
                   (recur es
                          last-type
                          (:type e)
                          (conj new-entries
                                (assoc e :come-go (add-0 prev-type (add-24 last-type (:come-go e)))))))))))

(comment
  ;(check-and-fill-alternating-for-type (fill-blanks (tracker 20 9 2014 (mo (come 12 0) (go 13 0)))) :work)
  (check-and-fill-alternating (fill-blanks (tracker 20 9 2014 (mo (come 12 0) (go 13 0)))))
  (check-and-fill-alternating (fill-blanks (tracker 20 9 2014 (mo (come 12 0)) (tu (go 13 0)))))
  (check-and-fill-alternating (fill-blanks (tracker 20 9 2014 (mo (come 12 0)) (we (go 13 0)))))
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
