(ns goodreads.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :refer [union]]
            [clojure.xml :as xml]
            [clojure.tools.cli :as cli]
            [manifold.deferred :as d :refer [let-flow] :rename {let-flow dlet}]
            [aleph.http :as http]))

;XML helpers
(defn child [parent tag]
  (some #(when (= (:tag %) tag) %) (:content parent)))

(def el-val (comp first :content))

(def child-val (comp el-val child))

(defn ->shelves [response]
  (reduce
   (fn [shelves review-el]
     (let [book-el (child review-el :book)
           book {:id (child-val book-el :id)
                 :title (child-val book-el :title)}
           shelf-names (->> (child review-el :shelves)
                            :content
                            (map (comp keyword :name :attrs)))]
       (reduce (fn [shelves shelf-name]
                 (update shelves shelf-name (fnil conj #{}) book))
               shelves
               shelf-names)))
   {}
   (-> response (child :reviews) :content)))

(defn ->similar-books [response]
  (map (fn [book-el]
         (let [rating (-> book-el (child-val :average_rating) Float/parseFloat)
               authors (->> (child book-el :authors)
                            :content
                            (map #(hash-map :id (child-val % :id)
                                            :name (child-val % :name))))]
           {:id (child-val book-el :id)
            :title (child-val book-el :title)
            :link (child-val book-el :link)
            :authors authors
            :rating rating}))
       (-> response (child :book) (child :similar_books) :content)))

(defn build-recommentations
  [{:keys [api-key user-id number-books read-books-limit]}]
  (dlet [r (http/get  "https://www.goodreads.com/review/list"
                     {:query-params {:v 2
                                     :key api-key
                                     :id user-id
                                     :sort "date_read"}})
         shelves (-> r :body xml/parse ->shelves)
         shelf-book-ids #(->> % shelves (map :id) set)
         read-book-ids (shelf-book-ids :read)
         rs (->> read-book-ids
                 (take read-books-limit)
                 (map #(http/get "https://www.goodreads.com/book/show"
                                 {:query-params {:key api-key :id %}}))
                 (apply d/zip'))
         similar-books (mapcat (comp ->similar-books xml/parse :body) rs)
         ignored-book-ids (union read-book-ids
                                 (shelf-book-ids :currently-reading))]
    (->> similar-books
         (remove #(ignored-book-ids (:id %)))
         (sort-by :rating)
         reverse
         (take number-books))))

(def cli-options [["-u"
                   "--user-id"
                   "User you recommend books to"
                   :required "USER ID"
                   :parse-fn #(Integer/parseInt %)
                   :missing "User must be specified"]
                  ["-n"
                   "--number-books"
                   "How many books you want to recommend"
                   :required "NUMBER"
                   :default 10
                   :parse-fn #(Integer/parseInt %)]
                  ["-l"
                   "--read-books-limit"
                   "Max amount of read books you want to consider"
                   :required "NUMBER"
                   :default 20
                   :parse-fn #(Integer/parseInt %)]
                  ["-t"
                   "--timeout-ms"
                   "Wait before finished"
                   :required "MILLISECONDS"
                   :default 5000
                   :parse-fn #(Integer/parseInt %)]
                  ["-h" "--help"]])

(defn book->str [{:keys [title link authors]}]
  (format "\"%s\" by %s\nMore: %s"
          title
          (->> authors
               (map :name)
               (str/join ", "))
          link))

(defn print-recommendations [options]
  (let [books (-> (build-recommentations (dissoc options :timeout-ms))
                  (d/timeout! (:timeout-ms options) ::timeout)
                  deref)]
    (cond
      (= ::timeout books) (println "Not enough time :(")
      (empty? books) (println "Nothing found, leave me alone :(")
      :else (doseq [[i book] (map-indexed vector books)]
              (println (str "#" (inc i)))
              (println (book->str book))
              (println)))))

(defn -main [& args]
  (let [{:keys [options errors summary]} (cli/parse-opts args cli-options)]
    (cond
      (contains? options :help) (do (println summary)
                                    (System/exit 0))
      (some? errors) (do (->> errors (str/join \newline) println)
                         (System/exit 1))
      (empty? args) (do (println "Please, specify API key")
                        (System/exit 1))
      :else (->> args first (assoc options :api-key) print-recommendations))))
