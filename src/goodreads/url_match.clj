(ns goodreads.url-match
  (:require [clojure.string :as str]))

(defprotocol Pattern
  (recognize [this string]))

(def url-re #"^(http|https):\/\/([\w-\.]+)/?([\w-/]*)\??(.*)")

(defn- match-segments [bindings [pattern value]]
  (cond
    (str/blank? value) (reduced nil)
    (keyword? pattern) (conj bindings [pattern value])
    (str/blank? pattern) (reduced nil)
    (= pattern value) bindings
    :else (reduced nil)))

(defn- zip-segments [s s']
  (take (max (count s) (count s'))
        (map vector
             (concat s (repeat nil))
             (concat s' (repeat nil)))))

(defrecord URLPattern [host path-segments query-params]
  Pattern
  (recognize [_ string]
    (when-let [[_ _ host' pathstr querystr] (re-find url-re string)]
      (when (or (nil? host) (= host' host))
        (let [path-segments' (str/split pathstr #"/")
              pairs (if (seq path-segments)
                      (zip-segments path-segments path-segments')
                      ())
              bindings (reduce match-segments [] pairs)
              query-params' (->> (str/split querystr #"&")
                                 (remove str/blank?)
                                 (map #(str/split % #"="))
                                 (into {}))]
          (when bindings
            (reduce
             (fn [bindings' [pattern bind]]
               (let [value (query-params' pattern)]
                 (if (nil? value)
                   (reduced nil)
                   (conj bindings' [bind value]))))
             bindings
             query-params)))))))

(defn url-pattern [pattern-str]
  (->> (str/split pattern-str #";")
       (remove str/blank?)
       (map str/trim)
       (reduce
        (fn [pattern part]
          (let [[_ ptype pstr] (re-find #"(\w+)\((.+)\)" part)]
            (case ptype
              "host"
              (assoc pattern :host pstr)
              "path"
              (let [segments (map #(if (= (first %) \?)
                                     (-> % (subs 1) keyword)
                                     %)
                                  (str/split pstr #"/"))]
                (assoc pattern :path-segments segments))
              "queryparam"
              (let [[param bind] (str/split pstr #"=\?")]
                (update pattern :query-params assoc param (keyword bind))))))
        (->URLPattern nil nil {}))))

(comment
  (def twitter (url-pattern "host(twitter.com); path(?user/status/?id);"))
  (recognize twitter "http://twitter.com/bradfitz/status/562360748727611392")
  ;; => [[:id 562360748727611392] [:user "bradfitz"]]

  (def dribbble (url-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);"))
  (recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
  ;; => [[:id "1905065-Travel-Icons-pack"] [:offset "1"]]
  (recognize dribbble "https://twitter.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
  ;; => nil ;; host mismatch
  (recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users")
  ;; => nil ;; offset queryparam missing
  (def some-site (url-pattern "host(some-site.com);   path(?foo/bar/?baz/?qux); queryparam(offset=?o)  ; queryparam(limit=?l); "))
  (recognize some-site "https://some-site.com/111/bar/222/333?limit=10&offset=72&gg=22"))
  ;; => [[:foo "111"] [:baz "222"] [:qux "333"] [:o "72"] [:l "10"]]
