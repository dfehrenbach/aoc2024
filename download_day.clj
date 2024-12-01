#!/usr/bin/env bb

(require '[babashka.http-client :as http])

(def session-cookie "session=53616c7465645f5f98ce9c0660053e48e1ab4ae9cfb39836492efd3e877880a3657bf24efb303f4b7408bce5e60033651bddcee81cee99efb21fb7aed1485685")

(defn day-url [day] (str "https://adventofcode.com/2024/day/" day))
(defn input-url [day] (str (day-url day) "/input"))
(defn day-dir [day] (str "clojure/src/day" day))

(defn get-url [url]
  (println "Downloading url:" url)
  (http/get url {:headers {"Cookie" session-cookie}}))

(defn write-html [file html]
  (println "Writing file:" file)
  (spit file html))

(let [[day] *command-line-args*]
  (when (empty? day)
    (println "Usage: <url> <file>")
    (System/exit 1))
  (let [day (read-string day) dir (day-dir day)]
    (write-html (str dir "/prompt.html") (:body (get-url (day-url day))))
    (write-html (str dir "/input.txt") (:body (get-url (input-url day))))))
