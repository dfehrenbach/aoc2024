#!/usr/bin/env bb

(require '[babashka.http-client :as http]
         '[babashka.fs :as fs])

(def session-cookie (System/getenv "AOC_SESSION"))

(defn day-url [day] (str "https://adventofcode.com/2024/day/" day))
(defn input-url [day] (str (day-url day) "/input"))
(defn day-dir [day] (str "clojure/src/day" day))

(defn get-url [url]
  (println "Downloading url:" url)
  (http/get url {:headers {"Cookie" session-cookie}}))

(defn write-file [file text]
  (println "Writing file:" file)
  (spit file text))

(let [[day] *command-line-args*]
  (when (empty? day)
    (println "Usage: <day>")
    (System/exit 1))
  (let [day (read-string day) dir (day-dir day)]
    (fs/create-dirs dir)
    (write-file (str dir "/prompt.txt") (:body (get-url (day-url day))))
    (write-file (str dir "/input.txt") (:body (get-url (input-url day))))
    (write-file (str dir "/core.clj") (str "(ns day" day ".core (:require [clojure.string :as string]))"))))
