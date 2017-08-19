(ns rss-utils.core
  (:require [clojure.zip          :as zip]
            [clojure.data.zip.xml :as dzx]
            [clojure.java.io      :as io]
            [clojure.data.xml     :as xml]
            [clj-http.client      :as http]
            [clojure.string :as str]))

; FIXME: somehow don't hardcode this?
(def user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36")
(def default-headers {"User-Agent" user-agent})

(defn -get-host
  "Get the host portion of a URL"
  [url]
  (.getHost (new java.net.URL url)))

; FIXME: make headers an optional arg
(defn -get-body
  "Plain old http get with a friendly user agent"
  [url]
  (:body (http/get url {:headers default-headers})))

(defn original-tmpfile
  "Get tempfile name"
  [url]
  (str "/tmp/tmp-" (-get-host url) ".xml"))

(defn -fetch-local
  "Fetch `url` contents to `tmpfile`"
  [url]
  (let [tmpfile (original-tmpfile url)]
    (spit tmpfile (-get-body url))
    (str "file://" tmpfile)))

; FIXME: hardcoded tmpfile is awful
(defn parse-feed
  "Takes `url` of a feed and returns it fetched and parsed as an xml object"
  [url]
  (xml/parse (io/input-stream (-fetch-local url))))

(defn zip-at-first-item
  "Takes an rss `feed` and returns a zip located at the first item"
  [feed]
  (dzx/xml1-> (zip/xml-zip feed) :channel :item))

(defn apply-to-items
  "Takes body of an RSS feed `feed` and applies `func` to each item where func must return a zip location"
  [feed func]
  (loop [loc (func (zip-at-first-item feed))]
    (if (nil? (zip/right loc))
      (zip/root loc)
      (recur (func (zip/right loc))))))

(defn for-each-item
  "Takes body of an RSS feed `feed` and returns a list of the results of applying `func` to each item"
  [feed func]
  (loop [loc (zip-at-first-item feed)
         ret [(func loc)]]
    (if (nil? (zip/right loc))
      ret
      (recur (zip/right loc) (conj ret (func loc))))))
