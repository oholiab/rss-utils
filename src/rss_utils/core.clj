(ns rss-utils.core
  (:require [clojure.zip          :as zip]
            [clojure.data.zip.xml :as dzx]
            [clojure.java.io      :as io]
            [clojure.data.xml     :as xml]
            [clj-http.client      :as http]))

; FIXME: somehow don't hardcode this?
(def user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36")
(def default-headers {"User-Agent" user-agent})

; FIXME: make headers an optional arg
(defn get-body
  "Plain old http get with a friendly user agent"
  [url]
  (:body (http/get url {:headers default-headers})))

(defn fetch-local
  "Fetch `url` contents to `tmpfile`"
  [url tmpfile]
  (spit tmpfile (get-body url))
  (str "file://" tmpfile))

; FIXME: hardcoded tmpfile is awful
(defn parse-feed
  "Takes `url` of a feed and returns it fetched and parsed as an xml object"
  [url]
  (xml/parse (io/input-stream (fetch-local url "/tmp/parse-feed"))))

(defn for-each-item
  "Takes body of an RSS feed `feed` and applies `func` to each item`"
  [feed func]
  (loop [loc (func (dzx/xml1-> (zip/xml-zip feed) :channel :item))]
    (if (nil? (zip/right loc))
      (zip/root loc)
      (recur (func (zip/right loc))))))
