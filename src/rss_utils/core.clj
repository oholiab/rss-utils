(ns rss-utils.core
  (:require [clojure.zip          :as zip]
            [clojure.data.zip.xml :as dzx]
            [clojure.java.io      :as io]
            [clojure.data.xml     :as xml]
            [clj-http.client      :as http]
            [clojure.string :as str]))

;; ----- BEGIN TERRIBLE HACK ----- ;;
; Stops data.xml 0.2.0-alpha3 from throwing an exception when xmlns prefix is
; correctly defined - this is a bug which has a fix out, but not in a release
; version

(in-ns 'clojure.data.xml.pu-map)
(defn assoc! [{:as put :keys [p->u u->ps]} prefix uri]
  ; Monkey patching the following bit out
  #_(when (or (core/get #{"xml" "xmlns"} prefix)
            (core/get #{name/xml-uri name/xmlns-uri} uri))
    (throw (ex-info "Mapping for xml: and xmlns: prefixes are fixed by the standard"
                    {:attempted-mapping {:prefix prefix
                                         :uri uri}})))
  (let [prev-uri (core/get p->u prefix)]
    (core/assoc! put
                 :p->u (if (str/blank? uri)
                         (core/dissoc! p->u prefix)
                         (core/assoc! p->u prefix uri))
                 :u->ps (if (str/blank? uri)
                          (dissoc-uri! u->ps prev-uri prefix)
                          (cond
                            (= uri prev-uri) u->ps
                            (not prev-uri) (assoc-uri! u->ps uri prefix)
                            :else (-> u->ps
                                      (dissoc-uri! prev-uri prefix)
                                      (assoc-uri! uri prefix)))))))

;; ----- END TERRIBLE HACK ----- ;;

(in-ns 'rss-utils.core)
(defmacro define-content-xmlns
  "Define `content` xmlns in current scope"
  []
  (def xmlns-content-url "http://purl.org/rss/1.0/modules/content/")
  (def xmlns-content-hash {:xmlns/content xmlns-content-url})
  (xml/alias-uri 'content xmlns-content-url))

(defmacro define-atom-xmlns
  "Define `atomfeed` xmlns in current scope. Renamed from `atom` so as to not cause confusion with the clojure mutable"
  []
  (def xmlns-atomfeed-url "http://www.w3.org/2005/Atom")
  (def xmlns-atomfeed-hash {:xmlns/atomfeed xmlns-atomfeed-url})
  (xml/alias-uri 'atomfeed xmlns-atomfeed-url))

(define-content-xmlns)
(define-atom-xmlns)

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
  (try
    (:body (http/get url {:headers default-headers}))
    (catch java.net.ConnectException e (do (println (str "Caught exception for URL " url ": " (.getMessage e)))
                                           (throw e)))))

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

(defn -fetch-or-local
  "If the url is local (`file:///...`) then return the url, if it is remote then fetch it and return the local path)"
  [url]
  (if (re-matches #"^file:///.+$" url)
    url
    (-fetch-local url)))

; FIXME: hardcoded tmpfile is awful
(defn parse-feed
  "Takes `url` of a feed and returns it fetched and parsed as an xml object"
  [url]
  (xml/parse (io/input-stream (-fetch-or-local url))))

(defn fields-atom?
  "Returns true if the list `fields` contains any atom fields, else returns false. FIXME: this should probably just check for a single instance of the namespace"
  [fields]
  (not (nil? (some #{::atomfeed/title ::atomfeed/link ::atomfeed/content ::atomfeed/author} fields))))

(defn is-atom?
  "Returns true if `feed` is an atom feed, else returns false"
  [feed]
  (= (:tag feed) ::atomfeed/feed))

(defn is-rss?
  "Returns true if `feed` is an rss feed, else returns false"
  [feed]
  (= (:tag feed) :rss))

(defn zip-at-first-item
  "Takes an rss `feed` and returns a zip located at the first item"
  [feed]
  (cond
    (is-rss? feed) (dzx/xml1-> (zip/xml-zip feed) :channel :item)
    (is-atom? feed) (dzx/xml1-> (zip/xml-zip feed) ::atomfeed/feed ::atomfeed/entry)
    :else (throw (RuntimeException. (str "Feed is not atom or rss. First tag is: " (:tag feed))))))

(defn is-atom-entry?
  "Returns true if `loc` is an atom entry, else returns false"
  [loc]
  (= (first (first loc)) [:tag ::atomfeed/entry]))

(defn is-rss-item?
  "Returns true if `loc` is an rss item, else returns false"
  [loc]
  (= (first (first loc)) [:tag :item]))

(defn is-item?
  "Retrurns true if `loc` is an rss-item-like entity (atom entry or rss item), else returns false"
  [loc]
  (or
   (is-rss-item? loc)
   (is-atom-entry? loc)))

(defn apply-if-item
  "If `loc` is an item, `func` will be applied to that loc and returned, otherwise `loc` is returned"
  [func loc]
  (if (is-item? loc)
    (func loc)
    loc))

(defn apply-to-items
  "Takes body of an RSS feed `feed` and applies `func` to each item where func must return a zip location"
  [feed func]
  (loop [loc (func (zip-at-first-item feed))]
    (let [next-loc (zip/right loc)]
      (if (nil? next-loc)
        (zip/root loc)
        (recur (apply-if-item func next-loc))))))

(defn for-each-item
  "Takes body of an RSS feed `feed` and returns a list of the results of applying `func` to each item"
  [feed func]
  (loop [loc (zip-at-first-item feed)
         ret [(func loc)]]
    (let [next-loc (zip/right loc)]
      (if (nil? next-loc)
        ret
        (recur
          next-loc
          (if (is-item? loc)
            (conj ret (func loc))
            ret))))))

(defn get-fields
  "Takes a feed item `item` and extracts the top level fields from it"
  [item]
  (remove nil? (map :tag
                    (:content (zip/node item)))))

(defn get-fields-from-first-item
  "Takes a feed `feed`, finds the first item and extracts the top level fields from it"
  [feed]
  (get-fields (zip-at-first-item feed)))

(defn get-value-from-item
  "Extracts the content from `field` inside `item`"
  [field item]
  (first (:content (zip/node (dzx/xml1-> item field)))))

(defn get-values-from-item
  "Extracts all of the values from `item` that exist under the list of `fields`"
  [fields item]
  (map #(get-value-from-item % item) fields))

(defn create-empty-feed
  "Generates an empty rss feed"
  []
  (xml/element :rss (merge {:version "2.0"} xmlns-content-hash)
               (xml/element :channel {}
                            (xml/element :item {}))))
