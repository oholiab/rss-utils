(ns rss-utils.core-test
  (:require [clojure.test :refer :all]
            [rss-utils.core :refer :all]
            [clojure.string :as str]
            [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.data.zip.xml :as dzx]
            [clojure.zip :as zip]))

(define-content-xmlns)
(define-atom-xmlns)

(defn string->stream
  ([s] (string->stream s "UTF-8"))
  ([s encoding]
   (-> s
       (.getBytes encoding)
       (java.io.ByteArrayInputStream.))))

(def example-atom-str
  (str/join "\n" [
                 "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
                 "<feed xmlns=\"http://www.w3.org/2005/Atom\">"
                 "  <title>Example Feed</title>"
                 "  <link href=\"http://example.org/\"/>"
                 "  <updated>2003-12-13T18:30:02Z</updated>"
                 "  <author>"
                 "    <name>John Doe</name>"
                 "  </author>"
                 "  <id>urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6</id>"
                 "  <entry>"
                 "    <title>Atom-Powered Robots Run Amok</title>"
                 "    <link href=\"http://example.org/2003/12/13/atom03\"/>"
                 "    <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>"
                 "    <updated>2003-12-13T18:30:02Z</updated>"
                 "    <summary>Some text.</summary>"
                 "  </entry>"
                 "</feed>"]))

(def example-rss-str
  (str/join "\n" [
                  "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
                  "<rss version=\"2.0\">"
                  "  <channel>"
                  "    <title>W3Schools Home Page</title>"
                  "    <link>https://www.w3schools.com</link>"
                  "    <description>Free web building tutorials</description>"
                  "    <item>"
                  "      <title>RSS Tutorial</title>"
                  "      <link>https://www.w3schools.com/xml/xml_rss.asp</link>"
                  "      <description>New RSS tutorial on W3Schools</description>"
                  "    </item>"
                  "    <item>"
                  "      <title>XML Tutorial</title>"
                  "      <link>https://www.w3schools.com/xml</link>"
                  "      <description>New XML tutorial on W3Schools</description>"
                  "    </item>"
                  "  </channel>"
                  "</rss>"]))

(def example-atom
  (xml/parse (string->stream example-atom-str)))

(def example-rss
  (xml/parse (string->stream example-rss-str)))

; FIXME: These two should be constructed data, otherwise they're being used to test the funcitons that produced them.
(def atom-first-item
  (zip-at-first-item example-atom))

(def rss-first-item
  (zip-at-first-item example-rss))

(deftest test--fetch-or-local
  (testing "knows the difference between a remote or local file"
    (binding [-fetch-local #(:not-local)]
      (is (= (-fetch-local "file:///blah") "file:///blah"))
      (is (= (-fetch-local "https:///grimmwa.re") :not-local)))))

(deftest test-original-tmpfile
  (testing "tmpfile name is correct"
    (is (=
          "/tmp/tmp-somesite.com.xml"
          (original-tmpfile "https://somesite.com/somestuff")))))

(deftest test--fetch-or-local
  (testing "-fetch-or-local on a local file just returns its name"
    (let [filename "file:///somelocalfile"]
      (is (=
           filename
           (-fetch-or-local filename))))))

(deftest test-fields-atom?
  (testing "returns true with a list containing atom field names"
    (let [all-atom-fields [::atomfeed/title ::atomfeed/link]
          some-atom-fields [::atomfeed/title :blerg]
          no-atom-fields [:blerg :blarg]]
      (is (= true (fields-atom? all-atom-fields)))
      (is (= true (fields-atom? some-atom-fields)))
      (is (= false (fields-atom? no-atom-fields))))))

(deftest test-is-atom?
  (testing "returns true for an atom feed and false for others"
    (is (= true (is-atom? example-atom)))
    (is (= false (is-atom? example-rss)))))

(deftest test-is-rss?
  (testing "returns true for an rss feed and false for others"
    (is (= true (is-rss? example-rss)))
    (is (= false (is-rss? example-atom)))))

(deftest test-is-item?
  (testing "returns true when an item is supplied"
    (is (= true (is-item? atom-first-item)))
    (is (= true (is-item? rss-first-item)))))

(deftest test-apply-if-item
  (testing "applies fn if the loc is an item, but returns the loc if not"
    (let [fn #(first (get-fields %))]
      (is (= (apply-if-item fn atom-first-item) ::atomfeed/title))
      (is (= (apply-if-item fn rss-first-item) :title))
      (is (= (apply-if-item fn example-atom) example-atom))
      (is (= (apply-if-item fn example-rss) example-rss)))))

(deftest test-is-atom-entry?
  (testing "returns true when an atom entry is supplied"
    (is (= true (is-atom-entry? atom-first-item)))
    (is (= false (is-atom-entry? rss-first-item)))))

(deftest test-is-rss-item?
  (testing "returns true when an atom entry is supplied"
    (is (= false (is-rss-item? atom-first-item)))
    (is (= true (is-rss-item? rss-first-item)))))

(deftest test-get-fields
  (testing "returns correct fields for feeds"
    (let [atom-fields '(::atomfeed/title
                        ::atomfeed/link
                        ::atomfeed/id
                        ::atomfeed/updated
                        ::atomfeed/summary)
          rss-fields  '(:title
                        :link
                        :description)]
      (is (= atom-fields (get-fields atom-first-item)))
      (is (= rss-fields (get-fields rss-first-item))))))

(deftest test-for-each-item
  ; FIXME: Crazy functional programming testing managing to use a partial wtf
  (testing "maps a function to items in a feed"
    (let [fn (fn
               [field item]
               (-> (dzx/xml1-> item field)
                   zip/node
                   :content
                   first))]
      (is (= (for-each-item example-rss (partial fn :title)) ["RSS Tutorial" "XML Tutorial"]))
      (is (= (for-each-item example-atom (partial fn ::atomfeed/title)) ["Atom-Powered Robots Run Amok"]))
      )))

(deftest test-get-fields-from-first-item
  (testing "gets list of feeds from the first item in the feed"
    (is (= (get-fields-from-first-item example-atom)
           '(::atomfeed/title
             ::atomfeed/link
             ::atomfeed/id
             ::atomfeed/updated
             ::atomfeed/summary)))
    (is (= (get-fields-from-first-item example-rss)
           '(:title :link :description)))))

(deftest test-get-value-from-item
  (testing "returns correct values for a given field"
    (let [atom-title "Atom-Powered Robots Run Amok"
          rss-title "RSS Tutorial"]
      (is (= atom-title (get-value-from-item ::atomfeed/title atom-first-item)))
      (is (= rss-title (get-value-from-item :title rss-first-item))))))

(deftest test-get-values-from-item
  (testing "returns correct values for a given list of fields"
    (is (= (get-values-from-item [::atomfeed/title ::atomfeed/link] atom-first-item) ["Atom-Powered Robots Run Amok" nil]))
    (is (= (get-values-from-item [:title :link] rss-first-item) ["RSS Tutorial" "https://www.w3schools.com/xml/xml_rss.asp"]))
    ))
; Unnecessary shite

(deftest test-create-empty-feed
  (testing "creates an empty feed"
    (is (= (xml/emit-str (create-empty-feed)) "<?xml version=\"1.0\" encoding=\"UTF-8\"?><rss xmlns:content=\"http://purl.org/rss/1.0/modules/content/\" version=\"2.0\"><channel><item/></channel></rss>"))))
