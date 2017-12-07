(ns rss-utils.core-test
  (:require [clojure.test :refer :all]
            [rss-utils.core :refer :all]
            [clojure.string :as str]
            [clojure.data.xml :as xml]
            [clojure.java.io :as io]))

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
