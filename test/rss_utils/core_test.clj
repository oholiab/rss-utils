(ns rss-utils.core-test
  (:require [clojure.test :refer :all]
            [rss-utils.core :refer :all]))

(deftest test-original-tmpfile
  (testing "tmpfile name is correct"
    (is (= 
          "/tmp/tmp-somesite.com.xml"
          (original-tmpfile "https://somesite.com/somestuff")
          ))))

(deftest test--fetch-or-local
  (testing "-fetch-or-local on a local file just returns its name"
    (let [filename "file:///somelocalfile"]
      (is (=
           filename
           (-fetch-or-local filename))))))
