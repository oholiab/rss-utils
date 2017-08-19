(ns rss-utils.core-test
  (:require [clojure.test :refer :all]
            [rss-utils.core :refer :all]))

(deftest test-original-tmpfile-out
  (testing "tmpfile is correct"
    (is (= 
          "/tmp/tmp-somesite.com.xml"
          (original-tmpfile "https://somesite.com/somestuff")
          ))))
