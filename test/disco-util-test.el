;;; disco-util-test.el --- Tests for disco-util -*- lexical-binding: t; -*-

(require 'ert)

(require 'disco-util)

(ert-deftest disco-util-json-true-p-recognizes-json-truthy-values ()
  (should (disco-util-json-true-p t))
  (should (disco-util-json-true-p 'true))
  (should (disco-util-json-true-p "true"))
  (should-not (disco-util-json-true-p nil))
  (should-not (disco-util-json-true-p :false)))

(ert-deftest disco-util-normalize-id-list-preserves-order-and-uniques ()
  (should (equal '("1" "2" "3")
                 (disco-util-normalize-id-list '(1 "2" 1 3 nil "2")))))

(ert-deftest disco-util-normalize-id-list-applies-max-items ()
  (should (equal '("a" "b")
                 (disco-util-normalize-id-list '("a" "b" "c") 2))))

(provide 'disco-util-test)

;;; disco-util-test.el ends here
