;;; Compiled snippets and support files for `js-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'js-mode
                     '(("describe" "describe('$1', function () {\n    $0\n});\n" "describe" nil
                        ("Jasmine")
                        nil nil nil nil)
                       ("describe" "expect($0)\n" "expect" nil
                        ("Jasmine")
                        nil nil nil nil)
                       ("it" "it ('$1', function () {\n   $0\n});\n" "it" nil
                        ("Jasmine")
                        nil nil nil nil)))


;;; Do not edit! File generated at Sat Jan 24 19:55:54 2015
