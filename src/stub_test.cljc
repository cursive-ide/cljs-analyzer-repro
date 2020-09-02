(ns stub-test
  #?(:cljs (:require-macros [stub-test])))

; These should not appear, since they will be found by indexing

(def normal-def nil)
(defn normal-defn [])
#?(:clj (defmacro normal-macro []))
(defmulti normal-multimethod :whatever)
(defprotocol NormalProtocol (normal-method [this]))

; These should all appear
#?(:clj
   (defmacro
     gen-stuff []
     `(do
        (def ~'hidden-def nil)
        (def ~'hidden-def2 "Doc" nil)
        (def ~(with-meta 'hidden-def3 {:author "Foo"}) nil)
        (def ~(with-meta 'hidden-def4 {:author "Foo" :doc "Moar doc"}) nil)
        (defn ~'hidden-defn [])
        (defn ~'hidden-defn2 "Doc" [])
        (defn ~(with-meta 'hidden-defn3 {:author "Foo"}) [])
        (defn ~(with-meta 'hidden-defn4 {:author "Foo" :doc "Moar doc"}) [])
        (defn ~'hidden-defn5 [foo#])
        (defn ~'hidden-defn6 ([foo#]) ([foo# bar#]))
        (defmulti ~'hidden-multi :unknown)
        (defmulti ~'hidden-multi2 "Doc" :unknown)
        (defmulti ~(with-meta 'hidden-multi3 {:author "Foo"}) :unknown)
        (defmulti ~(with-meta 'hidden-multi4 {:author "Foo" :doc "Moar doc"}) :unknown)
        (defprotocol ~'HiddenProtocol (~'hidden-method [this#] [this# that#]))
        (defprotocol ~'HiddenProtocol2 "Doc" (~'hidden-method2 [this#] [this# that#]))
        (defprotocol ~'HiddenProtocol3 (~'hidden-method3 [this#] [this# that#] "Method doc")))))

#?(:clj
   (defmacro
     gen-clj-stuff []
     `(do
        (defmacro ~'hidden-defmacro [])
        (defmacro ~'hidden-defmacro2 "Doc" [])
        (defmacro ~(with-meta 'hidden-defmacro3 {:author "Foo"}) [])
        (defmacro ~(with-meta 'hidden-defmacro4 {:author "Foo" :doc "Moar doc"}) [])
        (defmacro ~'hidden-defmacro5 [foo#])
        (defmacro ~'hidden-defmacro6 ([foo#]) ([foo# bar#])))))

#?(:clj
   (gen-clj-stuff))

(gen-stuff)
