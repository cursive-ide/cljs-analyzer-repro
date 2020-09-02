(ns cljs-analyzer-repro.core
  (:require [cljs.analyzer.api :as ana]
            [cljs.closure]
            [cljs.compiler.api :as comp]
            [cljs.env]
            [clojure.set]
            [clojure.java.io :as io]))

(defn- empty-seq?[x]
  (and (seqable? x) (not (seq x))))

(defn remove-empties[m]
  (into {} (filter (comp not empty-seq? second) m)))

(defn assoc-some
  "Associates a key with a value in a map, if and only if the value is not nil."
  ([m k v]
   (if (nil? v) m (assoc m k v)))
  ([m k v & kvs]
   (reduce (fn [m [k v]] (assoc-some m k v))
           (assoc-some m k v)
           (partition 2 kvs))))

(defn update-some
  "Updates a key in a map with a function, if and only if the return value from
  the function is not nil."
  [m k f & args]
  (assoc-some m k (apply f (m k) args)))

(defn- remove-quote [x]
  (if (and (seq? x) (= (first x) 'quote))
    (second x)
    x))

(defn- protocol-methods [protocol vars]
  (let [proto-name (name (:name protocol))]
    (filter #(when-let [p (:protocol %)] (= proto-name (name p))) vars)))

(defn- multimethod? [var]
  (= (:tag var) 'cljs.core/MultiFn))

(defn- var-type [opts]
  (cond
    (:macro opts) :macro
    (:protocol-symbol opts) :protocol
    (multimethod? opts) :multimethod
    :else :var))

(defn- read-var [vars var]
  (let [vt (var-type var)]
    (-> var
        (select-keys [:name :arglists :doc :dynamic
                      :added :deprecated])
        (update-some :name (comp symbol name))
        (update-some :arglists remove-quote)
        (assoc-some :type vt
                          :members (->> (protocol-methods var vars)
                                        (map #(read-var vars %))
                                        (map remove-empties)
                                        (map #(dissoc % :file :line))))
        remove-empties)))

(defn- read-publics [state namespace]
  (let [vars (vals (ana/ns-publics state namespace))]
    (->> vars
         (remove :anonymous)
         (map #(read-var vars %)))))

(defn- fake-js-deps
  "Generate a value for the analyzers :js-dependency-index that 'stubs out' all JS modules
  listed in `js-dependencies`.

  Additionally includes dependencies from deps.cljs, and also common
  dependencies like 'react'.  [[get-string-dependencies]] only detects requires
  from within the analyzed package but not from requires in transitive
  dependencies.

  Rational:
  Required namespaces that are strings correspond to JS library used by the namespace
  currently parsed. Analysis generally doesn't involved those JS libraries but will check
  that they are present via `:js-dependency-index`. Note that also regular namespaces can
  be required as strings, e.g. `\"clojure.string\"` isn't invalid in a `ns` form but it's very rarely
  done in any actual code.
  https://github.com/cljdoc/cljdoc-analyzer/issues/18"
  [js-dependencies]
  (let [deps (cljs.closure/get-upstream-deps)
        npm-deps (when (map? (:npm-deps deps))
                   (keys (:npm-deps deps)))
        foreign-libs (mapcat :provides (:foreign-libs deps))]
    (zipmap (concat js-dependencies
                    npm-deps foreign-libs
                    ["react" "react-dom" "cljsjs.react"])
            (repeatedly #(gensym "fake$module")))))

(defn- analyze-file [js-dependencies file]
  (let [state (cljs.env/default-compiler-env)
        faked-js-deps (fake-js-deps js-dependencies)]
    (swap! state update :js-dependency-index #(merge faked-js-deps %))
    (ana/no-warn
      ;; The 'with-core-cljs' wrapping function ensures the namespace 'cljs.core'
      ;; is available under the sub-call to 'analyze-file'.
      ;; https://github.com/cljdoc/cljdoc/issues/261
      (comp/with-core-cljs state nil #(ana/analyze-file state file nil)))
    state))

(defn- read-file [ns-name resource js-dependencies]
  (let [state (analyze-file js-dependencies resource)]
    (if-let [ns (ana/find-ns state ns-name)]
      (-> ns
          (select-keys [:name :doc])
          (merge (-> ns-name meta (select-keys [:author :deprecated :added])))
          (remove-empties)
          (assoc :publics (read-publics state ns-name))))))

(defn read-namespace
  "Read a ClojureScript namespace and return its public vars.

  The keys in the returned map are:
    :name      - the name of the namespace
    :doc       - the doc-string on the namespace
    :author    - if the metadata is there, we return it
    :publics
      :name       - the name of a public function, macro, or value
      :arglists   - the arguments the function or macro takes
      :doc        - the doc-string of the var
      :type       - one of :macro, :protocol, :multimethod or :var
      :added      - the library version the var was added in
      :deprecated - the library version the var was deprecated in"
  [resource]
  (let [{:keys [ns requires]} (ana/parse-ns resource)
        js-dependencies (into #{} (filter string?) requires)]
    (read-file ns resource js-dependencies)))

(comment
  (read-namespace (io/resource "helix/dom.cljc"))
  (read-namespace (io/resource "stub_test.cljc")))
