(ns readline
  (:import [com.sun.jna Function]))

(defn fancy-read-line
  "Uses native GNU readline, which provides emacs key bindings."
  []
  (let [prompt "user> "]
    (.invoke
      (Function/getFunction "readline" "readline")
      String
      (to-array [prompt]))))
