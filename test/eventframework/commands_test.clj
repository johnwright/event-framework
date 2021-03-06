(ns eventframework.commands-test
  (:use 
    eventframework.commands
    clojure.test
    midje.sweet))

(defn append-callback [sym]
  (fn [position commands] (do
    (dosync (ref-set sym (into (deref sym) commands)))
    (listen-commands position (append-callback sym)))))
  
(deftest channel-test
  (facts "commands"
    (with-clear-commands
      (put-command "uuid" "foo")
      ((get-commands-from initial-position) 1))
    => ["foo"]
    (with-clear-commands
      (let [res (ref [])]
        (listen-commands initial-position (append-callback res))
        (deref res)))
    => []
    (with-clear-commands
      (let [res (ref [])]
        (listen-commands initial-position (append-callback res))
        (put-command "uuid" "foo")
        (deref res)))
    => ["foo"]
    (with-clear-commands
      (put-command "uuid" "foo")
      (put-command "uuid2" "foo")
      ((get-commands-from initial-position) 1))
    => ["foo", "foo"]
    (with-clear-commands
      (put-command "uuid" "foo")
      (put-command "uuid" "foo")
      ((get-commands-from initial-position) 1))
    => ["foo"]
    (with-clear-commands
      (let [res (ref [])]
        (listen-commands initial-position (append-callback res))
        (put-command "uuid" "foo")
        (put-command "uuid2" "foo")
        (deref res)))
    => ["foo", "foo"]
    (with-clear-commands
      (let [res (ref [])]
        (listen-commands initial-position (append-callback res))
        (put-command "uuid" "foo")
        (put-command "uuid" "foo")
        (deref res)))
    => ["foo"]))


