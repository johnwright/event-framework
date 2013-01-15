(ns eventframework.commands-test
  (:use
    eventframework.commands
    eventframework.commands-support
    clojure.test
    midje.sweet))

(defn append-callback [sym]
  (fn [position commands] (do
    (dosync (ref-set sym (into (deref sym) commands)))
    (apply-or-enqueue-listener! position (append-callback sym)))))

(defn get-new-commands [position]
  (let [[_ commands] (get-next-position-and-commands-from
                      (@*command-state*)
                      position)]
    commands))

(deftest channel-test
  (facts "commands"
    (with-clear-commands
      (put-command! {:id "id" :foo "foo"})
      (get-new-commands initial-position))
    => [{:id "id" :foo "foo"}]
    (with-clear-commands
      (let [res (ref [])]
        (apply-or-enqueue-listener! initial-position (append-callback res))
        (deref res)))
    => []
    (with-clear-commands
      (let [res (ref [])]
        (apply-or-enqueue-listener! initial-position (append-callback res))
        (put-command! {:id "id" :foo "foo"})
        (deref res)))
    => [{:id "id" :foo "foo"}]
    (with-clear-commands
      (put-command! {:id "id" :foo "foo"})
      (put-command! {:id "id2" :foo "foo"})
      (get-new-commands initial-position))
    => [{:id "id" :foo "foo"}, {:id "id2" :foo "foo"}]
    (with-clear-commands
      (put-command! {:id "id" :foo "foo"})
      (put-command! {:id "id" :foo "foo"})
      (get-new-commands initial-position))
    => [{:id "id" :foo "foo"}]
    (with-clear-commands
      (let [res (ref [])]
        (apply-or-enqueue-listener! initial-position (append-callback res))
        (put-command! {:id "id" :foo "foo"})
        (put-command! {:id "id2" :foo "foo"})
        (deref res)))
    => [{:id "id" :foo "foo"}, {:id "id2" :foo "foo"}]
    (with-clear-commands
      (let [res (ref [])]
        (apply-or-enqueue-listener! initial-position (append-callback res))
        (put-command! {:id "id" :foo "foo"})
        (put-command! {:id "id" :foo "foo"})
        (deref res)))
    => [{:id "id" :foo "foo"}]))
