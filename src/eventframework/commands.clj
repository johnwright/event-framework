(ns eventframework.commands
  (:require [clojure.tools.logging :as log]))

(defprotocol CommandState
  (next-position-index [state]
    "Returns the index of the next command")
  (command-already-seen? [state command-id]
    "Returns whether the command with the given ID has already been received")
  (append-command-and-clear-waiting [state command command-id]
    "Appends the command to the state, clearing the waiting listeners")
  )

(defrecord ^{:doc "Stores the command state of the server.

    command-ids

    : The set of all command-ids already obtained

    commands

    : All submitted commands

    waiting

    : Listeners to be notified of new events (triggered by a command).

    world-id

    : This is a unique id for the whole environment/world; for a persistent
    backend this will be fixed at its creation. For transient backends (for
    testing etc.) it's generated on startup which means the client can easily
    diagnose if the server went down (because it will come up w/ a new,
    incompatible world-id)."}
    TransientCommandState
    [command-ids commands waiting world-id]

  CommandState
  (next-position-index [state]
    (count (:commands state)))
  (command-already-seen? [state command-id]
    (contains? (:command-ids state) command-id))
  (append-command-and-clear-waiting [state command command-id]
    (assoc state
      :command-ids (conj (:command-ids state) command-id)
      :commands    (conj (:commands state) command)
      :waiting     []))
 )

(defn new-uuid [] (str (java.util.UUID/randomUUID)))

(def initial-position "0")

(defn starting-state []
  (map->TransientCommandState
   {:command-ids #{} :commands [] :waiting [] :world-id (new-uuid)}))

(defn to-position [s i]
  (if (= i 0)
    initial-position
    (str (:world-id s) ":" i)))

(defn from-position [s p]
  (let [rm (re-matches #"([a-z0-9-]+):([0-9]+)" p)]
    (cond
     (= p initial-position) 0
     (nil? rm) nil
     (not= (rm 1) (:world-id s)) (do
                               (log/error "Client lives in the wrong world (expected"
                                          (:world-id s) ", got" (rm 1) ")")
                               nil)
     :else (let [i (Integer/parseInt (rm 2))]
             (if (> i (next-position-index s)) nil i)))))

(defn next-position [state]
  (to-position state (next-position-index state)))

(def ^{:dynamic true,
       :doc "This is dynamically scoped for testing purposes,
  don't mess w/ it outside tests."}
  command-state (ref (starting-state)))

(defn valid-position? [position]
  (not (nil? (from-position (deref command-state) position))))

(defn append-command-get-waiting! [command-id command]
  (dosync
   (let [state (deref command-state)]
     (if (command-already-seen? state command-id)
       [(next-position state) nil]
       (let [new-state
             (append-command-and-clear-waiting state command command-id)]
         (ref-set command-state new-state)
         [(next-position new-state) (:waiting state)])))))

(defn put-command! [command-id command]
  (let [[position waiting] (append-command-get-waiting! command-id command)]
    (doseq [listener waiting]
      (listener position [command]))))

(defn get-commands-before [position]
  (let [s  (deref command-state)
        cl (:commands s)
        ix (from-position s position)]
    (subvec cl 0 ix)))

(defn get-next-position-and-commands-from [state position]
  (let [next-ix (next-position-index state)
        ix (from-position state position)]
    (when (< ix next-ix)
      [(to-position state next-ix)
       (subvec (:commands state) ix)])))

;; FIXME(alexander): clean this up once more
(defn- get-from-or-add-waiting! [position listener]
  (dosync
   (let [state (deref command-state)]
     (or (get-next-position-and-commands-from state position)
         (do (ref-set command-state
                      (update-in state [:waiting] #(conj % listener)))
             nil)))))

(defn apply-or-enqueue-listener! [position listener]
  "Apply `listener` to all commands past `position`, or if none, enqueue it."
  (when-let [[new-pos new-commands] (get-from-or-add-waiting! position listener)]
    (listener new-pos new-commands)
    nil))
