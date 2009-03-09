(ns ring.cookie
  "Add support for reading and setting cookies.")

(defn- #^String trim-left [#^String s]
  (let [n (.length s)]
    (loop [i (int 0)]
      (cond 
        (= i n) ""
        (Character/isWhitespace (.charAt s i)) (recur (inc i))
        (zero? i) s
        :else (.substring s i)))))

(defn- unescape [#^String s]
  (let [sb (StringBuilder.)]
    (loop [i 0]
      (let [j (int (.indexOf s (int \\) i))]
        (if (neg? j)
          (if (zero? i)
            s
            (-> sb (.append (.substring s i)) str))
          (do
            (-> sb 
              (.append (.substring s i j))
              (.append (.charAt s (inc j))))
            (recur (int (+ 2 j)))))))))
            
(defn- parse-value [s]
  (let [s (trim-left s)]
    (if (= \" (first s))
      (let [v (re-find #"(?:[^\\\"]|\\.)*" (.substring s 1))
            ev (unescape v)]
        [ev (.substring s (+ 2 (count v)))])
      (let [v (re-find #"^[\p{ASCII}&&\P{Cntrl}&&[^()<>,;:\\\"/\[\]?={} ]]*" s)]
        [v (.substring s (count v))]))))

(defn- parse-kv [s]
  (let [[k s] (parse-value s)
        [x :as s] (trim-left s)]
    (let [[v #^String s] (if (= \= x)  
                  (-> s (.substring 1) parse-value)
                  [nil s])
          i (.indexOf s (int \;))
          j (.indexOf s (int \,))
          i (cond (neg? i) j (neg? j) i :else (min i j))
          s (if (neg? i) "" (.substring s (inc i)))]
        [[k v] s])))

(defn- parse-kvs [s]
  (let [kvs (take-while #(seq (ffirst %)) (rest (iterate #(parse-kv (second %)) [nil s])))]
    (map first kvs)))  

(defn- reserved? [[[c]]] (= \$ c)) 

(defn parse-cookie
 "Parse a cookie header and returns a seq of triples [cookie-name cookie-value options-map]."  
 [s]
  (let [kvs (drop-while reserved? (parse-kvs s))
        helper (fn helper [kvs]
                 (lazy-seq 
                   (when-let [[[k v] & etc] (seq kvs)]
                     (let [[options etc] (split-with reserved? etc)
                           options (reduce {} (fn [m [k v]] (assoc m k v)) options)]
                       (cons [k v options] (helper etc))))))]
    (helper kvs)))

(defn- #^String escape [s]
  (.replaceAll (str s) "[\"\\\\]" "\\\\$0"))

(defn write-cookie
 "Serialize a cookie (a triple [cookie-name cookie-value options-map])." 
 [[k v options]]
  (let [options (if (get options "Version") options (assoc options "Version" "1"))]
    (apply str 
      (interpose \;
        (map (fn [[k v]] (if v (str k \= (escape v)) (str k))) 
          (cons [k v] options))))))

(defn wrap
 "Wrap an app such that two maps are added to the request under the keys :cookies and :cookies-options,
  reflecting Cookie headers sent by the client.
  One can send cookies by associating a collection of triples [cookie-name cookie-value options-map]
  to the key :cookies in the response map. " 
 [app]
  (fn [{{cookie "cookie"} :headers :as req}]
    (let [cookies (if (string? cookie) [cookie] cookie)
          cookies (mapcat parse-cookie cookies)
          [cookies options] (reduce (fn [[vals opts] [k v o]] 
                                      [(assoc vals k v) (assoc opts k o)]) 
                              [{} {}] cookies)  
          resp (app (assoc req :cookies cookies :cookies-options options))]
      (if-let [set-cookies (seq (:cookies resp))]
        (update-in resp [:headers "Set-Cookie"]
          #(concat (if (string? %) (list %) %) 
             (map write-cookie set-cookies)))
        resp))))
          
(comment 
  (= '(["Customer" "WILE_E_COYOTE" {"$Path" "/acme"}] ["Part_Number" "Rocket_Launcher_0001" {"$Path" "/acme"}] ["Shipping" "FedEx" {"$Path" "/acme"}])
    (parse-cookie "$Version=\"1\";
                Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\";
                Part_Number=\"Rocket_Launcher_0001\"; $Path=\"/acme\";
                Shipping=\"FedEx\"; $Path=\"/acme\""))
  (= '(["JSESSIONID" "0871AB77875FCC308E960CEE6A513D9F" {}])
    (parse-cookie "JSESSIONID=0871AB77875FCC308E960CEE6A513D9F"))              
)

          
          
        