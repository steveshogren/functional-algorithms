(ns resume-timeline.handler
  (:use compojure.core)
  (:require [compojure.handler :as handler]
            [hiccup.core :as h]
            [resume-timeline.mergesort :as merger]
            [compojure.route :as route]))

(declare index basic-body show-list merge-sort parse-int string-to-int-list)

(defroutes app-routes
  (GET "/" [] (index))
  (GET "/mergesort" [] (merge-sort))
  (GET "/mergesortlist" [id] (merge-sort (string-to-int-list id)))
  (route/not-found "Not Found"))

(defn string-to-int-list [list] (map parse-int (.split list " ")))
(defn parse-int [s] (Integer. (re-find  #"\d+" s)))

(defn index []
  (h/html [:head
            [:title "Algorithm and Data Structure Fun"]]
          [:body
           [:div
            [:ul
             [:li [:a {:href "mergesort"} "Merge Sorting"]]
             [:li [:a {:href "linkedlist"} "Linked Lists"]]]]]))

(defn basic-body [content]
  (h/html [:head
           [:title "Merge Sort"]]
          [:body
           [:div [:a {:href "/"} "Home"]]
           [:h3 "Merge Sort"]
           [:div content]]))

(defn show-list [name list]
  [:div
   [:h4 name]
   [:ul (for [x list] [:li x])]])

(defn merge-sort
  ([] (basic-body [:form {:action "mergesortlist" :method "get"}
                   [:input {:type "text" :id "id" :name "id"}]
                   [:input {:type "submit"}]
                   [:span "ex: 3 4 5 34 62 2 1 3 5 9 10 1999 4 8 19 2 9"]]))
  ([list] (basic-body
           [:div
            (show-list "Unsorted" list)
            (show-list "Sorted" (merger/merge-sort list))])))

(def app
  (handler/site app-routes))
