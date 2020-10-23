(ns e05-stock-charts
  (:require [clj-http.client :as http]
            [vlaaad.reveal.ext :as rx])
  (:import [java.util Date]
           [java.time ZoneId LocalDate]))

;; load some historical information about a ticker

(defn stats! [ticker]
  (let [{:keys [timestamp events indicators]}
        (->> (http/request
               {:method :get
                :url (str "https://query1.finance.yahoo.com/v8/finance/chart/"
                          ticker
                          "?interval=1mo&range=max&events=divs,splits")
                :as :json})
             :body
             :chart
             :result
             first)
        prices (->> timestamp
                    (map-indexed
                      (fn [i ts]
                        [ts (get-in indicators [:quote 0 :close i])]))
                    (into (sorted-map)))
        divs (->> events
                  :dividends
                  vals
                  (map (juxt :date :amount))
                  (into {})
                  (group-by #(-> %
                                 key
                                 ^long (* 1000)
                                 Date.
                                 .toInstant
                                 (.atZone (ZoneId/systemDefault))
                                 .toLocalDate
                                 .getYear))
                  (map (juxt #(-> %
                                  ^int key
                                  (LocalDate/of 1 1)
                                  (.atStartOfDay (ZoneId/systemDefault))
                                  .toInstant
                                  .getEpochSecond)
                             #(->> % val (map val) (reduce +))))
                  (into (sorted-map)))
        yields (->> divs
                    (map (juxt key (fn [[date amount]]
                                     (/ amount
                                        (val (first (subseq prices >= date)))))))
                    (into (sorted-map)))]
    {:divs divs
     :prices prices
     :yields yields}))

;; define charts comparing same historical information for a bunch of tickers:

(defn charts! [tickers]
  (let [{:keys [divs prices yields]}
        (reduce (fn [acc ticker]
                  (let [{:keys [divs prices yields]} (stats! ticker)]
                    (-> acc
                        (assoc-in [:divs ticker] divs)
                        (assoc-in [:prices ticker] prices)
                        (assoc-in [:yields ticker] yields))))
                {:divs {} :prices {} :yields {}}
                tickers)]
    (rx/stream-as-is
      (rx/as
        {:fx/type :h-box
         :children [{:fx/type :v-box
                     :children [{:fx/type :label
                                 :text "Prices:"}
                                {:fx/type rx/scatter-chart-view
                                 :data prices}]}
                    {:fx/type :v-box
                     :children [{:fx/type :label
                                 :text "Dividends:"}
                                {:fx/type rx/scatter-chart-view
                                 :data divs}]}
                    {:fx/type :v-box
                     :children [{:fx/type :label
                                 :text "Yields:"}
                                {:fx/type rx/scatter-chart-view
                                 :data yields}]}]}
        (rx/horizontal
          (rx/raw-string "#charts" {:fill :object})
          (rx/stream tickers))))))

;; try it out:
(comment
  ;; Visa vs Mastercard
  (charts! ["V" "MA"])
  ;; Microsoft vs Apple
  (charts! ["MSFT" "AAPL"]))