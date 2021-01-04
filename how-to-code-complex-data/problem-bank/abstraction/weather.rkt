#lang htdp/isl

(require racket/file)


(define-struct weather (date max-tmp avg-tmp min-tmp rain snow precip))
;; Weather is (make-weather String Number Number Number Number Number Number)
;; interp. Data about weather in Vancouver on some date
;; (make-weather date xt at nt r s p) means that
;; on the day indicated by date:
;; - the maximum temperature in Celsius was xt
;; - the average temperature in Celsius was at
;; - the minimum temperature in Celsius was nt
;; - r millimeters of rain fell
;; - s millimeters of snow fell
;; - p millimeters of total precipitation fell

(define W0 (make-weather "7/2/88" 21.9 17.7 13.4 0.2 0 0.2))
(define W1 (make-weather "7/3/88" 20.9 14.9 10.3 0.3 0 0.2))
(define W2 (make-weather "7/4/88" 23.4 15.1 14.2 0.4 0 0.1))
(define W3 (make-weather "7/5/88" 21.3 11.1 10.2 0.1 0.2 0.1))

(define (fn-for-weather w)
  (... (weather-date w)
       (weather-max-tmp w)
       (weather-avg-tmp w)
       (weather-min-tmp w)
       (weather-rain w)
       (weather-snow w)
       (weather-precip w)))


;; PROBLEM:
;;
;; Complete the design of a function that takes a list of weather data
;; and produces the sum total of rainfall in millimeters on days where
;; the average temperature was greater than 15 degrees Celsius.
;;
;; The function that you design must make at least one call to
;; built-in abstract functions (there is a very nice solution that
;; composes calls to three built-in abstract functions).


;; If you would like to use the real daily weather data from Vancouver
;; from 10/3/1987 to 10/24/2013,
;; place the weather.ss file in your current directory and uncomment
;; the following definition and add the given check-expect to your
;; examples.

(define WEATHER-DATA
  (local [(define (data->weather d) (apply make-weather d))]
    (map data->weather (file->value "weather.ss"))))


;; (listOf Weather) -> Number
;; produce the total rainfall in millimeters of days with > 15 C average temp.

;; (define (total-warm-rain low) 0)

(define (total-warm-rain low)
  (local
    [(define (warm-weather? weather)
       (> (weather-avg-tmp weather) 15))]
    (foldr + 0 (map weather-rain (filter warm-weather? low)))))


;; Tests:

(check-expect (total-warm-rain empty) 0)
(check-expect (total-warm-rain (list W0 W1 W2 W3)) 0.6)
(check-within (total-warm-rain WEATHER-DATA) 2545.3 0.1)
