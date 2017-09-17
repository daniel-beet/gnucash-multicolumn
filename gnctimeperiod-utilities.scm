;; -*-scheme-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gnctimeperiod-utilities.scm : Facilitate specifying date periods
;;
;; Original  by D.B.Doughty <dbdoughty gmail.com> 2017.02.24
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; to link to this module add following line to your file.
;;  (use-modules (gnucash gnctimeperiod-utilities))
;;

;;note gnc:days-in-month is also exported by date-utilities.scm
(define-module (gnucash gnctimeperiod-utilities)
   #:export (gnc:get-dates gnc:getdates  gnc:getdatedelta gnc:getdates-average gnc:getdates-compare gnc:list-years gnc:list-periods gnc:list-lasts
             gnc:list-number gnc:list-average gnc:list-months gnc:list-operands gnc:list-comparechoices gnc:increment-date gnc:date-eq?
             gnc:daysinmonth gnc:dayofweek gnc:day-of-year gnc:set_yday gnc:day-of-week gnc:decrement-date
             gnc:increment-month gnc:increment-month-less-1-day gnc:decrement-month ))
(use-modules (gnucash gettext))

(use-modules (gnucash gnc-module))
(use-modules (gnucash core-utils))

(use-modules (gnucash main)) ;; for gnc:warn  - delete after we finish modularizing.

(gnc:module-load "gnucash/report/report-system" 0)


(define-macro (addto! alist element)
  `(set! ,alist (cons ,element ,alist)))

(define now (timespecCanonicalDayTime (cons (current-time) 0)))
(define today-tm (gnc:timepair->date now))
(define this_year (gnc:date-get-year today-tm))
(define thismonth (tm:mon today-tm))
(define thisday (tm:mday today-tm))
(define thisyear (tm:year today-tm))
(define whichmonth (tm:mon today-tm))


(define period_start (gnc:timepair->date now))
(define period_end (gnc:timepair->date now))


(define lastyr  (gnc-locale-to-utf8  "Last year"))
(define curryear (gnc-locale-to-utf8 "This year"))
(define next_yr (number->string (+ this_year 1)))
(define curr_yr (number->string (- this_year 0)))
(define prev_yr (number->string (- this_year 1)))
(define 2yr_ago (number->string (- this_year 2)))
(define 3yr_ago (number->string (- this_year 3)))
(define 4yr_ago (number->string (- this_year 4)))
(define 5yr_ago (number->string (- this_year 5)))
(define alltime (gnc-locale-to-utf8 "All Time"))



(define fullyear (gnc-locale-to-utf8 "Full year"))
(define last_qtr (gnc-locale-to-utf8 "Last Quarter"))
(define 1st_hlf (gnc-locale-to-utf8 "1st Half"))
(define 2nd_hlf (gnc-locale-to-utf8 "2nd Half"))
(define 1st_qtr (gnc-locale-to-utf8 "1st Quarter"))
(define 2nd_qtr (gnc-locale-to-utf8 "2nd Quarter"))
(define 3rd_qtr (gnc-locale-to-utf8 "3rd Quarter"))
(define 4th_qtr (gnc-locale-to-utf8 "4th Quarter"))
(define weektodate (gnc-locale-to-utf8 "Week to Date"))
(define mnthtodate (gnc-locale-to-utf8 "Month to Date"))
(define qrtrtodate (gnc-locale-to-utf8 "Quarter to date"))
(define yeartodate (gnc-locale-to-utf8 "Year to date"))
(define yesterday (gnc-locale-to-utf8 "Yesterday"))
(define today (gnc-locale-to-utf8 "Today"))
(define 2fullyears (gnc-locale-to-utf8 "2 full years"))
(define 3fullyears (gnc-locale-to-utf8 "3 full years"))
(define 4fullyears (gnc-locale-to-utf8 "4 full years"))
(define 5fullyears (gnc-locale-to-utf8 "5 full years"))


(define last_wk (gnc-locale-to-utf8 "Last week"))
(define lastmonth (gnc-locale-to-utf8 "Last month"))
(define last_3mnth (gnc-locale-to-utf8 "Last 3 months"))
(define last_6mnth (gnc-locale-to-utf8 "Last 6 months"))
(define last_12mnth (gnc-locale-to-utf8 "Last 12 months"))
(define prev_7days (gnc-locale-to-utf8 "previous 7 Days"))
(define prev_30days (gnc-locale-to-utf8 "previous 30 Days"))
(define prev_31days (gnc-locale-to-utf8 "previous 31 Days"))
(define prev_90days (gnc-locale-to-utf8 "previous 90 Days"))
(define prev_365days (gnc-locale-to-utf8 "previous 365 Days"))
(define last_7days (gnc-locale-to-utf8 "Last 7 Days"))
(define last_30 (gnc-locale-to-utf8 "Last 30 days"))
(define last_90 (gnc-locale-to-utf8 "Last 90 Days"))
(define last_180 (gnc-locale-to-utf8 "Last 180 Days"))
(define last_365 (gnc-locale-to-utf8 "Last 365 Days"))

(define 4weeks (gnc-locale-to-utf8 "4 weeks"))
(define 8weeks (gnc-locale-to-utf8 "8 weeks"))
(define 12weeks (gnc-locale-to-utf8 "12 weeks"))
(define 13weeks (gnc-locale-to-utf8 "13 weeks"))
(define 26weeks (gnc-locale-to-utf8 "26 weeks"))
(define 52weeks (gnc-locale-to-utf8 "52 weeks"))
(define 3months (gnc-locale-to-utf8 "3 months"))
(define 4months (gnc-locale-to-utf8 "4 months"))
(define 6months (gnc-locale-to-utf8 "6 months"))
(define 9months (gnc-locale-to-utf8 "9 months"))
(define 12months (gnc-locale-to-utf8 "12 months"))
(define 18months (gnc-locale-to-utf8 "18 months"))
(define 24months (gnc-locale-to-utf8 "24 months"))

(define Jan (gnc-locale-to-utf8 "January"))
(define Feb (gnc-locale-to-utf8 "February"))
(define Mar (gnc-locale-to-utf8 "March"))
(define Apr (gnc-locale-to-utf8 "April"))
(define May (gnc-locale-to-utf8 "May"))
(define Jun (gnc-locale-to-utf8 "June"))
(define Jul (gnc-locale-to-utf8 "July"))
(define Aug (gnc-locale-to-utf8 "August"))
(define Sep (gnc-locale-to-utf8 "September"))
(define Oct (gnc-locale-to-utf8 "October"))
(define Nov (gnc-locale-to-utf8 "November"))
(define Dec (gnc-locale-to-utf8 "December"))

(define multiply (gnc-locale-to-utf8 "multiply"))
(define divide (gnc-locale-to-utf8 "divide"))

(define month-of (gnc-locale-to-utf8 "Month of "))

(define  text-one (gnc-locale-to-utf8  "one          "))
(define text-days (gnc-locale-to-utf8  "days"))
(define text-weeks (gnc-locale-to-utf8  "weeks"))
(define text-twoweeks (gnc-locale-to-utf8 "two week periods"))
(define text-fourweeks (gnc-locale-to-utf8 "four week periods"))
(define text-halfmonths (gnc-locale-to-utf8 "half months"))
(define text-months (gnc-locale-to-utf8 "months"))
(define text-3months (gnc-locale-to-utf8 "three month periods"))
(define text-6months (gnc-locale-to-utf8 "six month periods"))
(define text-halfquarters (gnc-locale-to-utf8 "half quarters"))
(define text-quarters (gnc-locale-to-utf8 "quarters"))
(define text-halfyears (gnc-locale-to-utf8 "half years"))
(define text-years (gnc-locale-to-utf8 "years"))
(define text-customdates "Custom Dates")

(define gnc:list-years
   (list (list->vector
             (list 'alltime
                   (N_ alltime)
                   (N_ "from 1900 till now")))
            (list->vector
             (list 'last-yr
                   (N_ lastyr)
                   (N_ "always uses Last year")))
            (list->vector
             (list 'this-yr
                   (N_ curryear)
                   (N_ "this year")))
            (list->vector
             (list 'yr_plus1
                   (N_ next_yr)
                   (N_ "next year")))
            (list->vector
             (list 'yr_0
                   (N_ curr_yr)
                   (N_ "current year")))
            (list->vector
             (list 'yr_minus1
                   (N_ prev_yr)
                   (N_ "One year ago")))
            (list->vector
             (list 'yr_minus2
                   (N_ 2yr_ago)
                   (N_ "2 years ago")))
            (list->vector
             (list 'yr_minus3
                   (N_ 3yr_ago)
                   (N_ "3 years ago")))
            (list->vector
             (list 'yr_minus4
                   (N_ 4yr_ago)
                   (N_ "4 years ago")))
            (list->vector
             (list 'yr_minus5
                   (N_ 5yr_ago)
                   (N_ "5 years ago")))
    )
 )

(define gnc:list-periods
    (list (list->vector
             (list 'fullyear
                   (N_ fullyear)
                   (N_ "The Entire year")))
            (list->vector
             (list '1st_hlf
                   (N_ 1st_hlf)
                   (N_ "First six months of the year")))
            (list->vector
             (list '2nd_hlf
                   (N_ 2nd_hlf)
                   (N_ "Last six months of the year")))
            (list->vector
             (list '1st_qtr
                   (N_ 1st_qtr)
                   (N_ "Jan - March")))
            (list->vector
             (list '2nd_qtr
                   (N_ 2nd_qtr)
                   (N_ "April - June")))
            (list->vector
             (list '3rd_qtr
                   (N_ 3rd_qtr)
                   (N_ "July - September")))
            (list->vector
             (list '4th_qtr
                   (N_ 4th_qtr)
                   (N_ "October - December")))
            (list->vector
             (list 'weektodate
                   (N_ weektodate)
                   (N_ "Sunday thru today")))
            (list->vector
             (list 'mnthtodate
                   (N_ mnthtodate)
                   (N_ "from the 1st thru today")))
            (list->vector
             (list 'qrtrtodate
                   (N_ qrtrtodate)
                   (N_ "from the start of the quarter thru today")))
            (list->vector
             (list 'yeartodate
                   (N_ yeartodate)
                   (N_ "from January 1 thru today")))
            (list->vector
             (list 'yesterday
                   (N_ yesterday)
                   (N_ "yesterday")))
            (list->vector
             (list 'today
                   (N_ today)
                   (N_ "today")))
            (list->vector
             (list '2fullyears
                   (N_ 2fullyears)
                   (N_ "2 entire years")))
            (list->vector
             (list '3fullyears
                   (N_ 3fullyears)
                   (N_ "3 entire years")))
            (list->vector
             (list '4fullyears
                   (N_ 4fullyears)
                   (N_ "4 entire years")))
            (list->vector
             (list '5fullyears
                   (N_ 5fullyears)
                   (N_ "5 entire years")))

    )
)
(define gnc:list-lasts
    (list (list->vector
             (list 'last_qtr
                   (N_ last_qtr)
                   (N_ "last quarter")))
            (list->vector
             (list 'last_3mnth
                   (N_ last_3mnth)
                   (N_ "3 months ending with last month")))
            (list->vector
             (list 'lastmonth
                   (N_ lastmonth)
                   (N_ "last month")))
            (list->vector
             (list 'last_wk
                   (N_ last_wk)
                   (N_ "last week, ending with Saturday")))
            (list->vector
             (list 'last_6mnth
                   (N_ last_6mnth)
                   (N_ "6 months ending with last month ")))
            (list->vector
             (list 'last_12mnth
                   (N_ last_12mnth)
                   (N_ "12 months (one year) ending with last month")))
            (list->vector
             (list 'last_30
                   (N_ last_30)
                   (N_ "30 days including today")))
            (list->vector
             (list 'last_90
                   (N_ last_90)
                   (N_ "90 days including today")))
             (list->vector
             (list 'last_180
                   (N_ last_180)
                   (N_ "180 days including today")))
            (list->vector
             (list 'last_365
                   (N_ last_365)
                   (N_ "365 days including today")))
            (list->vector
             (list 'last_7days
                   (N_ last_7days)
                   (N_ "7 days including today")))
            (list->vector
             (list 'prev_7days
                   (N_ prev_7days)
                   (N_ "7 days ending yesterday")))
            (list->vector
             (list 'prev_30days
                   (N_ prev_30days)
                    (N_ "30 days ending yesterday")))
            (list->vector
             (list 'prev_31days
                   (N_ prev_31days)
                    (N_ "31 days ending yesterday")))
            (list->vector
             (list 'prev_90days
                   (N_ prev_90days)
                    (N_ "90 days ending yesterday")))
            (list->vector
             (list 'prev_365days
                   (N_ prev_365days)
                    (N_ "365 days ending yesterday")))




    )
)

(define gnc:list-months
    (list
            (list->vector
             (list '0
                   (N_ Jan)
                   (N_ "report just this month")))
            (list->vector
             (list '1
                   (N_ Feb)
                   (N_ "report just this month")))
            (list->vector
             (list '2
                   (N_ Mar)
                   (N_ "report just this month")))
            (list->vector
             (list '3
                   (N_ Apr)
                   (N_ "report just this month")))
            (list->vector
             (list '4
                   (N_ May)
                   (N_ "report just this month")))
            (list->vector
             (list '5
                   (N_ Jun)
                   (N_ "report just this month")))
            (list->vector
             (list '6
                   (N_ Jul)
                   (N_ "report just this month")))
            (list->vector
             (list '7
                   (N_ Aug)
                   (N_ "report just this month")))
            (list->vector
             (list '8
                   (N_ Sep)
                   (N_ "report just this month")))
            (list->vector
             (list '9
                   (N_ Oct)
                   (N_ "report just this month")))
            (list->vector
             (list '10
                   (N_ Nov)
                   (N_ "report just this month")))
            (list->vector
             (list '11
                   (N_ Dec)
                   (N_ "report just this month")))
    )
)

(define gnc:list-average
    (list (list->vector
             (list '4weeks
                   (N_ 4weeks)
                   (N_ "4 weeks")))
            (list->vector
             (list '8weeks
                   (N_ 8weeks)
                   (N_ "8 weeks")))
            (list->vector
             (list '12weeks
                   (N_ 12weeks)
                   (N_ "12 weeks")))
            (list->vector
             (list '13weeks
                   (N_ 13weeks)
                   (N_ "13 weeks")))
            (list->vector
             (list '26weeks
                   (N_ 26weeks)
                   (N_ "26 weeks")))
            (list->vector
             (list '52weeks
                   (N_ 52weeks)
                   (N_ "52 weeks")))
            (list->vector
             (list '3months
                   (N_ 3months)
                   (N_ "3 months")))
            (list->vector
             (list '4months
                   (N_ 4months)
                   (N_ "4 months")))
            (list->vector
             (list '6months
                   (N_ 6months)
                   (N_ "6 months")))
            (list->vector
             (list '9months
                   (N_ 9months)
                   (N_ "9 months")))
            (list->vector
             (list '12months
                   (N_ 12months)
                   (N_ "12 months")))
            (list->vector
             (list '18months
                   (N_ 18months)
                   (N_ "18 months")))
            (list->vector
             (list '24months
                   (N_ 24months)
                   (N_ "24 months")))
    )
)



(define gnc:list-operands
    (list (list->vector
             (list '*
                   (N_ multiply)
                   (N_ "multiply by scaling factor")))
            (list->vector
             (list '/
                   (N_ divide)
                   (N_ "divide by scaling factor")))
  )
)

(define gnc:list-comparechoices
   (list (list->vector
             (list 'one
                   (N_ text-one)
                   (N_ "Show as one period")))
            (list->vector
             (list 'days
                   (N_ text-days)
                   (N_ "show one day periods")))
            (list->vector
             (list 'weeks
                   (N_ text-weeks)
                   (N_ "show one week periods")))
            (list->vector
             (list 'twoweeks
                   (N_ text-twoweeks)
                   (N_ "show two week periods")))
            (list->vector
             (list 'fourweeks
                   (N_ text-fourweeks)
                   (N_ "show four week periods")))
            (list->vector
             (list 'halfmonths
                   (N_ text-halfmonths)
                   (N_ "show half month periods")))
            (list->vector
             (list 'months
                   (N_ text-months)
                   (N_ "show one month periods")))
            (list->vector
             (list '3months
                   (N_ text-3months)
                   (N_ "show three month periods")))
            (list->vector
             (list '6months
                   (N_ text-6months)
                   (N_ "show six month periods")))
            (list->vector
             (list 'halfquarters
                   (N_ text-halfquarters)
                   (N_ "show half quarter periods")))
            (list->vector
             (list 'quarters
                   (N_ text-quarters)
                   (N_ "show quarter periods")))
            (list->vector
             (list 'halfyears
                   (N_ text-halfyears)
                   (N_ "show half year periods")))
            (list->vector
             (list 'years
                   (N_ text-years)
                   (N_ "show full year periods")))
           )
)
(define (get-val alist key)
	    (let ((lst (assoc-ref alist key)))
	      (if lst (car lst) lst)))

(define (gnc:leap-year? n)
    (apply (lambda (a b c) (or a (and (not b) c)))
       (map (lambda (m) (zero? (remainder n m)))
            '(400 100 4))))

(define (gnc:days-in-feb y)
  (if (gnc:leap-year? y) 29 28))

 (define (gnc:daysinmonth m y)
 ;; (0-11)  (ie. jan=0)
 ;; assume y is year so do not need to add 1900
  (if (equal? m 1)
    (gnc:days-in-feb y)
     ;;else
  (vector-ref #(31 28 31 30 31 30 31 31 30 31 30 31) m))
 )

 (define (gnc:dayofweek year month day)
;; month (0-11)  (ie. jan=0)
;; 0- Sun  6-Sat
(set! month (+ month 1))
(if (< month 3)
    (begin (set! month (+ month 12)) (set! year (- year 1))))
(+ 0
   (remainder (+ 6 day (quotient (* (+ 1 month) 13) 5)
                 year (quotient year 4) (* (quotient year 100) 6) (quotient year 400))
              7)))

(define (dayofwk yr month day)
;; month (0-11)  (ie. jan=0)
;; 0- Sun  6-Sat
  ( set! yr (- yr 1))
  (remainder  (+ (- (quotient yr 4)(quotient yr 100)) (quotient yr 400) yr (dayofyear (+ yr 1) month day)) 7)
  )

(define (gnc:day-of-year date)
    (gnc:dayofyear (gnc:date-get-year date) (tm:mon date) (tm:mday date)))


(define (gnc:dayofyear year month day)
;; month (0-11)  (ie. jan=0)
;; for dec 31 2018 use (gnc:dayofyear 2018 11 31)
(let ((mnth 0)
     (dy day))
  (while (< mnth month )
    (begin
    (set! dy (+ dy (gnc:daysinmonth mnth year)))
    (set! mnth (+ mnth 1))))
    dy))

 (define (gnc:set_yday year yday)
;; month (0-11)  (ie. jan=0)
 (let ((date (gnc:timepair->date now))
    (mnth 0)
    (daysleft yday))
    (set-tm:year date (- year 1900))
    (while (> (+ 0 daysleft) (gnc:daysinmonth mnth year) )
        (begin
            (set! daysleft (- daysleft (gnc:daysinmonth mnth year)))
            (set! mnth (+ mnth 1))))
    (set-tm:mon date mnth)
    (set-tm:mday date (+ 0 daysleft))
    date))


(define (gnc:day-of-week date)
;; 0- Sun  6-Sat
  (let (
      (yr (-(gnc:date-get-year date) 1)))
    (remainder  (+ (- (quotient yr 4)(quotient yr 100)) (quotient yr 400) yr (gnc:day-of-year date)) 7)
    ))

(define (gnc:days-total date)
  (let ((the-date date ) )
  (set-tm:hour the-date 12)
  (quotient (gnc-mktime the-date)  (* 60 60 24)))

 )

 (define (gnc:decrement-date date decrement)
;;
  (let* (
      (yr (gnc:date-get-year date) )
      (month (tm:mon date))
     (day (tm:mday date) )
     (yday (gnc:day-of-year date)))
  (while (<= yday decrement )
    (begin
    (set! day 31)
    (set! month 11)
    (set! yr (- yr 1) )
    (set! yday (+ yday (gnc:dayofyear yr month day)))))

    (set! yday (- yday decrement))
    (gnc:set_yday yr yday)
   ))

(define (gnc:increment-date date increment)
;;
  (let* (
     (yr (gnc:date-get-year date) )
     (yday (gnc:day-of-year date))
     (endyear (gnc:dayofyear yr 11 31)))
  (while (> (+ yday increment) endyear)
    (begin
    (set! yr (+ yr 1) )
    (set! increment (- increment (- (+ endyear 1) yday)))
    (set! yday (gnc:dayofyear yr 0 1))
    (set! endyear (gnc:dayofyear yr 11 31))
    ))
    (set! yday (+ yday increment))
    (gnc:set_yday yr yday)
   ))

(define (gnc:decrement-month date decrement)
    (let (
        (newdate (gnc:timepair->date now))
        )
        (set-tm:year newdate (tm:year date))
        (set-tm:mon newdate (tm:mon date))
        (set-tm:mday newdate (tm:mday date))
        (if (< (tm:mon date) decrement)
            (begin
             (set-tm:mon newdate (- (+ (tm:mon newdate) 12) decrement))
             (set-tm:year newdate (- (tm:year newdate) 1))
             )
             (set-tm:mon newdate (- (tm:mon newdate) decrement)))
        newdate
    ))

(define (gnc:increment-month date increment)
    (let (
        (newmonth (+ (tm:mon date) increment))
        (newdate (gnc:timepair->date now))
        )
        (set-tm:year newdate (tm:year date))
        (set-tm:mon newdate (tm:mon date))
        (set-tm:mday newdate (tm:mday date))
        (if (> newmonth 11)
            (begin
            (set! newmonth (- newmonth 12))
            (set-tm:year newdate (+ (tm:year date) 1))
        ))
        (set-tm:mon newdate newmonth)
        newdate
    ))
(define (gnc:increment-month-less-1-day date increment)
    (let (
        (newdate (gnc:increment-month date increment))
        )
        (gnc:decrement-date newdate 1)
    ))

(define (gnc:date-eq? tm1 tm2)
    (and (= (tm:year tm1) (tm:year tm2))
         (=(tm:mday tm1) (tm:mday tm2))
         (=(tm:mon tm1) (tm:mon tm2))
    ))

(define (gnc:date-lt? tm1 tm2)
    (if (not (= (tm:year tm1) (tm:year tm2)))
        (< (tm:year tm1) (tm:year tm2))
        ; years match
        (< (gnc:day-of-year tm1) (gnc:day-of-year tm2))
    ))
(define (gnc:date-lte? tm1 tm2)
    (if (not (= (tm:year tm1) (tm:year tm2)))
        (< (tm:year tm1) (tm:year tm2))
        ; years match
        (if (= (gnc:day-of-year tm1) (gnc:day-of-year tm2))
           #t
          (< (gnc:day-of-year tm1) (gnc:day-of-year tm2)))
    ))
(define (min-date tm1 tm2)
 (if (gnc:date-lt? tm1 tm2)
    tm1
    tm2)
)

(define (gnc:delta-days tm1 tm2)
 (- (gnc:days-total tm2) (gnc:days-total tm1) )
)


;;
;; Determine which time period was picked and return the start date  and end date
;;
(define (gnc:getdates userpicks) ;;deprecated
        
        (gnc:get-dates  
                (list
                 (list 'whichperiod-val
                       (car userpicks))
                 (list 'year-val
                       (cadr userpicks))
                 (list 'period-val
                       (caddr userpicks))
                 (list 'last-val
                       (cadddr userpicks))
                 (list 'month-val
                       (cadddr (cdr userpicks)))
                 )))

(define (gnc:get-dates params)
      (let (
        (whichperiod-val (get-val params 'whichperiod-val))
        (year-val  (get-val params 'year-val))
        (period-val  (get-val params 'period-val))
        (last-val  (get-val params 'last-val))
        (month-val  (get-val params 'month-val))

        (year_end 2000)
        (fiscal-start (gnc:timepair->date (gnc:secs->timepair (gnc-accounting-period-fiscal-start))))
        (fiscal-month 0)
        (fiscal-day 1)
        (fiscal-related? #f)
        )
        (set! fiscal-month (tm:mon fiscal-start) )
        (set! fiscal-day (tm:mday fiscal-start))
        
        (set! year_end (+ thisyear 0))
        (set-tm:mday period_start 1)
        (set-tm:isdst period_start -1)
        (set! period_end today-tm)
        (set-tm:mday period_end 1)
        (set-tm:isdst period_end -1)

        (case year-val
        ((last-yr) (set! year_end (- thisyear 1) ))
        ((this-yr) (set! year_end (- thisyear 0)))
        ((yr_plus1) (set! year_end (+ thisyear 1)))
        ((yr_0)     (set! year_end (- thisyear 0)))
        ((yr_minus1) (set! year_end (- thisyear 1)))
        ((yr_minus2) (set! year_end (- thisyear 2)))
        ((yr_minus3) (set! year_end (- thisyear 3)))
        ((yr_minus4) (set! year_end (- thisyear 4)))
        ((yr_minus5) (set! year_end (- thisyear 5)))
        )
        (set-tm:year period_end year_end)
        (set-tm:year period_start year_end)
        ;; Determine which period  ie. the month and day for start and finish
        ;;
     (if (equal? whichperiod-val 'period)
          (begin
            (set-tm:sec period_start 0)
            (set-tm:min period_start 0)
            (set-tm:hour period_start 0)
            (set-tm:mon period_start fiscal-month)
            (set-tm:mday period_start fiscal-day)
         (case period-val
            ((fullyear)
                    (set! period_end (gnc:increment-month-less-1-day period_start 12))
                    )
            ((1st_hlf)
                    (set! period_end (gnc:increment-month-less-1-day period_start 6))
                    )
            ((2nd_hlf)
                (set! period_start (gnc:increment-month period_start 6))
                (set! period_end (gnc:increment-month-less-1-day period_start 6))
                       )
            ((1st_qtr)
                (set! period_end (gnc:increment-month-less-1-day period_start 3))
                       )
            ((2nd_qtr)
                (set! period_start (gnc:increment-month period_start 3))
                (set! period_end (gnc:increment-month-less-1-day period_start 3))
                       )
            ((3rd_qtr)
                (set! period_start (gnc:increment-month period_start 6))
                (set! period_end (gnc:increment-month-less-1-day period_start 3))
                       )
            ((4th_qtr)
                (set! period_start (gnc:increment-month period_start 9))
                (set! period_end (gnc:increment-month-less-1-day period_start 3))
                       )
            ((weektodate)
                     (set-tm:year period_end year_end)
                     (set-tm:mon period_end thismonth)
                       (set-tm:mday period_end thisday)
                     (let ((day (gnc:day-of-week period_end)))
                       (set! period_start (gnc:decrement-date period_end day)))
                    )
            ((mnthtodate)
                        (set-tm:mon period_end thismonth )
                        (set-tm:mday period_end thisday )
                        (set-tm:mon period_start thismonth)

                    (set-tm:mon period_start
                                        (if (> thisday fiscal-day)
                                            thismonth
                                            (if (= thismonth 0)
                                                11
                                                (- thismonth 1)
                                            )))
                    (set-tm:mday period_start fiscal-day)
                    (if (gnc:date-lt? period_end period_start)
                        (set-tm:year period_start (- thisyear 1)))
                    )
            ((qrtrtodate)
                        (set-tm:mon period_end thismonth )
                        (set-tm:mday period_end thisday )
                        (set-tm:year period_start (- year_end 1))
                        (while (gnc:date-lt? (gnc:increment-month period_start 3) period_end)
                            (set! period_start (gnc:increment-month period_start 3))
                         )
                    )
            ((yeartodate)
                        (set-tm:mon period_end thismonth )
                        (set-tm:mday period_end thisday )
                    (set-tm:mon period_start fiscal-month)
                    (set-tm:mday period_start fiscal-day)
                    (if (gnc:date-lt? period_end period_start)
                        (set-tm:year period_start (- year_end 1)))
                    )
            ((yesterday)
                            (set-tm:year period_end year_end)
                            (set-tm:mon period_end thismonth)
                            (set-tm:mday period_end thisday)
                            (set! period_end (gnc:decrement-date period_end 1 ))
                            (set! period_start (gnc:decrement-date period_end 0))
                            )
            ((today)
                            (set-tm:year period_end year_end)
                            (set-tm:mon period_end thismonth)
                            (set-tm:mday period_end thisday)
                            (set-tm:year period_start year_end)
                            (set-tm:mon period_start thismonth)
                            (set-tm:mday period_start thisday)
                            )
            ((2fullyears)
                        (set-tm:mon period_start fiscal-month)
                        (set-tm:mday period_start fiscal-day)
                        (if (gnc:date-lt? today-tm period_start)
                            (set-tm:year period_start (- year_end 1)))
                        (set! period_end (gnc:increment-month-less-1-day period_start 12))
                        (set-tm:year period_start (- (tm:year period_start) 1))
                    )
            ((3fullyears)
                        (set-tm:mon period_start fiscal-month)
                        (set-tm:mday period_start fiscal-day)
                        (if (gnc:date-lt? today-tm period_start)
                            (set-tm:year period_start (- year_end 1)))
                        (set! period_end (gnc:increment-month-less-1-day period_start 12))
                        (set-tm:year period_start (- (tm:year period_start) 2))
                    )
            ((4fullyears)
                        (set-tm:mon period_start fiscal-month)
                        (set-tm:mday period_start fiscal-day)
                        (if (gnc:date-lt? today-tm period_start)
                            (set-tm:year period_start (- year_end 1)))
                        (set! period_end (gnc:increment-month-less-1-day period_start 12))
                        (set-tm:year period_start (- (tm:year period_start) 3))
                    )
            ((5fullyears)
                        (set-tm:mon period_start fiscal-month)
                        (set-tm:mday period_start fiscal-day)
                        (if (gnc:date-lt? today-tm period_start)
                            (set-tm:year period_start (- year_end 1)))
                        (set! period_end (gnc:increment-month-less-1-day period_start 12))
                        (set-tm:year period_start (- (tm:year period_start) 4))
                    )
            )
     ))

     (if (equal? whichperiod-val 'last)
         (begin
         (set-tm:year period_end year_end)
         (set-tm:mon period_end thismonth)
         (set-tm:mday period_end thisday)

         (case last-val
          ((last_qtr)
                        (set-tm:mon period_start fiscal-month)
                        (set-tm:mday period_start fiscal-day)
                        (set-tm:year period_start (- year_end 1))
                        (set! period_end (gnc:decrement-date period_end 1))
                        (while (gnc:date-lt? (gnc:increment-month period_start 6) period_end)
                            (set! period_start (gnc:increment-month period_start 3))
                         )
                        (set! period_end (gnc:increment-month-less-1-day period_start 3))
                    )
            ((last_3mnth)
                        (set-tm:mon period_start thismonth)
                        (set-tm:mday period_start fiscal-day)
                        (set! period_end (gnc:decrement-date period_start 1 ))
                        (if (gnc:date-lt? today-tm period_end)
                            (begin
                                (set-tm:mon period_end (- thismonth 1))
                                (set-tm:mon period_start (- thismonth 1))
                            ))
                        (set! period_start (gnc:decrement-month period_start 3))
                        (set! period_end (gnc:increment-month-less-1-day period_start 3))
                        )
            ((lastmonth)
                        (set-tm:mon period_start thismonth)
                        (set-tm:mday period_start fiscal-day)
                        (set! period_end (gnc:decrement-date period_start 1 ))
                        (if (gnc:date-lt? today-tm period_end)
                            (begin
                                (set-tm:mon period_end (- thismonth 1))
                                (set-tm:mon period_start (- thismonth 1))
                            ))
                        (set! period_start (gnc:decrement-month period_start 1))
                        )
            ((last_wk)

                     (set-tm:year period_end year_end)
                     (set-tm:mon period_end thismonth)
                       (set-tm:mday period_end thisday)
                     (let ((day (gnc:day-of-week period_end)))
                       (set! period_end (gnc:decrement-date period_end (+ day 1) ))
                     (set! period_start (gnc:decrement-date period_end 6)))
                    )

            ((last_6mnth)
                        (set-tm:mon period_start thismonth)
                        (set-tm:mday period_start fiscal-day)
                        (set! period_end (gnc:decrement-date period_start 1 ))
                        (if (gnc:date-lt? today-tm period_end)
                            (begin
                                (set-tm:mon period_end (- thismonth 1))
                                (set-tm:mon period_start (- thismonth 1))
                            ))
                        (set! period_start (gnc:decrement-month period_start 6))
                        (set! period_end (gnc:increment-month-less-1-day period_start 6))
                        )
            ((last_12mnth)
                        (set-tm:mon period_start thismonth)
                        (set-tm:mday period_start fiscal-day)
                        (set! period_end (gnc:decrement-date period_start 1 ))
                        (if (gnc:date-lt? today-tm period_end)
                            (begin
                                (set-tm:mon period_end (- thismonth 1))
                                (set-tm:mon period_start (- thismonth 1))
                            ))
                        (set! period_start (gnc:decrement-month period_start 12))
                        (set! period_end (gnc:increment-month-less-1-day period_start 12))
                        )
            ((last_30)
                            (set-tm:year period_end year_end)
                            (set-tm:mon period_end thismonth)
                            (set-tm:mday period_end thisday)
                            (set! period_start (gnc:decrement-date period_end 29))
                            )
            ((last_90)
                            (set-tm:year period_end year_end)
                            (set-tm:mon period_end thismonth)
                            (set-tm:mday period_end thisday)
                            (set! period_start (gnc:decrement-date period_end 89))
                            )
            ((last_180)
                            (set-tm:year period_end year_end)
                            (set-tm:mon period_end thismonth)
                            (set-tm:mday period_end thisday)
                            (set! period_start (gnc:decrement-date period_end 179))
                            )
            ((last_365)
                            (set-tm:year period_end year_end)
                            (set-tm:mon period_end thismonth)
                            (set-tm:mday period_end thisday)
                            (set! period_start (gnc:decrement-date period_end 364))
                            )
            ((last_7days)
                            (set-tm:year period_end year_end)
                            (set-tm:mon period_end thismonth)
                            (set-tm:mday period_end thisday)
                            (set! period_start (gnc:decrement-date period_end 6))
                            )
            ((prev_7days)
                            (set-tm:year period_end year_end)
                            (set-tm:mon period_end thismonth)
                            (set-tm:mday period_end thisday)
                            (set! period_end (gnc:decrement-date period_end 1 ))
                            (set! period_start (gnc:decrement-date period_end 6))
                            )
            ((prev_30days)
                            (set-tm:year period_end year_end)
                            (set-tm:mon period_end thismonth)
                            (set-tm:mday period_end thisday)
                            (set! period_end (gnc:decrement-date period_end 1 ))
                            (set! period_start (gnc:decrement-date period_end 29))
                            )
            ((prev_31days)
                            (set-tm:year period_end year_end)
                            (set-tm:mon period_end thismonth)
                            (set-tm:mday period_end thisday)
                            (set! period_end (gnc:decrement-date period_end 1 ))
                            (set! period_start (gnc:decrement-date period_end 30))
                            )
            ((prev_90days)
                            (set-tm:year period_end year_end)
                            (set-tm:mon period_end thismonth)
                            (set-tm:mday period_end thisday)
                            (set! period_end (gnc:decrement-date period_end 1 ))
                            (set! period_start (gnc:decrement-date period_end 89))
                            )
            ((prev_365days)
                            (set-tm:year period_end year_end)
                            (set-tm:mon period_end thismonth)
                            (set-tm:mday period_end thisday)
                            (set! period_end (gnc:decrement-date period_end 1 ))
                            (set! period_start (gnc:decrement-date period_end 364))
                            )


            )
       ))


       ;; Determine which period  ie. the month and day for start and finish
        ;;
        (if (equal? whichperiod-val 'month)
         (begin
            (set! whichmonth month-val)
            (set-tm:mon period_start month-val)
            (if (eq? (tm:mday fiscal-start) 1)
                (begin
                    (set-tm:mon period_end month-val )
                    (set-tm:mday period_end (gnc:daysinmonth month-val (+ 1900 year_end)))
                )
                (begin
                    (if (= month-val 11)
                        (begin
                            (set-tm:mon period_end 0)
                            (set-tm:year period_end (+ 1 (tm:year period_end))))
                        (set-tm:mon period_end (+ 1 month-val)))
                    (set-tm:mday period_end (- (tm:mday fiscal-start) 1 ))
                    (set-tm:mday period_start (tm:mday fiscal-start))
                )
            )

         )
        )
        (if (equal? year-val 'alltime)
            (begin
                            (set-tm:year period_start 0)
                            (set-tm:mon period_start 0)
                            (set-tm:mday period_start 1)
                            (set-tm:mday period_start 1)
                            (set-tm:isdst period_start -1)
                            (set! period_end today-tm)
            )
        )



        (list (gnc:timepair-start-day-time  (gnc:date->timepair period_start))
          (gnc:timepair-end-day-time  (gnc:date->timepair period_end)) )
          )
)

(define (gnc:getdates-compare entrypicked numberperiods exclude-last-period?)
(let ((day (gnc:day-of-week (gnc:timepair->date now)))
      (fiscal-start (gnc:timepair->date (gnc:secs->timepair (gnc-accounting-period-fiscal-start))))
      (fiscal-month 0)
      (fiscal-day 1)
        )
   (set! period_start (gnc:timepair->date now))
   (set! period_end (gnc:timepair->date now))
   (set! today-tm (gnc:timepair->date now))
   (set! fiscal-day (tm:mday fiscal-start))
   (set! fiscal-month (tm:mon fiscal-start))
   (set-tm:mday period_start fiscal-day)

   (if (gnc:date-lt? today-tm (gnc:decrement-date period_start 1 ))
       (begin
          (set-tm:mon period_start (- (tm:mon period_start) 1))
        ))

    (case entrypicked
        ((one)
           (set-tm:mon period_start fiscal-month)
            (set-tm:mday period_start fiscal-day)
            (if (gnc:date-lt? today-tm period_start)
                  (set-tm:year period_start (- (tm:year period_start) 1)))
            (set-tm:year period_start (- (tm:year period_start) 1))
            (if exclude-last-period?
                 (set-tm:year period_start (- (tm:year period_start) 1)))
            (set! period_end (gnc:increment-month-less-1-day period_start 12))
                 )
        ((days)
            (if exclude-last-period?
            (set! period_end (gnc:decrement-date period_end 1 )))
            (set! period_start (gnc:decrement-date period_end (- numberperiods 1)))
                )
        ((weeks)

            (set! period_end (gnc:decrement-date period_end (+ day 1) ))
            (if exclude-last-period?
                (set! period_end (gnc:decrement-date period_end 7 )))
            (set! period_start (gnc:decrement-date period_end (- (* 7  numberperiods ) 1)))
            )
        ((twoweeks)
            (set! period_end (gnc:decrement-date period_end (+ day 1) ))
            (if exclude-last-period?
                (set! period_end (gnc:decrement-date period_end 14 )))
            (set! period_start (gnc:decrement-date period_end (- (* 14 numberperiods) 1)))
            )
        ((fourweeks)
            (set! period_end (gnc:decrement-date period_end (+ day 1) ))
            (if exclude-last-period?
                (set! period_end (gnc:decrement-date period_end 28 )))
            (set! period_start (gnc:decrement-date period_end (- (* 28 numberperiods) 1)))
            )
         ((halfmonths)
            (set-tm:mday period_start fiscal-day)   ;; assume 7th
            (set! period_end (gnc:decrement-date period_start 1 ))
            (let ((calc-mid (gnc:increment-date period_start 15 ));;assume 22nd
                  (counter 0))
               (if (gnc:date-lt? today-tm period_end)
                   (begin
                   ; (set-tm:mon period_end (- thismonth 1))
                     (set-tm:mon period_start (- thismonth 1))
                    ))
             ;;  (set! period_start (gnc:decrement-month period_start 1))

             ;; check for half month
            ;;            (set! period_start (gnc:increment-month period_start 1))
                        (set! calc-mid (gnc:increment-month period_start 1)) ;full month
                        (set! calc-mid (gnc:increment-date period_start
                             (- (floor (/ (gnc:delta-days period_start calc-mid) 2)) 1)))
               (if exclude-last-period?
                 (if (gnc:date-lt? (gnc:increment-month-less-1-day period_start 1) today-tm) ;;move by 1/2 month?
                    (begin
                        (set! period_end  calc-mid)
                         )
                    (begin
                        (set! period_start (gnc:decrement-month period_start 1))
                        (set! period_end (gnc:increment-month-less-1-day period_start 1)) ;full quarter
                        (set! period_start (gnc:increment-date period_start
                             (+ 1 (floor (/ (gnc:delta-days period_start period_end) 2))))); 1/2 way
                     )
                )
                (if (gnc:date-lt? calc-mid today-tm)
                    (begin
                        (set! period_end (gnc:increment-month period_start 1))
                        (set! period_end (gnc:increment-month calc-mid 0))
                     )
                   (begin
                        (set! period_start (gnc:decrement-month period_start 1))
                        (set! period_end (gnc:increment-month-less-1-day period_start 1)) ;full month
                        (set! period_start (gnc:increment-date period_start
                             (+ 1 (floor (/ (gnc:delta-days period_start period_end) 2))))); 1/2 way
                     )
                 )
             )
            ;;get rest of periods
             ;;have 1 period
              (set! counter 1)
              ;;and add 1 since moving by months not halfmonths
           (while (< (+ counter 1)  numberperiods);; add by quarters
              (begin
                   (set! period_start (gnc:decrement-month period_start 1))
                   (set! counter (+ 2 counter))
              ))
            (if (< counter numberperiods);; add half monh if needed
                (if (= (tm:mday period_start) fiscal-day)
                    (begin
                        (set! period_start (gnc:decrement-month period_start 1))
                        (set! calc-mid (gnc:increment-month-less-1-day period_start 1)) ;full month
                        (set! period_start (gnc:increment-date period_start
                             (+ (floor (/ (+ 1 (gnc:delta-days period_start calc-mid)) 2)) 0))); 1/2 way
                    )
                    (begin
                      (set-tm:mday period_start fiscal-day)
                    )
                )
            )
      ))

         ((months)
            (set-tm:mday period_start fiscal-day)
            (set! period_end (gnc:decrement-date period_start 1 ))
               (if (gnc:date-lt? today-tm period_end)
                   (begin
                     (set-tm:mon period_end (- thismonth 1))
                     (set-tm:mon period_start (- thismonth 1))
                    ))
               (set! period_start (gnc:decrement-month period_start 1))
               (if exclude-last-period?
                 (begin
                    (set! period_start (gnc:decrement-month period_start 1))
                    (set! period_end (gnc:increment-month-less-1-day period_start 1))
                 ))
                (set! period_start (gnc:decrement-month period_start (- numberperiods 1)))
                 )
         ((3months)
            (set-tm:mday period_start fiscal-day)
            (set! period_end (gnc:decrement-date period_start 1 ))
               (if (gnc:date-lt? today-tm period_end)
                   (begin
                     (set-tm:mon period_start (- thismonth 1))
                    ))
               (set! period_start (gnc:decrement-month period_start 3))
               (if exclude-last-period?
                    (set! period_start (gnc:decrement-month period_start 3)))
               (set! period_end (gnc:increment-month-less-1-day period_start 3))
               (set! period_start (gnc:decrement-month period_start (* 3 (- numberperiods 1))))
                 )
          ((halfquarters)
            (set-tm:mon period_start fiscal-month)
            (set-tm:mday period_start fiscal-day)  ;; assume 7th
            (let ((calc-mid (gnc:increment-date period_start 15 ));;assume 22nd
                  (counter 0))
            (set-tm:year period_start (- (tm:year period_start) 1))
           ; (set! period_end (gnc:decrement-date period_end 1))
           ;; determine last period
            (while (gnc:date-lt? (gnc:increment-month period_start 6) today-tm) ;; move by quarters
                (set! period_start (gnc:increment-month period_start 3))
              )
             ;; check for half quarter
                        (set! period_start (gnc:increment-month period_start 3))
                        (set! calc-mid (gnc:increment-month period_start 3)) ;full quarter
                        (set! calc-mid (gnc:increment-date period_start
                             (+ 1 (floor (/ (gnc:delta-days period_start calc-mid) 2)))))
               (if exclude-last-period?
                 (if (gnc:date-lt? (gnc:increment-month period_start 3) today-tm) ;;move by 1/2 quarter?
                    (begin
                        (set! period_end  calc-mid)
                         )
                    (begin
                        (set! period_start (gnc:decrement-month period_start 3))
                        (set! period_end (gnc:increment-month-less-1-day period_start 3)) ;full quarter
                        (set! period_start (gnc:increment-date period_start
                             (+ 1 (floor (/ (gnc:delta-days period_start period_end) 2))))); 1/2 way
                     )
                )
                (if (gnc:date-lt? calc-mid today-tm)
                    (begin
                       (set! period_end (gnc:increment-month-less-1-day period_start 3)) ;full quarter
                        (set! period_end (gnc:increment-date period_start
                             (- (floor (/ (gnc:delta-days period_start period_end) 2)) 1))); 1/2 way
                     )
                   (begin
                        (set! period_start (gnc:decrement-month period_start 3))
                        (set! period_end (gnc:increment-month-less-1-day period_start 3)) ;full quarter
                        (set! period_start (gnc:increment-date period_start
                             (+ 2 (floor (/ (gnc:delta-days period_start period_end) 2))))); 1/2 way
                     )
                 )

             )
            ;;get rest of periods
              ;;have 1 period
              (set! counter 1)
              ;;and add 1 since moving by quarters not halfquarters
           (while (< (+ counter 1)  numberperiods);; add by quarters
              (begin
                   (set! period_start (gnc:decrement-month period_start 3))
                   (set! counter (+ 2 counter))
              ))
            (if (< counter numberperiods);; add half quarter if needed
                (if (= (tm:mday period_start) fiscal-day)
                    (begin
                        (set! period_start (gnc:decrement-month period_start 3))
                        (set! calc-mid (gnc:increment-month-less-1-day period_start 3)) ;full quarter
                        (set! period_start (gnc:increment-date period_start
                             (+ (floor (/ (gnc:delta-days period_start calc-mid) 2)) 1))); 1/2 way
                    )
                    (begin
                    (set! period_start (gnc:decrement-month period_start 1))
                    (set-tm:mday period_start fiscal-day)
                    )
                )
            )
      ))
        ((quarters)
            (set-tm:mon period_start fiscal-month)
            (set-tm:mday period_start fiscal-day)
            (set-tm:year period_start (- (tm:year period_start) 1))
            (set! period_end (gnc:decrement-date period_end 1))
            (while (gnc:date-lt? (gnc:increment-month period_start 6) period_end)
                (set! period_start (gnc:increment-month period_start 3))
              )
             (if exclude-last-period?
               (set! period_start (gnc:decrement-month period_start 3)))
            (set! period_end (gnc:increment-month-less-1-day period_start 3))

            (set! period_start (gnc:decrement-month period_start (* (- numberperiods 1) 3 )))
          )
         ((6months)
            (set-tm:mday period_start fiscal-day)
            (set! period_end (gnc:decrement-date period_start 1 ))
               (if (gnc:date-lt? today-tm period_end)
                   (begin
                     (set-tm:mon period_start (- thismonth 1))
                    ))
               (set! period_start (gnc:decrement-month period_start 6))
               (if exclude-last-period?
                    (set! period_start (gnc:decrement-month period_start 6)))
                (set! period_end (gnc:increment-month-less-1-day period_start 6))
                (set! period_start (gnc:decrement-month period_start (* 6 (- numberperiods 1))))
                 )
        ((halfyears)
            (set-tm:mon period_start fiscal-month)
            (set-tm:mday period_start fiscal-day)
            (set-tm:year period_start (- (tm:year period_start) 2))
            (set! period_end (gnc:decrement-date period_end 1))
            (while (gnc:date-lt? (gnc:increment-month period_start 6) period_end)
                (set! period_start (gnc:increment-month period_start 6))
              )
             (if exclude-last-period?
                (set! period_start (gnc:decrement-month period_start 6)))
            (set! period_end (gnc:increment-month-less-1-day period_start 6))
            (set! period_start (gnc:decrement-month period_start (* (- numberperiods 1) 6 )))
          )
        ((years)
            (set-tm:mon period_start fiscal-month)
            (set-tm:mday period_start fiscal-day)
              (if (gnc:date-lt? today-tm period_start)
                  (set-tm:year period_start (- (tm:year period_start) 1)))
              (set-tm:year period_start (- (tm:year period_start) 1))
              (if exclude-last-period?
                 (set-tm:year period_start (- (tm:year period_start) 1)))
              (set! period_end (gnc:increment-month-less-1-day period_start 12))
              (set-tm:year period_start (- (tm:year period_start) numberperiods))
                 )
            )
;(list (string-append (gnc-print-date (gnc:date->timepair period_start)) " - "
;          (gnc-print-date (gnc:date->timepair period_end) ))
;          (gnc:timepair-start-day-time  (gnc:date->timepair period_start))
(list (gnc:timepair-start-day-time  (gnc:date->timepair period_start))
      (gnc:timepair-end-day-time  (gnc:date->timepair period_end)) )
))

(define (gnc:getdates-average entrypicked exclude-last-period?)
(begin
(set! period_start (gnc:timepair->date now))
(set! period_end (gnc:timepair->date now))
(set! today-tm (gnc:timepair->date now))
(let ((day (gnc:day-of-week period_end))
      (fiscal-start (gnc:timepair->date (gnc:secs->timepair (gnc-accounting-period-fiscal-start))))
       ; (fiscal-month 0)
      (fiscal-day 1)
        )
    ;use period_end for weeks
   (set! fiscal-day (tm:mday fiscal-start))
   (set! period_end (gnc:decrement-date period_end (+ day 1) ))
   (if exclude-last-period?
     (set! period_end (gnc:decrement-date period_end 7 )))

    ;use period_start for months
   (set-tm:mday period_start fiscal-day)
   (if (gnc:date-lt? today-tm (gnc:decrement-date period_start 1 ))
       (begin
          (set-tm:mon period_start (- (tm:mon period_start) 1))
        ))
    (if exclude-last-period?
    (set! period_start (gnc:decrement-month period_start 1)))

   (case entrypicked
   ((4weeks)
        (set! period_start (gnc:decrement-date period_end (+ 6 (* 7 3)))))
   ((8weeks)
       (set! period_start (gnc:decrement-date period_end (+ 6 (* 7 7)))))
   ((12weeks)
       (set! period_start (gnc:decrement-date period_end (+ 6 (* 7 11)))))
   ((13weeks)
       (set! period_start (gnc:decrement-date period_end (+ 6 (* 7 12)))))
   ((26weeks)
       (set! period_start (gnc:decrement-date period_end (+ 6 (* 7 25)))))
   ((52weeks)
       (set! period_start (gnc:decrement-date period_end (+ 6 (* 7 51)))))
   ((3months)
        (set! period_start (gnc:decrement-month period_start 3))
        (set! period_end (gnc:increment-month-less-1-day period_start 3))
       )
   ((4months)
        (set! period_start (gnc:decrement-month period_start 4))
        (set! period_end (gnc:increment-month-less-1-day period_start 4))
       )
   ((6months)
        (set! period_start (gnc:decrement-month period_start 6))
        (set! period_end (gnc:increment-month-less-1-day period_start 6))
       )
   ((9months)
        (set! period_start (gnc:decrement-month period_start 9))
        (set! period_end (gnc:increment-month-less-1-day period_start 9))
       )
   ((12months)
        (set! period_start (gnc:decrement-month period_start 12))
        (set! period_end (gnc:increment-month-less-1-day period_start 12))
       )
    ((18months)
        (set! period_start (gnc:decrement-month period_start 18))
        (set! period_end (gnc:increment-month-less-1-day period_start 18))
       )
    ((24months)
        (set! period_start (gnc:decrement-month period_start 24))
        (set! period_end (gnc:increment-month-less-1-day period_start 24))
       )
   )

(list (gnc:timepair-start-day-time  (gnc:date->timepair period_start))
      (gnc:timepair-end-day-time  (gnc:date->timepair period_end)) )
)))


  (define (gnc:getdatedelta userpicks)
  ;; returns list consisting of elements of  (begin-date end-date delta-days-between-begin-and-end)
    (let (
        (periods '())
        (start-date (gnc:timepair->date (car userpicks)))
        (end-date (gnc:timepair->date (cadr userpicks)))
        (deltas-val (caddr userpicks))
        (date-start  (gnc:timepair->date (gnc:timepair-start-day-time (car userpicks))))
        (date-end  (gnc:timepair->date (gnc:timepair-end-day-time (cadr userpicks))))
        )
    (define (addto-periods! date-start date-end)
        (let ( (element (list (gnc:timepair-start-day-time (gnc:date->timepair date-start))
               (gnc:timepair-end-day-time (gnc:date->timepair date-end))
               (+ (gnc:delta-days date-start date-end) 1)) ))
                (addto! periods element))
        )
        (case deltas-val
        ((one)
            (addto-periods! start-date end-date))
        ((days)
            (set! date-end (gnc:increment-date date-start 0 ))
            (while (gnc:date-lte? date-start end-date)
                (begin
                    (addto-periods! date-start date-end)
                    (set! date-start (gnc:increment-date date-start 1))
                    (set! date-end (gnc:increment-date date-end 1))
                ))
             )
        ((weeks)
            (set! date-end (gnc:increment-date date-start 6 ))
            (while (gnc:date-lte? date-start end-date)
                (begin
                    (addto-periods! date-start (min-date date-end end-date))
                    (set! date-start (gnc:increment-date date-start 7))
                    (set! date-end (gnc:increment-date date-end 7))
                )))
        ((twoweeks)
            (set! date-end (gnc:increment-date date-start 13 ))
            (while (gnc:date-lte? date-start end-date)
                (begin
                    (addto-periods! date-start (min-date date-end end-date))
                    (set! date-start (gnc:increment-date date-start 14))
                    (set! date-end (gnc:increment-date date-end 14))
                )))
        ((fourweeks)
            (set! date-end (gnc:increment-date date-start 27 ))
            (while (gnc:date-lte? date-start end-date)
                (begin
                    (addto-periods! date-start (min-date date-end end-date))
                    (set! date-start (gnc:increment-date date-start 28))
                    (set! date-end (gnc:increment-date date-end 28))
                )))

        ((halfmonths)
            (while (gnc:date-lte? date-start end-date)
                (begin
                    (set! date-end (gnc:increment-date date-start 14 ))
                    (addto-periods! date-start (min-date date-end end-date))
                    (set! date-end (gnc:increment-month-less-1-day date-start 1))
                    (set! date-start (gnc:increment-date date-start 15))
                    (if (gnc:date-lte? date-start end-date)
                    (addto-periods! date-start (min-date date-end end-date)))

                    (set! date-start (gnc:increment-date date-end 1))
                    ; (gnc:warn "date-start" (gnc-print-date (gnc:date->timepair date-start))) ;;need (use-modules (gnucash main)) 
                )))
        ((months)
            (while (gnc:date-lte? date-start end-date)
                (begin
                    (set! date-end (gnc:increment-month-less-1-day date-start 1))
                    (addto-periods! date-start (min-date date-end end-date))
                    (set! date-start (gnc:increment-month date-start 1))
                )))
        ((3months)
            (while (gnc:date-lte? date-start end-date)
                (begin
                    (set! date-end (gnc:increment-month-less-1-day date-start 3))
                    (addto-periods! date-start (min-date date-end end-date))
                    (set! date-start (gnc:increment-month date-start 3))
                )))

        ((6months)
            (while (gnc:date-lte? date-start end-date)
                (begin
                    (set! date-end (gnc:increment-month-less-1-day date-start 6))
                    (addto-periods! date-start (min-date date-end end-date))
                    (set! date-start (gnc:increment-month date-start 6))
                )))

        ((halfquarters)
            (while (gnc:date-lte? date-start end-date)
                (begin
                    (set! date-end (gnc:increment-month-less-1-day date-start 3)) ;full quarter
                    (set! date-end (gnc:increment-date date-start
                                (floor (/  (+ 1(gnc:delta-days date-start date-end)) 2)))); 1/2 way
                    (addto-periods! date-start (min-date date-end end-date))
                    (set! start-date (gnc:increment-date date-end 1))
                    (set! date-end (gnc:increment-month-less-1-day date-start 3))
                    (set! date-start (gnc:increment-date start-date 0))
                    (if (gnc:date-lt? date-start end-date)
                    (addto-periods! date-start (min-date date-end end-date)))
                    (set! date-start (gnc:increment-date date-end 1))
                )))
        ((quarters)
            (while (gnc:date-lte? date-start end-date)
                (begin
                    (set! date-end (gnc:increment-month-less-1-day date-start 3))
                    (addto-periods! date-start (min-date date-end end-date))
                    (set! date-start (gnc:increment-month date-start 3))
                )))
        ((halfyears)
            (while (gnc:date-lte? date-start end-date)
                (begin
                    (set! date-end (gnc:increment-month-less-1-day date-start 6))
                    (addto-periods! date-start (min-date date-end end-date))
                    (set! date-start (gnc:increment-month date-start 6))
                )))
        ((years)
            (while (gnc:date-lte? date-start end-date)
                (begin
                    (set! date-end (gnc:increment-month-less-1-day date-start 12))
                    (addto-periods! date-start (min-date date-end end-date))
                    (set! date-start (gnc:increment-month date-start 12))
                )))
            )
        (reverse periods)
)
)

