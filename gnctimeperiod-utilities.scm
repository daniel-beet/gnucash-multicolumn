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
   #:export (gnc:getdates gnc:list-years gnc:list-periods 
             gnc:list-lasts gnc:list-months gnc:list-operands
			 gnc:daysinmonth gnc:dayofweek gnc:day-of-year gnc:set_yday gnc:day-of-week gnc:decrement-date
			 ))
(use-modules (gnucash gettext))

(use-modules (gnucash gnc-module))

(gnc:module-load "gnucash/report/report-system" 0) 
;;dbd start
(define now (timespecCanonicalDayTime (cons (current-time) 0)))
(define today-tm (gnc:timepair->date now))
(define this_year (+ 1900 (tm:year today-tm)))
(define thismonth (tm:mon today-tm))
(define thisday (tm:mday today-tm))
(define thisyear (tm:year today-tm))
(define whichmonth (tm:mon today-tm))
	

(define period_start (gnc:timepair->date now))
(define period_end (gnc:timepair->date now))


;; can change following text for local language


(define lastyr  "Last year")
(define curryear "This year")
(define next_yr (number->string (+ this_year 1)))
(define curr_yr (number->string (- this_year 0)))
(define prev_yr (number->string (- this_year 1)))
(define 2yr_ago (number->string (- this_year 2)))
(define 3yr_ago (number->string (- this_year 3)))
(define 4yr_ago (number->string (- this_year 4)))
(define 5yr_ago (number->string (- this_year 5)))
(define alltime "All Time")



(define fullyear "Full year")
(define last_qtr "Last Quarter")
(define 1st_hlf "1st Half")
(define 2nd_hlf "2nd Half")
(define 1st_qtr "1st Quarter")
(define 2nd_qtr "2nd Quarter")
(define 3rd_qtr "3rd Quarter")
(define 4th_qtr "4th Quarter")
(define weektodate "Week to Date")
(define mnthtodate "Month to Date")
(define qrtrtodate "Quarter to date")
(define yeartodate "Year to date")
(define yesterday "Yesterday")
(define today "Today")
(define 2fullyears "2 full years")
(define 5fullyears "5 full years")


(define last_wk "Last week")
(define lastmonth "Last month")
(define last_3mnth "Last 3 months")
(define last_6mnth "Last 6 months")
(define last_12mnth "Last 12 months")
(define prev_7days "previous 7 Days")
(define prev_30days "previous 30 Days")
(define prev_31days "previous 31 Days")
(define prev_90days "previous 90 Days")
(define prev_365days "previous 365 Days")
(define last_7days "Last 7 Days")
(define last_30 "Last 30 days")
(define last_90 "Last 90 Days")
(define last_365 "Last 365 Days")

(define Jan "January")
(define Feb "February")
(define Mar "March")
(define Apr "April")
(define May "May")
(define Jun "June")
(define Jul "July")
(define Aug "August")
(define Sep "September")
(define Oct "October")
(define Nov "November")
(define Dec "December")

(define multiply "multiply")
(define divide "divide")

(define month-of "Month of ")

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

;; end of section to change to local text
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
	(gnc:dayofyear (+ (tm:year date) 1900) (tm:mon date) (tm:mday date)))
	
	
(define (gnc:dayofyear year month day)
;; month (0-11)  (ie. jan=0)
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
	(set-tm:mday date 1)
    (set-tm:mon date 0)
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
	  (yr (-(+ (tm:year date) 1900) 1)))
	(remainder  (+ (- (quotient yr 4)(quotient yr 100)) (quotient yr 400) yr (gnc:day-of-year date)) 7)
	))
 
 (define (gnc:decrement-date date decrement) 
;;
  (let* (
	  (yr (+ (tm:year date) 1900) )
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
(define (gnc:date-lt? tm1 tm2)	
	(if (not (= (tm:year tm1) (tm:year tm2)))
		(< (tm:year tm1) (tm:year tm2))
		; years match
		(< (gnc:day-of-year tm1) (gnc:day-of-year tm2))
	))
	
 	
		  
;;
;; Determine which time period was picked and return the start date  and end date
;;		  
	(define (gnc:getdates userpicks)
		(let (
		(whichperiod-val (car userpicks))
		(year-val (cadr userpicks))
		(period-val (caddr userpicks))
		(last-val (cadddr userpicks))
		(month-val (cadddr (cdr userpicks)))
		
		(year_end 2000)
		(isday (cdr userpicks) )
		(fiscal-start (gnc:timepair->date (gnc:secs->timepair (gnc-accounting-period-fiscal-start))))
		(fiscal-month 0)
		(fiscal-day 1)
		(fiscal-jan1? #t)
		(fiscal-related? #f)
		)
		(set! fiscal-month (tm:mon fiscal-start) )
		(set! fiscal-day (tm:mday fiscal-start))
		(set! fiscal-jan1? (if (and (= fiscal-month 0) (= fiscal-day 1))
							#t
							#f))
	
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
		((yr_0) 	(set! year_end (- thisyear 0)))
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
					(let ((day (gnc:day-of-week today-tm)))
					 (set-tm:year period_end year_end)
					 (set-tm:mon period_end thismonth)
		          	 (set-tm:mday period_end thisday)
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
					(let ((day (gnc:day-of-week today-tm)))
					 (set-tm:year period_end year_end)
					 (set-tm:mon period_end thismonth)
		          	 (set-tm:mday period_end thisday)
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
          ))
    
    
 