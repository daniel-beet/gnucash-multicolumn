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
			 gnc:days-in-month gnc:dayofweek gnc:day-of-year gnc:set_yday gnc:day-of-week gnc:decrement-date
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


(define last_wk "Last week")
(define lastmonth "Last month")
(define last_3mnth "Last 3 months")
(define last_6mnth "Last 6 months")
(define last_12mnth "Last 12 months")
(define prev_7days "previous 7 Days")
(define prev_30days "previous 30 Days")
(define prev_31days "previous 31 Days")
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

(define gnc:list-years 
   (list (list->vector
             (list 'alltime
                   (N_ alltime)
                   (N_ "from 1901 till now")))
            (list->vector
             (list 'last-yr
                   (N_ lastyr)
                   (N_ "always uses Last year")))
            (list->vector
             (list 'this-yr
                   (N_ curryear)
                   (N_ "this year")))
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
                   (N_ "Help for third option.")))
            (list->vector
             (list 'yr_minus5
                   (N_ 5yr_ago)
                   (N_ "The fourth option rules!")))
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
                   (N_ "Help for second option.")))
			(list->vector
             (list '2nd_hlf
                   (N_ 2nd_hlf)
                   (N_ "Help for first option.")))
			(list->vector
             (list '1st_qtr
                   (N_ 1st_qtr)
                   (N_ "Help for second option.")))
            (list->vector
             (list '2nd_qtr
                   (N_ 2nd_qtr)
                   (N_ "Help for third option.")))
			(list->vector
             (list '3rd_qtr
                   (N_ 3rd_qtr)
                   (N_ "Help for third option.")))
			(list->vector
             (list '4th_qtr
                   (N_ 4th_qtr)
                   (N_ "Help for third option.")))
			(list->vector
             (list 'weektodate
                   (N_ weektodate)
                   (N_ "this year")))
			(list->vector
             (list 'mnthtodate
                   (N_ mnthtodate)
                   (N_ "Help for third option.")))
			(list->vector
             (list 'qrtrtodate
                   (N_ qrtrtodate)
                   (N_ "Help for third option.")))
			(list->vector
             (list 'yeartodate
                   (N_ yeartodate)
                   (N_ "this year")))
			(list->vector
             (list 'yesterday
                   (N_ yesterday)
                   (N_ "this year")))
			(list->vector
             (list 'today
                   (N_ today)
                   (N_ "this year")))
           
	)
)
(define gnc:list-lasts
    (list (list->vector
             (list 'last_qtr
                   (N_ last_qtr)
                   (N_ "Help for first option.")))
			(list->vector
             (list 'last_3mnth
                   (N_ last_3mnth)
                   (N_ "Help for third option.")))
            (list->vector
             (list 'lastmonth
                   (N_ lastmonth)
                   (N_ "Help for first option.")))
			(list->vector
             (list 'last_wk
                   (N_ last_wk)
                   (N_ "Help for third option.")))
			(list->vector
             (list 'last_6mnth
                   (N_ last_6mnth)
                   (N_ "Help for third option.")))
			(list->vector
             (list 'last_12mnth
                   (N_ last_12mnth)
                   (N_ "Help for third option.")))
			(list->vector
             (list 'last_30
                   (N_ last_30)
                   (N_ "Help for third option.")))
			(list->vector
             (list 'last_90
                   (N_ last_90)
                   (N_ "Help for third option.")))
			(list->vector
             (list 'last_365
                   (N_ last_365)
                   (N_ "Help for third option.")))
			(list->vector
             (list 'last_7days
                   (N_ last_7days)
                   (N_ "Help for third option.")))
			(list->vector
             (list 'prev_7days
                   (N_ prev_7days)
                   (N_ "Help for third option.")))
			(list->vector
             (list 'prev_30days
                   (N_ prev_30days)
                    (N_ "Help for third option.")))
	        (list->vector
             (list 'prev_31days
                   (N_ prev_31days)
                    (N_ "Help for third option.")))
	          
			
			
	)
)

(define gnc:list-months
    (list 
			(list->vector
             (list '0
                   (N_ Jan)
                   (N_ "Help for third option.")))
			(list->vector
             (list '1
                   (N_ Feb)
                   (N_ "Help for third option.")))
		    (list->vector
             (list '2
                   (N_ Mar)
                   (N_ "Help for third option.")))
		    (list->vector
             (list '3
                   (N_ Apr)
                   (N_ "Help for third option.")))
            (list->vector
             (list '4
                   (N_ May)
                   (N_ "Help for third option.")))
            (list->vector
             (list '5
                   (N_ Jun)
                   (N_ "Help for third option.")))
            (list->vector
             (list '6
                   (N_ Jul)
                   (N_ "Help for third option.")))
            (list->vector
             (list '7
                   (N_ Aug)
                   (N_ "Help for third option.")))
            (list->vector
             (list '8
                   (N_ Sep)
                   (N_ "Help for third option.")))
            (list->vector
             (list '9
                   (N_ Oct)
                   (N_ "Help for third option.")))
            (list->vector
             (list '10
                   (N_ Nov)
                   (N_ "Help for third option.")))
            (list->vector
             (list '11
                   (N_ Dec)
                   (N_ "The fourth option rules!")))
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
  
 (define (gnc:days-in-month m y)
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
	(set! dy (+ dy (gnc:days-in-month mnth year)))
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
    (while (> (+ 0 daysleft) (gnc:days-in-month mnth year) )
		(begin 
			(set! daysleft (- daysleft (gnc:days-in-month mnth year)))
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
		)
		(set! year_end (+ thisyear 0))
		(set-tm:mday period_start 1)
		(set-tm:isdst period_start -1)
		(set! period_end today-tm)
		(set-tm:mday period_end 1)
		(set-tm:isdst period_end -1)
		
		
		(if (equal? year-val 'last-yr) (set! year_end (- thisyear 1) ) 
		(if (equal? year-val 'this-yr) (set! year_end (- thisyear 0))
		(if (equal? year-val 'yr_0) (set! year_end (- thisyear 0))
		(if (equal? year-val 'yr_minus1) (set! year_end (- thisyear 1))
		(if (equal? year-val 'yr_minus2) (set! year_end (- thisyear 2))
		(if (equal? year-val 'yr_minus3) (set! year_end (- thisyear 3))
		(if (equal? year-val 'yr_minus4) (set! year_end (- thisyear 4))
		(if (equal? year-val 'yr_minus5) (set! year_end (- thisyear 5))
		))))))))
		(set-tm:year period_end year_end)
		(set-tm:year period_start year_end)
		;; Determine which period  ie. the month and day for start and finish		
		;; 
		(if (equal? whichperiod-val 'period)
		 (begin	
			(if (equal? period-val 'fullyear)
					(begin
						(set-tm:mon period_end 11 ) 
		                (set-tm:mday period_end 31 )
						(set-tm:mon period_start 0) 
					)
			(if (equal? period-val '1st_hlf)
					(begin
						(set-tm:mon period_end 5 ) 
		                (set-tm:mday period_end 30 )
						(set-tm:mon period_start 0) 
		           	)
			(if (equal? period-val '2nd_hlf)
					(begin
						(set-tm:mon period_end 11 ) 
		                (set-tm:mday period_end 31 )
						(set-tm:mon period_start 6) 
		           	)
			(if (equal? period-val '1st_qtr)
					(begin
						(set-tm:mon period_end 2 ) 
		                (set-tm:mday period_end 31 )
						(set-tm:mon period_start 0) 
		           	)
			(if (equal? period-val '2nd_qtr)
					(begin
						(set-tm:mon period_end 5 ) 
		                (set-tm:mday period_end 30 )
						(set-tm:mon period_start 3) 
		           	)
			(if (equal? period-val '3rd_qtr)
					(begin
						(set-tm:mon period_end 8 ) 
		                (set-tm:mday period_end 30 )
						(set-tm:mon period_start 6) 
		           	)
			(if (equal? period-val '4th_qtr)
					(begin
						(set-tm:mon period_end 11 ) 
		                (set-tm:mday period_end 31 )
						(set-tm:mon period_start 9) 
		           	)
			(if (equal? period-val 'weektodate)
					(begin
					(let ((day (gnc:day-of-week today-tm)))
					 (set-tm:year period_end year_end)
					 (set-tm:mon period_end thismonth)
		          	 (set-tm:mday period_end thisday)
		          	 (set! period_start (gnc:decrement-date period_end day)))
		            )					
			(if (equal? period-val 'mnthtodate)
					(begin
						(set-tm:mon period_end thismonth ) 
		                (set-tm:mday period_end thisday )
						(set-tm:mon period_start thismonth) 
		            )
			(if (equal? period-val 'qrtrtodate)
					(begin
						(set-tm:mon period_end thismonth ) 
		                (set-tm:mday period_end thisday )
						(case (quotient thismonth 3)
						((0) (set-tm:mon period_start 0)  )
		                ((1) (set-tm:mon period_start 3 ) )
						((2) (set-tm:mon period_start 6) )
		                ((3) (set-tm:mon period_start 9))
						)
					)	
			(if (equal? period-val 'yeartodate)
					(begin
						(set-tm:mon period_end thismonth ) 
		                (set-tm:mday period_end thisday )
						(set-tm:mon period_start 0) 
		            )
			(if (equal? period-val 'yesterday)
						(begin
							(set-tm:year period_end year_end)
							(set-tm:mon period_end thismonth)
							(set-tm:mday period_end thisday)
							(set! period_end (gnc:decrement-date period_end 1 )) 
							(set! period_start (gnc:decrement-date period_end 0))	
							)
			(if (equal? period-val 'today)
						(begin
							(set-tm:year period_end year_end)
							(set-tm:mon period_end thismonth)
							(set-tm:mday period_end thisday)
							(set-tm:year period_start year_end)
							(set-tm:mon period_start thismonth)
							(set-tm:mday period_start thisday)
							
							)
			
			)))))))))))))
			(set-tm:sec period_start 0)
			(set-tm:min period_start 0)
			(set-tm:hour period_start 0)							
	   ))
	   
	 (if (equal? whichperiod-val 'last)
		 (begin
		  (if (equal? last-val 'last_qtr)
					(begin
						(case (quotient thismonth 3)
						((0) (set-tm:year period_start (- year_end 1))
						     (set-tm:year period_end (- year_end 1))
							 (set-tm:mon period_end 11 ) 
							 (set-tm:mday period_end 31 )
							 (set-tm:mon period_start 9) 
							 )
		                ((1) (set-tm:mon period_end 2 ) 
							 (set-tm:mday period_end 31 )
							 (set-tm:mon period_start 0))
						((2) (set-tm:mon period_end 5 ) 
							 (set-tm:mday period_end 30 )
							 (set-tm:mon period_start 3) )
		                ((3) (set-tm:mon period_end 8 ) 
							 (set-tm:mday period_end 30 )
							 (set-tm:mon period_start 6)))
					)		
			(if (equal? last-val 'last_3mnth)
		
						(if (<= thismonth 2)
							(begin
							(set-tm:year period_start (- year_end 1))
							(set-tm:mon period_start (+ 9 thismonth))
							(if (equal? thismonth 0) 
							  (begin
							    (set-tm:year period_end (- year_end 1))
								(set-tm:mon period_end 11)
								(set-tm:mday period_end (gnc:days-in-month 11 (- (+ year_end 1900) 1)))
						
								;; else 
							  ) (begin
							     (set-tm:mon period_end (- thismonth 1))
								 (set-tm:mday period_end (gnc:days-in-month (- thismonth 1) (+ year_end 1900)))
								 ))
							)
							;; else month is > 2
							(begin
								  (set-tm:mon period_start (- thismonth 3))
								  (set-tm:mon period_end (- thismonth 1))
								  (set-tm:mday period_end (gnc:days-in-month (- thismonth 1) (+ year_end 1900)))
							)
						)
			(if (equal? last-val 'lastmonth)
					(begin
						(if (equal? thismonth 0) 
							  (begin
							    (set-tm:year period_end (- year_end 1))
								(set-tm:mon period_end 11)
								(set-tm:year period_start (- year_end 1))
								)		
								;; else 
							     (set-tm:mon period_end (- thismonth 1))
								)
								
						(set-tm:mday period_end (gnc:days-in-month (tm:mon period_end) (+ year_end 1900)))
						(set-tm:mon period_start (tm:mon period_end))
					)
			(if (equal? last-val 'last_wk)
					(begin
					(let ((day (gnc:day-of-week today-tm)))
					 (set-tm:year period_end year_end)
					 (set-tm:mon period_end thismonth)
		          	 (set-tm:mday period_end thisday)
		          	 (set! period_end (gnc:decrement-date period_end (+ day 1) )) 
				     (set! period_start (gnc:decrement-date period_end 6)))
					)				
			
			(if (equal? last-val 'last_6mnth)
		
						(if (<= thismonth 5)
							(begin
							(set-tm:year period_start (- year_end 1))
							(set-tm:mon period_start (+ 6 thismonth))
							(if (equal? thismonth 0) 
							  (begin
							    (set-tm:year period_end (- year_end 1))
								(set-tm:mon period_end 11)
								(set-tm:mday period_end (gnc:days-in-month 11 (- (+ year_end 1900) 1)))
						
								;; else 
							  ) (begin
							     (set-tm:mon period_end (- thismonth 1))
								 (set-tm:mday period_end (gnc:days-in-month (- thismonth 1) (+ year_end 1900)))
								 ))
							)
							;; else month is > 2
							(begin
								  (set-tm:mon period_start (- thismonth 3))
								  (set-tm:mon period_end (- thismonth 1))
								  (set-tm:mday period_end (gnc:days-in-month (- thismonth 1) (+ year_end 1900)))
							)
						)
			(if (equal? last-val 'last_12mnth)
		
							(begin
							(set-tm:year period_start (- year_end 1))
							(set-tm:mon period_start  thismonth)
							(if (equal? thismonth 0) 
							  (begin
							    (set-tm:year period_end (- year_end 1))
								(set-tm:mon period_end 11)
								(set-tm:mday period_end (gnc:days-in-month 11 (- (+ year_end 1900) 1)))
						
								;; else 
							  ) (begin
							     (set-tm:mon period_end (- thismonth 1))
								 (set-tm:mday period_end (gnc:days-in-month (- thismonth 1) (+ year_end 1900)))
								 ))
							)
							
			(if (equal? last-val 'last_30)
							(begin
							(set-tm:year period_end year_end)
							(set-tm:mon period_end thismonth)
							(set-tm:mday period_end thisday)
							(set! period_start (gnc:decrement-date period_end 29))
							)							
			(if (equal? last-val 'last_90)
							(begin
							(set-tm:year period_end year_end)
							(set-tm:mon period_end thismonth)
							(set-tm:mday period_end thisday)
							(set! period_start (gnc:decrement-date period_end 89))	
							)
			(if (equal? last-val 'last_365)
						(begin
							(set-tm:year period_end year_end)
							(set-tm:mon period_end thismonth)
							(set-tm:mday period_end thisday)
							(set! period_start (gnc:decrement-date period_end 364))	
							)
			(if (equal? last-val 'last_7days)
						(begin
							(set-tm:year period_end year_end)
							(set-tm:mon period_end thismonth)
							(set-tm:mday period_end thisday)
							(set! period_start (gnc:decrement-date period_end 6))	
							)
			(if (equal? last-val 'prev_7days)
						(begin
							(set-tm:year period_end year_end)
							(set-tm:mon period_end thismonth)
							(set-tm:mday period_end thisday)
							(set! period_end (gnc:decrement-date period_end 1 )) 
							(set! period_start (gnc:decrement-date period_end 6))	
							)
			(if (equal? last-val 'prev_30days)
						(begin
							(set-tm:year period_end year_end)
							(set-tm:mon period_end thismonth)
							(set-tm:mday period_end thisday)
							(set! period_end (gnc:decrement-date period_end 1 )) 
							(set! period_start (gnc:decrement-date period_end 29))	
							)
			(if (equal? last-val 'prev_31days)
						(begin
							(set-tm:year period_end year_end)
							(set-tm:mon period_end thismonth)
							(set-tm:mday period_end thisday)
							(set! period_end (gnc:decrement-date period_end 1 )) 
							(set! period_start (gnc:decrement-date period_end 30))	
							)
			
			
			)))))))))))))						
	   ))
	     
	   
	   ;; Determine which period  ie. the month and day for start and finish		
		;; 
		(if (equal? whichperiod-val 'month)
		 (begin			
			(set! whichmonth month-val)
			(set-tm:mon period_end month-val ) 
		  	(set-tm:mday period_end (gnc:days-in-month month-val (+ 1900 year_end)))
			(set-tm:mon period_start month-val) 					
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
    
    
 