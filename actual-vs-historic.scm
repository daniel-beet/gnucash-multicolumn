;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; actual-vs-historic.scm: actual versus historic report
;;
;; (C) 2005 by Chris Shoemaker <c.shoemaker@cox.net>
;;
;; based on cash-flow.scm by:
;; Herbert Thoma <herbie@hthoma.de>
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report standard-reports actual-vs-historic))
;;following line needed to print in trace file use:  (gnc:warn "disp current-col is" current-col) see in C:\Users\USERNAME\AppData\Local\Temp
(use-modules (gnucash main)) 
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(use-modules (gnucash printf))
(use-modules (gnucash engine))

;; following neded for gnc-locale-to-utf8
(use-modules (gnucash core-utils))

;;following line is step 1 of 4 needed to add gnctimeperiod-utilities
(use-modules (gnucash gnctimeperiod-utilities))

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/gnome-utils" 0) ;for gnc-build-url

(define reportname (N_ "Actual vs Historic"))

;; define all option's names so that they are properly defined
;; in *one* place.
;(define optname-from-date (N_ "Start Date"))
;(define optname-to-date (N_ "End Date"))

(define pagename-base (N_ "Base"))
(define optname-display-depth (N_ "Account Display Depth"))
(define optname-show-subaccounts (N_ "Always show sub-accounts"))
(define optname-accounts (N_ "Account"))

;(define optname-price-source (N_ "Price Source"))
(define optname-show-rates (N_ "Show Exchange Rates"))
;(define optname-show-full-names (N_ "Show Full Account Names"))
(define optname-show-base (N_ "Show base period "))
(define opthelp-show-base (N_ "Display a column for the base (selected on base tab) values."))
(define optname-show-average (N_ "Show average"))
(define opthelp-show-average (N_ "Display a column for the average of the historic values."))
(define optname-select-columns (N_ "Select Columns"))
(define optname-show-historic-periods (N_ "Show historic periods"))
(define opthelp-show-historic-periods (N_ "Display actual and/or diff columns for the historic values."))
(define optname-show-actual-cols (N_ "Show actual coumns"))
(define opthelp-show-actual-cols (N_ "Display columns with the actual values."))
(define optname-show-difference (N_ "Show Difference"))
(define opthelp-show-difference (N_ "Displays the difference as actual - historic."))
(define optname-show-total-diff (N_ "Show Difference for Total"))
(define opthelp-show-total-diff (N_ "The difference for the Total is the sum of the difference between each period and the base period"))
(define optname-show-totalcol (N_ "Show Column with Totals"))
(define opthelp-show-totalcol (N_ "Display a column with the row totals."))
(define optname-show-zb-accounts (N_ "Include accounts with zero total balances and historic values"))
(define opthelp-show-zb-accounts (N_ "Include accounts with zero total (recursive) balances and historic values in this report."))
(define optname-compress-periods (N_ "Compress prior/later periods"))
(define opthelp-compress-periods (N_ "Accumulate columns for periods before and after the current period to allow focus on the current period."))
(define optname-bottom-behavior (N_ "Flatten list to depth limit"))
(define opthelp-bottom-behavior
  (N_ "Displays accounts which exceed the depth limit at the depth limit."))

;; this is step 2 of 4
;; needed for gnctimeperiod-utilities
;; define all option's names so that they are properly defined
;; in *one* place.
;; can change following text for local language

;; following value may need to be changed
(define the_tab pagename-base)

(define text-whichperiod "Select Time Period")
(define text-whichperiod-help "Select which time period to use")
(define text-customdates "Custom Dates")
(define text-customdates-help "Choose the start and end dates using custom date and ignore the section labeled specified choices")
(define custom-from-date (N_ "Custom Date - Start Date"))
(define custom-to-date (N_ "Custom Date - End Date"))
(define text-pick-year "Year for Specified Choices")
(define text-pick-year-help "Pick the year for report - Not used for custom dates, only used for specified choices")
(define text-period "Specified Choices - Period")
(define text-period-help "Choose for time periods such as full year or second quarter or month-to-date")
(define text-last "Specified Choices - Last")
(define text-last-help "Choose for choices like last 3 months,  last 90 days, last month, last week ")
(define text-month "Specified Choices - Month")
(define text-month-help "choose for choices consisting of months of the year, note the month is considered to start on the day specified in edit/preferences/accountingPeriod")

(define text-number-days "  Number of Days:")
(define text-scaled-to  "  Scaled to:")

(define text-exclude-average "Do not include last period in creating periods")
(define text-use-custom "Use custom date instead of choosing number of periods")
(define text-help-use-custom "Use the custom date in following section to determine comparison period instead of 'select number of comparison periods'")

(define pagename-compare (N_ "Compare"))
;(define optname-compare? (N_ "Compare periods?"))
(define optname-show-days (N_ "Show number of days"))
(define opthelp-show-days "Show a row with the number of days in each period")
(define optname-scale-automatically? (N_ "Scale results"))
(define opthelp-scale-automatically? (N_ "Scale all periods to number of days given below"))
(define optname-scale-to (N_ "scale to"))
(define opthelp-scale-to (N_ "Scale all periods including the base to this number of days"))
(define text-whichcompareperiod "Select Comparison Period")
(define text-numbercompareperiods "Select Number of Comparison Periods")
(define text-help-numcompareperiods "Number of periods - Selecting 3 will give 3 months if months was picked for divide periods  Remember you can select custom dates instead.")
(define text-compare-divide   "divide period into")


(define splits '())

(define gnc:list-datechoices
   (list (list->vector
             (list 'customdates
                   (N_ text-customdates)
                   (N_ text-customdates-help)))
            (list->vector
             (list 'period
                   (N_ text-period)
                   (N_ text-period-help)))
            (list->vector
             (list 'last
                   (N_ text-last)
                   (N_ "choices such as last 3 months, last 30 days, last month, last quarter")))
            (list->vector
             (list 'month
                   (N_ text-month)
                   (N_ text-month-help)))
           )
)
    (define scale-num
        (list
        (cons '*  (vector gnc-numeric-mul (N_ "multiplied by ")))
        (cons '/  (vector gnc-numeric-div (N_ "divided by ")))
        ))

;; end of section 2 needed for gnctimeperiod-utilities
(define (display-date-interval-columns  the-list)
  (let* (
        (begin-date (car the-list))
        (end-date (cadr the-list))
        (period-start (gnc:timepair->date begin-date))
        (start-day (tm:mday period-start))
        (end-start-1-month (gnc:increment-month-less-1-day period-start 1))
        (end-start-1-year (gnc:increment-month-less-1-day period-start 12))
        (period-end (gnc:timepair->date end-date))
        (fiscal-start (gnc:timepair->date (gnc:secs->timepair (gnc-accounting-period-fiscal-start))))
        (fiscal-day  (tm:mday fiscal-start))
        (display-date (sprintf #f (_ " %s To %s") (gnc-print-date begin-date) (gnc-print-date end-date) ))
        )

          ;check if it is an entire year or an entire month
       (if (and (gnc:date-eq? period-end end-start-1-year) (equal? start-day fiscal-day))
              (set! display-date (sprintf #f (_ " %s ") (number->string (gnc:date-get-year period-start))))
            )
       (if (and (gnc:date-eq? period-end end-start-1-month) (equal? start-day fiscal-day))
           (set! display-date (sprintf #f (_ (string-append (gnc-locale-to-utf8 (strftime "%B " period-start))
                                              "<br>" (gnc-locale-to-utf8 (strftime " %Y"  period-start)) )))))
         ;  (set! display-date (sprintf #f (_ (gnc:date-get-month-year-string  period-start)))))
       ;  ))
         display-date))

(define (display-num-days-in-period the-list)
   (_ (number->string (caddr the-list)))
   )


;; options generator
(define (budget-report-options-generator)
  (let* ((options (gnc:new-options))
    (add-option
     (lambda (new-option)
       (gnc:register-option options new-option))))

    ;; date interval
  ;;  (gnc:options-add-date-interval!
  ;;   options pagename-base
  ;;   optname-from-date optname-to-date "a")

  ;  (gnc:options-add-price-source!
  ;   options pagename-base optname-price-source "c" 'pricedb-nearest)

    ;;(gnc:register-option
    ;; options
    ;; (gnc:make-simple-boolean-option
    ;;  pagename-base optname-show-rates
    ;;  "d" (N_ "Show the exchange rates used") #f))

 ;   (gnc:register-option
 ;    options
 ;    (gnc:make-simple-boolean-option
 ;     pagename-base optname-show-full-names
 ;     "e" (N_ "Show full account names (including parent accounts).") #t))

    ;; General options

;; To add gnctimeperiod-utilities comment out  old  period over which to report income
;; and add section 3
;;

;;  (gnc:options-add-date-interval!
 ;;  gnc:*transaction-report-options*
 ;;  pagename-general (N_ "Start Date") (N_ "End Date") "a")
;;

;;
 ;; step 3 of 4 to add gnctimeperiod-utilities
     ;add  select custom date or a specific period
     ; changed add-option to gnc:register-option
(let ((periodoptions options));; see gnc:register option above for reason for entry
    (gnc:register-option
     options
        (gnc:make-multichoice-callback-option
    ;    (gnc:make-multichoice-option
        the_tab (N_ text-whichperiod)
        "ca" (N_ text-whichperiod-help)
        'last
;;        gnc:list-datechoices
;;        ))
        gnc:list-datechoices #f
        (lambda (x)
        (gnc-option-db-set-option-selectable-by-name
         periodoptions the_tab (N_ text-pick-year)
         (if (equal? x 'customdates) #f #t))
        (gnc-option-db-set-option-selectable-by-name
         periodoptions the_tab (N_ text-period)
         (if (equal? x 'period) #t #f))
        (gnc-option-db-set-option-selectable-by-name
         periodoptions the_tab (N_ text-last)
         (if (equal? x 'last) #t #f))
        (gnc-option-db-set-option-selectable-by-name
         periodoptions the_tab (N_ text-month)
         (if (equal? x 'month) #t #f))
        ))
    )
     ; add  custom date
    (gnc:options-add-date-interval!
     periodoptions the_tab
        custom-from-date custom-to-date "cb")
   )
     ;  add pick year for specific period
    (gnc:register-option
     options
        (gnc:make-multichoice-option the_tab (N_ text-pick-year)
        "ce" (N_ text-pick-year-help) 'this-yr
        gnc:list-years
        ))
     ;  add pick specific period
    (gnc:register-option
     options
        (gnc:make-multichoice-option the_tab (N_ text-period)
        "cf" (N_ text-period-help) 'fullyear
        gnc:list-periods
        ))
    ; add pick specific last XX
    (gnc:register-option
     options
        (gnc:make-multichoice-option the_tab (N_ text-last)
        "cg" (N_ text-last-help) 'lastmonth
        gnc:list-lasts
        ))
      ; add pick specific month
    (gnc:register-option
     options
        (gnc:make-multichoice-option the_tab (N_ text-month)
        "ch" (N_ text-month-help) '4
        gnc:list-months
        ))
  ;    ; add pick for multiply or divide
 ;   (gnc:register-option
 ;    options
  ;      (gnc:make-multichoice-option the_tab (N_ "Scale Results")
  ;      "ci" (N_ "Scale the results - multiply or divide by scale factor") '*
  ;      gnc:list-operands
  ;      ))
 ;   ; add where number for multiply or divide can be changed
 ;   (gnc:register-option
 ;    options
  ;      (gnc:make-number-range-option the_tab (N_ "Scale Number Option")
  ;      "cj" (N_ "Number to multiply or divide by")
  ;          1.0     ;; default
  ;          1       ;; lower bound
  ;        366.0     ;; upper bound
   ;         2.0     ;; number of decimals
   ;         1.0    ;; step size
   ;     ))
;;end of section 3 for gnctimeperiod-utilities
;;
     ;comparison period
 ;   (gnc:register-option
 ;   options
 ;     (gnc:make-complex-boolean-option
 ;       pagename-compare optname-compare?
 ;       "a1" (N_ "Compare multiple periods") #f
 ;       #f
;            (lambda (x)
 ;       (gnc-option-db-set-option-selectable-by-name
 ;        options pagename-compare optname-no-base?
 ;        x))
 ;        ))


    (gnc:register-option
    options
        (gnc:make-multichoice-option pagename-compare (N_ text-compare-divide)
        "ba" (N_ "Divide period into smaller periods") 'months
        gnc:list-comparechoices
        ))
    ;  add exclude last week or month from average
    (gnc:register-option
    options
        (gnc:make-simple-boolean-option
           pagename-compare (N_ text-exclude-average)
        "bb" (N_ "Do not include the last week in week averages, last month in month averages") #t
        ))
    ;  add pick specific period
    ; add where number for multiply or divide can be changed
    (gnc:register-option
    options
        (gnc:make-number-range-option pagename-compare (N_ text-numbercompareperiods)
        "ca" (N_ text-help-numcompareperiods)
            3.0     ;; default
            1       ;; lower bound
          55.0     ;; upper bound
            0     ;; number of decimals
            1.0    ;; step size
        ))
    (gnc:register-option
    options
        (gnc:make-complex-boolean-option
           pagename-compare (N_ text-use-custom)
        "cb" (N_ text-help-use-custom) #f
        #t
           (lambda (x)
       (gnc-option-db-set-option-selectable-by-name
        options pagename-compare (N_ text-numbercompareperiods)
        (not x)))
        ))


  ;  (gnc:register-option
  ;  options
 ;       (gnc:make-multichoice-option pagename-compare (N_ text-numbercompareperiods)
 ;       "ca" (N_ "Pick a custom date range or the number of periods to report") '3
 ;       gnc:list-number
 ;       ))
;    (gnc:register-option
;    options
;        (gnc:make-multichoice-callback-option
;    ;    (gnc:make-multichoice-option
;        pagename-compare (N_ text-whichcompareperiod)
;        "cc" (N_ "Select which time period to use")
;        'period
;;  ;      gnc:list-datechoices-avg
;;  ;      ))
 ;       gnc:list-datechoices-avg #f
 ;       (lambda (x)
 ;       (gnc-option-db-set-option-selectable-by-name
 ;        options pagename-compare (N_ text-pick-year)
 ;        (if (equal? x 'customdates) #f #t))
 ;       (gnc-option-db-set-option-selectable-by-name
 ;        options pagename-compare (N_ text-period)
 ;        (if (equal? x 'period) #t #f))
 ;       (gnc-option-db-set-option-selectable-by-name
 ;        options pagename-compare (N_ text-last)
 ;        (if (equal? x 'last) #t #f))
 ;       (gnc-option-db-set-option-selectable-by-name
 ;        options pagename-compare (N_ text-month)
 ;        (if (equal? x 'month) #t #f))
 ;        (gnc-option-db-set-option-selectable-by-name
  ;       options pagename-compare (N_ text-average)
 ;        (if (equal? x 'average) #t #f))
  ;       (gnc-option-db-set-option-selectable-by-name
  ;       options pagename-compare (N_ text-exclude-average)
  ;       (if (equal? x 'average) #t #f))
  ;      ))
  ;  )
     ; add  custom date
    (gnc:options-add-date-interval!
     options pagename-compare
        custom-from-date custom-to-date "cd")

 ;    ;  add pick year for specific period
 ;   (gnc:register-option
 ;   options
 ;       (gnc:make-multichoice-option pagename-compare (N_ text-pick-year)
 ;       "ce" (N_ "Pick the year for report") 'last-yr
 ;       gnc:list-years
 ;       ))
 ;    ;  add pick specific period
  ;  (gnc:register-option
 ;   options
 ;       (gnc:make-multichoice-option pagename-compare (N_ text-period)
 ;       "cf" (N_ "Pick portion of the year for report") 'fullyear
 ;       gnc:list-periods
 ;       ))
 ;   ; add pick specific last XX
 ;   (gnc:register-option
 ;   options
 ;       (gnc:make-multichoice-option pagename-compare (N_ text-last)
 ;       "cg" (N_ "Pick portion of the year for report") 'last_qtr
 ;       gnc:list-lasts
 ;       ))
 ;     ; add pick specific month
 ;   (gnc:register-option
 ;   options
 ;       (gnc:make-multichoice-option pagename-compare (N_ text-month)
 ;       "ch" (N_ "Pick which month for report") '4
  ;      gnc:list-months
 ;       ))

 ;    ;  add pick average period
 
 ;     ; add pick for multiply or divide
 ;   (gnc:register-option
 ;   options
 ;       (gnc:make-multichoice-option pagename-compare (N_ "Scale Results")
 ;       "da" (N_ "Scale the results - multiply or divide by scale factor") '*
 ;       gnc:list-operands
 ;       ))
 ;   ; add where number for multiply or divide can be changed
 ;   (gnc:register-option
 ;   options
 ;       (gnc:make-number-range-option pagename-compare (N_ "Scale Number Option")
  ;      "db" (N_ "Number to multiply or divide by")
  ;          1.0     ;; default
  ;          1       ;; lower bound
   ;       366.0     ;; upper bound
   ;         2.0     ;; number of decimals
   ;         1.0    ;; step size
    ;    ))
;;end of section 3 for gnctimeperiod-utilities


    (gnc:register-option
    options
        (gnc:make-simple-boolean-option
            gnc:pagename-general optname-scale-automatically?
            "e" opthelp-scale-automatically? #f))

    (gnc:register-option
    options
        (gnc:make-number-range-option gnc:pagename-general optname-scale-to
        "f" opthelp-scale-to
          365.0     ;; default
            1       ;; lower bound
          366.0     ;; upper bound
            0       ;; number of decimals
            1.0     ;; step size
        ))

;; end comparison


    ;; accounts to work on
    (gnc:options-add-account-selection!
     options gnc:pagename-accounts
     optname-display-depth optname-show-subaccounts
     optname-accounts "a" 2
     (lambda ()
       (gnc:filter-accountlist-type
        (list ACCT-TYPE-ASSET ACCT-TYPE-LIABILITY ACCT-TYPE-INCOME
                          ACCT-TYPE-EXPENSE)
        (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
     #f)
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-accounts optname-bottom-behavior
      "c" opthelp-bottom-behavior #f))

    ;; columns to display
    (add-option                    ;; delete me
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-base
      "da" opthelp-show-base #t))
    (add-option
     (gnc:make-simple-boolean-option
        gnc:pagename-general optname-show-average
        "dc" opthelp-show-average #t))
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-historic-periods
      "de" opthelp-show-historic-periods #t))
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-totalcol
      "dg" opthelp-show-totalcol #f))
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-actual-cols
      "dj" opthelp-show-actual-cols #t))
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-difference
      "dm" opthelp-show-difference #f))
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-total-diff
      "dp" opthelp-show-total-diff #f))
    (add-option
     (gnc:make-simple-boolean-option
        gnc:pagename-general optname-show-days
        "ds" opthelp-show-days #t))
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-zb-accounts
      "du" opthelp-show-zb-accounts #t))

      ;; Set the base page as default option tab
    (gnc:options-set-default-section options pagename-base)

    options)
  )

(define (gnc:scale-value unscaled-amount currency-frac  scaled? scaling-mul-val scaling-div-val)
    (if (not scaled?)
        unscaled-amount
        (let* (
          ;  (currency-frac (gnc-commodity-get-fraction currency))
            (value
                (if (= 1 scaling-mul-val)
               ; (gnc:gnc-monetary-amount unscaled-monetary)
                unscaled-amount
                (gnc-numeric-mul
                    (gnc:make-gnc-numeric (inexact->exact (* 100 scaling-mul-val)) 100)
                      unscaled-amount  currency-frac GNC-RND-ROUND)))
            (amount ;(gnc:make-gnc-monetary currency
                (if (= 1 scaling-div-val)
                    value
                    (gnc-numeric-div value
                        (gnc:make-gnc-numeric (inexact->exact (* 100 scaling-div-val)) 100)
                            currency-frac GNC-RND-ROUND))));)
            amount)
))
;; Create the html table for the actual verus historic report
;;
;; Parameters
;;   html-table - HTML table to fill in
;;   acct-table - Table of accounts to use
;;   budget - budget to use
;;   params - report parameters
(define (gnc:html-table-add-budget-values!
         html-table acct-table params)
  (let* ((get-val (lambda (alist key)
                    (let ((lst (assoc-ref alist key)))
                      (if lst (car lst) lst))))
         (show-base? (get-val params 'show-base))
         (show-average? (get-val params 'show-average))
         (show-historic-periods? (get-val params 'show-historic-periods))
         (show-actual-cols? (get-val params 'show-actual-cols))
         (show-diff? (get-val params 'show-difference))
         (show-total-diff? (get-val params 'show-total-diff))
         (show-totalcol? (get-val params 'show-totalcol))
         (show-days? (get-val params 'show-days))
         (num-rows (gnc:html-acct-table-num-rows acct-table))
         (rownum 0)
         (numcolumns (gnc:html-table-num-columns html-table))
         (list-base-period (get-val params 'list-base-period))
         (list-avg-period (get-val params 'list-avg-period))
         (list-of-periods (get-val params 'list-of-periods))
         (num-periods (+ 3 (length list-of-periods)))
         (compare-scale-automatically? (get-val params 'compare-scale-automatically?))
         (compare-scale-to-val (get-val params 'compare-scale-to-val))
     ;;(html-table (or html-table (gnc:make-html-table)))
         ;; WARNING: we implicitly depend here on the details of
         ;; gnc:html-table-add-account-balances.  Specifically, we
         ;; assume that it makes twice as many columns as it uses for
          ;; account labels.  For now, that seems to be a valid
         ;; assumption.
         (colnum (quotient numcolumns 2))

     )

  (define (negative-numeric-p x)
    (if (gnc-numeric-p x) (gnc-numeric-negative-p x) #f))
  (define (number-cell-tag x)
    (if (negative-numeric-p x) "number-cell-neg" "number-cell"))
  (define (total-number-cell-tag x)
    (if (negative-numeric-p x) "total-number-cell-neg" "total-number-cell"))

    (define (display-num-scale the-list)
   (_ (number->string compare-scale-to-val))
   )


  ;; Adds a line to tbe budget report.
  ;;
  ;; Parameters:
  ;;   html-table - html table being created
  ;;   rownum - row number
  ;;   colnum - starting column number
  ;;   acct - account being displayed
  ;;   exchange-fn - exchange function (not used)
  (define (gnc:html-table-add-budget-line!
           html-table rownum colnum
           acct column-list list-base-period list-avg-period list-of-periods exchange-fn)
    (let* (
           (period 0)
           (current-col (+ colnum 1))
           (act-total (gnc-numeric-zero))
           (act-total-unset? #t)
           (hist-total (gnc-numeric-zero))
           (comm (xaccAccountGetCommodity acct))
           (reverse-balance? (gnc-reverse-balance acct))
		   (income-acct? (eq? (xaccAccountGetType acct) ACCT-TYPE-INCOME))
           )

      ;; Displays a set of historic column values
      ;;
      ;; Parameters
      ;;   html-table - html table being created
      ;;   rownum - row number
      ;;   base?  - is this for the base period?
	  ;;   total? - is this for the total columns?
      ;;   act-numeric-val - base period value, or #f if column not to be shown
      ;;   hist-numeric-val - historic value, or #f if column not to be shown
      ;;   dif-numeric val - difference value, or #f if column not to be shown
      (define (gnc:html-table-display-historic-columns!
               html-table rownum base? total?
               act-numeric-val hist-numeric-val dif-numeric-val)
           (let* ((act-val #f)(hist-val #f)(dif-val #f)
		          (style-tag (if total? "total-number-cell" "number-cell"))
				  (style-tag-neg (string-append style-tag "-neg"))
		         )

             (if (or show-actual-cols? base?)
                (begin
                    (set! hist-val (gnc:make-gnc-monetary comm hist-numeric-val))
                    (gnc:html-table-set-cell/tag!
                    html-table rownum current-col
                    (if (gnc-numeric-negative-p hist-numeric-val) style-tag-neg style-tag)
                    hist-val)
                    (set! current-col (+ current-col 1))
                )
             )
             (if (not base?)
                (if (and show-diff? (or (not total?) show-total-diff?))              
                    (begin
                        (set! dif-val
                        (if (and (gnc-numeric-zero-p act-numeric-val) (gnc-numeric-zero-p hist-numeric-val))
                            "."
                            (gnc:make-gnc-monetary comm dif-numeric-val)))
                        (gnc:html-table-set-cell/tag!
                            html-table rownum current-col
                            (if (gnc-numeric-negative-p dif-numeric-val) style-tag-neg style-tag)
                            dif-val)
                    (set! current-col (+ current-col 1))
                ))
               (begin                 
                    (gnc:html-table-set-cell/tag!
                      html-table rownum current-col  "centered-label-cell"
                        (_ "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))
                   (set! current-col (+ current-col 1))
                )
             )
           )
        )

      ;; Adds a set of column values to the report for a specific list
      ;; of periods.
      ;;
      ;; Parameters:
      ;;   html-table - html table being created
      ;;   rownum - row number
      ;;   acct - account being displayed
      ;;   period-list - list of periods to use
      (define (gnc:html-table-add-entry-line-columns!
                html-table rownum acct act-numeric-val currency list-current-period)
        (let* (
            (currency-frac (gnc-commodity-get-fraction currency))
          ;; historic amount
         ;  (hist-numeric-collector
        ;            (gnc:account-get-comm-balance-interval
       ;           acct (car list-current-period) (cadr list-current-period) #t))
       ;     (hist-numeric-abscom
       ;         ;  (gnc-numeric-to-double
       ;                 (gnc:gnc-monetary-amount 
       ;         (gnc:sum-collector-commodity 
       ;                                   hist-numeric-collector currency exchange-fn))
           (hist-numeric-abs ;(gnc:get-account-periodlist-actual-value budget acct period-list list-base-period list-of-periods))
                   (gnc:account-get-balance-interval acct (car list-current-period) (cadr list-current-period) #t))

           (scaling-mul-val compare-scale-to-val) ;(caddr list-base-period))
           (scale-hist-div-val (caddr list-current-period)) ; set to numbr of days in  period

           (hist-scaled-numeric-abs (gnc:scale-value hist-numeric-abs currency-frac compare-scale-automatically? scaling-mul-val scale-hist-div-val) )

           (hist-numeric-val
            (if reverse-balance?
              (gnc-numeric-neg hist-scaled-numeric-abs)
              hist-scaled-numeric-abs))

          ;; difference (budget to historic)
          (dif-numeric-val
            (gnc-numeric-sub
              act-numeric-val hist-numeric-val
              GNC-DENOM-AUTO (+ GNC-DENOM-LCD GNC-RND-NEVER)))
          )
         
          (if (not (gnc-numeric-zero-p act-numeric-val))
            (begin
              (set! act-total (gnc-numeric-add act-total act-numeric-val GNC-DENOM-AUTO GNC-RND-ROUND))
              (set! act-total-unset? #f))
          )
          (set! hist-total (gnc-numeric-add hist-total hist-numeric-val GNC-DENOM-AUTO GNC-RND-ROUND))
		  (if income-acct?
            (set! dif-numeric-val
              (gnc-numeric-sub
                hist-numeric-val act-numeric-val
                GNC-DENOM-AUTO (+ GNC-DENOM-LCD GNC-RND-NEVER))))
          (if show-historic-periods?
              (gnc:html-table-display-historic-columns!
                   html-table rownum #f #f
                   act-numeric-val hist-numeric-val dif-numeric-val)
           )
        )
      )

    (let* (
           (currency (if (not (null? acct))
                      (xaccAccountGetCommodity acct)
                      (gnc-default-currency)))
           (currency-frac (gnc-commodity-get-fraction currency))
           (act-numeric-abs ; (gnc:get-account-periodlist-actual-value budget acct period-list list-base-period list-of-periods))
               (gnc:account-get-balance-interval acct  (car list-base-period) (cadr list-base-period) #t))

           (scaling-mul-val compare-scale-to-val) ;(caddr list-base-period))
           (scale-base-div-val (caddr list-base-period)) ; first set to numbr of days in base period ;(length list-of-periods))

           (act-scaled-numeric-abs (gnc:scale-value act-numeric-abs currency-frac  compare-scale-automatically? scaling-mul-val scale-base-div-val) )
           (act-numeric-val
             (if reverse-balance?
               (gnc-numeric-neg act-scaled-numeric-abs)
                  act-scaled-numeric-abs))

           (avg-numeric-abs
             (gnc:account-get-balance-interval acct  (car list-avg-period) (cadr list-avg-period) #t)) ;;(list begindate2 enddate2


           (scale-avg-div-val (caddr list-avg-period)) ; first set to numbr of days in base period ;(length list-of-periods))
           (avg-scaled-numeric-abs (if compare-scale-automatically?
                        (gnc:scale-value avg-numeric-abs currency-frac  compare-scale-automatically? scaling-mul-val scale-avg-div-val)
                        (gnc-numeric-div   avg-numeric-abs
                            (gnc:make-gnc-numeric (inexact->exact (* 100 (length list-of-periods))) 100)
                                 currency-frac GNC-RND-ROUND)))
          (avg-numeric-val
            (if reverse-balance?
              (gnc-numeric-neg avg-scaled-numeric-abs)
              avg-scaled-numeric-abs))

           ;; difference (actual to average)
          (dif-numeric-val
            (if income-acct? ; reverse sign
                (gnc-numeric-sub
                    avg-numeric-val act-numeric-val
                    GNC-DENOM-AUTO (+ GNC-DENOM-LCD GNC-RND-NEVER))
                (gnc-numeric-sub
                    act-numeric-val avg-numeric-val
                    GNC-DENOM-AUTO (+ GNC-DENOM-LCD GNC-RND-NEVER))))
           )
        ;; store base period
        (if show-base?
            (gnc:html-table-display-historic-columns!
                   html-table rownum #t #f
               ;;    act-numeric-val act-numeric-val dif-numeric-val) ;;fix me
                   act-numeric-val act-numeric-val #f)
        )
        ;;store average
        (if show-average?
          (gnc:html-table-display-historic-columns!
                   html-table rownum #f #f
                   act-numeric-val avg-numeric-val dif-numeric-val))

        ;(if show-historic-periods? ;; need to calculate total so put I if statement inside function
            (while (not (null? list-of-periods))
            (begin
                (gnc:html-table-add-entry-line-columns!
                    html-table rownum acct act-numeric-val currency (car list-of-periods))
                (set! list-of-periods (cdr list-of-periods))
            ))
        (if show-totalcol?
            (gnc:html-table-display-historic-columns!
                   html-table rownum #f #t  ;;#t is because this is for total
                   act-total hist-total
				   (if income-acct?
                     (gnc-numeric-sub
                        hist-total act-total
                        GNC-DENOM-AUTO (+ GNC-DENOM-LCD GNC-RND-NEVER))
                     (gnc-numeric-sub
                        act-total hist-total
                        GNC-DENOM-AUTO (+ GNC-DENOM-LCD GNC-RND-NEVER)))
                )
       ))
    )
  )

  ;; Adds header rows to the historic report.  The columns are specified by the
  ;; column-list parameter.
  ;;
  ;; Parameters:
  ;;   html-table - html table being created
  ;;   colnum - starting column number
  ;;   column-list - column info list
  (define (gnc:html-table-add-budget-headers!
           html-table colnum column-list)

    (let* (
           (period 0)
           (current-col (+ colnum 1))
           (col-list column-list)
           (period-list list-of-periods)
           (col-span 0)
           (rownum 0)
           )
       (define (display_heading_act_diff rownum show-diff?)
        (begin
          (if show-actual-cols?
            (begin
              (gnc:html-table-set-cell/tag!
               html-table rownum current-col "centered-label-cell"
               (_ "Act")) ;; Translators: Abbreviation for "actual"
              (set! current-col (+ current-col 1))
            )
          )
          (if show-diff?
            (begin
              (gnc:html-table-set-cell/tag!
               html-table rownum current-col "centered-label-cell"
               (_ "Diff")) ;; Translators: Abbrevation for "Difference"
              (set! current-col (+ current-col 1))
            )
          )
        )
        )
(define (add-heading the-command the-list)
 (the-command (car the-list))
; (display-date-interval-columns the-list)
 )

(define (display-heading the-function rownum row-title)

      (if row-title
          (let ((tc #f ))
            (gnc:html-table-set-cell!
               html-table rownum 0 (_ row-title))
            (set! tc (gnc:html-table-get-cell html-table rownum 0))
            (gnc:html-table-cell-set-colspan! tc 1)
            (gnc:html-table-cell-set-tag! tc "centered-label-cell")))

      (set! current-col (+ colnum 1))
      ;; base period
      (if show-base?
        (let* ( (list-base (car list-base-period))
                (tc #f)
              )
             (let* ((the-text (add-heading the-function list-base-period)))
              (gnc:html-table-set-cell!
               html-table rownum current-col the-text) ;(gnc-print-date date))
               )
          (set! tc (gnc:html-table-get-cell html-table rownum current-col))
          (gnc:html-table-cell-set-colspan! tc 1)
          (gnc:html-table-cell-set-tag! tc "centered-label-cell")
          (set! current-col (+ current-col 1))
          (gnc:html-table-set-cell/tag!
                      html-table rownum current-col  "centered-label-cell"
                     (_ "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))
               (let ( (tc (gnc:html-table-get-cell html-table rownum current-col)))
                    (gnc:html-table-cell-set-colspan! tc 1))
              (set! current-col (+ 1 current-col))         
        ))
      ;; average
        (set! period-list list-of-periods)
        (if show-average?
            (let* ( (list-current-period  period-list)
                    (tc #f)
                    )
            (let* ((the-text (if row-title
                                (add-heading the-function period-list )
                                 "Average")))
            (gnc:html-table-set-cell!
                html-table rownum current-col the-text)
            )
            (set! tc (gnc:html-table-get-cell html-table rownum current-col))
            (gnc:html-table-cell-set-colspan! tc col-span)
            (gnc:html-table-cell-set-tag! tc "centered-label-cell")
            (set! current-col (+ current-col 1))
        ))
      ;; historic periods
      (if show-historic-periods?
        (while (not (= (length period-list) 0))
            (let* (
                    (tc #f)
                  )
            (let* ((the-text (add-heading the-function period-list ))); (display-date-interval-columns (car list-current-period) (cadr list-current-period))))
            ; (let* ((the-text (display-date-interval-columns period-list)))
                (gnc:html-table-set-cell!
                   html-table rownum current-col the-text) ;(gnc-print-date date))
               )
            (set! tc (gnc:html-table-get-cell html-table rownum current-col))
            (gnc:html-table-cell-set-colspan! tc col-span)
            (gnc:html-table-cell-set-tag! tc "centered-label-cell")
            (set! current-col (+ current-col 1))
            (set! period-list (cdr period-list))
            )
        )
      )
 )

	(if show-actual-cols? (set! col-span (+ col-span 1)))
	(if show-diff? (set! col-span (+ col-span 1)))
	(if (eqv? col-span 0) (set! col-span 1))

      ;; prepend 2 empty rows
      (gnc:html-table-prepend-row! html-table '())
      (gnc:html-table-prepend-row! html-table '())

      (if show-days?
        (gnc:html-table-prepend-row! html-table '()))

       (if compare-scale-automatically?
         (gnc:html-table-prepend-row! html-table '()))

      ;; print column headings - time period of each column
      (display-heading display-date-interval-columns rownum #f)
      (if show-totalcol?
          (let ( (tc #f))
               (gnc:html-table-set-cell!
                html-table rownum current-col "Total")
                (set! tc (gnc:html-table-get-cell html-table rownum current-col))
               (gnc:html-table-cell-set-colspan! tc col-span)
               (gnc:html-table-cell-set-tag! tc "centered-label-cell")
               (set! current-col (+ current-col 1))
           )
       )
       (if show-days?
        (begin
            (set! rownum (+ 1 rownum))
            (display-heading display-num-days-in-period rownum text-number-days)
        ))
       (if compare-scale-automatically?
         (begin
            (set! rownum (+ 1 rownum))
            (display-heading display-num-scale rownum text-scaled-to)
         ))

      ;; make the column headers
      (set! rownum (+ 1 rownum))
      (set! current-col (+ colnum 1))
       (if show-base?
            (begin
              (gnc:html-table-set-cell/tag!
               html-table rownum current-col "centered-label-cell"
               (_ "Act")) ;; Translators: Abbreviation for "actual"
              (set! current-col (+ current-col 1))

              (gnc:html-table-set-cell/tag!
                      html-table rownum current-col  "centered-label-cell"
                     (_ "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))
            ;   (let ( (tc (gnc:html-table-get-cell html-table rownum current-col)))
             ;       (gnc:html-table-cell-set-colspan! tc 1))
              (set! current-col (+ 1 current-col))
            )
          )
        (if show-average?
        (display_heading_act_diff rownum show-diff?)
        )
      (set! period-list list-of-periods)
      (if show-historic-periods?
        (while (not (= (length period-list) 0))
            (begin
                (display_heading_act_diff rownum show-diff?)
                (set! period-list (cdr period-list))
        ))
      )
     (if show-totalcol?
       (display_heading_act_diff rownum (and show-diff? show-total-diff?))
        )
    )
  )

  (let* ((num-rows (gnc:html-acct-table-num-rows acct-table))
         (rownum 0)
;;		 (column-info-list '((0 1 2 3 4 5) 6 7 8 (9 10 11)))
		 (column-info-list '())
         (numcolumns (gnc:html-table-num-columns html-table))
	 ;;(html-table (or html-table (gnc:make-html-table)))
         ;; WARNING: we implicitly depend here on the details of
         ;; gnc:html-table-add-account-balances.  Specifically, we
         ;; assume that it makes twice as many columns as it uses for
          ;; account labels.  For now, that seems to be a valid
         ;; assumption.
         (colnum (quotient numcolumns 2))
		 (period 0)
         (work-done 0)
         (work-to-do  num-rows)

	 )

	(while (< period num-periods)
	  (set! column-info-list (append column-info-list (list period)))
	  (set! period (+ 1 period)))

	(if show-totalcol?
	  (set! column-info-list (append column-info-list (list 'total))))

(gnc:debug "column-info-list=" column-info-list)

    ;; call gnc:html-table-add-budget-line! for each account
    (while (< rownum num-rows)
       (let*
         (
           (env
             (append (gnc:html-acct-table-get-row-env acct-table rownum) params))
           (acct (get-val env 'account))
           (exchange-fn (get-val env 'exchange-fn))
         )
         (set! work-done (+ 1 work-done))
         (gnc:report-percent-done (* 90 (/ work-done work-to-do)))
         (gnc:html-table-add-budget-line!
            html-table rownum colnum
            acct column-info-list (car list-base-period)
                        (car list-avg-period)
                        list-of-periods exchange-fn)
         (set! rownum (+ rownum 1)) ;; increment rownum
       )
    ) ;; end of while

    ;; column headers
    (gnc:html-table-add-budget-headers! html-table colnum column-info-list)
    )

  )
) ;; end of define

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; budget-renderer
;; set up the document and add the table
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (budget-renderer report-obj)
  (define (get-option pagename optname)
    (gnc:option-value
     (gnc:lookup-option
      (gnc:report-options report-obj) pagename optname)))

  (gnc:report-starting reportname)

  ;; get all option's values
  (let* (
       ;; step 4 of 4 needed for gnctimeperiod-utilities
;; the let needs to be a let*
;; may need to change op-value to get-option
        (whichperiod-val      (get-option the_tab text-whichperiod))
        (cust-start-date-tp    (gnc:timepair-start-day-time
                                    (gnc:date-option-absolute-time
                                    (get-option the_tab
                                        custom-from-date))))

        (cust-end-date-tp    (gnc:timepair-end-day-time
                                    (gnc:date-option-absolute-time
                                    (get-option the_tab
                                        custom-to-date))))
        (datelist   (gnc:get-dates  
                (list
                 (list 'whichperiod-val
                       (get-option the_tab text-whichperiod))
                 (list 'year-val
                       (get-option the_tab text-pick-year))
                 (list 'period-val
                       (get-option the_tab text-period))
                 (list 'last-val
                       (get-option the_tab text-last))
                 (list 'month-val
                       (get-option the_tab text-month))
                 )))
        ;;
        ;; replace following two names with your names and comment out your old definitions
        (from-date-tp    (if (equal? whichperiod-val 'customdates )
                            cust-start-date-tp
                            (car datelist)))


        (to-date-tp      (if (equal? whichperiod-val 'customdates )
                            cust-end-date-tp
                            (cadr datelist)))

        (list-base-period (gnc:getdatedelta   ;use to get number of days in base
                            (list from-date-tp to-date-tp 'one)))

;; end of section 4 needed for using gnctimeperiod-utilities
;        (from-date-tp (gnc:timepair-start-day-time
;                        (gnc:date-option-absolute-time
;                         (get-option pagename-base
;                                     optname-from-date))))
;         (to-date-tp (gnc:timepair-end-day-time
;                      (gnc:date-option-absolute-time
;                       (get-option pagename-base
;                                   optname-to-date))))
; for comparison period

        (deltas-val (get-option pagename-compare text-compare-divide))
    ;    (whichperiod-val2 (get-option pagename-compare text-whichcompareperiod))

        (cust-start-date2-tp    (gnc:timepair-start-day-time
                                    (gnc:date-option-absolute-time
                                    (get-option pagename-compare
                                        custom-from-date))))

        (cust-end-date2-tp    (gnc:timepair-end-day-time
                                    (gnc:date-option-absolute-time
                                    (get-option pagename-compare
                                        custom-to-date))))
      ;  (year-val2     (get-option  pagename-compare text-pick-year))
      ;  (period-val2   (get-option  pagename-compare text-period))
      ;  (last-val2   (get-option pagename-compare text-last))
      ;  (month-val2   (get-option pagename-compare text-month))
     ;   (datelist2   (gnc:getdates
     ;       (list whichperiod-val2 year-val2 period-val2 last-val2 month-val2)) )
     ;   (averagelist2-old   (gnc:getdates-average-old average-val2 (get-option pagename-compare text-exclude-average)))
        (comparelist2   (gnc:getdates-compare (get-option pagename-compare text-compare-divide)
                              (inexact->exact (get-option pagename-compare text-numbercompareperiods))
                                              (get-option pagename-compare text-exclude-average)))
        ;;
       (begindate2    (if (get-option pagename-compare text-use-custom )
                            cust-start-date2-tp
                            (car comparelist2)
                          ))

        (enddate2      (if (get-option pagename-compare text-use-custom )
                            cust-end-date2-tp
                            (cadr comparelist2)
                          ))
        (list-of-periods (gnc:getdatedelta
                            (list begindate2 enddate2 deltas-val)))

       (compare-scale-automatically? (get-option gnc:pagename-general optname-scale-automatically?))
       (compare-scale-to-val         (get-option gnc:pagename-general optname-scale-to))




;; end for comparison period

;;
         (display-depth (get-option gnc:pagename-accounts
                                    optname-display-depth))
         (show-subaccts? (get-option gnc:pagename-accounts
                                     optname-show-subaccounts))
         (accounts (get-option gnc:pagename-accounts
                               optname-accounts))
         (bottom-behavior (get-option gnc:pagename-accounts optname-bottom-behavior))
	 (show-zb-accts? (get-option gnc:pagename-general
                                     optname-show-zb-accounts))
         (rownum 0) ;; ???
         
         ;;(report-currency (get-option pagename-base
         ;;                             optname-report-currency))
    ;     (show-full-names? (get-option pagename-base
    ;                                   optname-show-full-names))
         (doc (gnc:make-html-document))
         ;;(table (gnc:make-html-table))
         ;;(txt (gnc:make-html-text))
         )

    ;; end of defines

    ;; add subaccounts if requested
    (if show-subaccts?
        (let ((sub-accounts (gnc:acccounts-get-all-subaccounts accounts)))
          (for-each
            (lambda (sub-account)
              (if (not (account-in-list? sub-account accounts))
                  (set! accounts (append accounts sub-accounts))))
            sub-accounts)))

    (cond
      ((null? accounts)
        ;; No accounts selected.
        (gnc:html-document-add-object!
         doc
         (gnc:html-make-no-account-warning
      reportname (gnc:report-id report-obj))))

      (else (begin
        (let* ((tree-depth (if (equal? display-depth 'all)
                               (accounts-get-children-depth accounts)
                               display-depth))
               ;;(account-disp-list '())

               (env (list
			   (list 'start-date from-date-tp)
			   (list 'end-date to-date-tp)
               (list 'display-tree-depth tree-depth)
               (list 'depth-limit-behavior
                      (if bottom-behavior 'flatten 'summarize))
			   (list 'zero-balance-mode
				(if show-zb-accts? 'show-leaf-acct 'omit-leaf-acct))
                          ))
               (acct-table #f)
               (html-table (gnc:make-html-table))
               (params '())
               (paramsHistoric
                (list
                 (list 'show-base
                       (get-option gnc:pagename-general optname-show-base))
                 (list 'show-average
                       (get-option gnc:pagename-general optname-show-average))
                 (list 'show-historic-periods
                       (get-option gnc:pagename-general optname-show-historic-periods))
                 (list 'show-actual-cols
                       (get-option gnc:pagename-general optname-show-actual-cols))
                 (list 'show-difference
                       (get-option gnc:pagename-general optname-show-difference))
                 (list 'show-total-diff
                        (get-option gnc:pagename-general optname-show-total-diff))
                 (list 'show-totalcol
                       (get-option gnc:pagename-general optname-show-totalcol))
                 (list 'show-days
                       (get-option gnc:pagename-general optname-show-days))
                 (list 'list-base-period list-base-period)
                 (list 'list-avg-period (gnc:getdatedelta
                            (list begindate2 enddate2 'one)))
                 (list 'list-of-periods list-of-periods )
                 (list 'compare-scale-automatically? compare-scale-automatically?)
                 (list 'compare-scale-to-val compare-scale-to-val)
                )
               )
               (report-name (get-option gnc:pagename-general
                                        gnc:optname-reportname))
               )
          (gnc:html-document-set-title!
           doc (sprintf #f (_ "%s: %s")
                        report-name (if compare-scale-automatically? "Note: Numbers have been scaled" "")))

          (set! accounts (sort accounts account-full-name<?))

          (set! acct-table
                (gnc:make-html-acct-table/env/accts env accounts))

          ;; We do this in two steps: First the account names...  the
          ;; add-account-balances will historicly compute and add a
          ;; bunch of current account balances, too, but we'll
          ;; overwrite them.
          (set! html-table (gnc:html-table-add-account-balances
                            #f acct-table params))

          ;; ... then the budget values
        (gnc:html-table-add-budget-values!
           html-table acct-table paramsHistoric)

          ;; hmmm... I expected that add-budget-values would have to
          ;; clear out any unused columns to the right, out to the
          ;; table width, since the add-account-balance had put stuff
          ;; there, but it doesn't seem to matter.

          (gnc:html-document-add-object! doc html-table))))
      ) ;; end cond

    (gnc:report-finished)
    doc))

(gnc:define-report
 'version 1
 'name reportname
 'report-guid "810ed4b25efdbd6ea43bbd3dddb32b11"
 'menu-path (list gnc:menuname-budget)
 'options-generator budget-report-options-generator
 'renderer budget-renderer)

