;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cash-flow.scm: cash flow report
;;
;; By Herbert Thoma <herbie@hthoma.de>
;; Comparison and scaling added by D.B. Doughty <dbdoughty at gmail.com>
;;
;; based on balance-sheet.scm by:
;; Robert Merkel <rgmerk@mira.net>
;; and pnl.scm by:
;; Christian Stimming <stimming@tu-harburg.de>
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

(define-module (gnucash report standard-reports cash-flow-v2))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))
(use-modules (gnucash engine))
(use-modules (gnucash printf))

;; following neded for gnc-locale-to-utf8
(use-modules (gnucash core-utils))

;;following line is step 1 of 4 needed to add gnctimeperiod-utilities
(use-modules (gnucash gnctimeperiod-utilities))

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/gnome-utils" 0) ;for gnc-build-url

(export cash-flow-calc-money-in-out)

(define-macro (addto! alist element)
  `(set! ,alist (cons ,element ,alist)))

(define reportname (N_ "Compare Cash Flows"))

;; define all option's names so that they are properly defined
;; in *one* place.

(define optname-display-depth (N_ "Account Display Depth"))
(define optname-show-subaccounts (N_ "Always show sub-accounts"))
(define optname-accounts (N_ "Account"))

(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))
(define optname-show-rates (N_ "Show Exchange Rates"))
(define optname-show-full-names (N_ "Show Full Account Names"))
(define optname-include-trading-accounts (N_ "Include Trading Accounts in report"))

;; add for scaling
(define scaling-text "Note: amount and balance have been scaled.")

;; added for flagging imbalances when account name is primary key
(define optname-show-imbalance
  (N_ "Note any imbalance"))
(define opthelp-show-imbalance
  (N_ "Make a footnote if there is an imbalance when account name or account code is selected as the primary key and find not used"))

 (define optname-show-delta (N_ "Show delta column"))
 (define opthelp-show-delta (N_ "show delta if just two periods"))

(define optname-show-days (N_ "Show number of days"))
(define opthelp-show-days (N_ "Display the number of days in each period"))
(define text-note-account " Note - account   ")
(define text-changed-in-value " in the base period  changed in value by ")
(define text-dash (_ "-")) ; printed to indicate imbalance was checked for

;; this is step 2 of 4
;; needed for gnctimeperiod-utilities
;; define all option's names so that they are properly defined
;; in *one* place.
;; can change following text for local language

;; following value may need to be changed
(define the_tab gnc:pagename-general)

(define text-whichperiod "Select Period")
(define text-customdates "Custom Dates")
(define custom-from-date (N_ "Custom_Start Date"))
(define custom-to-date (N_ "Custom_End Date"))
(define text-pick-year "Year for Specified Pick")
(define text-period "Specified Period")
(define text-last "Specified Last")
(define text-month "Specified Month")

(define text-average "More periods")
(define text-exclude-average "Do not include last period in creating periods")
(define text-use-custom "Use custom date instead of choosing number of periods")

; for compare
(define pagename-compare (N_ "Compare"))
(define optname-compare? (N_ "Compare periods?"))
(define optname-no-base?  (N_ "Do not show base period, only comparison periods"))
(define optname-scale-automatically? (N_ "Ignore other scaling and"))
(define optname-scale-to (N_ "scale to"))
(define text-whichcompareperiod "Select Comparison Period")
(define text-numbercompareperiods "Select Number of Comparison Periods")
(define text-compare-divide   "divide period into")

(define optname-show-total?  (N_ "Show total of compare periods"))
(define optname-show-average?  (N_ "Show average of compare periods"))

(define gnc:list-datechoices
   (list (list->vector
             (list 'customdates
                   (N_ text-customdates)
                   (N_ "use selected dates and ignore specific choices")))
            (list->vector
             (list 'period
                   (N_ text-period)
                   (N_ "which period to use")))
            (list->vector
             (list 'last
                   (N_ text-last)
                   (N_ "when to use")))
            (list->vector
             (list 'month
                   (N_ text-month)
                   (N_ "use specific month")))
           )
)
(define gnc:list-datechoices-avg
   (list (list->vector
             (list 'customdates
                   (N_ text-customdates)
                   (N_ "use selected dates and ignore specific choices")))
            (list->vector
             (list 'period
                   (N_ text-period)
                   (N_ "which period to use")))
            (list->vector
             (list 'last
                   (N_ text-last)
                   (N_ "when to use")))
            (list->vector
             (list 'month
                   (N_ text-month)
                   (N_ "use specific month")))
            (list->vector
             (list 'average
                   (N_ text-average)
                   (N_ "Use time period from average list of choices")))
           )
)

    (define scale-num
        (list
        (cons '*  (vector gnc-numeric-mul (N_ "multiplied by ")))
        (cons '/  (vector gnc-numeric-div (N_ "divided by ")))
        ))

;; end of section 2 needed for gnctimeperiod-utilities

;;for scaling
(define scale-op-val '*)
(define scale-num-val 1)
(define scaled? #f)
(define compare-scale-automatically? #f)
(define compare-scale-to-val 1)

;;for scaling comparison period
(define scale-op-val2 '*)
(define scale-num-val2 1)


(define money-in-accounts '() )
(define money-in-alist '() )
(define money-in-collector '() )
(define money-out-accounts '() )
(define money-out-alist '() )
(define money-out-collector '() )
(define row-number-in 4)  ; row 0 heading         row 1 number of days in the period row 2 total row 3 diff
(define row-number-out 4)
(define col-num 3)
(define money-in-list '() )
(define money-out-list '() )

(define money-in-pointer-hash (make-hash-table))
(define money-out-pointer-hash (make-hash-table))
(define money-in-array (make-array #f 2 3)) ; col 0 = currency type ; col 1 and greater - period
(define money-out-array (make-array #f 2 3)) ; col 0 = currency type ; col 1 and greater - period

;; for imbalance and orphan accounts
(define (gnc:accounts-imbalance-or-orphan start-date-tp end-date-tp)
;; returns list of accounts starting with the letters "imbalanc"  which do not have a zero (0)
;; change in the balance between the start date and the end date
    (let* (
        (all-accounts (gnc-account-get-descendants-sorted
                            (gnc-get-current-root-account)))
        )
        (filter (lambda (acct)
                (if  (or (string-prefix-ci? "imbalance" (gnc-account-get-full-name acct))
                    (string-prefix-ci? "orphan" (gnc-account-get-full-name acct)))
                ;; check if the change in balance from the 'from' date to the 'to' date.
                ;; is 0
                (if (equal? (gnc-numeric-zero) (gnc:account-get-balance-interval acct start-date-tp end-date-tp #f))
                    #f
                    #t)
                #f))
          all-accounts)
))
;; end for imbalance


(define (display-date-interval-columns begin end)
  (let* (
        (period-start (gnc:timepair->date begin))
        (start-day (tm:mday period-start))
        (end-start-1-month (gnc:increment-month-less-1-day period-start 1))
        (end-start-1-year (gnc:increment-month-less-1-day period-start 12))
        (period-end (gnc:timepair->date end))
        (fiscal-start (gnc:timepair->date (gnc:secs->timepair (gnc-accounting-period-fiscal-start))))
        (fiscal-day  (tm:mday fiscal-start))
        (display-date (sprintf #f (_ " %s To %s") (gnc-print-date begin) (gnc-print-date end) ))
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

 (define (display-num-days days)
  (if (number? days)
   (_ (number->string days))
    (_ " ")))

(define (make-heading-period table row-style column)
  (let ((heading-list '())
        (col 2))
        (addto!  heading-list
            (gnc:make-html-text (gnc:html-markup-b
                (_ "  "))))
        (while (< col 2)
            (addto!  heading-list
                (_ " "))
            (set! col (+ col 1)))

    (let* ( (col 3))
        (while (< col column)
            (addto!  heading-list
                (gnc:make-html-table-header-cell/markup
                  "column-heading-center"
                (_ (array-ref money-in-array 0 col))))
            (set! col (+ col 1))))
    (gnc:html-table-append-row/markup!
        table row-style
    (reverse heading-list)))
)

(define (make-heading-days table row-style column)
  (let ((heading-list '())
        (col 2))
        (addto!  heading-list
            (gnc:make-html-text (gnc:html-markup-b
                (_ "  Number of Days:"))))
        (while (< col 2)
            (addto!  heading-list
                (_ " "))
            (set! col (+ col 1)))

    (let ( (col 3))
        (while (< col column)
            (addto!  heading-list
                (gnc:make-html-table-header-cell/markup
                  "column-heading-center"
                (display-num-days
                 (array-ref money-in-array
                            1 col
                             ))))
            (set! col (+ col 1))))
    (gnc:html-table-append-row/markup!
        table row-style
    (reverse heading-list)))
)

(define (make-heading-scale table row-style column compare? no-base?)
  (let ((heading-list '())
        (col 2))
        (addto!  heading-list
            (gnc:make-html-text (gnc:html-markup-b
                (if compare-scale-automatically?
                    (_ "  Scaled to:")
                    (_ "  Scaled by:")))))
        (while (< col 2)
            (addto!  heading-list
                (_ " "))
            (set! col (+ col 1)))

        (let ( (colnum 3))
        (if (not (and  compare? no-base?))
            (begin
        (addto!  heading-list
            ;(gnc:make-html-text (gnc:html-markup-b
            (gnc:make-html-table-cell/markup
                       "column-heading-center"
                (if compare-scale-automatically?
                    (_ (number->string compare-scale-to-val))
                    (_ (string-append (vector-ref (cdr (assq scale-op-val  scale-num)) 1) (number->string scale-num-val))))))
            (set! colnum (+ colnum 1)))
        )

        (while (< colnum column)
            (addto!  heading-list
            (gnc:make-html-table-header-cell/markup
                  "column-heading-center"
              (if (not (number? (array-ref money-in-array 1 colnum)));if no number for days it is delta column
                " "
                (if compare-scale-automatically?
                    (_ (number->string compare-scale-to-val))
                    (_ (string-append (vector-ref (cdr (assq scale-op-val2  scale-num)) 1) (number->string scale-num-val2)))))))
            (set! colnum (+ colnum 1))))
    (gnc:html-table-append-row/markup!
        table row-style
    (reverse heading-list)))
)

(define (gnc:scale-sum-collector-commodity commodity report-currency exchange-fn scaled? scaling-mul-val scaling-div-val)
    (if (not scaled?)
        (gnc:sum-collector-commodity commodity report-currency exchange-fn)
        (let* (
            (amount-monetary (gnc:sum-collector-commodity commodity report-currency exchange-fn))
            (currency-frac (gnc-commodity-get-fraction report-currency))
            (value
                (if (= 1 scaling-mul-val)
                (gnc:gnc-monetary-amount amount-monetary)
                (gnc-numeric-mul
                    (gnc:make-gnc-numeric (inexact->exact (* 100 scaling-mul-val)) 100)
                     (gnc:gnc-monetary-amount amount-monetary)  currency-frac GNC-RND-ROUND)))
            (amount (gnc:make-gnc-monetary report-currency
                (if (= 1 scaling-div-val)
                    value
                    (gnc-numeric-div value
                        (gnc:make-gnc-numeric (inexact->exact (* 100 scaling-div-val)) 100)
                            currency-frac GNC-RND-ROUND)))))
            amount)
))


;; options generator
(define (cash-flow-options-generator)
  (let ((options (gnc:new-options)))

 ;   ;; date interval
 ;   (gnc:options-add-date-interval!
 ;    options gnc:pagename-general
 ;    optname-from-date optname-to-date "a")

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
     ; changed add-option to gnc:register-trep-option
    (gnc:register-option
    options
        (gnc:make-multichoice-callback-option
    ;    (gnc:make-multichoice-option
        the_tab (N_ text-whichperiod)
        "ca" (N_ "Select which time period to use")
        'customdates
;;        gnc:list-datechoices
;;        ))
        gnc:list-datechoices #f
        (lambda (x)
        (gnc-option-db-set-option-selectable-by-name
         options the_tab (N_ text-pick-year)
         (if (equal? x 'customdates) #f #t))
        (gnc-option-db-set-option-selectable-by-name
         options the_tab (N_ text-period)
         (if (equal? x 'period) #t #f))
        (gnc-option-db-set-option-selectable-by-name
         options the_tab (N_ text-last)
         (if (equal? x 'last) #t #f))
        (gnc-option-db-set-option-selectable-by-name
         options the_tab (N_ text-month)
         (if (equal? x 'month) #t #f))
        ))
    )
     ; add  custom date
    (gnc:options-add-date-interval!
     options the_tab
        custom-from-date custom-to-date "cb")

     ;  add pick year for specific period
    (gnc:register-option
    options
        (gnc:make-multichoice-option the_tab (N_ text-pick-year)
        "ce" (N_ "Pick the year for report") 'this-yr
        gnc:list-years
        ))
     ;  add pick specific period
    (gnc:register-option
    options
        (gnc:make-multichoice-option the_tab (N_ text-period)
        "cf" (N_ "Pick portion of the year for report") 'fullyear
        gnc:list-periods
        ))
    ; add pick specific last XX
    (gnc:register-option
    options
        (gnc:make-multichoice-option the_tab (N_ text-last)
        "cg" (N_ "Pick portion of the year for report") 'last_qtr
        gnc:list-lasts
        ))
      ; add pick specific month
    (gnc:register-option
    options
        (gnc:make-multichoice-option the_tab (N_ text-month)
        "ch" (N_ "Pick which month for report") '4
        gnc:list-months
        ))

      ; add pick for multiply or divide
    (gnc:register-option
     options
        (gnc:make-multichoice-option the_tab (N_ "Scale Results")
        "ci" (N_ "Scale the results - multiply or divide by scale factor") '*
        gnc:list-operands
        ))
    ; add where number for multiply or divide can be changed
    (gnc:register-option
    options
        (gnc:make-number-range-option the_tab (N_ "Scale Number Option")
        "cj" (N_ "Number to multiply or divide by")
            1.0     ;; default
            1       ;; lower bound
          366.0     ;; upper bound
            2.0     ;; number of decimals
            1.0    ;; step size
        ))
;;end of section 3 for gnctimeperiod-utilities
;;

    ;; all about currencies
    (gnc:options-add-currency!
     options gnc:pagename-general
     optname-report-currency "d1")

    (gnc:options-add-price-source!
     options gnc:pagename-general
     optname-price-source "d2" 'pricedb-nearest)

     (gnc:register-option
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-imbalance
      "d3" opthelp-show-imbalance #t))

    (gnc:register-option
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-days
      "d3" opthelp-show-days #f))

    (gnc:register-option
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-delta
      "d4" opthelp-show-delta #t))

    (gnc:register-option
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-rates
      "d5" (N_ "Show the exchange rates used.") #f))


    (gnc:register-option
     options
     (gnc:make-simple-boolean-option
      gnc:pagename-general optname-show-full-names
      "e" (N_ "Show full account names (including parent accounts).") #t))

      ;comparison period
    (gnc:register-option
    options
      (gnc:make-complex-boolean-option
        pagename-compare optname-compare?
        "a1" (N_ "Compare multiple periods") #f
        #f
            (lambda (x)
        (gnc-option-db-set-option-selectable-by-name
         options pagename-compare optname-no-base?
         x))
         ))


    (gnc:register-option
    options
    (gnc:make-simple-boolean-option
        pagename-compare optname-no-base?
        "a2" (N_ "If compare periods is unchecked and this box checked only comparison period will be displayed") #f))

    (gnc:register-option
    options
    (gnc:make-simple-boolean-option
        pagename-compare optname-show-average?
        "a3" (N_ "Show average for comparison period along with delta from average period") #t))

    (gnc:register-option
    options
    (gnc:make-simple-boolean-option
        pagename-compare optname-show-total?
        "a4" (N_ "Show total for comparison period ") #t))


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
        "ca" (N_ "Number of periods - Selecting 3 will give 3 months if months was picked for divide periods")
            3.0     ;; default
            1       ;; lower bound
          52.0     ;; upper bound
            0     ;; number of decimals
            1.0    ;; step size
        ))
     (gnc:register-option
    options
        (gnc:make-simple-boolean-option
           pagename-compare (N_ text-use-custom)
        "cb" (N_ "Use the custom date in following section to determine comparison period instead of 'select number of comparison periods'") #f
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
 ;   (gnc:register-option
 ;   options
 ;       (gnc:make-multichoice-option pagename-compare (N_ text-average)
  ;      "cj" (N_ "Pick portion of the year for report") '3months
  ;      gnc:list-average
  ;      ))
      ; add pick for multiply or divide
    (gnc:register-option
    options
        (gnc:make-multichoice-option pagename-compare (N_ "Scale Results")
        "da" (N_ "Scale the results - multiply or divide by scale factor") '*
        gnc:list-operands
        ))
    ; add where number for multiply or divide can be changed
    (gnc:register-option
    options
        (gnc:make-number-range-option pagename-compare (N_ "Scale Number Option")
        "db" (N_ "Number to multiply or divide by")
            1.0     ;; default
            1       ;; lower bound
          366.0     ;; upper bound
            2.0     ;; number of decimals
            1.0    ;; step size
        ))
;;end of section 3 for gnctimeperiod-utilities


    (gnc:register-option
    options
        (gnc:make-simple-boolean-option
            pagename-compare optname-scale-automatically?
            "e" (N_ "Scale all periods to number of days given below") #f))

    (gnc:register-option
    options
        (gnc:make-number-range-option pagename-compare optname-scale-to
        "f" (N_ "Scale all periods including the base to this number of days")
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
        (list ACCT-TYPE-BANK ACCT-TYPE-CASH ACCT-TYPE-ASSET
              ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL)
        (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
     #f)

     ;; Trading accounts?
     (gnc:register-option
      options
      (gnc:make-simple-boolean-option
       gnc:pagename-accounts optname-include-trading-accounts
       "b" (N_ "Include transfers to and from Trading Accounts in the report.")  #f))

    ;; Set the general page as default option tab
    (gnc:options-set-default-section options gnc:pagename-general)

    options))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cash-flow-renderer
;; set up the document and add the table
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cash-flow-renderer report-obj)
  (define (get-option pagename optname)
    (gnc:option-value
     (gnc:lookup-option
      (gnc:report-options report-obj) pagename optname)))

  (define acct-full-nam<? (lambda (a b)
  (string<? (gnc-account-get-full-name (car a)) (gnc-account-get-full-name (car b)))))

  (gnc:report-starting reportname)

  ;; get all option's values
  (let* (
      ;; step 4 of 4 needed for gnctimeperiod-utilities
;; the let needs to be a let*
;; may need to change op-value to get-option
          (whichperiod-val (get-option the_tab text-whichperiod))

        (cust-start-date-tp    (gnc:timepair-start-day-time
                                    (gnc:date-option-absolute-time
                                    (get-option the_tab
                                        custom-from-date))))

        (cust-end-date-tp    (gnc:timepair-end-day-time
                                    (gnc:date-option-absolute-time
                                    (get-option the_tab
                                        custom-to-date))))
        (year-val     (get-option  the_tab text-pick-year))
        (period-val   (get-option  the_tab text-period))
        (last-val   (get-option the_tab text-last))
        (month-val   (get-option the_tab text-month))
     ;   (datelist   (gnc:getdates
     ;       (list whichperiod-val year-val period-val last-val month-val)) )
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
;;
        (display-depth (get-option gnc:pagename-accounts
                                    optname-display-depth))
         (show-subaccts? (get-option gnc:pagename-accounts
                                     optname-show-subaccounts))
         (accounts (get-option gnc:pagename-accounts
                               optname-accounts))
         (include-trading-accounts (get-option gnc:pagename-accounts
                               optname-include-trading-accounts))
         (row-num 0)
     (work-done 0)
     (work-to-do 0)
         (report-currency (get-option gnc:pagename-general
                                      optname-report-currency))
         (price-source (get-option gnc:pagename-general
                                   optname-price-source))
         (show-rates? (get-option gnc:pagename-general
                                  optname-show-rates))
         (show-full-names? (get-option gnc:pagename-general
                                       optname-show-full-names))
 ;        (from-date-tp (gnc:timepair-start-day-time
;                        (gnc:date-option-absolute-time
;                         (get-option gnc:pagename-general
;                                     optname-from-date))))
;         (to-date-tp (gnc:timepair-end-day-time
;                      (gnc:date-option-absolute-time
;                       (get-option gnc:pagename-general
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
     ;;   (datelist2   (gnc:getdates
     ;;       (list whichperiod-val2 year-val2 period-val2 last-val2 month-val2)) )
     ;    (datelist2   (gnc:get-dates
     ;           (list
     ;            (list 'whichperiod-val2
     ;                  (get-option pagename-compare text-whichcompareperiod))
     ;            (list 'year-val2
     ;                  (get-option  pagename-compare text-pick-year))
     ;            (list 'period-val2
      ;                 (get-option  pagename-compare text-period))
      ;           (list 'last-val2
      ;                 (get-option pagename-compare text-last))
      ;           (list 'month-val2
      ;                 (get-option pagename-compare text-month))
      ;           )))
     ;   (average-val2   (get-option pagename-compare text-average))
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



;; end for comparison period
         ;; calculate the exchange rates  NOTE MAY WANT TO MOVE into function which calculates for each period
         (exchange-fn (gnc:case-exchange-fn
                       price-source report-currency to-date-tp))

         (doc (gnc:make-html-document))
         (table (gnc:make-html-table))
         (txt (gnc:make-html-text)))

;;for scaling
        (set! scale-op-val      (get-option the_tab "Scale Results"))
        (set! scale-num-val  (get-option the_tab "Scale Number Option"))


;; for compare period scaling
        (set! scale-op-val2      (get-option pagename-compare "Scale Results"))
        (set! scale-num-val2  (get-option pagename-compare "Scale Number Option"))

        (set! compare-scale-automatically? (get-option pagename-compare optname-scale-automatically?))
        (set! compare-scale-to-val         (get-option pagename-compare optname-scale-to))
        (set! scaled? (if (or (not (= 1 scale-num-val)) (not (= 1 scale-num-val2)) compare-scale-automatically?)
                        #t
                        #f))

         (set! money-in-pointer-hash (make-hash-table))
         (set! money-out-pointer-hash (make-hash-table))
         (set! money-in-array (make-array (gnc:make-gnc-monetary report-currency (gnc-numeric-zero))
                (+ (length accounts) 100) (+ (length list-of-periods) 25))); length list-of-periods
         (set! money-out-array (make-array (gnc:make-gnc-monetary report-currency (gnc-numeric-zero))
                (+ (length accounts) 100) (+ (length list-of-periods) 25))); length list-of-periods

    (gnc:html-document-set-title!
     doc (string-append
      (get-option gnc:pagename-general gnc:optname-reportname)
      (if (not (or (get-option pagename-compare optname-compare?) (get-option pagename-compare optname-show-average?)))
        (begin
            (string-append
                " - "
                (sprintf #f (_ "%s to %s")
                    (gnc-print-date from-date-tp) (gnc-print-date to-date-tp))))
        ""; change back to ""  dbd fix
        )
        (if scaled?
        (begin
            (string-append
                " \n"
                (sprintf #f (_ " Note the values shown have been scaled")
                    )))
        ""; change back to ""  dbd fix
        )
    ))


    ;; add subaccounts if requested
    (if show-subaccts?
        (let ((sub-accounts (gnc:acccounts-get-all-subaccounts accounts)))
          (for-each
            (lambda (sub-account)
              (if (not (account-in-list? sub-account accounts))
                  (set! accounts (append accounts sub-accounts))))
            sub-accounts)))


    (if (not (null? accounts))

        (let* ((tree-depth (if (equal? display-depth 'all)
                               (accounts-get-children-depth accounts)
                               display-depth))

               (money-diff-collector (gnc:make-commodity-collector))
           (account-disp-list '())

           (time-exchange-fn #f)
           (commodity-list (gnc:accounts-get-commodities
                accounts
                report-currency))
           ;; Get an exchange function that will convert each transaction using the
           ;; nearest available exchange rate if that is what is specified
           (time-exchange-fn (gnc:case-exchange-time-fn
                  price-source report-currency
                  commodity-list to-date-tp
                  0 0)))

      ;; Helper function to convert currencies
      (define (to-report-currency currency amount date)
        (gnc:gnc-monetary-amount
         (time-exchange-fn (gnc:make-gnc-monetary currency amount)
                   report-currency
                   date)))
(define (get-delta the-array the-list currency)
    (begin
    (for-each
        (lambda (pair)
    (let (
        (row (cadr pair)))
    (array-set! the-array
            (gnc:make-gnc-monetary report-currency
            (gnc-numeric-sub
            (gnc:gnc-monetary-amount (array-ref the-array row 4))
            (gnc:gnc-monetary-amount (array-ref the-array row 3)) GNC-DENOM-AUTO GNC-RND-ROUND))
                row 5)))
                the-list)
    (let( (count 2))
    (while (< count 4)
    (array-set! the-array ; handle totals and difference
            (gnc:make-gnc-monetary report-currency
            (gnc-numeric-sub
            (gnc:gnc-monetary-amount (array-ref the-array count 4))
            (gnc:gnc-monetary-amount (array-ref the-array count 3)) GNC-DENOM-AUTO GNC-RND-ROUND))
                count 5)
    (set! count (+ count 1))
    ))

    )
)

; Here is the function that gets all of the amounts for a period
    (define (get-results  from-date-tp to-date-tp scaling-mul-val scaling-div-val)
          (let ((result (cash-flow-calc-money-in-out
             (list (cons 'accounts accounts)
                   (cons 'to-date-tp to-date-tp)
                   (cons 'from-date-tp from-date-tp)
                   (cons 'report-currency report-currency)
                   (cons 'include-trading-accounts include-trading-accounts)
                   (cons 'to-report-currency to-report-currency)))))
        (set! money-in-accounts (cdr (assq 'money-in-accounts result)))
        (set! money-in-alist (cdr (assq 'money-in-alist result)))
        (set! money-in-collector (cdr (assq 'money-in-collector result)))
        (set! money-out-accounts (cdr (assq 'money-out-accounts result)))
        (set! money-out-alist (cdr (assq 'money-out-alist result)))
        (set! money-out-collector (cdr (assq 'money-out-collector result)))
        )
        (set! money-in-list '() )
        (set! money-out-list '() )

        (set! money-diff-collector (gnc:make-commodity-collector))
        (money-diff-collector 'merge money-in-collector #f)
        (money-diff-collector 'minusmerge money-out-collector #f)

         (array-set! money-in-array (display-date-interval-columns from-date-tp to-date-tp) 0 col-num);store time period title
         (array-set! money-in-array
                (gnc:scale-sum-collector-commodity money-in-collector report-currency exchange-fn
                        scaled? scaling-mul-val scaling-div-val)
                        2 col-num); store total in
         (array-set! money-out-array
                (gnc:scale-sum-collector-commodity money-out-collector report-currency exchange-fn
                        scaled? scaling-mul-val scaling-div-val)
         2 col-num) ; store total out
         (array-set! money-out-array
                (gnc:scale-sum-collector-commodity money-diff-collector report-currency exchange-fn
                        scaled? scaling-mul-val scaling-div-val)
         3 col-num)


          (for-each
           (lambda (account-in)
           (let* ((pair (account-in-alist account-in money-in-alist))
            (acct (car pair))
            (row (hash-ref money-in-pointer-hash acct row-number-in))) ;find which row in array acct is stored on
            (array-set! money-in-array
                    (gnc:scale-sum-collector-commodity (cadr pair) report-currency exchange-fn scaled? scaling-mul-val scaling-div-val)
                    row col-num)
            (if (eq? row row-number-in)    ; if we wrote it on row-number than we wrote on a new row,
            (begin                    ;  set counter "row-number" to point to next unused line and store info about acct
                (hash-set! money-in-pointer-hash acct row-number-in ) ;store acct and the row number where this acct data is stored
                ; store this accounts report currency
                (array-set! money-in-array report-currency row-number-in 0 )
                (set! row-number-in  (+ 1 row-number-in))
            )))
           )
           money-in-accounts)

            (for-each
           (lambda (account-out)
           (let* ((pair (account-in-alist account-out money-out-alist))
            (acct (car pair))
            (row (hash-ref money-out-pointer-hash acct row-number-out))) ;find which row in array acct is stored on
            (array-set! money-out-array
                (gnc:scale-sum-collector-commodity (cadr pair) report-currency exchange-fn scaled? scaling-mul-val scaling-div-val)
                    row col-num)
            (if (eq? row row-number-out)    ; if we wrote it on row-number than we wrote on a new row,
            (begin                    ;  set counter "row-number" to point to next unused line and store info about acct
                (hash-set! money-out-pointer-hash acct row-number-out ) ;store acct and the row number where this acct data is stored
                ; store this lines key which contains description and or account name etcetra
                (array-set! money-out-array report-currency row-number-out 0 )
                (set! row-number-out  (+ 1 row-number-out))
            ))
           ))
           money-out-accounts)

           )


        (set! row-number-in 4)  ; row 0 heading         row 1 number of days in the period row 2 total row 3 diff
        (set! row-number-out 4)
        (set! col-num 3)


         ; following calls function to get the results (amounts) for base period
         (if (not (and (get-option pagename-compare optname-compare?) (get-option pagename-compare optname-no-base?)))
            (begin
            (let (
                (scaling-mul-val
                    (if compare-scale-automatically?
                        compare-scale-to-val
                        (if (eq? scale-op-val '* )
                            scale-num-val
                            1)))
                (scaling-div-val (if compare-scale-automatically?
                    (caddr (car list-base-period))
                    (if (eq? scale-op-val '/ )
                        scale-num-val
                        1)))
                )
            (get-results from-date-tp to-date-tp scaling-mul-val scaling-div-val)
            (array-set! money-in-array (caddr (car list-base-period)) 1 col-num)    ; store number of days
            (set! col-num (+ col-num 1))
         )))
         ; following calls function to get the results (amounts) for average period
         (if (get-option pagename-compare optname-show-average?)
            (begin
            (let* (
                (date-start begindate2 )
                (date-end enddate2)
                (num-periods (length list-of-periods))
                (avg-num-days (round (/ (caddr (car (gnc:getdatedelta (list begindate2 enddate2 'one)))) num-periods)))
                (save-scaled? scaled?)
                (scaling-mul-val
                    (if compare-scale-automatically?
                        compare-scale-to-val
                        (if (eq? scale-op-val2 '* )
                            scale-num-val2
                            1)))
                (scaling-div-val (if compare-scale-automatically?
                    (* avg-num-days num-periods)
                    (if (eq? scale-op-val2 '/ )
                        (* scale-num-val2 num-periods)
                       num-periods))) ;; need to divide to get average
                )
            (set! scaled? #t) ; need scaling since dividing to calculate average
            (get-results  date-start date-end scaling-mul-val scaling-div-val)
            (set! scaled? save-scaled?)
            (array-set! money-in-array avg-num-days 1 col-num)    ; store number of days
            (array-set! money-in-array
                    (sprintf #f (_ "Average for %s %s ending %s") ;store time period title
                            (number->string num-periods)
                            deltas-val
                           (gnc-print-date date-end))
                        0 col-num)
            (set! col-num (+ col-num 1))
         )
         (if (and (= col-num 5) (get-option gnc:pagename-general optname-show-delta))
            (begin
              (set! accounts (sort accounts account-full-name<?))
              (set! money-in-list (sort (hash-map->list (lambda (k v) (list k v)) money-in-pointer-hash) acct-full-nam<?))
              (set! money-out-list (sort (hash-map->list (lambda (k v) (list k v)) money-out-pointer-hash) acct-full-nam<?))

              (array-set! money-in-array (_ "Delta") 0 col-num) ; store title
              (get-delta money-in-array money-in-list report-currency)
              (get-delta money-out-array money-out-list report-currency)
              (set! col-num (+ col-num 1))
            ;insert blank column after delta column
            (array-set! money-in-array " " 0 col-num);put in blank space for title doesn't work
            (set! col-num (+ col-num 1))
           ))
         ))
        ; following handles the comparison periods
        (if (get-option pagename-compare optname-compare?)
            ;; compare period is divided up into smaller parts based on users selection
            ;;  for loop reads in and stores as column in array each small period
        (for-each
            (lambda (dates)
            (let (
                (date-start (car dates))
                (date-end (cadr dates))
                (scaling-mul-val
                    (if compare-scale-automatically?
                        compare-scale-to-val
                            (if (eq? scale-op-val2 '* )
                                scale-num-val2
                                1)))
                (scaling-div-val (if compare-scale-automatically?
                    (caddr dates)
                    (if (eq? scale-op-val2 '/ )
                        scale-num-val2
                        1)))
                )
            ;    (store-period-values col-num list_of_trans scaling-mul-val scaling-div-val)
                (get-results  date-start date-end scaling-mul-val scaling-div-val)
                (array-set! money-in-array
                    (display-date-interval-columns date-start date-end) ;store time period title
                        0 col-num)
                (array-set! money-in-array (caddr dates) 1 col-num) ;store number of days
                (set! col-num (+ col-num 1))
            ))
     list-of-periods)
     )

     ; following calls function to get the results (amounts) to create a total for compare periods
         (if (get-option pagename-compare optname-show-total?)
            (begin
            (let* (
                (date-start begindate2 )
                (date-end enddate2)
                (num-periods (length list-of-periods))
                (num-days  (caddr (car (gnc:getdatedelta (list begindate2 enddate2 'one))) ))
                (scaling-mul-val
                    (if compare-scale-automatically?
                        compare-scale-to-val
                        (if (eq? scale-op-val2 '* )
                            scale-num-val2
                            1)))
                (scaling-div-val (if compare-scale-automatically?
                       num-days
                    (if (eq? scale-op-val2 '/ )
                         scale-num-val2
                       1))) ;; need to divide to get average
                )
            (get-results  date-start date-end scaling-mul-val scaling-div-val)
            (array-set! money-in-array num-days 1 col-num)    ; store number of days
            (array-set! money-in-array
                    (sprintf #f (_ "Total for %s %s ending %s") ;store time period title
                            (number->string num-periods)
                            deltas-val
                           (gnc-print-date date-end))
                        0 col-num)
            (set! col-num (+ col-num 1))
         )
         ))

           (set! accounts (sort accounts account-full-name<?))
           (set! money-in-list (sort (hash-map->list (lambda (k v) (list k v)) money-in-pointer-hash) acct-full-nam<?))
           (set! money-out-list (sort (hash-map->list (lambda (k v) (list k v)) money-out-pointer-hash) acct-full-nam<?))

          (if (and (= col-num 5) (get-option gnc:pagename-general optname-show-delta))
            (begin
            (array-set! money-in-array (_ "Delta") 0 col-num) ; store title
            (get-delta money-in-array money-in-list report-currency)
            (get-delta money-out-array money-out-list report-currency)
            (set! col-num (+ col-num 1))
            )
           )
          (set! work-done 0)
          (set! work-to-do (length accounts))
          (for-each
           (lambda (account)
         (set! work-done (+ 1 work-done))
         (gnc:report-percent-done (+ 85 (* 5 (/ work-done work-to-do))))
         (if (<= (gnc-account-get-current-depth account) tree-depth)
             (let* ((anchor (gnc:html-markup/format
                     (if (and (= (gnc-account-get-current-depth account) tree-depth)
                          (not (eq? (gnc-account-get-children account) '())))
                     (if show-subaccts?
                         (_ "%s and subaccounts")
                         (_ "%s and selected subaccounts"))
                     "%s")
                     (gnc:html-markup-anchor
                      (gnc:account-anchor-text account)
                      (if show-full-names?
                      (gnc-account-get-full-name account)
                      (xaccAccountGetName account))))))

               (set! account-disp-list (cons anchor account-disp-list))
               )
             )
         )
           accounts
           )


          (gnc:html-document-add-object!
           doc
           (gnc:make-html-text (_ "Selected Accounts")))

          (gnc:html-document-add-object!
           doc
           (gnc:make-html-text
        (gnc:html-markup-ul
         (reverse account-disp-list))))

          (gnc:html-table-append-ruler! table 2)
        (if (or (get-option pagename-compare optname-compare?) (get-option pagename-compare optname-show-average?))
            (make-heading-period table "primary-subheading" col-num))
        (if (get-option gnc:pagename-general optname-show-days)
             (make-heading-days table "primary-subheading" col-num))
        (if scaled?
            (make-heading-scale table "primary-subheading" col-num
                    (get-option pagename-compare optname-compare?) (get-option pagename-compare optname-no-base?)))

         (if (or scaled?  (get-option gnc:pagename-general optname-show-days))
        (gnc:html-table-append-ruler! table 1))


          (gnc:html-table-append-row/markup!
           table
           "primary-subheading"
           (list
        (_ "Money into selected accounts comes from")
        ""))

          (set! row-num 0)
          (set! work-done 0)
          (set! work-to-do (length money-in-list))
          (for-each
             (lambda (account-row)
                (set! row-num (+ 1 row-num))
                (set! work-done (+ 1 work-done))
                (gnc:report-percent-done (+ 90 (* 5 (/ work-done work-to-do))))
                (let* ((acct (car account-row))
                    (col 3)
                    (row-contents '() )
                    (row (cadr account-row)))

                (addto! row-contents
                    (gnc:make-html-text
                        (gnc:html-markup-anchor
                        (gnc:account-anchor-text acct)
                        (if show-full-names?
                        (gnc-account-get-full-name acct)
                        (xaccAccountGetName acct)))))

                (while (< col col-num)
                    (addto! row-contents
                      (if (equal? (array-ref money-in-array 0 col) " ")
                        ""
                        (gnc:make-html-table-header-cell/markup
                            "number-cell" (array-ref money-in-array row col))))
                        (set! col (+ col 1)))
                (gnc:html-table-append-row/markup!
                    table
                    (if (odd? row-num) "normal-row" "alternate-row")
                    (reverse row-contents))
            ))
           money-in-list
           )
            (let* ((col 3)
                   (row-contents '() ))

          (addto! row-contents
            (gnc:make-html-table-header-cell/markup "text-cell" (_ "Money In")))

          (while (< col col-num)
          (addto! row-contents
            (if (equal? (array-ref money-in-array 0 col) " ")
              ""
              (gnc:make-html-table-header-cell/markup ;
                "total-number-cell" (array-ref money-in-array 2 col) )))
            (set! col (+ col 1)))

        (gnc:html-table-append-row/markup!
            table
           "grand-total"
           (reverse row-contents))
        )

          (gnc:html-table-append-ruler! table 2)

          (gnc:html-table-append-row/markup!
           table
           "primary-subheading"
           (list
        (_ "Money out of selected accounts goes to")
        ""))

          (set! row-num 0)
          (set! work-done 0)
          (set! work-to-do (length money-out-list))
          (for-each
           (lambda (account)
         (set! row-num (+ 1 row-num))
         (set! work-done (+ 1 work-done))
         (gnc:report-percent-done (+ 95 (* 5 (/ work-done work-to-do))))
         (let* ((acct (car account))
            (row-contents '() )
            (col 3)
            (row (cadr account)))
            (addto! row-contents
             (gnc:make-html-text
              (gnc:html-markup-anchor
               (gnc:account-anchor-text acct)
               (if show-full-names?
               (gnc-account-get-full-name acct)
               (xaccAccountGetName acct)))))
              (while (< col col-num)
              (addto! row-contents
                (if (equal? (array-ref money-in-array 0 col) " ")
                   ""
                  (gnc:make-html-table-header-cell/markup
                    "number-cell" (array-ref money-out-array row col))))
              (set! col (+ col 1)))

             (gnc:html-table-append-row/markup!
                    table
                    (if (odd? row-num) "normal-row" "alternate-row")
                    (reverse row-contents))
           )
         )
           money-out-list
           )

          (let* ((col 3)
                   (row-contents '() ))
          (addto! row-contents
            (gnc:make-html-table-header-cell/markup "text-cell" (_ "Money Out")))

         (while (< col col-num)
         (addto! row-contents
            (if (equal? (array-ref money-in-array 0 col) " ")
              ""
              (gnc:make-html-table-header-cell/markup
                "total-number-cell" (array-ref money-out-array 2 col))))
          (set! col (+ col 1)))

        (gnc:html-table-append-row/markup!
            table
           "grand-total"
           (reverse row-contents))
        )

         (gnc:html-table-append-ruler! table 2)

        (let* ((col 3)
                   (row-contents '() ))
          (addto! row-contents
            (gnc:make-html-table-header-cell/markup "text-cell" (_ "Difference")))
          (while (< col col-num)
            (addto! row-contents
              (if (equal? (array-ref money-in-array 0 col) " ")
                 (gnc:make-html-table-header-cell/markup
                  "column-heading-center"
                 "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")
                (gnc:make-html-table-header-cell/markup
                    "total-number-cell" (array-ref money-out-array 3 col))))
            (set! col (+ col 1)))

          (gnc:html-table-append-row/markup!
            table
           "grand-total"
           (reverse row-contents))
        )

          (gnc:html-document-add-object! doc table)


          ;; add currency information
          (if show-rates?
          (gnc:html-document-add-object!
           doc ;;(gnc:html-markup-p
           (gnc:html-make-exchangerates
            report-currency exchange-fn accounts)))

;;
;; show any imbalances if option choosen
    (if (get-option gnc:pagename-general optname-show-imbalance)
    (begin
    (let* (
       (count 0)
       (imbalance-val "")
       (accounts-imbalance (gnc:accounts-imbalance-or-orphan from-date-tp to-date-tp))
        (numtransact (length accounts-imbalance ))
       )
    (gnc:html-document-add-object!
        doc
       (gnc:make-html-text
       (gnc:html-markup-p
         (gnc:html-markup/format
          text-dash   ; Few will realize it but report will show dash  if imbalance was checked for
          ))))

      (while (< count numtransact )
        (begin
        (let* (
            (current-account (list-ref accounts-imbalance count ))
            )
         (set! imbalance-val (string-append
              text-note-account
            (gnc-account-get-full-name current-account)
              text-changed-in-value
            (gnc:monetary->string (gnc:make-gnc-monetary (xaccAccountGetCommodity current-account)
                (gnc:account-get-balance-interval current-account from-date-tp to-date-tp #f)))
        ))
          (set! count (+ count 1))
      )
        (gnc:html-document-add-object!
        doc
        (gnc:make-html-text
        (gnc:html-markup-p
         (gnc:html-markup/format
          (_ "  %s")
          (gnc:html-markup-b imbalance-val)))
        ))
      ))
          )
    )
    )
;end for showing imbalance

        )

        ;; error condition: no accounts specified

    (gnc:html-document-add-object!
     doc
     (gnc:html-make-no-account-warning
      reportname (gnc:report-id report-obj))))

    (gnc:report-finished)
    doc))


;; function to add inflow and outflow of money
(define (cash-flow-calc-money-in-out settings)
  (let* ((accounts (cdr (assq 'accounts settings)))
     (to-date-tp (cdr (assq 'to-date-tp settings)))
     (from-date-tp (cdr (assq 'from-date-tp settings)))
     (report-currency (cdr (assq 'report-currency settings)))
     (include-trading-accounts (cdr (assq 'include-trading-accounts settings)))
     (to-report-currency (cdr (assq 'to-report-currency settings)))

     (is-report-account? (account-in-list-pred accounts))

     (money-in-accounts '())
     (money-in-hash (make-hash-table))
     (money-in-collector (gnc:make-commodity-collector))

     (money-out-accounts '())
     (money-out-hash (make-hash-table))
     (money-out-collector (gnc:make-commodity-collector))

     (all-splits (gnc:account-get-trans-type-splits-interval accounts '() from-date-tp to-date-tp))
     (splits-to-do (length all-splits))
     (splits-seen-table (make-hash-table))
     (work-done 0))

    (define (split-seen? split)
      (if (split-hashtable-ref splits-seen-table split) #t
      (begin
        (split-hashtable-set! splits-seen-table split #t)
        #f)))

    (define (work-per-split split)
      (set! work-done (+ 1 work-done))
      (if (= (modulo work-done 100) 0)
      (gnc:report-percent-done (* 85 (/ work-done splits-to-do))))
      (let ((parent (xaccSplitGetParent split)))
    (if (and (gnc:timepair-le (gnc-transaction-get-date-posted parent) to-date-tp)
         (gnc:timepair-ge (gnc-transaction-get-date-posted parent) from-date-tp))
        (let* ((parent-description (xaccTransGetDescription parent))
           (parent-currency (xaccTransGetCurrency parent)))
                    ;(gnc:debug parent-description
                    ;           " - "
                    ;           (gnc-commodity-get-printname parent-currency))
          (for-each
           (lambda (s)
         (let* ((s-account (xaccSplitGetAccount s))
            (s-account-type (xaccAccountGetType s-account))
            (s-amount (xaccSplitGetAmount s))
            (s-value (xaccSplitGetValue s))
            (s-commodity (xaccAccountGetCommodity s-account)))
           ;; Check if this is a dangling split
           ;; and print a warning
           (if (null? s-account)
               (display
            (string-append
             "WARNING: s-account is NULL for split: "
             (gncSplitGetGUID s) "\n")))
                    ;(gnc:debug (xaccAccountGetName s-account))
           (if (and     ;; make sure we don't have
            (not (null? s-account)) ;;  any dangling splits
            (or include-trading-accounts (not (eq? s-account-type ACCT-TYPE-TRADING)))
            (not (is-report-account? s-account)))
               (if (not (split-seen? s))
               (begin
                 (if (gnc-numeric-negative-p s-value)
                 (let ((s-account-in-collector (account-hashtable-ref money-in-hash s-account)))
                    ;(gnc:debug "in:" (gnc-commodity-get-printname s-commodity)
                    ;         (gnc-numeric-to-double s-amount)
                    ;         (gnc-commodity-get-printname parent-currency)
                    ;         (gnc-numeric-to-double s-value))
                   (if (not s-account-in-collector)
                       (begin
                     (set! s-account-in-collector (gnc:make-commodity-collector))
                     (account-hashtable-set! money-in-hash s-account
                                 s-account-in-collector)
                     (set! money-in-accounts (cons s-account money-in-accounts))
                     )
                       )
                   (let ((s-report-value (to-report-currency parent-currency
                                         (gnc-numeric-neg s-value)
                                         (gnc-transaction-get-date-posted
                                          parent))))
                     (money-in-collector 'add report-currency s-report-value)
                     (s-account-in-collector 'add report-currency s-report-value))
                   )
                 (let ((s-account-out-collector (account-hashtable-ref money-out-hash s-account)))
                    ;(gnc:debug "out:" (gnc-commodity-get-printname s-commodity)
                    ;         (gnc-numeric-to-double s-amount)
                    ;         (gnc-commodity-get-printname parent-currency)
                    ;         (gnc-numeric-to-double s-value))
                   (if (not s-account-out-collector)
                       (begin
                     (set! s-account-out-collector (gnc:make-commodity-collector))
                     (account-hashtable-set! money-out-hash s-account
                                 s-account-out-collector)
                     (set! money-out-accounts (cons s-account money-out-accounts))
                     )
                       )
                   (let ((s-report-value (to-report-currency parent-currency
                                         s-value
                                         (gnc-transaction-get-date-posted
                                          parent))))
                     (money-out-collector 'add report-currency s-report-value)
                     (s-account-out-collector 'add report-currency s-report-value))
                   )
                 )
                 )
               )
               )
           )
         )
           (xaccTransGetSplitList parent)
           )
          )
        )
    )
      )

    (define (calc-money-in-out-internal accounts)
      (for-each work-per-split all-splits))

    ;; And calculate
    (calc-money-in-out-internal accounts)
    ;; Return an association list of results
    (list (cons 'money-in-accounts money-in-accounts)
      (cons 'money-in-alist (hash-map->list (lambda (k v) (list k v)) money-in-hash))
      (cons 'money-in-collector money-in-collector)
      (cons 'money-out-accounts money-out-accounts)
      (cons 'money-out-alist (hash-map->list (lambda (k v) (list k v)) money-out-hash))
      (cons 'money-out-collector money-out-collector))))

(gnc:define-report
 'version 1
 'name reportname
 'report-guid "f8748b813fadbd20ba26e743aedf38da"
 'menu-path (list gnc:menuname-income-expense)
 'options-generator cash-flow-options-generator
 'renderer cash-flow-renderer)
