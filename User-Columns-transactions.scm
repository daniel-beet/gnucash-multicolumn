;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User-Columns-transactions.scm : Show multiple columns of transactions
;; or accounts the use can control
;;
;; Based on transaction.scm originally by Robert Merkel <rgmerk@mira.net>
;; Contributions by Bryan Larsen <blarsen@ada-works.com>,
;;  Christian Stimming <stimming@tuhh.de>, Michael T. Garrison Stuber,
;;  Tomas Pospisek <tpo_deb@sourcepole.ch> with a lot of help from "warlord",
;;  D.B.Doughty <dbdoughty at gmail.com>
;;  Comparison report developed by D.B.Doughty <dbdoughty at gmail.com>


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

;; set find1-field , find1-text and find2-field and find2-text before calling , better yet pass in as arguements to
;; filtersplits-found and to get-periods-results (see around line 4015)


(define-module (gnucash report standard-reports comparison-transactions))

;;following line needed to print in trace file use:  (gnc:warn "disp current-col is" current-col) see in C:\Users\USERNAME\AppData\Local\Temp
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (srfi srfi-1))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(use-modules (gnucash printf))
;

;;following line is step 1 of 4 needed to add gnctimeperiod-utilities
(use-modules (gnucash gnctimeperiod-utilities))

;;following line is needed for consolidating/ compostite transactions
;; since gnc:register-guid is not being exported in  report-system.scm for html-utilities.scm
(gnc:module-load "gnucash/html" 0)

;; following neded for gnc-locale-to-utf8
(use-modules (gnucash core-utils))

(gnc:module-load "gnucash/report/report-system" 0)

(define-macro (addto! alist element)
  `(set! ,alist (cons ,element ,alist)))

;; Define the strings here to avoid typos and make changes easier.
(define reportname (N_ "User-Columns-Transactions"))
(define pagename-accounts (N_ "Accounts"))
(define pagename-base (N_ "Base"))
(define pagename-sorting (N_ "Sorting"))
(define pagename-general (N_ "General"))
(define pagename-display (N_ "Display"))
(define pagename-col-1-2 (N_ "Columns_1-2"))
(define pagename-col-3-4 (N_ "Columns_3-4"))
(define pagename-col-5-6 (N_ "Columns_5-6"))
(define pagename-col-7-8 (N_ "Columns_7-8"))
(define pagename-col-9-a (N_ "Columns_9-a"))

(define optname-prime-sortkey (N_ "Primary Key"))
(define optname-prime-subtotal (N_ "Primary Subtotal"))
(define optname-prime-date-subtotal (N_ "Primary Subtotal for Date Key"))
(define optname-sec-sortkey (N_ "Secondary Key"))
(define optname-sec-subtotal (N_ "Secondary Subtotal"))
(define optname-sec-date-subtotal (N_ "Secondary Subtotal for Date Key"))
(define optname-void-transactions (N_ "Void Transactions"))
(define optname-table-export (N_ "Table for Exporting"))
(define optname-common-currency (N_ "Common Currency"))
(define optname-currency (N_ "Report's currency"))
(define def:grand-total-style "grand-total")
(define def:normal-row-style "normal-row")
(define def:alternate-row-style "alternate-row")
(define def:primary-subtotal-style "primary-subheading")
(define def:secondary-subtotal-style "secondary-subheading")

;; added for find
(define optname-find-text? (N_ "Find Text"))
(define optname-find1-field (N_ "Search"))
(define optname-find1-text   (N_ "finding the text"))
(define optname-find2-operand (N_ "Search for entries "))
(define optname-find2-field (N_ "Search for 2nd string in "))
(define optname-find2-text (N_ "2nd string"))
(define optname-find3-operand (N_ "Search for entries containing above "))
(define optname-find3-field (N_ "Search for 3rd string in "))
(define optname-find3-text (N_ "3rd string"))

(define optname-col1-text (N_ "Column 1 text"))
(define optname-col2-text (N_ "Column 2 text"))
(define optname-col3-text (N_ "Column 3 text"))
(define optname-col4-text (N_ "Column 4 text"))
(define optname-col5-text (N_ "Column 5 text"))
(define optname-col6-text (N_ "Column 6 text"))
(define optname-col7-text (N_ "Column 7 text"))
(define optname-col8-text (N_ "Column 8 text"))
(define optname-col9-text (N_ "Column 9 text"))
(define optname-cola-text (N_ "Column a text"))

(define optname-show-col1? (N_ "Show column 1"))
(define optname-show-col2? (N_ "Show column 2"))
(define optname-show-col3? (N_ "Show column 3"))
(define optname-show-col4? (N_ "Show column 4"))
(define optname-show-col5? (N_ "Show column 5"))
(define optname-show-col6? (N_ "Show column 6"))
(define optname-show-col7? (N_ "Show column 7"))
(define optname-show-col8? (N_ "Show column 8"))
(define optname-show-col9? (N_ "Show column 9"))
(define optname-show-cola? (N_ "Show column a"))

(define optname-a-find1-field (N_ "Search"))
(define optname-a-find1-text   (N_ "Finding the text"))
(define optname-a-find2-operand (N_ "Search for entries "))
(define optname-a-find2-field (N_ "Search for 2nd string in "))
(define optname-a-find2-text (N_ "2nd String"))
(define optname-a-find3-operand (N_ "Search for entries containing above "))
(define optname-a-find3-field (N_ "Search for 3rd string in "))
(define optname-a-find3-text (N_ "3rd String"))

;;note following are lower case to make different from col a entries
(define optname-b-find1-field (N_ "search"))
(define optname-b-find1-text   (N_ "finding the text"))
(define optname-b-find2-operand (N_ "search for entries "))
(define optname-b-find2-field (N_ "search for 2nd string in "))
(define optname-b-find2-text (N_ "2nd string"))
(define optname-b-find3-operand (N_ "search for entries containing above "))
(define optname-b-find3-field (N_ "search for 3rd string in "))
(define optname-b-find3-text (N_ "3rd string"))

(define optname-find-min (N_ "Find minimum amount"))
(define optname-find-max (N_ "Find maximum ammount"))

(define optname-days (N_ "Days"))

(define optname-show-assets (N_ "Show Assets"))
(define optname-show-assets-help (N_ "Show Assets"))
(define optname-show-liabilities (N_ "Show Liabilities"))
(define optname-show-liabilities-help (N_ "Show Liabilities"))
(define optname-show-income (N_ "Show Income"))
(define optname-show-income-help (N_ "Show Income"))
(define optname-show-expenses (N_ "Show Expenses"))
(define optname-show-expenses-help (N_ "Show Expenses"))
(define optname-show-equities (N_ "Show Equities"))
(define optname-show-equities-help (N_ "Show Equities"))

(define text-containing " containing " )
(define text-and " and ")
(define text-or " or ")
(define text-but-not " but not ")
(define text-minimum " Minimum ")
(define text-maximum " Maximum ")

;; added for consolidating
(define consolidated-text "Consolidated")
(define optname-consolidate-case-sensitive  (N_ "--  make consolidation case-sensitive"))


(define optname-descript-titlecase (N_ "Titlecase the first chracter in each word in description"))

;; add for scaling
(define scaling-text "Note: amount and balance have been scaled.")

(define optname-show-find (N_ "Show find requirements"))
(define opthelp-show-find (N_ "Display all of the search or find requirements for each column."))

(define optname-show-average (N_ "Show average"))
(define opthelp-show-average (N_ "Display a column for the average of the historic values."))
(define optname-show-totalcol (N_ "Show Column with Totals"))
(define opthelp-show-totalcol (N_ "Display a column with the row totals."))

;; added for flagging imbalances when account name is primary key
(define optname-show-imbalance
  (N_ "Note any imbalance"))
(define opthelp-show-imbalance
  (N_ "Make a footnote if there is an imbalance and find not used"))

(define text-note-account " Note - account   ")
(define text-changed-in-value " in the base period  changed in value by ")
(define text-dash (_ "-")) ; printed to indicate imbalance was checked for

(define text-initial-help (_ " 1. on accounts tab select accounts <br />  2. select time period on base tab <br />
         &nbsp;&nbsp;   (to just see accounts with activity, scroll down select find text for text enter a blank <br />
         &nbsp;&nbsp;     for second string select but exclude entries with second string   and for second string enter a blank) <br />
        3. On columns-x-x tabs check the box to show column, enter a title, select where to look for text and enter the text to find (scroll down) <br />
        4. On display tab select what items to display <br />
        5. On General tab enter the title to put on the report"))

;; for find
(define list-findchoices
   (list (list->vector
             (list 'description
                   (N_ "description  ")
                   (N_ "search descriptions or transactions for the text - note a blank is added at start and end of description")))
            (list->vector
             (list 'account-name
                   (N_ "account name")
                   (N_ "search full account name ")))
            (list->vector
             (list 'account-code
                   (N_ "account code")
                   (N_ "search account code")))
            (list->vector
             (list 'memo
                   (N_ "memo    ")
                   (N_ "Search only memo field")))
            (list->vector
             (list 'notes
                   (N_ "notes    ")
                   (N_ "search only notes")))
            (list->vector
             (list 'memo/notes
                   (N_ "memo/notes ")
                   (N_ "search both memo and notes")))
            (list->vector
             (list 'number
                   (N_ "check number ")
                   (N_ "search check number and transaction number field")))
            (list->vector
             (list 'date
                   (N_ "date    ")
                   (N_  "search date ( a space has been added in front date and after date) ")))
            (list->vector
             (list 'reconciled-date
                   (N_ "reconciled date ")
                   (N_ "search reconciled date ( a space has been added in front of and after reconciled date)")))
            (list->vector
             (list 'reconcile
                   (N_ "reconcile ")
                   (N_ "search reconcile field (y,n,c,f) (a space has been added in front of and after reconcile character)")))
            (list->vector
             (list 'any
                   (N_ "any    ")
                   (N_ "search description and account-name and account-code and memo and notes ")))
           )
)

(define list-find2-operands
   (list (list->vector
             (list 'none
                   (N_ "    ignore 2nd string                 " )
                   (N_ "do not look for a second string of text")))
            (list->vector
             (list 'and
                   (N_ "and the 2nd string ")
                   (N_ "the transaction must include the second text string")))
            (list->vector
             (list 'or
                   (N_ "or the 2nd string")
                   (N_ "search for either of the two text strings ")))
            (list->vector
             (list 'not
                   (N_ "but exclude entries with 2nd string")
                   (N_ "Search for transactions that do not include the second text string")))
           )
)

(define list-find3-operands
   (list (list->vector
             (list 'none
                   (N_ "    ignore 3rd string                 " )
                   (N_ "do not look for a third string of text")))
            (list->vector
             (list 'and
                   (N_ "and the 3rd string ")
                   (N_ "the transaction also must include the third text string")))
            (list->vector
             (list 'or
                   (N_ "or the 3rd string")
                   (N_ "search for either the above two text strings or the third string ")))
            (list->vector
             (list 'not
                   (N_ "but exclude entries with 3rd string")
                   (N_ "Search for transactions that do not include the third text string")))
           )
)

;; this is step 2 of 4
;; needed for gnctimeperiod-utilities
;; define all option's names so that they are properly defined
;; in *one* place.
;; can change following text for local language

;; following value may need to be changed
(define the_tab pagename-base)

(define text-whichperiod "Select Time Period")
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
(define text-scaled-by  "  Scaled by:")
(define text-scaled-to  "  Scaled to:")

; for compare
(define optname-show-base?  (N_ "Show base period "))
(define optname-scale-automatically? (N_ "Ignore other scaling and"))
(define optname-scale-to (N_ "scale to"))

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
                   (N_ text-last-help)))
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

(define type-namelist (list
                         "Assets"
                         "Liabilities"
                         "Income"
                         "expense"
                         "Equity"
                         ))
(define show-days? #t)
(define show-find? #f)

(define show-average? #t)
(define show-totalcol? #t)
(define show-assets? #t)
(define show-liabilities? #t)
(define show-income? #t)
(define show-expenses? #t)
(define show-equities? #t)
(define no-grand-total? #f)

(define asset-accounts #f)
(define liability-accounts #f)
(define income-accounts #f)
(define expense-accounts #f)
(define equity-accounts #f)
(define liability-accounts #f)
(define previous-type #f)

;for find

(define do-find? #f)
(define do-findamount? #f)
(define find-min? #f)
(define find-max? #f)
(define find-text? #f)

(define find-min 100.00)
(define find-max 100.00)
(define findtitle "")


;;for running balance
(define amount-total-hash (make-hash-table))

;;for scaling
(define scale-op-val '*)
(define scale-num-val 1)
(define scaled? #f)
(define compare-scale-automatically? #f)
(define compare-scale-to-val 1)


(define description-titlecase? #t)
(define columns-headings 0)
;remove these commented lines after other reports are converted to using markers
;; for consolidate descriptions (create composite  - to combine multiple entries with same payee)
;(define marker1 "#Yw;" ) ;all of the markers need to be 4 characters in length
;(define marker2 "#Yx;" )
;(define marker3 "#Yy;" )
;(define marker4 "#Yz;" )
;(define marker5 "#Zy;" )
;(define marker6 "#Zx;" )
;(define marker7 "#Zw;" )
;(define marker8 "#Zv;" )
;(define marker9 "#Zu;" )
;(define markerA "#Yv;" )
;(define markerB "#Vv;" )

;; for consolidate descriptions (create composite  - to combine multiple entries with same payee)
(define marker1 (string #\# #\1 #\w #\backspace)) ;all of the markers need to be 4 characters in length
(define marker2 (string #\# #\2 #\x #\backspace))
(define marker3 (string #\# #\3 #\y #\backspace))
(define marker4 (string #\# #\4 #\z #\backspace))
(define marker5 (string #\# #\5 #\y #\backspace))
(define marker6 (string #\# #\6 #\x #\backspace))
(define marker7 (string #\# #\7 #\w #\backspace))
(define marker8 (string #\# #\8 #\v #\backspace))
(define marker9 (string #\# #\9 #\u #\backspace))
(define markerA (string #\# #\A #\t #\backspace))
(define markerB (string #\# #\B #\s #\backspace))


(define curr " ")
(define comm-curr? #f)
(define currency-type-num 1)
(define currency-type-str "1")
(define now (timespecCanonicalDayTime (cons (current-time) 0)))
(define today-tm (gnc:timepair->date now))
(define list_of_trans '())
(define sort-date-type 7)

(define currency-col-type 1)

(define primary-subtotal-collector-hash (make-hash-table))
(define secondary-subtotal-collector-hash (make-hash-table))
(define total-collector-hash (make-hash-table))
(define type-total-collector-hash (make-hash-table))
(define total-current-type #f)

(define (sort-test x y var-p var-s comp-p1 comp-p2 comp-s1 comp-s2  )

    (define (test?  x y)
        (let ((primary-x  (var-p x))
            (primary-y  (var-p y))
            (secondary-x  (var-s x))
            (secondary-y  (var-s y)) )
            (if (not (comp-p1 primary-x primary-y ))
                    (comp-p2 primary-x primary-y)
                    (if (not (comp-s1 secondary-x secondary-y))
                        (comp-s2 secondary-x secondary-y )
                        (string-ci<=? (get-description x) (get-description y)))
                ))
    )
    (test? x y)
)



(define payee-hash (make-hash-table) )
(define payee-account-guid-hash (make-hash-table))
(define currency-type-hash (make-hash-table) )
(define currency-lookup-hash (make-hash-table) )

(define pointer-hash (make-hash-table))
(define amounts-array (make-array #f 2 3)) ; col 0 = currency type ; col 1 and greater - period
(define guid-array (make-array #f 2 3))
(define row-number 11); row 0 heading         row 1 number of days in the period
       ;                                      row 2 - 1st string field   row 3 - 1st string text
       ;         row 4 - 2nd operand          row 5  - 2nd string field      row 6- 2nd string text
       ;         row 7 - 3rd string operand   row 8 - 3rd string field       row 9 - 3rd string text row 10 - blank
(define last-column 1)
(define period-number 0)

(define acct-depth-name-list (make-list 10 #f))
(define type-total-list (make-list 10 #f))
(define type-root-acct-list (make-list 10 #f))



;; routine to sum up all descriptions with same payee and to store account guid and also non uppercase description
 (define (total-payee-hash-add! payee-hash payee amount payee-account-guid-hash guids+description+acct )
    (begin
          (hash-set! payee-hash payee  (gnc-numeric-add amount  (hash-ref payee-hash payee (gnc-numeric-zero))  GNC-DENOM-AUTO GNC-RND-ROUND))
         (hash-set! payee-account-guid-hash payee guids+description+acct )))

(define (get-primary-key transaction)
    (let* (
    ;;
            (thekey (car transaction))
            (endprimary (string-contains thekey marker1))
            (primary (if (< 0 endprimary)
                            (string-copy thekey 0 endprimary)
                    " ")) ;default
                    )
            primary
             )
)

(define (get-secondary-key transaction)
    (let* (
            (thekey (car transaction))
            (startsecond (string-contains thekey marker1))
            (endsecond (string-contains thekey marker2))
            (secondary (if  (< 4 (- endsecond startsecond))
                        (string-copy thekey (+ 4 startsecond) endsecond)
                        " " ));default
                  )
            secondary
            )
    )

(define (get-date-key-tm the-key)
    (let* (
            (date (if  (< 7 (string-length the-key))
                    the-key
                    "20000114" ;default date
                  ))

            (year (string->number (string-copy date 0 4)))
            (month (string->number (string-copy date 4 6)))
            (day (string->number (string-copy date 6 8)))
        ;    (tm2 (strptime "%Y %m %d" (string-append (string-copy date 0 4)
        ;     " " (string-copy date 4 6) " " (string-copy date 6 8) )))
            (tm today-tm)
            )
            (set-tm:year tm (- year 1900))
            (set-tm:mon tm (- month 1))
            (set-tm:mday tm day)
             tm
             )
    )
(define (get-date-key-tp the-key)
    (gnc:date->timepair (get-date-key-tm the-key))
    )
(define (get-date-tm transaction)
    (let* (
            (thekey (car transaction))
            (startdate (string-contains thekey marker2))
            (enddate (string-contains thekey marker3))
            (date (if  (< 11 (- enddate startdate))
                    (string-copy thekey (+ 4 startdate) enddate)
                    "20000114" ;default date
                  ))

            (year (string->number (string-copy date 0 4)))
            (month (string->number (string-copy date 4 6)))
            (day (string->number (string-copy date 6 8)))
            (tm today-tm)
            )
            (set-tm:year tm (- year 1900))
            (set-tm:mon tm (- month 1))
            (set-tm:mday tm day)
             tm
             )
    )
(define (get-date-tp transaction)
    (gnc:date->timepair (get-date-tm transaction))
    )
(define (get-currency-type transaction)
    (let* (
            (thekey (car transaction))
            (startcurr (string-contains thekey marker9))
            (currency-type-str (if (< (+ startcurr 4) (string-length thekey))
                            (string-copy thekey (+ startcurr 4))
                            ;(string-take-right thekey (+ startcurr 4))
                            " ")))
            currency-type-str)
)
(define (get-memo transaction)
    (let* (
            (thekey (car transaction))
            (endmemo  (string-contains thekey marker8))
            (startmem (string-contains thekey marker7))
            (memo (if  (< 4 (- endmemo startmem))
                        (string-copy thekey  (+ startmem 4) endmemo)
                        " ")))
            memo)
)

(define (get-reverse-sign? transaction)
    (let* (
            (thekey (car transaction))
            (end  (string-contains thekey marker9))
            (start (string-contains thekey marker8))
            (rev-sign (if  (< 4 (- end start))
                        #t
                        #f)))
            rev-sign)
)

(define (get-description transaction)
    (let* (
            (thekey (car transaction))
            (startdescrip (string-contains thekey marker3))
            (enddescrip (string-contains thekey marker4))
            (description (if  (< 4 (- enddescrip startdescrip))
                        (string-copy thekey (+ 4 startdescrip) enddescrip)
                        " ")))
            description)
    )
(define (get-acct transaction)
;; the account
    (if transaction
    (let* ( (row (get-row-in-array transaction))
        (acct (cadddr (array-ref guid-array row 0))))
        (if acct
        acct
        #f))
    #f)
        )
(define (get-acctcode transaction)
    (let* ( (acct   (get-acct transaction)))
       (xaccAccountGetCode acct)
        ))

(define (get-description-verbatim row)
;; the actual description entered by user - removes problems with upper and lower case
    (let* (
        (descript (caddr (array-ref guid-array row 0))))
        (if descript
        descript
        "")))
(define (get-namecode transaction)
    (let* (
            (thekey (car transaction))
            (startnamcode (string-contains thekey marker4))
            (endnamcode (string-contains thekey marker5))
            (namcode (if  (< 4 (- endnamcode startnamcode))
                        (string-copy thekey (+ 4 startnamcode) endnamcode)
                        " ")))
            namcode)
    )

(define (get-account-code transaction)
    (let* (
            (thekey (car transaction))
            (startcod  (string-contains thekey marker5))
            (endothercod (string-contains thekey marker6))
            (acctcod (if  (< 4 (- endothercod startcod))
                        (string-copy thekey  (+ startcod 4) endothercod)
                        " ")))
            acctcod)
    )


(define (get-other-name transaction)
    (let* (
            (thekey (car transaction))
            (endacctname  (string-contains thekey marker6))
            (endothernam (string-contains thekey marker7))
            (othernam (if  (< 4 (- endothernam endacctname))
                        (string-copy thekey  (+ endacctname 4) endothernam)
                        " ")))
            othernam)
    )
(define (get-main-acct-type transaction)
    (let* (
        (acct (get-acct transaction))
        (type 0)
        (acct-list #f)
        (found-type #f))
    (if acct
        (begin
            (while (and (< type (length type-root-acct-list)) (not found-type))
                (set! acct-list (list-ref type-root-acct-list type))
                (if (member acct acct-list)
                    (set! found-type type))
                (set! type (+ type 1))
            )
            found-type)
        #f
    )
  ))

(define (get-acct-depth to-depth acct)
;; the account
    (let* (
        (depth (gnc-account-get-current-depth acct)))

        (while (< to-depth depth)
            (set! acct (gnc-account-get-parent acct) )
            (set! depth (- depth 1))
        )
     acct
        ))
(define (get-acct-trans-depth to-depth transaction); was a call to get-acctname-from-sort
;; the account
    (let* ( (thekey (car transaction))
        (row (get-row-in-array transaction))
        (acct (cadddr (array-ref guid-array row 0)))
        )
        (get-acct-depth to-depth acct)
   ))
 (define (get-acctname-trans-depth to-depth transaction)
    (let* (
           (acct (get-acct-trans-depth to-depth transaction)))
        (xaccAccountGetName acct)
  ))
;(define (get-accountname-from-sort thekey transaction)
;    (let ((start-account (string-contains thekey markerA)))
;        (if start-account
;            (if (< (+ 4 start-account) (string-length thekey) )
;                (string-append (get-account-code transaction)  (substring thekey (+ 4 (string-contains thekey markerA))))
;                " ")
;        thekey)
;        ))

(define (get-accountguid transaction)
;; we stored (account-guid (gncAccountGetGUID account))
    (let* ( (row (get-row-in-array transaction))
        (accountguid (cadr (array-ref guid-array row 0))))
        (if accountguid
        accountguid
        "")))

(define (get-transguid row column)
;; we stored (account-guid (gncAccountGetGUID account))
    (let* (
        (transguid (if (array-ref guid-array row column)
            (car (array-ref guid-array row column))
            "")))
        transguid
        ))

(define (get-split-value-numer transaction)
    (let (
            (split-value-comp (cdr transaction))
            ;  (split-value-comp (gnc:make-gnc-numeric 12 1))
            )
            split-value-comp)
)


(define (get-row-in-array transaction)
    (cdr transaction)
)
(define (get-array-value-num-base transaction) ;note values are scaled when stored in array
    (let* ( (row (get-row-in-array transaction))
            (split-value-ar
                    (array-ref amounts-array row 1)) ;column is 1 since for base period
            (value (if (gnc:gnc-monetary? split-value-ar)
                    (gnc:gnc-numeric-num (gnc:gnc-monetary-amount split-value-ar))
                        0.0)))
            value
            )
)

(define (get-monetary key column) ;note values are scaled when stored in array
    (let* ( (row (get-row-in-array key))
            (split-value-ar
                    (array-ref amounts-array row column))
            (value (if (gnc:gnc-monetary? split-value-ar)
                        split-value-ar
                    ;(gnc:make-gnc-monetary report-currency (gnc:make-gnc-numeric 2 1))))
                    (gnc:make-gnc-monetary
                       (hash-ref currency-lookup-hash (get-currency-type key))
                        split-value-ar)))
            )
            value
))





(define (gnc:comp-register-guid type guid)
  (gnc-build-url URL-TYPE-REGISTER (string-append type guid) ""))

(define (gnc-comp:transaction-anchor-text trans)
  (gnc:comp-register-guid "trans-guid=" (gncTransGetGUID trans)))

 (define (get-gnc:comp-transaction-anchor-text row column)
  (gnc:comp-register-guid "trans-guid=" (get-transguid row column)))

(define (gnc:comp-html-transaction-anchor row column text)
  (gnc:make-html-text (gnc:html-markup-anchor
                       (get-gnc:comp-transaction-anchor-text row column)
                       text)))

(define (get-gnc:account-anchor-text split-trans)
    ;(gnc:register-guid "acct-guid=" (gncAccountGetGUID acct)))
    (let* ( (acctguid (get-accountguid split-trans)))
        (if acctguid
            (gnc:comp-register-guid "acct-guid=" acctguid)
            "")))


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

(define (getpos element lst)
 ; (lambda (element lst)
    (if (eqv? (list? (member element lst)) #t)
        (- (length lst) (length (member element lst)))
        #f
        )
)
  ;; wrapper around gnc:html-table-append-ruler!
(define (add-rule table num-cells)
      (gnc:html-table-append-ruler!
       table
       num-cells)) ;;dbd fix me
(define (pad n)
  (if (> n 0)
      (string-append "&nbsp;" (pad (- n 1)) )
      "")
)
(define (make-tab n)
  (if (> n 0)
      (string-append "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" (make-tab (- n 1)))
      "")
)

(define (indent-names table row-style transaction)
    (let* ((header-row-contents '() )
        (acct (get-acct transaction))
        (parent (gnc-account-get-parent acct) )
        (depth (gnc-account-get-current-depth acct))
        (current-depth (gnc-account-get-current-depth acct))
        )

    (list-set! acct-depth-name-list depth acct)
    (set! depth (- depth 1))
    (while (and (< 0 depth)
                (not (equal? parent (list-ref acct-depth-name-list depth))))
        (list-set! acct-depth-name-list depth parent )
        (set! parent (gnc-account-get-parent parent) )
        (set! depth (- depth 1))
    )
    ;; we matched parent names at this level so difference starts at next level
    (set! depth (+ depth 1))
    (if (= depth 1)
        (begin
            (add-rule table (+ last-column columns-headings
                            (if #f 1 0)))
            (gnc:html-table-append-row/markup! table def:grand-total-style ;row-style
                (gnc:make-html-text
                  (gnc:html-markup-h3
             (string-append  (xaccAccountGetName (list-ref acct-depth-name-list depth))
                           (pad (- 25 (string-length (xaccAccountGetName (list-ref acct-depth-name-list depth)))))) )))
            (set! depth (+ depth 1))
        )
    )
    (while (< depth current-depth)
        (if (= depth 2)
            (gnc:html-table-append-row/markup! table row-style
                (gnc:make-html-text
                    (gnc:html-markup-b (string-append (make-tab (- depth 2))
                        (xaccAccountGetName (list-ref acct-depth-name-list depth))))))

            (gnc:html-table-append-row/markup! table row-style
                (string-append (make-tab (- depth 2))   (xaccAccountGetName (list-ref acct-depth-name-list depth))))
        )
        (set! depth (+ depth 1))
    )

   (if (= depth 2)
        (begin
            (gnc:make-html-text
                (gnc:html-markup-b (xaccAccountGetName acct)))
        )
        (string-append (make-tab (- depth 2))  (xaccAccountGetName acct))
    )
  )
)


;; for consolidating
(define (sortkeys list-thekeys var-p var-s comp-p1 comp-p2 comp-s1 comp-s2 primary-key secondary-key acct-ascend accounts)
            ;1 sort by primary key and secondary key
            ;2 sort by primary key and amount
            ;3 sort by amount and secondary key
            ;4 sort by amount and amount
        ; will do 3rd sort on description

        (if (equal? acct-ascend "Prime")
                    (sort list-thekeys
                            (lambda (x y)
                                (let (  (primary-x  (getpos (get-acct x) accounts))
                                        (primary-y  (getpos (get-acct y) accounts))
                                        (secondary-x  (var-s x))
                                        (secondary-y  (var-s y)) )
                                (if (not (= primary-x primary-y ))
                                    (< primary-x primary-y)
                                    (if (not (comp-s1 secondary-x secondary-y))
                                        (comp-s2 secondary-x secondary-y )
                                        (string-ci<=? (get-description x) (get-description y)))
                                ))
                            )
                      )
          (if (equal? acct-ascend "Second")
                    (sort list-thekeys
                            (lambda (x y)
                                (let (  (primary-x  (var-p x))
                                        (primary-y  (var-p y))
                                        (secondary-x (getpos (get-acct x) accounts))
                                        (secondary-y  (getpos (get-acct y) accounts)))
                                (if (not (comp-p1 primary-x primary-y ))
                                    (comp-p2 primary-x primary-y)
                                    (if (not (= secondary-x secondary-y))
                                        (< secondary-x secondary-y )
                                        (string-ci<=? (get-description x) (get-description y)))
                                ))
                            )
                      )
                    (sort list-thekeys
                            (lambda (x y)
                                (let (  (primary-x  (var-p x))
                                        (primary-y  (var-p y))
                                        (secondary-x  (var-s x))
                                        (secondary-y  (var-s y)) )
                                    (if (not (comp-p1 primary-x primary-y ))
                                        (comp-p2 primary-x primary-y)
                                        (if (not (comp-s1 secondary-x secondary-y))
                                            (comp-s2 secondary-x secondary-y )
                                            (string-ci<=? (get-description x) (get-description y)))
                                ))
                            )
                    )
        ))
)
;; The option-values of the sorting key multichoice option, for
;; which a subtotal should be enabled.
(define comp-subtotal-enabled '(account-name
                           account-code
                           corresponding-acc-name
                           corresponding-acc-code))

(define (comp-split-account-full-name-same-p a b)
  (equal? a  b))

(define (comp-split-account-code-same-p a b)
  (equal? a b))

(define (comp-split-same-corr-account-full-name-p a b)
  (equal? a b))

(define (comp-split-same-corr-account-code-p a b)
  (equal? a b))

(define (comp-timepair-same-year tp-a tp-b)
  (= (gnc:timepair-get-year tp-a)
     (gnc:timepair-get-year tp-b)))

(define (comp-timepair-same-quarter tp-a tp-b)
  (and (comp-timepair-same-year tp-a tp-b)
       (= (gnc:timepair-get-quarter tp-a)
          (gnc:timepair-get-quarter tp-b))))

(define (comp-timepair-same-month tp-a tp-b)
  (and (comp-timepair-same-year tp-a tp-b)
       (= (gnc:timepair-get-month tp-a)
          (gnc:timepair-get-month tp-b))))

(define (comp-timepair-same-week tp-a tp-b)
  (and (comp-timepair-same-year tp-a tp-b)
       (= (gnc:timepair-get-week tp-a)
      (gnc:timepair-get-week tp-b))))

(define (comp-split-same-week-p a b)
  (let ((tp-a (get-date-key-tp a))
        (tp-b (get-date-key-tp b)))
    (comp-timepair-same-week tp-a tp-b)))

(define (comp-split-same-month-p a b)
  (let ((tp-a (get-date-key-tp a))
        (tp-b (get-date-key-tp b)))
    (comp-timepair-same-month tp-a tp-b)))

(define (comp-split-same-quarter-p a b)
  (let ((tp-a (get-date-key-tp a))
        (tp-b (get-date-key-tp b)))
    (comp-timepair-same-quarter tp-a tp-b)))

(define (comp-split-same-year-p a b)
  (let ((tp-a (get-date-key-tp a))
        (tp-b (get-date-key-tp b)))
    (comp-timepair-same-year tp-a tp-b)))
;;

(define (comp-set-last-row-style! table tag . rest)
  (let ((arg-list
         (cons table
               (cons (- (gnc:html-table-num-rows table) 1)
                     (cons tag rest)))))
    (apply gnc:html-table-set-row-style! arg-list)))

(define (comp-add-subheading-row data table width subheading-style)
  (let ((heading-cell (gnc:make-html-table-cell data)))
    (gnc:html-table-cell-set-colspan! heading-cell width)
    (gnc:html-table-append-row/markup!
     table
     subheading-style
     (list heading-cell))))

;; render an account subheading - column-vector determines what is displayed
(define (comp-render-account-subheading
         split-trans the-key table width subheading-style column-vector)
  (let ((accountname  (get-acctname-trans-depth 2 split-trans)))
  (if (not no-grand-total?)
    (comp-add-subheading-row (gnc:make-html-text
                         (gnc:html-markup-anchor
                           (get-gnc:account-anchor-text split-trans)
                           accountname) )
                        table width subheading-style))))

(define (comp-render-corresponding-account-subheading
         split-trans the-key table width subheading-style column-vector)
  (let ((accountname the-key))
    (comp-add-subheading-row (gnc:make-html-text
                         (gnc:html-markup-anchor
                          (if (not (null? split-trans))
                           (get-gnc:account-anchor-text split-trans)
                           "")
                           accountname) )
                        table width subheading-style)))

(define (comp-render-week-subheading split-trans the-key table width subheading-style column-vector)
  (comp-add-subheading-row (gnc:date-get-week-year-string
               (get-date-tm split-trans))
              table width subheading-style))

(define (comp-render-month-subheading split-trans the-key table width subheading-style column-vector)
  (comp-add-subheading-row (gnc:date-get-month-year-string
                      (get-date-tm split-trans))
                     table width subheading-style))

(define (comp-render-quarter-subheading split-trans the-key table width subheading-style column-vector)
  (comp-add-subheading-row (gnc:date-get-quarter-year-string
                      (get-date-tm split-trans))
                     table width subheading-style))

(define (comp-render-year-subheading split-trans the-key table width subheading-style column-vector)
  (comp-add-subheading-row (gnc:date-get-year-string
                      (get-date-tm split-trans))
                      table width subheading-style))


(define (comp-add-subtotal-row table width subtotal-string subtotal-collector subtotal-collector-hash
                          subtotal-style export?)
  (let ((currency-totals (subtotal-collector
                          'format gnc:make-gnc-monetary #f))
        (blanks (gnc:make-html-table-cell/size 1 (- width 1) #f))
        (row-contents '())
        (the-currency 1))

    (while (< the-currency currency-type-num)
        (let ((currency (hash-ref currency-lookup-hash (number->string the-currency)))
              (col 1)
              (count-spaces 1))
        (if (= the-currency 1)
         (addto! row-contents
                    (gnc:make-html-table-cell/markup "total-label-cell" subtotal-string))
         (addto! row-contents
                    (gnc:make-html-table-cell/markup "total-label-cell" " " )))
        (set! count-spaces (+ count-spaces 1))
        ; I can't figure out how to get blanks so brute force
            (while (< count-spaces columns-headings )
                (addto! row-contents
                    (gnc:make-html-table-cell/markup
                        "total-number-cell"
                " " ))
            (set! count-spaces (+ 1 count-spaces))
            )
          (while (<= col last-column)
           (addto! row-contents
                (gnc:make-html-table-cell/markup
                                "total-number-cell" (hash-ref subtotal-collector-hash
                                (string-append (number->string col) markerB (number->string the-currency))
                                (gnc:make-gnc-monetary currency (gnc-numeric-zero)))))
        (set! col (+ col 1))
        )
        (set! the-currency (+ the-currency 1))
    )
    (gnc:html-table-append-row/markup! table subtotal-style
            (reverse row-contents))
    (set! row-contents '()))
    ))

(define (comp-add-subtotal-row-account table width subtotal-string subtotal-collector subtotal-collector-hash
                          subtotal-style export?)
  (let ((currency-totals (subtotal-collector
                          'format gnc:make-gnc-monetary #f))
        (blanks (gnc:make-html-table-cell/size 1 (- width 1) #f))
        (row-contents '())
        (value #f)
        (cell #f)
        (the-currency 1))

    (while (< the-currency currency-type-num)
        (let ((currency (hash-ref currency-lookup-hash (number->string the-currency)))
              (col 1)
              (count-spaces 1))
        (if (= the-currency 1)
         (addto! row-contents
                    (gnc:make-html-text
                        (gnc:html-markup-b subtotal-string)))
         (addto! row-contents
                    (gnc:make-html-table-cell/markup "total-label-cell" " " )))
        (set! count-spaces (+ count-spaces 1))
        ; I can't figure out how to get blanks so brute force
        (while (< count-spaces columns-headings )
            (addto! row-contents
                (gnc:make-html-table-cell/markup
                    "total-number-cell"
            " " ))
            (set! count-spaces (+ 1 count-spaces))
        )
        (while (<= col last-column)
            (addto! row-contents
                (gnc:make-html-table-cell/markup
                          "total-number-cell"
                    (hash-ref subtotal-collector-hash
                       (string-append (number->string col) markerB (number->string the-currency))
                          (gnc:make-gnc-monetary currency (gnc-numeric-zero)))))
       ;     (set! value (hash-ref subtotal-collector-hash
       ;                 (string-append (number->string col) markerB (number->string the-currency))
       ;                 (gnc:make-gnc-monetary currency (gnc-numeric-zero))))
       ;    (set! cell
       ;         (gnc:make-html-table-cell/markup
       ;             "number-cell"  value
       ;             ))
        ;    (addto! row-contents
        ;     (gnc:html-table-cell-set-style-internal!
        ;       cell
        ;            'attribute (list "align" "right")
         ;          ; 'attribute '("align" "right")
        ;           ; 'attribute '("valign" "top")
        ;        )
            (set! col (+ col 1))
        )
        (set! the-currency (+ the-currency 1))
    )
    (gnc:html-table-append-row/markup! table subtotal-style
            (reverse row-contents))
    (set! row-contents '()))
    ))

(define (comp-total-string str) (string-append (_ "Total For ") str))

(define (comp-render-account-subtotal
          table width split the-key  total-collector total-collector-hash subtotal-style column-vector export?)
    (let* ( (row (get-row-in-array split))
        (acct (cadddr (array-ref guid-array row 0)))
        (depth (gnc-account-get-current-depth acct)))
        (if (> depth 2)
   ; (if no-grand-total?
    (comp-add-subtotal-row-account table width
                      (total-string (get-acctname-trans-depth 2 split))
                      total-collector total-collector-hash subtotal-style export?)
    )));)

(define (comp-render-corresponding-account-subtotal
         table width split the-key  total-collector total-collector-hash subtotal-style column-vector export?)
    (comp-add-subtotal-row table width
                      (total-string the-key)
                    total-collector total-collector-hash subtotal-style export?))

(define (comp-render-week-subtotal
     table width split the-key total-collector total-collector-hash subtotal-style column-vector export?)
  (let ((tm (get-date-tm split)))
    (comp-add-subtotal-row table width
              (total-string (gnc:date-get-week-year-string tm))
              total-collector total-collector-hash subtotal-style export?)))

(define (comp-render-month-subtotal
         table width split the-key total-collector total-collector-hash subtotal-style column-vector export?)
  (let ((tm (get-date-tm split)))
    (comp-add-subtotal-row table width
                      (total-string (gnc:date-get-month-year-string tm))
                      total-collector total-collector-hash subtotal-style export?)))


(define (comp-render-quarter-subtotal
         table width split the-key total-collector total-collector-hash subtotal-style column-vector export?)
  (let ((tm (get-date-tm split)))
    (comp-add-subtotal-row table width
                      (total-string (gnc:date-get-quarter-year-string tm))
                     total-collector total-collector-hash subtotal-style export?)))

(define (comp-render-year-subtotal
         table width split the-key total-collector total-collector-hash subtotal-style column-vector export?)
  (let ((tm (get-date-tm split)))
    (comp-add-subtotal-row table width
                      (total-string (strftime "%Y" tm))
                      total-collector total-collector-hash subtotal-style export?)))


(define (comp-render-grand-total
         table width total-collector total-collector-hash export?)
  (comp-add-subtotal-row table width
                    (_ "Grand Total")
                    total-collector total-collector-hash def:grand-total-style export?))

;; end section for consolidating


;;following part of original transaction.scm
;; The option-values of the sorting key multichoice option, for
;; which a subtotal should be enabled.
(define subtotal-enabled '(account-name
                           account-code
                           corresponding-acc-name
                           corresponding-acc-code))

(define (split-account-full-name-same-p a b)
  (= (xaccSplitCompareAccountFullNames a b) 0))

(define (split-account-code-same-p a b)
  (= (xaccSplitCompareAccountCodes a b) 0))

(define (split-same-corr-account-full-name-p a b)
  (= (xaccSplitCompareOtherAccountFullNames a b) 0))

(define (split-same-corr-account-code-p a b)
  (= (xaccSplitCompareOtherAccountCodes a b) 0))

(define (timepair-same-year tp-a tp-b)
  (= (gnc:timepair-get-year tp-a)
     (gnc:timepair-get-year tp-b)))

(define (timepair-same-quarter tp-a tp-b)
  (and (timepair-same-year tp-a tp-b)
       (= (gnc:timepair-get-quarter tp-a)
          (gnc:timepair-get-quarter tp-b))))

(define (timepair-same-month tp-a tp-b)
  (and (timepair-same-year tp-a tp-b)
       (= (gnc:timepair-get-month tp-a)
          (gnc:timepair-get-month tp-b))))

(define (timepair-same-week tp-a tp-b)
  (and (timepair-same-year tp-a tp-b)
       (= (gnc:timepair-get-week tp-a)
      (gnc:timepair-get-week tp-b))))

(define (split-same-week-p a b)
  (let ((tp-a (gnc-transaction-get-date-posted (xaccSplitGetParent a)))
    (tp-b (gnc-transaction-get-date-posted (xaccSplitGetParent b))))
    (timepair-same-week tp-a tp-b)))

(define (split-same-month-p a b)
  (let ((tp-a (gnc-transaction-get-date-posted (xaccSplitGetParent a)))
        (tp-b (gnc-transaction-get-date-posted (xaccSplitGetParent b))))
    (timepair-same-month tp-a tp-b)))

(define (split-same-quarter-p a b)
  (let ((tp-a (gnc-transaction-get-date-posted (xaccSplitGetParent a)))
        (tp-b (gnc-transaction-get-date-posted (xaccSplitGetParent b))))
    (timepair-same-quarter tp-a tp-b)))

(define (split-same-year-p a b)
  (let ((tp-a (gnc-transaction-get-date-posted (xaccSplitGetParent a)))
        (tp-b (gnc-transaction-get-date-posted (xaccSplitGetParent b))))
    (timepair-same-year tp-a tp-b)))

(define (set-last-row-style! table tag . rest)
  (let ((arg-list
         (cons table
               (cons (- (gnc:html-table-num-rows table) 1)
                     (cons tag rest)))))
    (apply gnc:html-table-set-row-style! arg-list)))


;; display an account name depending on the options the user has set
(define (account-namestring account show-account-code show-account-name show-account-full-name)
  ;;# on multi-line splits we can get an empty ('()) account
  (if (null? account)
        (_ "Split Transaction")
        (string-append
           ;; display account code?
           (if show-account-code
                 (string-append (xaccAccountGetCode account) " ")
                 "")
           ;; display account name?
           (if show-account-name
                 ;; display full account name?
                 (if show-account-full-name
                      (gnc-account-get-full-name account)
                      (xaccAccountGetName account))
                 ""))))


(define (total-string str) (string-append (_ "Total For ") str))


(define account-types-to-reverse-assoc-list
  (list (cons 'none '())
        (cons 'income-expense
              (list ACCT-TYPE-INCOME ACCT-TYPE-EXPENSE))
        (cons 'credit-accounts
              (list ACCT-TYPE-LIABILITY ACCT-TYPE-PAYABLE ACCT-TYPE-EQUITY
                    ACCT-TYPE-CREDIT ACCT-TYPE-INCOME))))

(define (get-account-types-to-reverse options)
    (cdr (assq (gnc:option-value
                (gnc:lookup-option options
                                   pagename-display
                                   (N_ "Sign Reverses")))
               account-types-to-reverse-assoc-list)))

(define (used-date columns-used)
  (vector-ref columns-used 0))
(define (used-reconciled-date columns-used)
  (vector-ref columns-used 1))
(define (used-num columns-used)
  (vector-ref columns-used 2))
(define (used-description columns-used)
  (vector-ref columns-used 3))
(define (used-account-name columns-used)
  (vector-ref columns-used 4))
(define (used-other-account-name columns-used)
  (vector-ref columns-used 5))
(define (used-shares columns-used)
  (vector-ref columns-used 6))
(define (used-price columns-used)
  (vector-ref columns-used 7))
(define (used-amount-single columns-used)
  (vector-ref columns-used 8))
(define (used-amount-double-positive columns-used)
  (vector-ref columns-used 9))
(define (used-amount-double-negative columns-used)
  (vector-ref columns-used 10))
(define (used-running-balance columns-used)
  (vector-ref columns-used 11))
(define (used-account-full-name columns-used)
  (vector-ref columns-used 12))
(define (used-memo columns-used)
  (vector-ref columns-used 13))
(define (used-account-code columns-used)
  (vector-ref columns-used 14))
(define (used-other-account-code columns-used)
  (vector-ref columns-used 15))
(define (used-other-account-full-name columns-used)
  (vector-ref columns-used 16))
(define (used-sort-account-code columns-used)
  (vector-ref columns-used 17))
(define (used-sort-account-full-name columns-used)
  (vector-ref columns-used 18))
(define (used-notes columns-used)
  (vector-ref columns-used 19))

(define columns-used-size 20)

(define (num-columns-required columns-used)  ; this routine is called but results don't seem to be used
  (do ((i 0 (+ i 1))
       (col-req 0 col-req))
      ((>= i columns-used-size) col-req)
    ; If column toggle is true, increase column count. But attention:
    ; some toggles only change the meaning of another toggle. Don't count these modifier toggles
    (if (and (not (= i 12)) ; Skip Account Full Name toggle - modifies Account Name column
             (not (= i 16)) ; Skip Other Account Full Name toggle - modifies Other Account Name column
             (not (= i 17)) ; Skip Sort Account Code - modifies Account Name subheading
             (not (= i 18)) ; Skip Sort Account Full Name - modifies Account Name subheading
             (not (= i 19)) ; Skip Note toggle - modifies Memo column
             (vector-ref columns-used i))
      (set! col-req (+ col-req 1)))
    ; Account Code and Account Name share one column so if both were ticked the
    ; the check above would have set up one column too much. The check below
    ; will compensate these again.
    (if (or (and (= i 14) (vector-ref columns-used 14) (vector-ref columns-used 4)) ; Account Code and Name
            (and (= i 15) (vector-ref columns-used 15) (vector-ref columns-used 5))) ; Other Account Code and Name
       (set! col-req (- col-req 1)))
      ))

(define (build-column-used options)
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option options section name)))
  (let ((column-list (make-vector columns-used-size #f)))
    (if (opt-val pagename-display (N_ "Description"))
        (vector-set! column-list 3 #t))
    (if (opt-val pagename-display (N_ "Account Name"))
        (vector-set! column-list 4 #t))
    (if (opt-val pagename-display (N_ "Other Account Name"))
        (vector-set! column-list 5 #t))
 ;   (if (opt-val pagename-display (N_ "Shares"))
 ;       (vector-set! column-list 6 #t))
;    (if (opt-val pagename-display (N_ "Price"))
;        (vector-set! column-list 7 #t))
    (vector-set! column-list 8 #t) ; set as always single
   ; (if (opt-val pagename-display (N_ "Running Balance"))
   ;     (vector-set! column-list 11 #t))
    (if (opt-val pagename-display  (N_ "Use Full Account Name"))
        (vector-set! column-list 12 #t))
    (if (opt-val pagename-display (N_ "Memo"))
        (vector-set! column-list 13 #t))
    (if (opt-val pagename-display (N_ "Account Code"))
        (vector-set! column-list 14 #t))
    (if (opt-val pagename-display (N_ "Other Account Code"))
        (vector-set! column-list 15 #t))
    (if (opt-val pagename-display (N_ "Use Full Other Account Name"))
        (vector-set! column-list 16 #t))
    (if (opt-val pagename-sorting (N_ "Show Account Code"))
        (vector-set! column-list 17 #t))
    (if (opt-val pagename-sorting (N_ "Show Full Account Name"))
        (vector-set! column-list 18 #t))
    (if (opt-val pagename-display (N_ "Notes"))
        (vector-set! column-list 19 #t))
    column-list))

(define (make-heading-list column-vector options)
  (let ((heading-list '()))
    (if (used-account-code column-vector)
        (addto! heading-list (_ "Account Code")))
   (if (used-account-name column-vector)
        (addto! heading-list (_ "Account")))
    (if (used-description column-vector)
        (addto! heading-list (_ "Description")))
    (if (used-memo column-vector)
        (if (used-notes column-vector)
            (addto! heading-list (string-append (_ "Memo") "/" (_ "Notes")))
            (addto! heading-list (_ "Memo"))))
    (if (or (used-other-account-name column-vector) (used-other-account-code column-vector))
        (addto! heading-list (_ "Transfer from/to")))
    (if (used-shares column-vector)
        (addto! heading-list (_ "Shares")))
    (if (used-price column-vector)
        (addto! heading-list (_ "Price")))
    (if (used-amount-single column-vector)
       ; (addto! heading-list (_ "Amount")))
        (addto!  heading-list
        (gnc:make-html-table-cell/markup
                       "column-heading-center"
                  (array-ref amounts-array
                            0 1
                             ))))
    ;; FIXME: Proper labels: what?
    (if (used-amount-double-positive column-vector)
        (addto! heading-list (_ "Debit")))
    (if (used-amount-double-negative column-vector)
        (addto! heading-list (_ "Credit")))
    (if (used-running-balance column-vector)
        (addto! heading-list (_ "Balance")))
    (set! columns-headings (length heading-list))
    ;(if compare?
    (let* ( (col 2))
            (while (<= col last-column)
        (addto!  heading-list
        (gnc:make-html-table-cell/markup
                       "column-heading-center"
                (array-ref amounts-array
                            0 col
                             )))
        (set! col (+ col 1))))
;    )
    (reverse heading-list)))
;;

;;
(define (make-heading-days table row-style)
  (let ((heading-list '())
        (col 2))
        (addto!  heading-list
            (gnc:make-html-text (gnc:html-markup-b
                (_ text-number-days))))
        (while (< col columns-headings)
            (addto!  heading-list
                (_ " "))
            (set! col (+ col 1)))

    (let* ( (col 1))
        (while (<= col last-column)
            (addto!  heading-list
                (gnc:make-html-table-cell/markup
                  "column-heading-right"
                (display-num-days
                (array-ref amounts-array
                            1 col
                             ))))
            (set! col (+ col 1))))
    (gnc:html-table-append-row! table
    (reverse heading-list)))
)

(define (make-heading-scale table row-style)
  (let ((heading-list '())
        (col 2))
        (addto!  heading-list
            (gnc:make-html-text (gnc:html-markup-b
                (if compare-scale-automatically?

                    (_ text-scaled-to)
                    (_ text-scaled-by)))))
        (while (< col columns-headings)
            (addto!  heading-list
                (_ " "))
            (set! col (+ col 1)))
        (addto!  heading-list
            ;(gnc:make-html-text (gnc:html-markup-b
            (gnc:make-html-table-cell/markup
                       "column-heading-right"
                (if compare-scale-automatically?
                    (_ (number->string compare-scale-to-val))
                    (_ (string-append (vector-ref (cdr (assq scale-op-val  scale-num)) 1) (number->string scale-num-val))))))

    (let* ( (col 2))
        (while (<= col last-column)
            (addto!  heading-list
            (gnc:make-html-table-cell/markup
                  "column-heading-right"
                (if compare-scale-automatically?
                    (_ (number->string compare-scale-to-val))
                    (_ (string-append (vector-ref (cdr (assq scale-op-val  scale-num)) 1) (number->string scale-num-val))))))
                ;    (_ (number->string scale-num-val2)))))
            (set! col (+ col 1))))
    (gnc:html-table-append-row! table
    (reverse heading-list)))
)

(define (make-heading-find table row-style)

(for-each
    (lambda (r)
  (let ((heading-list '())
        (col 2))
        (addto!  heading-list
            (gnc:make-html-text (gnc:html-markup-b
                (case r
                  ((2)    (_ "Search in "))
                  ((3)    (_ "Find "))
                  ((4)    (_ "2nd string "))
                  ((5)    (_ "Search in "))
                  ((6)    (_ "Find "))
                  ((7)    (_ "3rd String"))
                  ((8)    (_ "Search in"))
                  ((9)    (_ "Find "))
                )  
           )))
        (while (< col columns-headings)
            (addto!  heading-list
                (_ " "))
            (set! col (+ col 1)))

    (let* ( (col 1))
        (while (<= col last-column)
            (addto!  heading-list
                (gnc:make-html-table-cell/markup
                  "column-heading-right"
                (array-ref amounts-array
                            r col
                             )))
            (set! col (+ col 1))))
    (gnc:html-table-append-row! table
    (reverse heading-list)))
    )
    (list 2 3 4 5 6 7 8 9))
)

;; following section for composite/consolidate transactions
(define (add-split-row-comp table split-trans column-vector options
                       row-style account-types-to-reverse transaction-row?)

  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option options section name)))

  (let* ((row-contents '())
     (dummy  (gnc:debug "split-trans is originally" split-trans))
        (report-currency (hash-ref currency-lookup-hash (get-currency-type split-trans)))
        (currency-frac (gnc-commodity-get-fraction report-currency))
        (row-in-array (get-row-in-array split-trans))
        (split-value-ar (get-monetary split-trans 1))
        (split-value-ar-coll (if (get-reverse-sign? split-trans)
                (gnc:make-gnc-monetary report-currency (gnc-numeric-neg (gnc:gnc-monetary-amount split-value-ar)))
                split-value-ar))
        (found? #t)
        (col 1))

        (if do-findamount?
            (begin
                (set! found? #f)
                (while (<= col last-column)
                (if (find-monetary? (get-monetary split-trans col))
                ;(if (>= (gnc:gnc-numeric-num (gnc:gnc-monetary-amount (get-monetary split-trans col))) find-min )
                    (set! found? #t))
        (set! col (+ col 1)))
        ))

    (if (used-account-code column-vector)
        (addto! row-contents (get-acctcode split-trans)))
    (if (used-account-name column-vector)
       (addto! row-contents (indent-names table row-style split-trans)))

    (if (used-description column-vector)
        (addto! row-contents
                (if transaction-row?
                    (gnc:make-html-table-cell/markup "text-cell"
                       (if description-titlecase?
                        (string-titlecase (get-description-verbatim row-in-array))
                        (get-description-verbatim row-in-array)))
                     " ")))

    (if (used-memo column-vector)
        (addto! row-contents
            (get-memo split-trans)
            ))


    (if (or (used-other-account-name column-vector) (used-other-account-code column-vector))
       (addto! row-contents (get-other-name split-trans)))

    (if (used-shares column-vector)
        (addto! row-contents " "))

    (if (used-price column-vector)
        (addto!
         row-contents
         " "))

    (if (used-amount-single column-vector);; always true

            (let* ( (colmn 1))
            (while (<= colmn last-column)
                (addto! row-contents
                ; the amount is printed here
                (gnc:make-html-table-cell/markup "number-cell"
                    (gnc:comp-html-transaction-anchor
                        row-in-array colmn
                        (get-monetary split-trans colmn))))
                (set! colmn (+ colmn 1))))
        )
    (if found?
    (gnc:html-table-append-row/markup! table row-style
                                       (reverse row-contents)))
    split-value-ar-coll))

;end section -consolite

(define date-sorting-types (list 'date 'exact-time 'register-order))

(define (trep-options-generator)
  (define gnc:*transaction-report-options* (gnc:new-options))
  (define (gnc:register-trep-option new-option)
    (gnc:register-option gnc:*transaction-report-options* new-option))







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
     ; changed add-option to gnc:register-trep-option
(let ((periodoptions gnc:*transaction-report-options*));; see gnc:register option above for reason for entry
    ;;;;;;;;;;;;;;;;;;;;;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (make-col-tab pagename-col show-col-a? col-a-text show-col-b? col-b-text)
    (gnc:register-trep-option
    (gnc:make-complex-boolean-option
     pagename-col (N_ show-col-a?)
    "b" (N_ "Only show entries (could be descriptions, accounts,... what ever is choosen to search on) containing the string") #f
      #f
    (lambda (x)
        (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-col col-a-text x)
         (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-col optname-a-find1-field x)
        (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-col optname-a-find1-text x)
         (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-col optname-a-find2-operand x)
         (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-col optname-a-find2-field x)
        (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-col optname-a-find2-text x)
        (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-col optname-a-find3-operand x)
         (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-col optname-a-find3-field x)
        (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-col optname-a-find3-text
         x)
         )
    ))

       (gnc:register-trep-option
     (gnc:make-string-option
      pagename-col col-a-text
      "c" (N_ "Column heading ") (N_ "")))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      pagename-col  optname-a-find1-field
      "d1" (N_ "Select which field or category to use")
      'description
      list-findchoices))

    (gnc:register-trep-option
     (gnc:make-string-option
      pagename-col optname-a-find1-text
      "d2" (N_ "text to look for (all transactions have a space added at front so ' a' will include all which start with a ") (N_ "")))


;
;     (let ((periodoptions gnc:*transaction-report-options*))
    (gnc:register-trep-option
;        (gnc:make-multichoice-callback-option
        (gnc:make-multichoice-option
        pagename-col optname-a-find2-operand
        "d3" (N_ "Specify how to use the second text string")
        'none
        list-find2-operands))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      pagename-col  optname-a-find2-field
      "d4" (N_ "Select which field or category to use")
      'description
      list-findchoices))

        (gnc:register-trep-option
     (gnc:make-string-option
      pagename-col optname-a-find2-text
      "d5" (N_ "text to look for  ") (N_ "")))

      (gnc:register-trep-option
;        (gnc:make-multichoice-callback-option
        (gnc:make-multichoice-option
        pagename-col optname-a-find3-operand
        "e1" (N_ "Specify how to use the third text string.")
        'none
        list-find3-operands))


    (gnc:register-trep-option
     (gnc:make-multichoice-option
      pagename-col  optname-a-find3-field
      "e2" (N_ "Select which field or category to use")
      'description
      list-findchoices))

        (gnc:register-trep-option
     (gnc:make-string-option
      pagename-col optname-a-find3-text
      "e3" (N_ "text to look for  ") (N_ "")))

 ;     ; add pick for multiply or divide
 ;   (gnc:register-trep-option
 ;       (gnc:make-multichoice-option pagename-col (N_ "Scale Compare Period Results")
 ;       "ci" (N_ "Scale all of the compare periods values - multiply or divide by scale factor -  Also consider scaling to a number of days - see option below") '*
 ;       gnc:list-operands
 ;       ))
 ;   ; add where number for multiply or divide can be changed
 ;   (gnc:register-trep-option
 ;       (gnc:make-number-range-option pagename-col (N_ "Scale Number Option")
 ;       "cj" (N_ "Number to multiply or divide by")
 ;           1.0     ;; default
 ;           1       ;; lower bound
 ;         366.0     ;; upper bound
 ;           2.0     ;; number of decimals
 ;           1.0    ;; step size
 ;       ))

      ;;
    (gnc:register-trep-option
    (gnc:make-complex-boolean-option
     pagename-col (N_ show-col-b?)
    "g" (N_ "Only show entries (could be descriptions, accounts,... what ever is choosen to search on) containing the string") #f
      #f
    (lambda (x)
        (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-col col-b-text x)
        (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-col optname-b-find1-field x)
        (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-col optname-b-find1-text x)
         (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-col optname-b-find2-operand x)
         (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-col optname-b-find2-field x)
        (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-col optname-b-find2-text x)
        (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-col optname-b-find3-operand x)
         (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-col optname-b-find3-field x)
        (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-col optname-b-find3-text
         x)
         )
    ))

       (gnc:register-trep-option
     (gnc:make-string-option
      pagename-col col-b-text
      "h" (N_ "Column heading ") (N_ "")))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      pagename-col  optname-b-find1-field
      "i1" (N_ "Select which field or category to use")
      'description
      list-findchoices))

    (gnc:register-trep-option
     (gnc:make-string-option
      pagename-col optname-b-find1-text
      "i2" (N_ "text to look for (all transactions have a space added at front so ' a' will include all which start with a ") (N_ "")))


;
;     (let ((periodoptions gnc:*transaction-report-options*))
    (gnc:register-trep-option
;        (gnc:make-multichoice-callback-option
        (gnc:make-multichoice-option
        pagename-col optname-b-find2-operand
        "i3" (N_ "Specify how to use the second text string")
        'none
        list-find2-operands))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      pagename-col  optname-b-find2-field
      "i4" (N_ "Select which field or category to use")
      'description
      list-findchoices))

        (gnc:register-trep-option
     (gnc:make-string-option
      pagename-col optname-b-find2-text
      "i5" (N_ "text to look for  ") (N_ "")))

      (gnc:register-trep-option
;        (gnc:make-multichoice-callback-option
        (gnc:make-multichoice-option
        pagename-col optname-b-find3-operand
        "j1" (N_ "Specify how to use the third text string.")
        'none
        list-find3-operands))


    (gnc:register-trep-option
     (gnc:make-multichoice-option
      pagename-col  optname-b-find3-field
      "j2" (N_ "Select which field or category to use")
      'description
      list-findchoices))

        (gnc:register-trep-option
     (gnc:make-string-option
      pagename-col optname-b-find3-text
      "j3" (N_ "text to look for  ") (N_ "")))
)
    ;;;;;;;;;;;;;;;;;;;;;;;

    (make-col-tab pagename-col-1-2 optname-show-col1? optname-col1-text optname-show-col2? optname-col2-text)
    (make-col-tab pagename-col-3-4 optname-show-col3? optname-col3-text optname-show-col4? optname-col4-text)
    (make-col-tab pagename-col-5-6 optname-show-col5? optname-col5-text optname-show-col6? optname-col6-text)
    (make-col-tab pagename-col-7-8 optname-show-col7? optname-col7-text optname-show-col8? optname-col8-text)
    (make-col-tab pagename-col-9-a optname-show-col9? optname-col9-text optname-show-cola? optname-cola-text)

    (gnc:register-trep-option
        (gnc:make-multichoice-callback-option
    ;    (gnc:make-multichoice-option
        the_tab (N_ text-whichperiod)
        "ca" (N_ "Select which time period to use")
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
    ))
     ; add  custom date
    (gnc:options-add-date-interval!
     gnc:*transaction-report-options* the_tab
        custom-from-date custom-to-date "cb")

     ;  add pick year for specific period
    (gnc:register-trep-option
        (gnc:make-multichoice-option the_tab (N_ text-pick-year)
        "ce" (N_ "Pick the year for report") 'this-yr
        gnc:list-years
        ))
     ;  add pick specific period
    (gnc:register-trep-option
        (gnc:make-multichoice-option the_tab (N_ text-period)
        "cf" (N_ "Pick portion of the year for report") 'fullyear
        gnc:list-periods
        ))
    ; add pick specific last XX
    (gnc:register-trep-option
        (gnc:make-multichoice-option the_tab (N_ text-last)
        "cg" (N_ "Pick portion of the year for report") 'lastmonth
        gnc:list-lasts
        ))
      ; add pick specific month
    (gnc:register-trep-option
        (gnc:make-multichoice-option the_tab (N_ text-month)
        "ch" (N_ "Pick which month for report") '4
        gnc:list-months
        ))
      ; add pick for multiply or divide
    (gnc:register-trep-option
        (gnc:make-multichoice-option the_tab (N_ "Scale Results")
        "ci" (N_ "Scale the base periods results - multiply or divide by scale factor  NOTE Consider scaling ALL periods to a given number of days using option in middle of Compare tab") '*
        gnc:list-operands
        ))
    ; add where number for multiply or divide can be changed
    (gnc:register-trep-option
        (gnc:make-number-range-option the_tab (N_ "Scale Number Option")
        "cj" (N_ "Number to multiply or divide by")
            1.0     ;; default
            1       ;; lower bound
          366.0     ;; upper bound
            2.0     ;; number of decimals
            1.0    ;; step size
        ))

    (gnc:register-trep-option
        (gnc:make-simple-boolean-option
            pagename-base optname-scale-automatically?
            "cm" (N_ "Scale all periods to number of days given below") #f))

    (gnc:register-trep-option
        (gnc:make-number-range-option pagename-base optname-scale-to
        "cn" (N_ "Scale all periods including the base to this number of days")
          365.0     ;; default
            1       ;; lower bound
          366.0     ;; upper bound
            0       ;; number of decimals
            1.0     ;; step size
        ))
;;end of section 3 for gnctimeperiod-utilities
;;
     ;add  select custom date or a specific period
     ; changed add-option to gnc:register-trep-option



    (gnc:register-trep-option
    (gnc:make-simple-boolean-option
        pagename-base optname-show-base?
        "a2" (N_ "If unchecked the base period will not be displayed") #f))


  (gnc:register-trep-option
   (gnc:make-complex-boolean-option
    pagename-general optname-common-currency
    "e" (N_ "Convert all transactions into a common currency.") #f
    #f
    (lambda (x) (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-general
         optname-currency
         x))
    ))

  (gnc:options-add-currency!
   gnc:*transaction-report-options* pagename-general optname-currency "f")

  (gnc:register-trep-option
   (gnc:make-simple-boolean-option
    pagename-general optname-table-export
    "g" (N_ "Formats the table suitable for cut & paste exporting with extra cells.") #f))

  ;; Accounts options

  ;; account to do report on
  (gnc:register-trep-option
   (gnc:make-account-list-option
    pagename-accounts (N_ "Accounts")
    "a" (N_ "Report on these accounts.")
    ;; select, by default, no accounts! Selecting all accounts will
    ;; always imply an insanely long waiting time upon opening, and it
    ;; is almost never useful. So we instead display the normal error
    ;; message saying "Click here", and the user knows how to
    ;; continue.
    (lambda ()
      '())
    #f #t))

  (gnc:register-trep-option
   (gnc:make-account-list-option
    pagename-accounts (N_ "Filter By...")
    "b" (N_ "Filter on these accounts.")
    (lambda ()
      ;; FIXME : gnc:get-current-accounts disappeared.
      (let* ((current-accounts '())
         (root (gnc-get-current-root-account))
         (num-accounts (gnc-account-n-children root))
         (first-account (gnc-account-nth-child root 0)))
    (cond ((not (null? current-accounts))
           (list (car current-accounts)))
          ((> num-accounts 0) (list first-account))
          (else '()))))
    #f #t))

  (gnc:register-trep-option
   (gnc:make-multichoice-option
    pagename-accounts (N_ "Filter Type")
    "c" (N_ "Filter account.")
    'none
    (list (vector 'none
          (N_ "None")
          (N_ "Do not do any filtering."))
      (vector 'include
          (N_ "Include Transactions to/from Filter Accounts")
          (N_ "Include transactions to/from filter accounts only."))
      (vector 'exclude
          (N_ "Exclude Transactions to/from Filter Accounts")
          (N_ "Exclude transactions to/from all filter accounts."))
      )))

  ;;

  (gnc:register-trep-option
   (gnc:make-multichoice-option
    pagename-accounts optname-void-transactions
    "d" (N_ "How to handle void transactions.")
    'non-void-only
    (list (vector
       'non-void-only
       (N_ "Non-void only")
       (N_ "Show only non-voided transactions."))
      (vector
       'void-only
       (N_ "Void only")
       (N_ "Show only voided transactions."))
      (vector
       'both
       (N_ "Both")
       (N_ "Show both (and include void transactions in totals).")))))

  ;; Sorting options

  (let ((options gnc:*transaction-report-options*)

        (key-choice-list
         (if (qof-book-use-split-action-for-num-field (gnc-get-current-book))
             (list (vector 'none
                           (N_ "None")
                           (N_ "Do not sort."))

                   (vector 'account-name
                           (N_ "Account Name")
                           (N_ "Sort & subtotal by account name."))

                   (vector 'account-code
                           (N_ "Account Code")
                           (N_ "Sort & subtotal by account code."))

                   (vector 'date
                           (N_ "Date")
                           (N_ "Sort by date."))

                   (vector 'corresponding-acc-name
                           (N_ "Other Account Name")
                           (N_ "Sort by account transferred from/to's name."))

                   (vector 'corresponding-acc-code
                           (N_ "Other Account Code")
                           (N_ "Sort by account transferred from/to's code."))

                   (vector 'amount
                           (N_ "Amount")
                           (N_ "Sort by amount."))

                   (vector 'description
                           (N_ "Description")
                           (N_ "Sort by description."))

                   (vector 'memo
                           (N_ "Memo")
                           (N_ "Sort by memo.")))
             (list (vector 'none
                           (N_ "None")
                           (N_ "Do not sort."))

                   (vector 'account-name
                           (N_ "Account Name")
                           (N_ "Sort & subtotal by account name."))

                   (vector 'account-code
                           (N_ "Account Code")
                           (N_ "Sort & subtotal by account code."))

                   (vector 'date
                           (N_ "Date")
                           (N_ "Sort by date."))

                   (vector 'corresponding-acc-name
                           (N_ "Other Account Name")
                           (N_ "Sort by account transferred from/to's name."))

                   (vector 'corresponding-acc-code
                           (N_ "Other Account Code")
                           (N_ "Sort by account transferred from/to's code."))

                   (vector 'amount
                           (N_ "Amount")
                           (N_ "Sort by amount."))

                   (vector 'description
                           (N_ "Description")
                           (N_ "Sort by description."))

                   (vector 'memo
                           (N_ "Memo")
                           (N_ "Sort by memo.")))))

        (ascending-choice-list
         (list
          (vector 'ascend
                  (N_ "Ascending")
                  (N_ "Smallest to largest, earliest to latest."))
          (vector 'descend
                  (N_ "Descending")
                  (N_ "Largest to smallest, latest to earliest."))))

        (subtotal-choice-list
         (list
          (vector 'none (N_ "None") (N_ "None."))
          (vector 'weekly (N_ "Weekly") (N_ "Weekly."))
          (vector 'monthly (N_ "Monthly") (N_ "Monthly."))
          (vector 'quarterly (N_ "Quarterly") (N_ "Quarterly."))
          (vector 'yearly (N_ "Yearly") (N_ "Yearly.")))))

    ;; primary sorting criterion
    (gnc:register-trep-option
     (gnc:make-multichoice-callback-option
      pagename-sorting optname-prime-sortkey
      "a" (N_ "Sort by this criterion first.")
      'account-name
      key-choice-list #f
      (lambda (x)
        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-prime-subtotal
         (and (member x subtotal-enabled) #t))
        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-prime-date-subtotal
         (if (member x date-sorting-types) #t #f)))))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      pagename-sorting (N_ "Show Full Account Name")
      "a1"
      (N_ "Show the full account name for subtotals and subtitles?")
      #f))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      pagename-sorting (N_ "Show Account Code")
      "a2"
      (N_ "Show the account code for subtotals and subtitles?")
      #f))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      pagename-sorting optname-prime-subtotal
      "c"
      (N_ "Subtotal according to the primary key?")
      #f))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      pagename-sorting optname-prime-date-subtotal
      "d" (N_ "Do a date subtotal.")
      'monthly
      subtotal-choice-list))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      pagename-sorting (N_ "Primary Sort Order")
      "e" (N_ "Order of primary sorting.")
      'ascend
      ascending-choice-list))

    ;; Secondary sorting criterion
    (gnc:register-trep-option
     (gnc:make-multichoice-callback-option
      pagename-sorting optname-sec-sortkey
      "f"
      (N_ "Sort by this criterion second.")
      'none
      key-choice-list #f
      (lambda (x)
        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-sec-subtotal
         (and (member x subtotal-enabled) #t))
        (gnc-option-db-set-option-selectable-by-name
         options pagename-sorting optname-sec-date-subtotal
         (if (member x date-sorting-types) #t #f)))))

    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      pagename-sorting optname-sec-subtotal
      "g"
      (N_ "Subtotal according to the secondary key?")
      #t))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      pagename-sorting optname-sec-date-subtotal
      "h" (N_ "Do a date subtotal.")
      'yearly
      subtotal-choice-list))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      pagename-sorting (N_ "Secondary Sort Order")
      "i" (N_ "Order of Secondary sorting.")
      'ascend
      ascending-choice-list))


;;for find
 (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      pagename-base (N_ optname-find-min)
      "j1"
      (N_ "Only show lines where at least one entry is an amount greater than or equal to")
     #f))

    (gnc:register-trep-option
        (gnc:make-number-range-option pagename-base (N_ "Min Amount")
        "j2" (N_ "Minimum amount to show")
            100.0   ;; default
        -900000.0       ;; lower bound
        4000000.0   ;; upper bound
            2.0     ;; number of decimals
           100.0    ;; step size
        ))


    (gnc:register-trep-option
     (gnc:make-simple-boolean-option
      pagename-base (N_ optname-find-max)
      "j3"
      (N_ "Only show lines where at least one entry is less than or equal")
      #f))

    (gnc:register-trep-option
        (gnc:make-number-range-option pagename-base (N_ "Max Amount")
        "j4" (N_ "Maximum amount to show")
            20000.0  ;; default
          -800000.0  ;; lower bound
          5000000.0 ;; upper bound
            2.0     ;; number of decimals
          100.0     ;; step size
        ))

      ;;
    (gnc:register-trep-option
    (gnc:make-complex-boolean-option
     pagename-base (N_ optname-find-text?)
    "k1" (N_ "Only show entries (could be descriptions, accounts,... what ever is choosen to search on) containing the string") #f
      #f
    (lambda (x)
        (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-base optname-find1-field x)
        (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-base optname-find1-text x)
         (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-base optname-find2-operand x)
         (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-base optname-find2-field x)
        (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-base optname-find2-text x)
        (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-base optname-find3-operand x)
         (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-base optname-find3-field x)
        (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-base optname-find3-text
         x)
         )
    ))

    (gnc:register-trep-option
     (gnc:make-multichoice-option
      pagename-base  optname-find1-field
      "k2" (N_ "Select which field or category to use")
      'description
      list-findchoices))

    (gnc:register-trep-option
     (gnc:make-string-option
      pagename-base optname-find1-text
      "k3" (N_ "text to look for (all transactions have a space added at front so ' a' will include all which start with a ") (N_ "")))


;
;     (let ((periodoptions gnc:*transaction-report-options*))
    (gnc:register-trep-option
;        (gnc:make-multichoice-callback-option
        (gnc:make-multichoice-option
        pagename-base optname-find2-operand
        "k4" (N_ "Specify how to use the second text string")
        'none
        list-find2-operands))
;        list-findchoices #f
;        (lambda (x)
;        (gnc-option-db-set-option-selectable-by-name
;         gnc:*transaction-report-options*
 ;        pagename-base (N_ text-pick-year)
 ;        (if (equal? x 'customdates) #f #t))
;        (gnc-option-db-set-option-selectable-by-name
 ;         gnc:*transaction-report-options*
;        pagename-base (N_ text-period)
 ;        (if (equal? x 'period) #t #f))
;        (gnc-option-db-set-option-selectable-by-name
;         periodoptions the_tab (N_ text-last)
;         (if (equal? x 'last) #t #f))
;        (gnc-option-db-set-option-selectable-by-name
 ;        periodoptions the_tab (N_ text-month)
  ;       (if (equal? x 'month) #t #f))
;        ))
;    ))
;
;


    (gnc:register-trep-option
     (gnc:make-multichoice-option
      pagename-base  optname-find2-field
      "k5" (N_ "Select which field or category to use")
      'description
      list-findchoices))

        (gnc:register-trep-option
     (gnc:make-string-option
      pagename-base optname-find2-text
      "k6" (N_ "text to look for  ") (N_ "")))

      (gnc:register-trep-option
;        (gnc:make-multichoice-callback-option
        (gnc:make-multichoice-option
        pagename-base optname-find3-operand
        "m1" (N_ "Specify how to use the third text string.")
        'none
        list-find3-operands))


    (gnc:register-trep-option
     (gnc:make-multichoice-option
      pagename-base  optname-find3-field
      "m2" (N_ "Select which field or category to use")
      'description
      list-findchoices))

        (gnc:register-trep-option
     (gnc:make-string-option
      pagename-base optname-find3-text
      "m3" (N_ "text to look for  ") (N_ "")))

   ;;;;;;;;;;;;;;;;;;;;;;;;;

 )


  ;; Display options

  (for-each
   (lambda (l)
     (gnc:register-trep-option
      (gnc:make-simple-boolean-option
       pagename-display (car l) (cadr l) (caddr l) (cadddr l))))
   ;; One list per option here with: option-name, sort-tag,
   ;; help-string, default-value
   (list
  (list optname-days                         "a1"  (N_ "Display number of days in each period") #t)
    (list optname-show-find                    "a2"  opthelp-show-find #f)
    (list optname-show-average                 "a3"  opthelp-show-average #t)
    (list optname-show-totalcol                "a4"  opthelp-show-totalcol #t)
    (list optname-show-assets                  "a5"  optname-show-assets-help #t)
    (list optname-show-liabilities             "a7"  optname-show-liabilities-help #t)
    (list optname-show-income                  "a9"  optname-show-income-help #t)
    (list optname-show-expenses                "aa"  optname-show-expenses-help #t)
    (list optname-show-equities                "ac"  optname-show-equities-help #t)
    (list optname-show-imbalance              "b2"    opthelp-show-imbalance #t)
    (list optname-consolidate-case-sensitive  "b4"  (N_ "when not checked \"A\" and \"a\" are considered to be the same letter in comparing descriptions") #f)
    (list (N_ "Description")                  "c"  (N_ "Display the description?") #f)
    (list optname-descript-titlecase            "s"  (N_ "Titlecase The First Letter in Each Word") #t)
  ;; note the "memo"  option in between here
    (list (N_ "Notes")                        "d2" (N_ "Display the notes if the memo is unavailable?") #t)
    (list (N_ "Account Name")                 "e"  (N_ "Display the account name?") #t)
    (list (N_ "Use Full Account Name")        "f"  (N_ "Display the full account name?") #t)
    (list (N_ "Account Code")                 "g"  (N_ "Display the account code?") #f)
    (list (N_ "Other Account Name")           "h"  (N_ "Display the other account name?\
 (if this is a split transaction, this parameter is guessed).") #f)
    (list (N_ "Use Full Other Account Name")  "i"  (N_ "Display the full other account name?") #t)
    (list (N_ "Other Account Code")           "j"  (N_ "Display the other account code?") #f)
 ;   (list (N_ "Shares")                       "k"  (N_ "Display the number of shares?") #f)
 ;   (list (N_ "Price")                        "l"  (N_ "Display the shares price?") #f)
 ;   (list (N_ "Running Balance")              "n"  (N_ "Display a running balance?") #f)
    (list (N_ "Totals")                       "o"  (N_ "Display the totals?") #t)
    ;; note the "sign reverse" multichoice option in between here
 ;   (list (N_ "Use old running balance")      "q0"  (N_ "Use old method of computing running balance , may need for different currencies") #f)

    ; optname-descript-titlecase is being shown here instead of line d1
      ))

  (if (qof-book-use-split-action-for-num-field (gnc-get-current-book))
      (gnc:register-trep-option
       (gnc:make-simple-boolean-option
        pagename-display (N_ "Trans Number")
                                    "b2" (N_ "Display the trans number?") #f)))

  ;; Add an option to display the memo, and disable the notes option
  ;; when memos are not included.
  (gnc:register-trep-option
   (gnc:make-complex-boolean-option
    pagename-display (N_ "Memo")
    "d"  (N_ "Display the memo?") #f
    #f
    (lambda (x) (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-display
         (N_ "Notes")
         x))))

  (gnc:register-trep-option
   (gnc:make-multichoice-option
    pagename-display (N_ "Sign Reverses")
    "p" (N_ "Reverse amount display for certain account types.")
    'income-expense
    (list
     (vector 'none (N_ "None") (N_ "Don't change any displayed amounts."))
     (vector 'income-expense (N_ "Income and Expense")
             (N_ "Reverse amount display for Income and Expense Accounts."))
     (vector 'credit-accounts (N_ "Credit Accounts")
             (N_ "Reverse amount display for Liability, Payable, Equity, \
Credit Card, and Income accounts.")))))


  (gnc:options-set-default-section gnc:*transaction-report-options*
                                   pagename-base)

  gnc:*transaction-report-options*)


(define (display-date-interval-find begin end findtitle)
  (let ((begin-string (gnc-print-date begin))
        (end-string (gnc-print-date end)))
    (sprintf #f (_ "From %s To %s%s") begin-string end-string findtitle)))

(define (display-find findtitle)
    (sprintf #f (_ "%s") findtitle ))

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

(define (get-primary-subtotal-style options)
  (let ((bgcolor (gnc:lookup-option options
                                    (N_ "Colors")
                                    (N_ "Primary Subtotals/headings"))))
    (list 'attribute (list "bgcolor" (gnc:color-option->html bgcolor)))))

(define (get-secondary-subtotal-style options)
  (let ((bgcolor (gnc:lookup-option options
                                    (N_ "Colors")
                                    (N_ "Secondary Subtotals/headings"))))
    (list 'attribute (list "bgcolor" (gnc:color-option->html bgcolor)))))

(define (get-grand-total-style options)
  (let ((bgcolor (gnc:lookup-option options
                                    (N_ "Colors")
                                    (N_ "Grand Total"))))
    (list 'attribute (list "bgcolor" (gnc:color-option->html bgcolor)))))

(define (get-odd-row-style options)
  (let ((bgcolor (gnc:lookup-option options
                                    (N_ "Colors")
                                    (N_ "Split Odd"))))
    (list 'attribute (list "bgcolor" (gnc:color-option->html bgcolor)))))

(define (get-even-row-style options)
  (let ((bgcolor (gnc:lookup-option options
                                    (N_ "Colors")
                                    (N_ "Split Even"))))
    (list 'attribute (list "bgcolor" (gnc:color-option->html bgcolor)))))


;; check if the transaction meets the find requirements
;;

(define (filtersplits-found splits account-types-to-reverse
                          find1-field find1-text
            find2-operand find2-field find2-text
            find3-operand find3-field find3-text)

    (define (splitfound? currentsplit )
        ;  (if (not (null? splits))

        (let* (
            (parent (xaccSplitGetParent currentsplit))
            (descript (xaccTransGetDescription parent))

            (account (xaccSplitGetAccount currentsplit))
            (account-type (xaccAccountGetType account))
            (damount (if (gnc:split-voided? currentsplit)
                            (xaccSplitVoidFormerAmount currentsplit)
                            (xaccSplitGetAmount currentsplit)))
            (currency (if (not (null? account))
                      (xaccAccountGetCommodity account)
                      (gnc-default-currency)))
            (report-currency (if comm-curr?
                                curr
                                currency))
            (trans-date (gnc-transaction-get-date-posted parent))

            (split-value (gnc:exchange-by-pricedb-nearest
                            (gnc:make-gnc-monetary
                            currency
                            (if (member account-type account-types-to-reverse)
                                (gnc-numeric-neg damount)
                                damount))
                            report-currency
                            ;; Use midday as the transaction time so it matches a price
                            ;; on the same day.  Otherwise it uses midnight which will
                            ;; likely match a price on the previous day
                            (timespecCanonicalDayTime trans-date)))
                )
;;;;;
        (define (found-text? which-field text-to-find ); based on entries in find-field-number
        (case which-field
        ((10) ; 'description
            (string-contains (string-append " " (string-upcase (xaccTransGetDescription parent) ) " ")  text-to-find))
        ((13) ; 'memo
            (string-contains (string-append " " (string-upcase (xaccSplitGetMemo currentsplit) ) " ")  text-to-find))
        ((14) ; 'notes
            (string-contains (string-append " " (string-upcase (xaccTransGetNotes parent) ) " ")  text-to-find))
        (( 1) ; 'account-name
            (string-contains (string-append " " (string-upcase (gnc-account-get-full-name account) ) " ")  text-to-find))
        (( 2) ;'accountcodee
            (string-contains (string-append " " (string-upcase (xaccAccountGetCode account) ) " ")  text-to-find))
        ((16 ) ; 'memo/notes
            (or (string-contains (string-append " " (string-upcase (xaccSplitGetMemo currentsplit) ) " ")  text-to-find) ;memo
            (string-contains (string-append " " (string-upcase (xaccTransGetNotes parent) ) " ")  text-to-find))) ;notes
        ((15) ; 'any
            (or (string-contains (string-append " " (string-upcase (xaccTransGetDescription parent) ) " ")  text-to-find) ;description
            (string-contains (string-append " " (string-upcase (xaccSplitGetMemo currentsplit) ) " ")  text-to-find) ; memo
            (string-contains (string-append " " (string-upcase (xaccTransGetNotes parent) ) " ")  text-to-find) ;notes
            (string-contains (string-append " " (string-upcase (gnc-account-get-full-name account) ) " ")  text-to-find) ;account-name
            (string-contains (string-append " " (string-upcase (xaccAccountGetCode account) ) " ")  text-to-find))) ; account-code
        ((11) ; 'number
            (or (string-contains (string-append " " (string-upcase (gnc-get-num-action parent currentsplit) ) " ")  text-to-find) ;num
                (if (gnc-get-num-action parent #f)
            (string-contains (string-append " " (string-upcase (gnc-get-num-action parent #f) ) " ")  text-to-find); Trans Number
             #f)))
        (( 3) ; 'date
            (string-contains (string-append " " (string-upcase (gnc-print-date (gnc-transaction-get-date-posted parent) )) " ")  text-to-find)) ;date
        (( 5) ; 'reconciled-date
                 (let* ((date (gnc-split-get-date-reconciled currentsplit))
                        (printed-date (if (equal? date (cons 0 0))
                                            ""
                                            (gnc-print-date date))))
            (string-contains (string-append " " (string-upcase printed-date) " ")  text-to-find))) ;reconciled date
        ((17) ; 'reconcile
            (string-contains (string-append " " (string-upcase (string (xaccSplitGetReconcile currentsplit)) ) " ")  text-to-find))

        ))
;;;;;;

                    (let* ((found1? (found-text? find1-field  find1-text))
                            (foundBoth? #f))
                        (if (equal? find2-operand 'none)
                                (set! foundBoth? found1?)
                        (if (equal? find2-operand 'and)
                               (set! foundBoth? (and found1? (found-text? find2-field  find2-text)))
                        (if (equal? find2-operand 'or)
                               (set! foundBoth? (or found1? (found-text? find2-field  find2-text)))
                        (if (equal? find2-operand 'not)
                               (set! foundBoth? (and found1? (not (found-text? find2-field  find2-text))))
                               (set! foundBoth? #f)))))

                        (if (equal? find3-operand 'none)
                                foundBoth?
                        (if (equal? find3-operand 'and)
                               (and foundBoth? (found-text? find3-field  find3-text))
                        (if (equal? find3-operand 'or)
                               (or foundBoth? (found-text? find3-field  find3-text))
                        (if (equal? find3-operand 'not)
                               (and foundBoth? (not (found-text? find3-field  find3-text)))
                               #f))))
                    )
        ))
   (filter splitfound? splits)
 )

    ;

(define (find-monetary? value-monetary) ; only handles ammount - the find for text was handled earlier
        (let ((value-num (gnc:gnc-numeric-num (gnc:gnc-monetary-amount value-monetary))))
            (and
                (or (not find-min?) (>= value-num find-min ))
                (or (not find-max?) (<= value-num find-max ))
            )
))


;;

;; ;;;;;;;;;;;;;;;;;;;;
;; for working on consolidating descriptions
;; Here comes the big function that builds the whole table.
(define (make-split-table-comp list-thekeys options
                            primary-comp-key secondary-comp-key
                          comp-primary-subtotal-pred
                          comp-secondary-subtotal-pred
                          comp-primary-subheading-renderer
                          comp-secondary-subheading-renderer
                          comp-primary-subtotal-renderer
                          comp-secondary-subtotal-renderer)


 (let ((work-to-do (length list-thekeys))
       (work-done 0)
       (used-columns (build-column-used options)))

  (define (transaction-report-multi-rows-p options)
  ;  (eq? (gnc:option-value
  ;        (gnc:lookup-option options pagename-general (N_ "Style")))
  ;       'multi-line)
         #f
         )

  (define (transaction-report-export-p options)
    (gnc:option-value
     (gnc:lookup-option options pagename-general
       optname-table-export)))
   (define (msg-line-location table msg)
   (gnc:html-table-append-row/markup! table def:grand-total-style ;row-style
                (gnc:make-html-text
                  (gnc:html-markup-h3
                  msg))))
  (define (print-type-totals table acct-type total-collector total-collector-hash
                             type-total-collector type-total-collector-hash
                             width export?) ; def:grand-total-style )
      (let* (
          (heading-list '() )
          )
    (if  (not (equal? total-current-type acct-type))
        (begin
          (gnc:html-table-append-row/markup!
           table
           def:grand-total-style
           (list
            (gnc:make-html-table-cell/size
             1 width (gnc:make-html-text (gnc:html-markup-hr)))))

           ;;determine which type of account
           ;;save to this type of account the totals
      (if (gnc:option-value (gnc:lookup-option options pagename-display "Totals"))
            (begin
                (comp-add-subtotal-row table width
                    (_ (string-append "&nbsp;  Total " (list-ref type-namelist total-current-type)))
                            total-collector total-collector-hash def:grand-total-style export?)
                 ;; check if should print total assets - liabilities  or total icome - expenses
                 ;;    or total assets - liabilities + equity
                (if (or (and (= total-current-type 1) (= previous-type 0))
                        (and (= total-current-type 3) (= previous-type 2)))
                    (begin
                        (add-rule table (+ last-column columns-headings
                            (if #f 1 0)))
                        (comp-add-subtotal-row table width
                            (_ (string-append "&nbsp;  Total " (list-ref type-namelist previous-type)
                                " - " (list-ref type-namelist total-current-type)))
                                type-total-collector type-total-collector-hash def:grand-total-style export?)
                    )
                        ;; check if should print total assets - liabilities + equity
                    (if (and (= total-current-type 4) (= previous-type 7))
                        (begin
                            (add-rule table (+ last-column columns-headings
                                (if #f 1 0)))
                            (comp-add-subtotal-row table width
                                (_ (string-append "&nbsp;  Total " (list-ref type-namelist 1)
                                    " - " (list-ref type-namelist 2)
                                    " + " (list-ref type-namelist total-current-type)))
                                type-total-collector type-total-collector-hash def:grand-total-style export?))
                    )
                 )
     ))
    (set! previous-type (if (and (= total-current-type 1) (= previous-type 0))
                            7
                            total-current-type))
    (set! total-current-type acct-type)
    #t)
    #f)
    ))

  ;;;;;;;

  (define (add-other-split-rows split split-trans table used-columns
                                row-style account-types-to-reverse)
    (define (comp-other-rows-driver split split-trans parent table used-columns i)
      (let ((current-trans split-trans)
            ;(current (xaccTransGetSplit parent i))  ;needs work?
            )
        (cond ((null? current-trans) #f)
              ((equal? current-trans split-trans) ;always true - can remove?
               (comp-other-rows-driver split split-trans parent table used-columns (+ i 1)))
              (else (begin
                      (add-split-row-comp table current-trans
                                    used-columns options
                                     row-style account-types-to-reverse #f)
                      (comp-other-rows-driver split split-trans parent table used-columns
                                         (+ i 1)))))))

    (comp-other-rows-driver split split-trans (xaccSplitGetParent split)
                       table used-columns 0))

  (define (add-to-total subtotal-hash split-trans)
  ;;
  ; now to add to the sum for each column but first need to handle case where multiple currencies
                    (let (
                        (the-currency (get-currency-type split-trans))
                        (currency (hash-ref currency-lookup-hash (get-currency-type split-trans)))
                        )
                       ;; add to total for the account column
                        (let* ( (col 1))
                        (while (<= col last-column) ;; change column
                        (hash-set! subtotal-hash (string-append (number->string col) markerB the-currency)
                            (gnc:make-gnc-monetary currency
                            (gnc-numeric-add
                            (gnc:gnc-monetary-amount
                                (get-monetary split-trans col))
                            (gnc:gnc-monetary-amount
                            (hash-ref subtotal-hash (string-append (number->string col) markerB the-currency)
                                (gnc:make-gnc-monetary currency (gnc-numeric-zero))))
                                    GNC-DENOM-AUTO GNC-RND-ROUND))
                            )
                        (set! col (+ col 1))
                    )
)
))
  ;;

  (define (do-rows-with-subtotals list-thekeys
                                  table
                                  used-columns
                                  width
                                  multi-rows?
                                  odd-row?
                                  export?
                                  account-types-to-reverse
                                  comp-primary-subtotal-pred
                                  comp-secondary-subtotal-pred
                                  comp-primary-subheading-renderer
                                  comp-secondary-subheading-renderer
                                  comp-primary-subtotal-renderer
                                  comp-secondary-subtotal-renderer
                                  primary-subtotal-collector
                                  secondary-subtotal-collector
                                  total-collector
                                  type-total-collector)

    (gnc:report-percent-done (* 100 (/ work-done work-to-do)))
    (set! work-done (+ 1 work-done))

    (if (null? list-thekeys)
        (begin
            (gnc:html-table-append-row/markup!
                table
                def:grand-total-style
                (list
                    (gnc:make-html-table-cell/size
                        1 width (gnc:make-html-text (gnc:html-markup-hr)))))
            (if (gnc:option-value (gnc:lookup-option options pagename-display "Totals"))
                (if (not no-grand-total?)
                    (comp-render-grand-total table width total-collector total-collector-hash export?)
                    (print-type-totals table #f
                              total-collector total-collector-hash
                              type-total-collector type-total-collector-hash width export? ) ; def:grand-total-style)
            ))
        )
        (let* (
            (current-trans (car list-thekeys))

            (current-row-style (if multi-rows? def:normal-row-style
                                      (if odd-row? def:normal-row-style
                                          def:alternate-row-style)))
              (rest-trans (cdr list-thekeys))
             (next-trans (if (null? rest-trans) #f
                        (car rest-trans)))
            (split-value (add-split-row-comp ;;this currently adds a row of output and then returns amount in base column
                            table
                            current-trans
                            used-columns
                            options
                            current-row-style
                            account-types-to-reverse
                            #t)))
       ;   (if multi-rows?
       ;       (add-other-split-rows
       ;        current current-trans table used-columns def:alternate-row-style
       ;        account-types-to-reverse))

          (primary-subtotal-collector 'add ;; currently adds in amount from base column
                                      (gnc:gnc-monetary-commodity
                                       split-value)
                                      (gnc:gnc-monetary-amount
                                       split-value))
           (add-to-total primary-subtotal-collector-hash current-trans)  ;adds each non-base  col to the total
          (secondary-subtotal-collector 'add  ;; currently adds in amount from base column
                                        (gnc:gnc-monetary-commodity
                                         split-value)
                                        (gnc:gnc-monetary-amount
                                         split-value))
           (add-to-total secondary-subtotal-collector-hash current-trans) ;; adds each non-base col to the total
           (add-to-total total-collector-hash current-trans)
           (add-to-total type-total-collector-hash current-trans)

          (total-collector 'add
                           (gnc:gnc-monetary-commodity split-value)
                           (gnc:gnc-monetary-amount split-value))

          (type-total-collector 'add
                           (gnc:gnc-monetary-commodity split-value)
                           (gnc:gnc-monetary-amount split-value))

          (if (and comp-primary-subtotal-pred
                   (or (not next-trans) ;;if not another transaction after this
                       (and next-trans  ;; or next transaction doesnt match same primary criteria as this transaction
                           (not (comp-primary-subtotal-pred (get-primary-key current-trans) (get-primary-key next-trans))))))

              (begin
                (if comp-secondary-subtotal-pred   ;if there is a secondary sort criteria then print out its total
                    (begin
                      (comp-secondary-subtotal-renderer ;; changed to show each columns total for secondary instead of just base
                       table width current-trans (get-secondary-key current-trans)
                       secondary-subtotal-collector
                       secondary-subtotal-collector-hash
                       def:secondary-subtotal-style used-columns export?)
                      (set! secondary-subtotal-collector-hash (make-hash-table))
                      (secondary-subtotal-collector 'reset #f #f)))
                             ;; now print out the primary sort total
                      (if (print-type-totals table (get-main-acct-type next-trans)
                              total-collector total-collector-hash
                              type-total-collector type-total-collector-hash width export? ) ; def:grand-total-style)
                        (begin
                            (total-collector 'reset #f #f)
                            (set! total-collector-hash (make-hash-table))
                            (if (and next-trans (= (get-main-acct-type next-trans) 2))
                                (begin
                                    (type-total-collector 'reset #f #f)
                                    (set! type-total-collector-hash (make-hash-table))
                            ))
                        )
                      )

                (comp-primary-subtotal-renderer table width current-trans (get-primary-key current-trans)
                                           primary-subtotal-collector
                                           primary-subtotal-collector-hash
                                           def:primary-subtotal-style used-columns
                                           export?)
                (set! primary-subtotal-collector-hash (make-hash-table))
                (primary-subtotal-collector 'reset #f #f)
                       ;if there is a new category then print out the primary heading
                (if next-trans
                    (begin
                      (comp-primary-subheading-renderer
                       next-trans (get-primary-key next-trans) table width def:primary-subtotal-style used-columns)
                            ;if there is a secondary sort then print its heading
                      (if comp-secondary-subtotal-pred
                          (comp-secondary-subheading-renderer
                           next-trans (get-secondary-key next-trans)
                           table
                           width def:secondary-subtotal-style used-columns)))))

              (if (and comp-secondary-subtotal-pred  ; primary is not changing yet , just check on secondary
                       (or (not next-trans)
                           (and next-trans  ;; if secondary sort is changing to a new "category" such as new month then print
                                (not (comp-secondary-subtotal-pred
                                      (get-secondary-key current-trans) (get-secondary-key next-trans))))))
                  (begin
                    (comp-secondary-subtotal-renderer
                          table width current-trans (get-secondary-key current-trans)
                          secondary-subtotal-collector
                          secondary-subtotal-collector-hash
                          def:secondary-subtotal-style used-columns export?)
                    (set! secondary-subtotal-collector-hash (make-hash-table))
                    (secondary-subtotal-collector 'reset #f #f)
                    (if next-trans
                        (comp-secondary-subheading-renderer
                            next-trans (get-secondary-key next-trans) table width
                            def:secondary-subtotal-style used-columns)
                  ))
               )
          )
   ; (msg-line-location table "around 2860 test 3 here")
     (if (print-type-totals table (get-main-acct-type next-trans)
                              total-collector total-collector-hash
                              type-total-collector type-total-collector-hash width export? ) ; def:grand-total-style)
            (begin
                (total-collector 'reset #f #f)
                (set! total-collector-hash (make-hash-table))
                (if (and next-trans (= (get-main-acct-type next-trans) 2))
                    (begin
                        (type-total-collector 'reset #f #f)
                        (set! type-total-collector-hash (make-hash-table))
                ))
            )
        )

   (if no-grand-total?
       (if (print-type-totals table (get-main-acct-type next-trans)
                              total-collector total-collector-hash
                              type-total-collector type-total-collector-hash width export? ) ; def:grand-total-style)
            (begin
                (total-collector 'reset #f #f)
                (set! total-collector-hash (make-hash-table))
                (if (and next-trans (= (get-main-acct-type next-trans) 2))
                    (begin
                        (type-total-collector 'reset #f #f)
                        (set! type-total-collector-hash (make-hash-table))
                ))
            )
        )
    )

          (do-rows-with-subtotals rest-trans
                                  table
                                  used-columns
                                  width
                                  multi-rows?
                                  (not odd-row?)
                                  export?
                                  account-types-to-reverse
                                  comp-primary-subtotal-pred
                                  comp-secondary-subtotal-pred
                                  comp-primary-subheading-renderer
                                  comp-secondary-subheading-renderer
                                  comp-primary-subtotal-renderer
                                  comp-secondary-subtotal-renderer
                                  primary-subtotal-collector
                                  secondary-subtotal-collector
                                  total-collector
                                  type-total-collector))))


  (let* ((table (gnc:make-html-table))
         (width (+ columns-headings 0))
         (multi-rows? (transaction-report-multi-rows-p options))
     (export? (transaction-report-export-p options))
         (account-types-to-reverse
          (get-account-types-to-reverse options)))

    (gnc:html-table-set-col-headers!
     table
    (make-heading-list used-columns options))

    (if show-days?
     (make-heading-days table def:primary-subtotal-style))
     
    (if show-find?
    (make-heading-find table def:primary-subtotal-style))
    
    (if scaled?
     (make-heading-scale table def:primary-subtotal-style))
    ;;     (gnc:warn "Split-trans:" split-transs)
    (if (not (null? list-thekeys))
        (begin
          (set! amount-total-hash (make-hash-table))
          (if comp-primary-subheading-renderer  ;;print heading above the items we will print (be they transactions or accounts or ...)
              (comp-primary-subheading-renderer
               (car list-thekeys) (get-primary-key (car list-thekeys)) table width def:primary-subtotal-style used-columns))
          (if comp-secondary-subheading-renderer
              (comp-secondary-subheading-renderer
               (car list-thekeys) (get-secondary-key (car list-thekeys)) table width def:secondary-subtotal-style used-columns))
         (set! primary-subtotal-collector-hash (make-hash-table))
         (set! secondary-subtotal-collector-hash (make-hash-table))
         (set! total-collector-hash (make-hash-table))
         (set! type-total-collector-hash (make-hash-table))
         (set! total-current-type (get-main-acct-type (car list-thekeys)))
          (do-rows-with-subtotals  list-thekeys table used-columns width
                                  multi-rows? #t
                                  export?
                                  account-types-to-reverse
                                  comp-primary-subtotal-pred
                                  comp-secondary-subtotal-pred
                                  comp-primary-subheading-renderer
                                  comp-secondary-subheading-renderer
                                  comp-primary-subtotal-renderer
                                  comp-secondary-subtotal-renderer
                                  (gnc:make-commodity-collector)
                                  (gnc:make-commodity-collector)
                                  (gnc:make-commodity-collector)
                                  (gnc:make-commodity-collector))))

    table)))



;; end of the big function for composite / consolidating

;; ;;;;;;;;;;;;;;;;;;;;
;; Here comes the renderer function for this report.
(define (trep-renderer report-obj)

  (define options (gnc:report-options report-obj))

  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option options section name)))

  ;;(define (get-other-account-names account-list)
  ;;  ( map (lambda (acct)  (gnc-account-get-full-name acct)) account-list))

  (define (is-filter-member split account-list)
    (let* ((txn (xaccSplitGetParent split))
           (splitcount (xaccTransCountSplits txn)))

      (cond
        ;; A 2-split transaction - test separately so it can be optimized
        ;; to significantly reduce the number of splits to traverse
        ;; in guile code
        ((= splitcount 2)
         (let* ((other      (xaccSplitGetOtherSplit split))
                (other-acct (xaccSplitGetAccount other))
                (acct (xaccSplitGetAccount split)))
               (or (member acct account-list) (member other-acct account-list))))

        ;; A multi-split transaction - run over all splits
        ((> splitcount 2)
         (let ((splits (xaccTransGetSplitList txn)))

                ;; Walk through the list of splits.
                ;; if we reach the end, return #f
                ;; if the 'this' != 'split' and the split->account is a member
                ;; of the account-list, then return #t, else recurse
                (define (is-member splits)
                  (if (null? splits)
                      #f
                      (let* ((this (car splits))
                             (rest (cdr splits))
                             (acct (xaccSplitGetAccount this)))
                        (if (and (not (eq? this split))
                                 (member acct account-list))
                            #t
                            (is-member rest)))))

                (is-member splits)))

        ;; Single transaction splits
        (else #f))))


    ;; for composite
    ;;
    (define comp-comp-funcs-assoc-list
    ;; Defines the different sorting keys, together with the
    ;; subtotal functions. Each entry: (cons
    ;; 'sorting-key-option-value (vector 'query-sorting-key
    ;; subtotal-function subtotal-renderer))
;;  (let* ((used-columns (build-column-used options))) ;; tpo: gives unbound variable options?
    (let* ((used-columns (build-column-used (gnc:report-options report-obj))))
      (list (cons 'account-name  (vector
                                  (list SPLIT-ACCT-FULLNAME)
                                  comp-split-account-full-name-same-p
                                  comp-render-account-subheading
                                  comp-render-account-subtotal))
            (cons 'account-code  (vector
                                  (list SPLIT-ACCOUNT ACCOUNT-CODE-)
                                  comp-split-account-code-same-p
                                  comp-render-account-subheading
                                  comp-render-account-subtotal))
            (cons 'date          (vector
                                  (list SPLIT-TRANS TRANS-DATE-POSTED)
                                  #f #f #f))
            (cons 'corresponding-acc-name
                                 (vector
                                  (list SPLIT-CORR-ACCT-NAME)
                                  comp-split-same-corr-account-full-name-p
                                  comp-render-corresponding-account-subheading
                                  comp-render-corresponding-account-subtotal))
            (cons 'corresponding-acc-code
                                 (vector
                                  (list SPLIT-CORR-ACCT-CODE)
                                  comp-split-same-corr-account-code-p
                                  comp-render-corresponding-account-subheading
                                  comp-render-corresponding-account-subtotal))
            (cons 'amount        (vector (list SPLIT-VALUE) #f #f #f))
            (cons 'description   (vector (list SPLIT-TRANS TRANS-DESCRIPTION) #f #f #f))
            (cons 'memo          (vector (list SPLIT-MEMO) #f #f #f))
            (cons 'none          (vector '() #f #f #f)))))

  (define comp-date-comp-funcs-assoc-list
    ;; Extra list for date option. Each entry: (cons
    ;; 'date-subtotal-option-value (vector subtotal-function
    ;; subtotal-renderer))
    (list
     (cons 'none (vector #f #f #f))
     (cons 'weekly (vector comp-split-same-week-p comp-render-week-subheading
                            comp-render-week-subtotal))
     (cons 'monthly (vector comp-split-same-month-p comp-render-month-subheading
                            comp-render-month-subtotal))
     (cons 'quarterly (vector comp-split-same-quarter-p comp-render-quarter-subheading
                            comp-render-quarter-subtotal))
     (cons 'yearly (vector comp-split-same-year-p comp-render-year-subheading
                           comp-render-year-subtotal))))

  (define (comp-get-subtotalstuff-helper
           name-sortkey name-subtotal name-date-subtotal
           comp-index date-index)
    ;; The value of the sorting-key multichoice option.
    (let ((sortkey (opt-val pagename-sorting name-sortkey)))
      (if (member sortkey date-sorting-types)
          ;; If sorting by date, look up the value of the
          ;; date-subtotalling multichoice option and return the
          ;; corresponding funcs in the assoc-list.
          (vector-ref
           (cdr (assq (opt-val pagename-sorting name-date-subtotal)
                      comp-date-comp-funcs-assoc-list))
           date-index)
          ;; For everything else: 1. check whether sortkey has
          ;; subtotalling enabled at all, 2. check whether the
          ;; enable-subtotal boolean option is #t, 3. look up the
          ;; appropriate funcs in the assoc-list.
          (and (member sortkey subtotal-enabled)
               (and (opt-val pagename-sorting name-subtotal)
                    (vector-ref
                     (cdr (assq sortkey comp-comp-funcs-assoc-list))
                     comp-index))))))

  (define (comp-get-query-sortkey sort-option-value)
    (vector-ref
     (cdr (assq sort-option-value comp-comp-funcs-assoc-list))
     0))

  (define (comp-get-subtotal-pred
           name-sortkey name-subtotal name-date-subtotal)
    (comp-get-subtotalstuff-helper
     name-sortkey name-subtotal name-date-subtotal
     1 0))

  (define (comp-get-subheading-renderer
           name-sortkey name-subtotal name-date-subtotal)
    (comp-get-subtotalstuff-helper
     name-sortkey name-subtotal name-date-subtotal
     2 1))

  (define (comp-get-subtotal-renderer
           name-sortkey name-subtotal name-date-subtotal)
    (comp-get-subtotalstuff-helper
     name-sortkey name-subtotal name-date-subtotal
     3 2))


    ;;


    (define timeperiods
        (list
        (cons 'weekly 1)
        (cons 'monthly 2)
        (cons 'quarterly 3)
        (cons 'yearly 4)
        (cons 'none 5)
        ))

;;
(define sort-key-number
    (list
    (cons 'none 0)
    (cons 'account-name 1)
    (cons 'account-code 2)
    (cons 'date         3)
    (cons 'exact-time     4)
    (cons 'reconciled-date 5)
    (cons 'register-order 6)
    (cons 'corresponding-acc-name 7)
    (cons 'corresponding-acc-code 8)
    (cons 'amount         9)
    (cons 'description  10)
    (cons 'memo         13)
    )
  )


(define (get-sort-type primary-key secondary-key)
        (if (not (equal? primary-key 'amount))
                        (if (equal? secondary-key 'amount)
                                    'primary_amount
                                    'primary_secondary)
                        (if (equal? secondary-key 'amount)
                                    'amount_amount
                                    'amount_secondary)
    ))


(define comp-sort-list
    ;; List for sorting consolidated transactions. Each entry: (cons
    ;; 'type-of-comparison (vector get-first-variable compare-first-variable compare-first-if-normal-order
    ;;   compare-first-variable-if-reverse-sort get-second-variable compare-second-variable
    ;;   compare-second-standard-oder  compare-second-variable-reverse-order ))
    (list
    ;;      comparing                how to get variable           match      ascend            descend
    (cons 'primary_secondary (vector get-primary-key             string-ci=? string-ci<?     string-ci>?
                                     get-secondary-key           string-ci=? string-ci<?     string-ci>? ))

    (cons 'primary_amount    (vector get-primary-key             string-ci=? string-ci<?     string-ci>?
                                     get-array-value-num-base     =           <               >       ))

    (cons 'amount_secondary  (vector get-array-value-num-base     =           <               >
                                     get-secondary-key           string-ci=? string-ci<?     string-ci>?))

    (cons 'amount_amount     (vector get-array-value-num-base     =           <               >
                                     get-array-value-num-base     =           <               >       )))
)

(define (comp-sort-helper sort-option-value col-index)
    (vector-ref
     (cdr (assq sort-option-value comp-sort-list))
     col-index))


;;
(define (get-the-transactions splits account-full-name? account-code? consolidate-case-sensitive? primary-comp-key secondary-comp-key)
    ;; get-the-transactions  takes the query results and stores them in hash tables named payee-hash
    ;;  and payee-account-guid-hash - consolidates transactions having same description and or account name
    ;;
(define (set-date-tm trans-date-tm sort-date-type )
    (begin
        (set-tm:sec trans-date-tm 0)
        (set-tm:min trans-date-tm 0)
        (set-tm:hour trans-date-tm 12)
      (case sort-date-type
        ((5) ; 'none
            (set-tm:mday trans-date-tm 1 )
            )
        ((1) ; 'weekly
            (let ((day (gnc:day-of-week trans-date-tm)))
                (set! trans-date-tm (gnc:decrement-date trans-date-tm day ))
            ))
        ((2) ; 'monthly
            (set-tm:mday  trans-date-tm 1))
        ((3) ; 'quarterly
            (begin
                (set-tm:mday trans-date-tm 1 )
                (case (quotient (tm:mon trans-date-tm) 3)
                    ((0) (set-tm:mon trans-date-tm 0)  )
                    ((1) (set-tm:mon trans-date-tm 3 ) )
                    ((2) (set-tm:mon trans-date-tm 6) )
                    ((3) (set-tm:mon trans-date-tm 9))
                    )
                ))
        ((4) ; 'yearly
            (begin
                (set-tm:mday  trans-date-tm 1)
                (set-tm:mon  trans-date-tm 0))
            )
        )
        trans-date-tm
   ))
;;
  (define (keep-account? account)
    (let ((keep-account #f))
             (if show-expenses?
                (if (member account expense-accounts)
                    (set! keep-account #t))
            )
            (if show-income?
                (if (member account income-accounts)
                    (set! keep-account #t))
            )
            (if show-assets?
                (if (member account asset-accounts)
                    (set! keep-account #t))
            )
            (if show-liabilities?
                (if (member account liability-accounts)
                    (set! keep-account #t))
            )

            (if show-equities?
                (if (member account equity-accounts)
                    (set! keep-account #t))
            )
         keep-account
    ))
  (define (composite-sort key split column-vector)

    (case key
        ((0) ; 'none
             " "
            )
        ((1)  ; 'account-name
          (let* (
                (account (xaccSplitGetAccount split))
                (depth (gnc-account-get-current-depth account) ))
                (while (< 2 depth)
                    (set! account (gnc-account-get-parent account))
                    (set! depth (- depth 1))
                )
                (if (< depth 2)
                    (string-append markerA ";" (gnc-account-get-full-name account))
                    (string-append markerA (gnc-account-get-full-name (gnc-account-get-parent account) ) ";" (gnc-account-get-full-name  account))
                )
              ;  (if account-full-name?
              ;      (string-append  markerA (gnc-account-get-full-name account) )
             ;       (string-append  (gnc-account-get-full-name account) markerA (xaccAccountGetName account))))
            ))
        ((2)  ; 'account-code
            (let (
                (account (xaccSplitGetAccount split)))
                (if account-full-name?
                    (string-append  (xaccAccountGetCode account)  " " (gnc-account-get-full-name account) )
                    (string-append  (xaccAccountGetCode account) " " (xaccAccountGetName account)))
             )
            )
        ((3)  ; 'date
            (let* (
                (parent (xaccSplitGetParent split))
                (trans-date (gnc-transaction-get-date-posted parent))
                (tm (gnc:timepair->date trans-date))
                (date-string (if (> 5  sort-date-type)
                        (strftime "%Y%m%d" (set-date-tm tm sort-date-type))
                        "")))
            date-string)
            )
        ((4)  ; 'exact-time
             " "
            )
        ((5)  ; 'reconciled-date
            (let* ((date (gnc-split-get-date-reconciled split))
                (primary-type (if (equal? primary-comp-key 5)
                                        (cdr (assq (opt-val pagename-sorting optname-prime-date-subtotal)  timeperiods  ))
                                        5)) ;no date requirement
                (sec-type (if (equal? secondary-comp-key 5)
                                    (assq-ref  timeperiods (opt-val pagename-sorting optname-sec-date-subtotal) )
                                        5)) ;no date requirement
                (sort-date-type (min primary-type sec-type ))
                (date-string
                    (if (equal? date (cons 0 0))
                        " "
                        (strftime "%Y%m%d" (set-date-tm (gnc:timepair->date date) sort-date-type))
                    )))
                date-string)
                )
        ((6)  ; 'register-order
             " "
            )
        ((7)  ; 'corresponding-acc-name   currently not connected
             " "
            )
        ((8)  ; 'corresponding-acc-code  currently not connected
             " "
            )
        ((9)  ; 'amount     - handled in another part of program
             " "
            )
        ((10) ; 'description
            (let* (
                (parent (xaccSplitGetParent split))
                (descript (if consolidate-case-sensitive?
                        (xaccTransGetDescription parent)
                        (string-capitalize (xaccTransGetDescription parent))))
                )
                (if (< 0 (string-length  descript))
                    descript
                    " ")
                )
            )
        ((11) ; 'number
             " "
            )
        ((12) ; 't-number
             " "
            )
        ((13) ; 'memo
            (let* (
                (parent (xaccSplitGetParent split))
                (memo  (xaccSplitGetMemo split))
                (notes (xaccTransGetNotes parent))
                (note-memo (if (< 0 (string-length  memo))
                    memo
                    (if (and (< 0 (string-length  notes)) (used-notes column-vector))
                    notes
                    " ")))
                )
                (if consolidate-case-sensitive?
                    note-memo
                    (string-upcase note-memo))
            ))
    ))



    (let*
        ((split (car splits))
        (rest (cdr splits))
        (parent (xaccSplitGetParent split))
        (account (xaccSplitGetAccount split))
;        (account-other (xaccSplitGetOtherSplit split))
        (account-guid (gncAccountGetGUID account))
         (trans-guid (gncTransGetGUID parent ))
        (acctfull (gnc-account-get-full-name account))
        (acctgetnam  (xaccAccountGetName account))

;       (acct-comm (xaccAccountGetCommodity account))
;        (shares (xaccSplitGetAmount split))
;        (comodmul  (gnc-commodity-numeric->string acct-comm shares))
;        (transcurr (xaccTransGetCurrency parent))
;       (shareprice  (xaccSplitGetSharePrice split))

        (column-vector (build-column-used options))
        (descript-raw  (if (used-description column-vector)
                        (xaccTransGetDescription parent)
                        " "))
        (guids+description+acct (list
                trans-guid
                account-guid
                descript-raw
                account
                )
                )

        (descript (if consolidate-case-sensitive?
                    descript-raw
                    (string-upcase descript-raw)
                    ))

        (currency (if (not (null? account))
                       (xaccAccountGetCommodity account)
                       (gnc-default-currency)))
        (damount (if (gnc:split-voided? split)
                     (xaccSplitVoidFormerAmount split)
                     (xaccSplitGetAmount split)))
        (report-currency (if (opt-val pagename-general optname-common-currency)
                   (opt-val pagename-general optname-currency)
                   currency))
        (account-type (xaccAccountGetType account))
        (account-types-to-reverse    (get-account-types-to-reverse options))
        (trans-date (gnc-transaction-get-date-posted parent))
        (member-reverse-sign (if (member account-type account-types-to-reverse)
                                "r"
                                ""))
        (split-value (gnc:exchange-by-pricedb-nearest
               (gnc:make-gnc-monetary
                currency
            (if (equal? member-reverse-sign "r")
                (gnc-numeric-neg damount)
                damount))
               report-currency
               ;; Use midday as the transaction time so it matches a price
               ;; on the same day.  Otherwise it uses midnight which will
               ;; likely match a price on the previous day
               (timespecCanonicalDayTime trans-date)))
        (split-value-mon (gnc:gnc-monetary-amount split-value))

        ; to print weekly, quarterly ...
        (tm (gnc:timepair->date (gnc-transaction-get-date-posted parent)))
        (date-string (if (> 5 sort-date-type)
                        (strftime "%Y%m%d" (set-date-tm tm sort-date-type))
                        ""))

        ; use composite sort to read into primary and secondary sort the item, such as description
        ; can not convert everything to upper case here because the marker #Yv; is used in account code
        (primary-sort (composite-sort primary-comp-key split column-vector))
        (secondary-sort (composite-sort secondary-comp-key split column-vector))

    ;
    ;

    ;     (notes (xaccTransGetNotes parent))
    ;     (num (gnc-get-num-action parent split))


        (acct-code (if account-code?
                    (string-append (xaccAccountGetCode account) " ")
                    ""))

        (memo (if (used-memo column-vector)
                (let ((memo (xaccSplitGetMemo split)))
                    (if (and (equal? memo "") (used-notes column-vector))
                        (xaccTransGetNotes parent)
                        memo))
                ""))
        (memo (if consolidate-case-sensitive?
                    memo
                    (string-upcase memo)))

        (acctnamecode (if (used-account-name column-vector)
                        (account-namestring account
                                #f  ;aact-code is handled separately
                                (used-account-name      column-vector)
                                (used-account-full-name column-vector))
                                ""))
        (acctothernamcod  (if (or (used-other-account-name column-vector) (used-other-account-code column-vector))
                        (account-namestring (xaccSplitGetAccount
                                   (xaccSplitGetOtherSplit split))
                                   (used-other-account-code      column-vector)
                                   (used-other-account-name      column-vector)
                                   (used-other-account-full-name column-vector))
                                   "")
                )
        (hashed-currency (hash-ref currency-type-hash report-currency currency-type-str)) ; in case transactions with same description have different currencies

        (hashkey (string-append  primary-sort marker1 secondary-sort marker2 date-string marker3
                         descript marker4
            acctnamecode marker5 acct-code marker6 acctothernamcod marker7 memo marker8 member-reverse-sign marker9 hashed-currency  ))
        (keep-transaction #t)
         )
     ;    (gnc:warn "(gnc-account-get-full-name account)" (gnc-account-get-full-name account))
     ;    (gnc:warn " account"  account)
     ;    (gnc:warn "(gnc-account-get-current-depth account)" (gnc-account-get-current-depth account))
    ;     (gnc:warn "(gnc-account-get-children account)" (gnc-account-get-children account))
    ;     (gnc:warn "(gnc-account-get-parent account)" (gnc-account-get-parent account))
    ;     (gnc:warn "(xaccAccountGetType account)" (xaccAccountGetType account))


    ;;
        (if (keep-account? account)
          (begin
            ;(gnc:warn "keep account" (gnc-account-get-full-name account) )
            (total-payee-hash-add! payee-hash hashkey split-value-mon payee-account-guid-hash guids+description+acct)
            (if (equal? currency-type-str hashed-currency); if we used the current number - add to hash and prepare new number
                (begin
                (hash-set! currency-type-hash report-currency hashed-currency )
                (set! currency-type-num (+ 1 currency-type-num))
                (set! currency-type-str (number->string currency-type-num))
            ))
        ))
        (if (> (length rest) 0)
            (get-the-transactions rest account-full-name? account-code? consolidate-case-sensitive? primary-comp-key secondary-comp-key ))
        )
    )
;;end of section for consolidate
    (define find-field-number
    (list
    (cons 'none 0)
    (cons 'account-name 1)
    (cons 'account-code 2)
    (cons 'date         3)
    (cons 'corresponding-acc-name 7)
    (cons 'corresponding-acc-code 8)
    (cons 'amount         9)
    (cons 'description  10)
    (cons 'memo         13)
    (cons 'notes        14)
    (cons 'any          15)
    (cons 'memo/notes   16)
    (cons 'reconcile    17)
    )
  )

  (let* ((document (gnc:make-html-document))
    (c_account_1 (opt-val pagename-accounts "Accounts"))
    (c_account_2 (opt-val pagename-accounts "Filter By..."))
    (filter-mode (opt-val pagename-accounts "Filter Type"))
    (report-title (opt-val
                       pagename-general
                       gnc:optname-reportname))
    )


(define (store-period-values column list_of_trans scaling-mul-val scaling-div-val)
;; transfers results for a single period from hash table into a column in the amounts-array along with
;; storing the guid in the same array location in the guid-array.  Entries are put on the same row as
;; the description or account name from earlier periods by using the hash table pointer-hash
;;  new names are added to the hash table and added at the bottom of the array

(if  (not (null? splits))
(for-each
    (lambda (split-trans)
        (let* (
        (thekey (car split-trans))
        (guids+description+acct (hash-ref payee-account-guid-hash thekey ))

        (report-currency (hash-ref currency-lookup-hash (get-currency-type split-trans)))
        (currency-frac (gnc-commodity-get-fraction report-currency))
        (value
            (if (= 1 scaling-mul-val)
                (get-split-value-numer split-trans)
                (gnc-numeric-mul
                    (gnc:make-gnc-numeric (inexact->exact (* 100 scaling-mul-val)) 100)
                            (get-split-value-numer split-trans)  currency-frac GNC-RND-ROUND)))
        (split-value (gnc:make-gnc-monetary report-currency
            (if (= 1 scaling-div-val)
                value
            (gnc-numeric-div value
                (gnc:make-gnc-numeric (inexact->exact (* 100 scaling-div-val)) 100)
                      currency-frac GNC-RND-ROUND))))
        (split-value-coll (if (get-reverse-sign? split-trans)
                (gnc:make-gnc-monetary report-currency (gnc-numeric-neg (gnc:gnc-monetary-amount split-value)))
                split-value))
        (row (hash-ref pointer-hash thekey row-number))
        )
        (array-set! amounts-array split-value-coll row column)
        (array-set! amounts-array (gnc-numeric-add (gnc:gnc-monetary-amount split-value-coll)
                    (array-ref amounts-array row 25) GNC-DENOM-AUTO GNC-RND-ROUND)
                                row 25)  ;; add to total
        (array-set! guid-array guids+description+acct row column)
        (if (eq? row row-number)    ; if we wrote it on row-number than we wrote on a new row,
            (begin                    ;  set counter "row-number" to point to next unused line and store info about thekey
                (hash-set! pointer-hash thekey row-number ) ;store key and the row number where this keys data is stored
                ; store this lines key which contains description and or account name etcetra
                (array-set! amounts-array report-currency row-number 0 )
                  ;store guid for account name along with how user stored name
                (array-set! guid-array guids+description+acct row-number 0 )
                (set! row-number  (+ 1 row-number))
          ))
        ))
        list_of_trans
))
)


(define (get-periods-results begin-date end-date
                          find1-field find1-text
            find2-operand find2-field find2-text
            find3-operand find3-field find3-text)
;; based on transaction.scm  this routine is basically the transaction.scm program
;; converted to a define statement.  It
;; queries gnucash then stores the results in hash table by calling get-the-transactions
;; the store-period-values routine will then transfer this periods results to a columnn in an array
  (let* (
    (primary-key (opt-val pagename-sorting optname-prime-sortkey))
    (primary-order (opt-val pagename-sorting "Primary Sort Order"))
    (secondary-key (opt-val pagename-sorting optname-sec-sortkey))
    (secondary-order (opt-val pagename-sorting "Secondary Sort Order"))

;for consolidate
    (primary-comp-key  (cdr (assq primary-key  sort-key-number  )))
    (secondary-comp-key  (cdr (assq secondary-key  sort-key-number  )))
    (consolidate-case-sensitive? (opt-val pagename-display optname-consolidate-case-sensitive))

    (void-status (opt-val pagename-accounts optname-void-transactions))

        (query (qof-query-create-for-splits)))

    ;;(gnc:warn "accts in trep-renderer:" c_account_1)
    ;;(gnc:warn "Report Account names:" (get-other-account-names c_account_1))

    (if (not (or (null? c_account_1) (and-map not c_account_1)))
        (begin
          (qof-query-set-book query (gnc-get-current-book))
          ;;(gnc:warn "query is:" query)
          (xaccQueryAddAccountMatch query
                                       c_account_1
                                       QOF-GUID-MATCH-ANY QOF-QUERY-AND)
          (xaccQueryAddDateMatchTS
           query #t begin-date #t end-date QOF-QUERY-AND)
          (qof-query-set-sort-order query
                    (comp-get-query-sortkey primary-key)
                    (comp-get-query-sortkey secondary-key)
                    '())

          (qof-query-set-sort-increasing query
                                         (eq? primary-order 'ascend)
                                         (eq? secondary-order 'ascend)
                                         #t)

      (case void-status
       ((non-void-only)
        (gnc:query-set-match-non-voids-only! query (gnc-get-current-book)))
       ((void-only)
        (gnc:query-set-match-voids-only! query (gnc-get-current-book)))
       (else #f))
        ; (set! splits '())    ;may be able to delete
          (set! splits (qof-query-run query))

          ;;(gnc:warn "Splits in trep-renderer:" splits)

      ;;(gnc:warn "Filter account names:" (get-other-account-names c_account_2))

      ;;This should probably a cond or a case to allow for different filter types.
      ;;(gnc:warn "Filter Mode: " filter-mode)
      (if (eq? filter-mode 'include)
          (begin
        ;;(gnc:warn "Including Filter Accounts")
        (set! splits (filter (lambda (split)
                       (is-filter-member split c_account_2))
                     splits))
        )
          )

      (if (eq? filter-mode 'exclude)
          (begin
       ;; (gnc:warn "Excluding Filter Accounts")
        (set! splits (filter (lambda (split)
                       (not (is-filter-member split c_account_2)))
                     splits))
        )
          )



(if (not (null? splits))
    (let(
        (account-types-to-reverse    (get-account-types-to-reverse options)))
        (set! splits (filtersplits-found splits account-types-to-reverse
                              find1-field find1-text
                find2-operand find2-field find2-text
                find3-operand find3-field find3-text
                        ))))
    ))

    (qof-query-destroy query)

;;   for composite transaction corr

    (if  (not (null? splits))
        (begin

        (hash-clear! payee-hash  )
        (hash-clear! payee-account-guid-hash)

         (get-the-transactions splits  ;; routine to consolidate the transactions
                        (opt-val pagename-sorting (N_ "Show Full Account Name")) ;for sort subheadings , not for display on each trans
                        (opt-val pagename-sorting (N_ "Show Account Code")) ; not the display tab flag, used for sort headings
                        consolidate-case-sensitive?
                        primary-comp-key secondary-comp-key
                        )

        (set! list_of_trans (hash-map->list cons payee-hash)) ;list of all of the transactions in the period


        ; swap hash table keys and values so can look up stored currency string and get currency
        (hash-for-each  (lambda (key val)
                        (hash-set! currency-lookup-hash val key))
                        currency-type-hash)
    ))
    )
    );; end of define - get-periods-results
;;;;;;;;;;;;;;;;;;
(define (add-column date-start date-end dates page-name optname-show-cola? optname-cola-text optname-show-colb? optname-colb-text)
(let (
                (scaling-mul-val
                     1) ;to activate scaling-mul-val, delete this line
           ;         (if compare-scale-automatically?
           ;             compare-scale-to-val
           ;                 (if (eq? (opt-val page-name optname-a-scale-op-val) '* )
           ;                     (opt-val page-name optname-a-scale-num-val)
          ;                      1)))
           ;     (scaling-div-val (if compare-scale-automatically?
          ;          (caddr dates)
           ;         (if (eq? (opt-val page-name optname-a-scale-op-val) '/ )
           ;             (opt-val page-name optname-a-scale-num-val)
           ;             1)))
                )
                
            (if (opt-val page-name optname-show-cola?)
                (begin
                (set! last-column (+ last-column 1))
                (array-set! amounts-array
                    (opt-val page-name optname-cola-text)
                   ; (display-date-interval-columns date-start date-end) ;store time period title
                        0 last-column)
                (array-set! amounts-array
                     (opt-val page-name optname-a-find1-field)
                        2 last-column)
                (gnc:warn "step 3 now here")
                (array-set! amounts-array
                    (string-upcase (opt-val page-name optname-a-find1-text))
                        3 last-column)
                (array-set! amounts-array
                    (opt-val page-name optname-a-find2-operand)
                         4 last-column)                          
                (array-set! amounts-array
                    (opt-val page-name optname-a-find2-field)
                        5 last-column)
                (array-set! amounts-array
                    (string-upcase (opt-val page-name optname-a-find2-text))
                        6 last-column)
                (array-set! amounts-array
                    (opt-val page-name optname-a-find3-operand)
                        7 last-column)
                (array-set! amounts-array
                    (opt-val page-name optname-a-find3-field)
                        8 last-column)
                (array-set! amounts-array
                    (string-upcase (opt-val page-name optname-a-find3-text))
                       9 last-column)
                (get-periods-results  date-start date-end
                            (cdr (assq (opt-val page-name optname-a-find1-field)  find-field-number  ))
                            (string-upcase (opt-val page-name optname-a-find1-text))
                            (opt-val page-name optname-a-find2-operand)
                            (cdr (assq (opt-val page-name optname-a-find2-field)  find-field-number  ))
                            (string-upcase (opt-val page-name optname-a-find2-text))
                            (opt-val page-name optname-a-find3-operand)
                            (cdr (assq (opt-val page-name optname-a-find3-field)  find-field-number  ))
                            (string-upcase (opt-val page-name optname-a-find3-text)))
                (array-set! amounts-array (caddr dates) 1 last-column) ;store number of days
              ;  (array-set! amounts-array  ;add number of days to total number of days
              ;            (+ (caddr dates) (array-ref amounts-array 1 25)) 1 25)
                (store-period-values last-column list_of_trans scaling-mul-val 1); scaling-div-val)
            ))
              ;  (date-start (car dates) );(gnc:decrement-date (car dates) 0))
              ;  (date-end (cadr dates))
          ;      (set! scaling-mul-val
          ;          (if compare-scale-automatically?
          ;              compare-scale-to-val
          ;                  (if (eq? (opt-val page-name optname-b-scale-op-val) '* )
          ;                      (opt-val page-name optname-b-scale-num-val)
          ;      (set! scaling-div-val (if compare-scale-automatically?
          ;          (caddr dates)
          ;          (if (eq? (opt-val page-name optname-b-scale-op-val) '/ )
          ;              (opt-val page-name optname-b-scale-num-val)
          ;              1)))
            (if (opt-val page-name optname-show-colb?)
                (begin
                (set! last-column (+ last-column 1))
                (array-set! amounts-array
                    (opt-val page-name optname-colb-text)
                   ; (display-date-interval-columns date-start date-end) ;store time period title
                        0 last-column)
                (array-set! amounts-array
                    (opt-val page-name optname-b-find1-field)
                        2 last-column)
                (gnc:warn "step 3 now here")
                (array-set! amounts-array
                    (string-upcase (opt-val page-name optname-b-find1-text))
                        3 last-column)
                (array-set! amounts-array
                    (opt-val page-name optname-b-find2-operand)
                         4 last-column)                          
                (array-set! amounts-array
                    (opt-val page-name optname-b-find2-field)
                        5 last-column)
                (array-set! amounts-array
                    (string-upcase (opt-val page-name optname-b-find2-text))
                        6 last-column)
                (array-set! amounts-array
                    (opt-val page-name optname-b-find3-operand)
                        7 last-column)
                (array-set! amounts-array
                    (opt-val page-name optname-b-find3-field)
                        8 last-column)
                (array-set! amounts-array
                    (string-upcase (opt-val page-name optname-b-find3-text))
                       9 last-column)
                (get-periods-results  date-start date-end
                            (cdr (assq (opt-val page-name optname-b-find1-field )  find-field-number  ))
                            (string-upcase (opt-val page-name optname-b-find1-text))
                            (opt-val page-name optname-b-find2-operand)
                            (cdr (assq (opt-val page-name optname-b-find2-field )  find-field-number  ))
                            (string-upcase (opt-val page-name optname-find2-text))
                            (opt-val page-name optname-b-find3-operand)
                            (cdr (assq (opt-val page-name optname-b-find3-field )  find-field-number  ))
                            (string-upcase (opt-val page-name optname-b-find3-text)))
                (array-set! amounts-array (caddr dates) 1 last-column) ;store number of days
                (store-period-values last-column list_of_trans scaling-mul-val 1 ); scaling-div-val)
            )))
            )

;;;;;;;;;;;;;;;;;;
(define (calculate-delta list-thekeys)
    (for-each
        (lambda (trans)
    (let (
        (row (get-row-in-array trans)))
    (array-set! amounts-array
            (gnc-numeric-sub
            (gnc:gnc-monetary-amount (get-monetary trans 1))
            (gnc:gnc-monetary-amount (get-monetary trans 2)) GNC-DENOM-AUTO GNC-RND-ROUND)
            row 3)))
    list-thekeys)
)


    (gnc:report-starting reportname)
(if (not (or (null? c_account_1) (and-map not c_account_1)))
        (begin
    (let* (
    ;; step 4 of 4 needed for gnctimeperiod-utilities
;; the let needs to be a let*
;; may need to change op-value to get-option
        (whichperiod-val   (opt-val the_tab text-whichperiod))
        (cust-start-date-tp    (gnc:timepair-start-day-time
                                    (gnc:date-option-absolute-time
                                    (opt-val the_tab
                                        custom-from-date))))

        (cust-end-date-tp    (gnc:timepair-end-day-time
                                    (gnc:date-option-absolute-time
                                    (opt-val the_tab
                                        custom-to-date))))
        (datelist   (gnc:get-dates
                (list
                 (list 'whichperiod-val
                       (opt-val the_tab text-whichperiod))
                 (list 'year-val
                       (opt-val the_tab text-pick-year))
                 (list 'period-val
                       (opt-val the_tab text-period))
                 (list 'last-val
                       (opt-val the_tab text-last))
                 (list 'month-val
                       (opt-val the_tab text-month))
                 )))
        ;;
        ;; replace following two names with your names and comment out your old definitions
        (begindate    (if (equal? whichperiod-val 'customdates )
                            cust-start-date-tp
                            (car datelist)))


        (enddate      (if (equal? whichperiod-val 'customdates )
                            cust-end-date-tp
                            (cadr datelist)))

        (list-base-period (gnc:getdatedelta   ;use to get number of days in base
                            (list begindate enddate 'one)))

;; end of section 4 needed for using gnctimeperiod-utilities
;;

        ;;


        (list-avg-period (gnc:getdatedelta
                            (list begindate enddate 'one)))
;; end for comparison period

        (primary-key (opt-val pagename-sorting optname-prime-sortkey))
        (primary-order (opt-val pagename-sorting "Primary Sort Order"))
        (secondary-key (opt-val pagename-sorting optname-sec-sortkey))
        (secondary-order (opt-val pagename-sorting "Secondary Sort Order"))

;for consolidate
        (primary-comp-key  (cdr (assq primary-key  sort-key-number  )))
        (secondary-comp-key  (cdr (assq secondary-key  sort-key-number  )))
        (consolidate-case-sensitive? (opt-val pagename-display optname-consolidate-case-sensitive))

    (base?   (opt-val pagename-base optname-show-base?))

 (split-up-accounts (gnc:decompose-accountlist c_account_1))

        )

    (set! show-days? (opt-val pagename-display optname-days))
    
    (set! show-find? (opt-val pagename-display optname-show-find))

    (set! show-average? (opt-val pagename-display optname-show-average))
    (set! show-totalcol? (opt-val pagename-display optname-show-totalcol))
    (set! show-assets? (opt-val pagename-display optname-show-assets))
    (set! show-liabilities? (opt-val pagename-display optname-show-liabilities))
    (set! show-income? (opt-val pagename-display optname-show-income))
    (set! show-expenses? (opt-val pagename-display optname-show-expenses))
    (set! show-equities? (opt-val pagename-display optname-show-equities))
    (set! no-grand-total? (or (equal? primary-key 'account-name) (equal? secondary-key  'account-name)))

;for consolidate
    (set! comm-curr? (opt-val pagename-general optname-common-currency))
    (set! curr        (opt-val pagename-general optname-currency))
    (set! sort-date-type (let (
                    ( primary-type (if (equal? primary-key 'date)
                                        (cdr (assq (opt-val pagename-sorting optname-prime-date-subtotal)  timeperiods  ))
                                        5)) ;no date requirement
                    (sec-type (if (equal? secondary-key 'date)
                                    (assq-ref  timeperiods (opt-val pagename-sorting optname-sec-date-subtotal) )
                                        5)) ;no date requirement
                             )
                    (min primary-type sec-type )))
;;end section for consolidate

;;for scaling
    (set! scale-op-val      (opt-val the_tab "Scale Results"))
    (set! scale-num-val  (opt-val the_tab "Scale Number Option"))

    (set! compare-scale-automatically? (opt-val pagename-base optname-scale-automatically?))
    (set! compare-scale-to-val         (opt-val pagename-base optname-scale-to))
    (set! scaled? (if (or (not (= 1 scale-num-val)) (not (= 1 scale-num-val)) compare-scale-automatically?)
                    #t
                    #f))

     ;; for find option
     ;for find
    (set! find-text? (opt-val pagename-base optname-find-text?))

    (set! find-min? (opt-val pagename-base optname-find-min))
    (set! find-max? (opt-val pagename-base optname-find-max))
    (set! find-min  (opt-val pagename-base "Min Amount"))
    (set! find-max  (opt-val pagename-base "Max Amount"))
    (set! description-titlecase? (opt-val pagename-display optname-descript-titlecase))
    (set! do-find? (or find-text? find-min? find-max?)    )
    (set! do-findamount? (or find-min? find-max?))
    (set! findtitle "")

    (set! type-root-acct-list (make-list 10 #f))
    (set! previous-type 9999)
    (set! asset-accounts
            (assoc-ref split-up-accounts ACCT-TYPE-ASSET))
    (list-set! type-root-acct-list 0 asset-accounts)
    (set! liability-accounts
            (assoc-ref split-up-accounts ACCT-TYPE-LIABILITY))
    (list-set! type-root-acct-list 1 liability-accounts)
    (set! income-accounts
            (assoc-ref split-up-accounts ACCT-TYPE-INCOME))
    (list-set! type-root-acct-list 2 income-accounts)
    (set! expense-accounts
            (assoc-ref split-up-accounts ACCT-TYPE-EXPENSE))
    (list-set! type-root-acct-list 3 expense-accounts)
    (set! equity-accounts
            (assoc-ref split-up-accounts ACCT-TYPE-EQUITY))
    (list-set! type-root-acct-list 4 equity-accounts)

    (set! acct-depth-name-list (make-list 10 #f))
    (set! type-total-list (make-list 10 #f))


    (if (or find-text? find-min? find-max?)
        (let* (     (report-currency (if comm-curr?
                                        curr
                                        (gnc-default-currency))))

        (begin
        (if find-text?
            (begin
            (set! findtitle (string-append findtitle text-containing find1-text))
            (if (not (equal? find2-operand 'none))
                (set! findtitle (string-append findtitle " "
                (if (equal? find2-operand 'and)
                text-and
                (if (equal? find2-operand 'or)
                text-or
                (if (equal? find2-operand 'not)
                text-but-not
                "")))
                find2-text)))
            ))
        (if find-min?
            (begin
            (set! findtitle (string-append findtitle (if find-text? "," "") text-minimum (gnc:monetary->string
                                    (gnc:make-gnc-monetary report-currency
                                    (gnc:make-gnc-numeric (inexact->exact (* 100 find-min)) 100)  ))))
            (set! find-min (* 100 find-min))
            ))
        (if find-max?
            (begin
            (set! findtitle (string-append findtitle (if (or find-text? find-min?) "," "") text-maximum (gnc:monetary->string
                                    (gnc:make-gnc-monetary report-currency
                                    (gnc:make-gnc-numeric (inexact->exact (* 100 find-max)) 100)  ))))
            (set! find-max (* 100 find-max))
            ))
        )))

    (set! pointer-hash (make-hash-table))
    (set! row-number 11) ; row 0 heading         row 1 number of days in the period
              ;                                      row 2 - 1st string field   row 3 - 1st string text
              ;         row 4 - 2nd operand          row 5  - 2nd string field      row 6- 2nd string text
              ;         row 7 - 3rd string operand   row 8 - 3rd string field       row 9 - 3rd string text row 10 - blank
    (set! last-column 1)
    (set! period-number 1)
    (set! currency-type-hash (make-hash-table))
    (set! currency-lookup-hash (make-hash-table))
    (set! currency-type-num 1)
    (set! currency-type-str (number->string currency-type-num))

; The following few lines are main section where query for data and store in array

     ;get transactions for the base period

     (get-periods-results begindate enddate; get base periods transactions - results stored in list_of_trans and 2 arrays
                (cdr (assq (opt-val pagename-base optname-find1-field )  find-field-number  ))
            (string-upcase (opt-val pagename-base optname-find1-text))
            (opt-val pagename-base optname-find2-operand)
            (cdr (assq (opt-val pagename-base optname-find2-field )  find-field-number  ))
            (string-upcase (opt-val pagename-base optname-find2-text))
            (opt-val pagename-base optname-find3-operand)
            (cdr (assq (opt-val pagename-base optname-find3-field )  find-field-number  ))
            (string-upcase (opt-val pagename-base optname-find3-text)))
        (set! amounts-array (make-array (gnc-numeric-zero) (+ (length list_of_trans) 3000)  27))
        (gnc:warn "step 2 here")
        (set! guid-array (make-array #f (+ (length list_of_trans) 3000)  27))
       ; (array-set! amounts-array 0 1 25) ;initialize total number of days
     (array-set! amounts-array ;store heading / title for the column
        (display-date-interval-columns begindate enddate)
            0 last-column)
     (array-set! amounts-array (caddr (car list-base-period)) 1 last-column)    ; store number of days
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
     (if base?  ;; if user wants to see base period then store results
        (begin
        (store-period-values last-column list_of_trans scaling-mul-val scaling-div-val)
        (array-set! amounts-array
                    (opt-val pagename-base optname-find1-field)
                        2 last-column)
                (gnc:warn "step 3 now here")
                (array-set! amounts-array
                    (string-upcase (opt-val pagename-base optname-find1-text))
                        3 last-column)
                (array-set! amounts-array
                    (opt-val pagename-base optname-find2-operand)
                         4 last-column)                          
                (array-set! amounts-array
                    (opt-val pagename-base optname-find2-field)
                        5 last-column)
                (array-set! amounts-array
                    (string-upcase (opt-val pagename-base optname-find2-text))
                        6 last-column)
                (array-set! amounts-array
                    (opt-val pagename-base optname-find3-operand)
                        7 last-column)
                (array-set! amounts-array
                    (opt-val pagename-base optname-find3-field)
                        8 last-column)
                (array-set! amounts-array
                    (string-upcase (opt-val pagename-base optname-find3-text))
                       9 last-column)
        )
        (set! last-column 0))
     )
;; average period
(if show-average?
   (set! last-column (+ last-column 1))
)

;;
;;  read in and store as column in array each column user has selected
            (let (
                (dates (car list-base-period))
                )
                (add-column begindate enddate dates pagename-col-1-2 optname-show-col1? optname-col1-text optname-show-col2? optname-col2-text)
                (add-column begindate enddate dates pagename-col-3-4 optname-show-col3? optname-col3-text optname-show-col4? optname-col4-text)
                (add-column begindate enddate dates pagename-col-5-6 optname-show-col5? optname-col5-text optname-show-col6? optname-col6-text)
                (add-column begindate enddate dates pagename-col-7-8 optname-show-col7? optname-col7-text optname-show-col8? optname-col8-text)
                (add-column begindate enddate dates pagename-col-9-a optname-show-col9? optname-col9-text optname-show-cola? optname-cola-text)
            )


;;;;
(if (not (= row-number 2)) ;if row number is greater than 2 then some entries were found
 (let (
    ;; pointer-hash contains all of the transactions and what row in the array they are stored in
    ;; convert to list named list-thekeys
     (list-thekeys (hash-map->list cons pointer-hash)))

   ;; average period
     (if (and show-average? (or (> last-column 1) (and base? (> last-column 2))))
        (let* (
            (calculate-delta list-thekeys)       
            (avg-column
                (if base?
                    2
                    1))    
            (num-cols (- last-column avg-column))
            (num-cols-numeric (gnc:make-gnc-numeric num-cols 1))
            )
   (array-set! amounts-array
        "Average" ;store time period title
            0 avg-column)
    (for-each
        (lambda (r)
        (array-set! amounts-array
                    " "
                        r avg-column)
        )
        (list 2 3 4 5 6 7 8 9))
   ;(array-set! amounts-array (round (/ (array-ref amounts-array 1 25) num-cols)) 1 avg-column) ;store average number of days  
    (for-each
        (lambda (trans)
    (let (
        (row (get-row-in-array trans)))
    (array-set! amounts-array
            (gnc-numeric-div
            (gnc:gnc-monetary-amount (get-monetary trans 25))
            num-cols-numeric GNC-DENOM-AUTO GNC-RND-ROUND)
            row avg-column)))
    list-thekeys)
))

;; total
     (if show-totalcol?
        (begin
            (set! last-column (+ last-column 1))
            (array-set! amounts-array
                "Total" ;store time period title
                    0 last-column)
      ;  (array-set! amounts-array (array-ref amounts-array 1 25) 1 last-column) ;store number of days
    (for-each
        (lambda (r)
        (array-set! amounts-array
                    " "
                        r last-column)
        )
        (list 2 3 4 5 6 7 8 9))        
    (for-each
        (lambda (trans)
    (let (
        (row (get-row-in-array trans)))
        (array-set! amounts-array (array-ref amounts-array row 25) row last-column)))
    list-thekeys)
    ))
 

     ; if only two periods, calculate delta
     (if (= last-column 2)
        (begin
            (calculate-delta list-thekeys)
            (set! last-column 3)
            (array-set! amounts-array "Delta" 0 3)
        )
     )

     (let* (

        ; sort based on primary and secondary keys
        (sort-type (get-sort-type primary-key secondary-key))
        (var-p (comp-sort-helper sort-type 0))
        (comp-p1 (comp-sort-helper sort-type 1))
        (comp-p2 (if (equal? primary-order 'ascend)
                        (comp-sort-helper sort-type 2)
                        (comp-sort-helper sort-type 3)
                        ))
        (var-s (comp-sort-helper sort-type 4))
        (comp-s1 (comp-sort-helper sort-type 5))
        (comp-s2 (if (equal? secondary-order 'ascend)
                    (comp-sort-helper sort-type 6)
                        (comp-sort-helper sort-type 7)
                        ))
        (acct-ascend  (if (and (equal? primary-key 'account-name) (equal? primary-order 'ascend))
                           "Prime"
                           (if (and (equal? secondary-key 'account-name) (equal? secondary-order 'ascend))
                                "Second"
                                "None")))

        (as-li-in-ex-eq-list (append asset-accounts
                                     liability-accounts
                                     income-accounts
                                     expense-accounts
                                     equity-accounts))

             )

     (set! list-thekeys (sortkeys list-thekeys var-p var-s comp-p1 comp-p2 comp-s1 comp-s2
                    primary-key secondary-key acct-ascend as-li-in-ex-eq-list))
        )

        (if  (> (length list-thekeys) 0)
        (begin
              (let ((table
                     (make-split-table-comp
                      list-thekeys
                      options
                      primary-comp-key secondary-comp-key
                      (comp-get-subtotal-pred optname-prime-sortkey
                                         optname-prime-subtotal
                                         optname-prime-date-subtotal)
                      (comp-get-subtotal-pred optname-sec-sortkey
                                         optname-sec-subtotal
                                         optname-sec-date-subtotal)
                      (comp-get-subheading-renderer optname-prime-sortkey
                                               optname-prime-subtotal
                                               optname-prime-date-subtotal)
                      (comp-get-subheading-renderer optname-sec-sortkey
                                               optname-sec-subtotal
                                               optname-sec-date-subtotal)
                      (comp-get-subtotal-renderer   optname-prime-sortkey
                                               optname-prime-subtotal
                                               optname-prime-date-subtotal)
                      (comp-get-subtotal-renderer   optname-sec-sortkey
                                               optname-sec-subtotal
                                               optname-sec-date-subtotal))
                        )
                    )
                ;; print out title for report
                (gnc:html-document-set-title! document
                    (string-append consolidated-text " " report-title) )
                (gnc:html-document-add-object!
                 document
                 (gnc:make-html-text
                  (gnc:html-markup-h3
           ;       (if #f ;compare?
           ;        (display-find findtitle)
            ;       (display-date-interval-find begindate enddate findtitle)))))
                    (display-date-interval-columns begindate enddate))))

                (if scaled? ; for scaling
                (gnc:html-document-add-object!
                 document
                 (gnc:make-html-text
                  (gnc:html-markup-h2
                   (string-append scaling-text " "  ))))
                )

;;

  ;;optional section for troubeshooting gnctimeperiod-utilities
    ;; change to (equal? 1 1) to see variables
    ;;       use (equal? 1 "a") to hide variables
    (if (equal? 1 "a") ;;
    (begin
        (set! period-val " ")
      (let* (

      (numtransact (length list-thekeys ))

       (count 0)
       )

      (while (< count numtransact )
        (begin
 ;    get string for  1st payee    (set! firstpayee (car (list-ref total-payee-get-alist 0)))
  ;   get number for  1st amount   (set! firstamount (cdr (list-ref total-payee-get-alist 0)))

        (let* (

        (well (if #t
                    "match"
                    "no_match"))

            (current-trans     (list-ref list-thekeys count ))
            (next-one (if (< count (- numtransact 1 ))
                        (list-ref list-thekeys (+ count 1))
                        current-trans))

            (thekey (car (list-ref list-thekeys count ) ))
            (namcode (get-namecode current-trans))
            (othernam (get-other-name current-trans))
            (description (get-description current-trans))
            (amount (get-array-value-num-base current-trans))
        ;    (amount (get-row-in-array current-trans))
            (amou (cdr current-trans))
            (accountguid (get-accountguid current-trans))

    ;        (tp (get-date-tp current-trans))



        ; (comp-timepair-same-year tp-a tp-b)
    ;        (wellm (if (comp-timepair-same-week (get-date-tp current-trans)  (get-date-tp next-one))
    ;                "match"
    ;                "no"))
    ;        (currencyz (gnc-default-currency))
    ;        (currency-frac (gnc-commodity-get-fraction currencyz))
            )

         (set! period-val (string-append period-val
            " "
            (number->string count)
        ;    (txn (xaccSplitGetParent split))
        ;   (splitcount (xaccTransCountSplits txn)
            "hg"
        "guid:"
            accountguid
            "date:"
    ;        (strftime "%Y%m%d" (get-date-tm current-trans))
            " "
            well
            "Prim:"
            (get-primary-key current-trans)
            "Sec:"
            (get-secondary-key current-trans)
            "row"
            (number->string amount)
            "row1"
            (number->string amou)
            "namcod"
            namcode
            " "
            othernam
            "descrip"
            description
    ;        "currency type"
    ;        (get-currency-type current-trans)
            "the_key"
             thekey
            " "
        "  "
        "num 12 "
        (if (equal? month-val2 '4)
            "four"
            "three")

        "end test"
        )))
          (set! count (+ count 1))
      ))
    )
    (gnc:html-document-add-object!
        ;; may need to change next line or not include these 4-5 lines when adding gnctimeperiod-utilities
       document
       (gnc:make-html-text

        (gnc:html-markup-p
         (gnc:html-markup/format
          (_ "The Pick  which period option is %s.")
          (gnc:html-markup-b whichperiod-val)))




    ;    (gnc:html-markup-p
   ;      (gnc:html-markup/format
   ;       (_ "Number of items in the list is %s.")
   ;       (gnc:html-markup-b (number->string   (comp-sort-helper 'primary_secondary 8)) )))


        (gnc:html-markup-p
         (gnc:html-markup/format
          (_ "The custom date pick is %s .")
          (gnc:html-markup-b (if (equal? whichperiod-val 'customdates ) (_ "true") (_ "false")))))

        (gnc:html-markup-p
         (gnc:html-markup/format
          (_ "The year selection is %s.")
          (gnc:html-markup-b year-val)))

        (gnc:html-markup-p
         (gnc:html-markup/format
          (_ "The Pick period option is %s.")
          (gnc:html-markup-b period-val)))

        (gnc:html-markup-p
         (gnc:html-markup/format
          (_ "The Pick last option is %s.")
          (gnc:html-markup-b last-val)))

          (gnc:html-markup-p
         (gnc:html-markup/format
          (_ "The month period option is %s.")
          (gnc:html-markup-b month-val)))

     ;; Here we print the value of the number option formatted as
        ;; currency. When printing currency values, you should use
        ;; the function (xaccPrintAmount), which is defined in
        ;; report-utilities. This functions will format the number
        ;; appropriately in the current locale. Don't try to format
        ;; it yourself -- it will be wrong in other locales.
        (gnc:html-markup-p
         (gnc:html-markup/format
          (_ "The number option formatted as currency is %s.")
          (gnc:html-markup-b
           (xaccPrintAmount
            (gnc:make-gnc-numeric (inexact->exact (* 100 scale-num-val)) 100)
            (gnc-default-print-info #f)))))

          (gnc:html-markup-p
         (gnc:html-markup/format
          (_ "The find-text option is %s.")
          (gnc:html-markup-b (if find-text? (_ "true") (_ "false")))))

          (gnc:html-markup-p
         (gnc:html-markup/format
          (_ "The find-min option is %s.")
          (gnc:html-markup-b (if find-min? (_ "true") (_ "false")))))

        (gnc:html-markup-p
         (gnc:html-markup/format
          (_ "The find-max option is %s.")
          (gnc:html-markup-b (if find-max? (_ "true") (_ "false")))))

    ;      (gnc:html-markup-p
    ;     (gnc:html-markup/format
    ;      (_ "The number option is %s.")
    ;      (gnc:html-markup-b (number->string scale-num-val))))

    ;    (gnc:html-markup-p
    ;     (gnc:html-markup/format
    ;      (_ "The period start is %s.")
    ;        (gnc:html-markup-b (number->string (car start-date-tp)))))

    ;    (gnc:html-markup-p
    ;     (gnc:html-markup/format
    ;      (_ "The period start is %s.")
    ;      (gnc:html-markup-b (gnc-print-date start-date-tp))))

    ;    (gnc:html-markup-p
    ;     (gnc:html-markup/format
    ;      (_ "The period end is %s.")
    ;      (gnc:html-markup-b (gnc-print-date end-date-tp))))

     )
    )
     ))

    ;end of optional section for gnctimeperiod-utilities

                (gnc:html-document-add-object!
                 document
                 table
                )


;; show any imbalances if option choosen
    (if (and (opt-val pagename-display optname-show-imbalance)
           ; (or (equal? primary-key 'account-name) (equal? primary-key 'account-code))
            (not do-find?))
    (begin
      (let* (
       (count 0)
       (imbalance-val "")
       (accounts-imbalance (gnc:accounts-imbalance-or-orphan begindate enddate))
        (numtransact (length accounts-imbalance ))
       )
       (gnc:html-document-add-object!
        document
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
                (gnc:account-get-balance-interval current-account begindate enddate #f)))
        ))
          (set! count (+ count 1))
      )
        (gnc:html-document-add-object!
        document
        (gnc:make-html-text
        (gnc:html-markup-p
         (gnc:html-markup/format
          (_ "  %s")
          (gnc:html-markup-b imbalance-val)))
        ))
      ))
    )))
;end for showing imbalance


                ))))
              ;; error condition: no splits found
;(if  (not (or (null? c_account_1) (and-map not c_account_1)))
 ;       (begin
              (let ((p (gnc:make-html-text)))
                (gnc:html-text-append!
                 p
                 (gnc:html-markup-h2
                  (_ "No matching transactions found"))
                 (gnc:html-markup-p
                  (_ "No transactions were found that \
match the time interval and account selection specified, \
and find requirements \
in the Options panel.")))
                (gnc:html-document-add-object! document p))
;    ))
)))


        ;; error condition: no accounts specified
        (begin
        (gnc:html-document-add-object!
        document
       (gnc:make-html-text
       (gnc:html-markup-p
         (gnc:html-markup/format
          text-initial-help
          ))))

        (gnc:html-document-add-object!
         document
     (gnc:html-make-no-account-warning
      report-title (gnc:report-id report-obj))
      )))
    (gnc:report-finished)
    document)
    )

;; Define the report.
(gnc:define-report

 'version 1

 'name reportname
 'report-guid "3be3b9833af044abb929a8dbdf49620f"

 'options-generator trep-options-generator

 'renderer trep-renderer)
