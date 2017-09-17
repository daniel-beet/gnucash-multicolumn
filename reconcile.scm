;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reconcile.scm : Report reconciled, cleared, and unreconciled transactions in account(s)
;;
;; Based on transaction.scm report by Robert Merkel <rgmerk@mira.net>
;; Contributions by Bryan Larsen <blarsen@ada-works.com>
;; More contributions for new report generation code by Robert Merkel
;; More contributions by Christian Stimming <stimming@tuhh.de>
;; Modified to support the intersection of two account lists by
;; Michael T. Garrison Stuber
;; Modified account names display by Tomas Pospisek
;; <tpo_deb@sourcepole.ch> with a lot of help from "warlord"
;; reconcile.scm by D.B.Doughty <dbdoughty at gmail.com>
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

(define-module (gnucash report standard-reports reconcile))

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

(gnc:module-load "gnucash/report/report-system" 0)

(define-macro (addto! alist element)
  `(set! ,alist (cons ,element ,alist)))

;; Define the strings here to avoid typos and make changes easier.

(define reportname (N_ "Reconciliation Report"))
(define pagename-sorting (N_ "Sorting"))
(define optname-prime-sortkey (N_ "Primary Key"))
(define optname-sec-sortkey (N_ "Secondary Key"))
(define optname-void-transactions (N_ "Void Transactions"))
(define optname-table-export (N_ "Table for Exporting"))
(define optname-common-currency (N_ "Common Currency"))
(define optname-currency (N_ "Report's currency"))
(define def:grand-total-style "grand-total")
(define def:normal-row-style "normal-row")
(define def:alternate-row-style "alternate-row")
(define def:base-subtotal-style "primary-subheading")
(define def:secondary-subtotal-style "secondary-subheading")

(define optname-show-reconciled "Show reconciled transactions")
(define optname-show-cleared "Show cleared transactions")
(define optname-show-not-reconciled "Show not reconciled transactions")
(define optname-only-reconciled-after-date? "Only show transactions reconciled after the date selected below")
(define optname-reconciled-from-date "Date selected  ")
(define opname-separate-credits-debits "Show credits and debits separately")

;; added for find
(define optname-find-text? (N_ "Find Text"))
(define optname-find1-field (N_ "Search"))
(define optname-find1-text   (N_ "finding the text"))
(define optname-find2-operand (N_ "Search for entries containing above string "))
(define optname-find2-field (N_ "Search for 2nd string in "))
(define optname-find2-text (N_ "2nd string"))
(define optname-find-min (N_ "Find minimum amount"))
(define optname-find-max (N_ "Find maximum ammount"))

(define text-containing " containing " )
(define text-and " and ")
(define text-or " or ")
(define text-but-not " but not ")
(define text-minimum " Minimum ")
(define text-maximum " Maximum ")

(define text-reconciled "Reconciled")
(define text-cleared "Cleared")
(define text-not-reconciled "Not Reconciled")

(define text-decimal ".")

(define optname-descript-titlecase (N_ "Titlecase the first chracter in each word in description"))

;; added for flagging imbalances when account name is base key
(define optname-show-imbalance
  (N_ "Note any imbalance"))
(define opthelp-show-imbalance
  (N_ "Make a footnote if there is an imbalance"))

(define text-note-account " Note - account   ")
(define text-changed-in-value " changed in value by ")
(define text-dash (_ "-")) ; printed to indicate imbalance was checked for



;; this is step 2 of 4
;; needed for gnctimeperiod-utilities
;; define all option's names so that they are properly defined
;; in *one* place.
;; can change following text for local language

;; following value may need to be changed
(define the_tab gnc:pagename-general)

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

;; end of section 2 needed for gnctimeperiod-utilities
;; for find
(define list-findchoices
   (list (list->vector
             (list 'description
                   (N_ "description  ")
                   (N_ "search descriptions  for the text - note a blank is added at start and end of description")))
            (list->vector
             (list 'amount
                   (N_ "amount ")
                   (N_ "search amount for the entry, such as $(100.32  - note a blank is added at start and end of each so '7 ' finds all entries ending with 7")))
           (list->vector
             (list 'corresponding-acc-name
                   (N_ "Other account name")
                   (N_ "search full account name ")))
            (list->vector
             (list 'corresponding-acc-code
                   (N_ "Other account code")
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
                   (N_ "    ONLY                            " )
                   (N_ "do not look for a second string of text")))
            (list->vector
             (list 'and
                   (N_ "and the 2nd string ")
                   (N_ "the transaction also must include the second text string")))
            (list->vector
             (list 'or
                   (N_ "or the 2nd string")
                   (N_ "search for either of the two text strings ")))
            (list->vector
             (list 'not
                   (N_ "but exclude entries with 2nd string")
                   (N_ "Search transactions that do not include the second text string")))
           )
)


(define do-find? #f)
(define find-min? #f)
(define find-max? #f)
(define find-text? #f)
(define find1-field 'description )
(define find2-operand 'and)
(define find1-text  "text to find")
(define find2-field 'description )
(define find2-text "description to find")

(define find-min 100.00)
(define find-max 100.00)
(define findtitle "")


;;for running balance
(define amount-total-hash (make-hash-table))

(define description-titlecase? #t)

(define curr " ")
(define comm-curr? #f)
(define currency-type-num 1)
(define currency-type "1")
(define now (timespecCanonicalDayTime (cons (current-time) 0)))
(define today-tm (gnc:timepair->date now))
(define list_of_trans '((a 1) (b 2)))

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

(define (get-account split)
    (let (
    (account (xaccSplitGetAccount split)))
         (gnc-account-get-full-name account)
   ))
(define (get-reconcile-field split)
   (let ((value (string (xaccSplitGetReconcile split))))
    (if (string-ci=? value "y")
        1
        (if (string-ci=? value "c")
           2
          (if (string-ci=? value "n")
             3
             4)))
 ))
 (define (get-description split)
 (let* (
                (parent (xaccSplitGetParent split))
                (descript
                        (xaccTransGetDescription parent)
                 ))
                (if (< 0 (string-length  descript))
                    descript
                    " ")
 ))
(define (get-item-to-sort key split column-vector account-types-to-reverse)

    (case key
        ((0) ; 'none
             " "
            )
        ((1)  ; 'account-name
          (let (
                (account (xaccSplitGetAccount split)))
                (if account-full-name?
                    (gnc-account-get-full-name account)
                    (xaccAccountGetName account)))
            )
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
                (date-string
                        (strftime "%Y%m%d"  tm ))
                        )
            date-string)
            )
        ((4)  ; 'exact-time
             " "
            )
        ((5)  ; 'reconciled-date
            (let* ((date (gnc-split-get-date-reconciled split))
                (date-string
                    (if (equal? date (cons 0 0))
                        " "
                        (strftime "%Y%m%d" (gnc:timepair->date date))
                    )))
                date-string)
                )
        ((6)  ; 'register-order
             " "
            )
        ((7)  ; 'corresponding-acc-name
            (let* (( account (xaccSplitGetAccount (xaccSplitGetOtherSplit split))))
                (gnc-account-get-full-name account))
           )
        ((8)  ; 'corresponding-acc-code
            (let* (( account (xaccSplitGetAccount (xaccSplitGetOtherSplit split))))
                (xaccAccountGetCode account))
            )
        ((9)  ; 'amount
             (let* (
            (parent (xaccSplitGetParent split))

            (account (xaccSplitGetAccount split))
            (account-type (xaccAccountGetType account))
            (damount (if (gnc:split-voided? split)
                            (xaccSplitVoidFormerAmount split)
                            (xaccSplitGetAmount split)))
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
                            (timespecCanonicalDayTime trans-date))) )
            (gnc:gnc-numeric-num (gnc:gnc-monetary-amount split-value)))
            )
        ((10) ; 'description
            (let* (
                (parent (xaccSplitGetParent split))
                (descript
                        (xaccTransGetDescription parent))
                )
                (if (< 0 (string-length  descript))
                    descript
                    " ")
                )
            )
        ((11) ; 'number
             (let* (
                (parent (xaccSplitGetParent split))
                (num (gnc-get-num-action parent split))
                (num-string (if num
                               num
                               " "))
                        )
                num-string)
            )
        ((12) ; 't-number
             (let* (
                (parent (xaccSplitGetParent split))
                (num (gnc-get-num-action parent split))
                    (t-num (if (if (gnc:lookup-option options
                         gnc:pagename-display
                               (N_ "Trans Number"))
                         (opt-val gnc:pagename-display
                               (N_ "Trans Number"))
                                  #f)
                      (gnc-get-num-action parent #f)
                             ""))
                     (num-string (if (equal? t-num "")
                             num
                            (string-append num "/" t-num))))
                num-string)
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
                note-memo
            ))
    ))
(define (get-split-value-num transaction listq)
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
(define (accounts-for-cols-make-hash splits)
 (let* ((split (car splits))
        (rest (cdr splits))
        (txn (xaccSplitGetParent split))
        (splitcount (xaccTransCountSplits txn))
      )
      (if (< 1 splitcount)
      ;;
      (cond
        ;; A 2-split transaction - test separately so it can be optimized
        ;; to significantly reduce the number of splits to traverse
        ;; in guile code
        ((= splitcount 2)
         (let* ((other      (xaccSplitGetOtherSplit split))
                (other-acct (xaccSplitGetAccount other)))
                   (hash-set! accounts-for-cols-hash other-acct other-acct)
         ))

        ;; A multi-split transaction - run over all splits
        ((> splitcount 2)
         (let ((splits (xaccTransGetSplitList txn)))

                ;; Walk through the list of splits.
                ;; if the 'this' != 'split'
                ;; add acount name to list of accounts to use as column names
                (define (is-member splits)
                  (if (not (null? splits))
                      (let* ((this (car splits))
                             (rest (cdr splits))
                             (acct (xaccSplitGetAccount this)))
                        (if (not (eq? this split))
                            (hash-set! accounts-for-cols-hash acct acct)
                              )
                            (is-member rest))))

                (is-member splits)))

        ;; Single transaction splits which we already skipped
        ;(else #f)
        ))

    (hash-set! accounts-for-cols-hash (xaccSplitGetAccount split) (xaccSplitGetAccount split))
    (if (> (length rest) 0)
        (accounts-for-cols-make-hash rest)
        )
))


(define (filter-acct-cols-trans splits)
    (let (
    (have-trans-hash (make-hash-table) ))

  (define (only-one-copy? split)
    (let* (
        (parent (xaccSplitGetParent split))
          (trans-guid (gncTransGetGUID parent ))
        )
        (if (hash-ref have-trans-hash trans-guid #f)
        #f  ; already have a copy of this transaction
        (begin
            (hash-set! have-trans-hash trans-guid #t)
            #t)
        ))
    )
    (filter only-one-copy? splits)
    ))


;; end of account names as column headings


(define (sortreconcile splits p-key s-key var-p comp-p1 comp-p2 var-s comp-s1 comp-s2 column-vector account-types-to-reverse) (sort splits
            ;1 sort by account then
            ;2 sort by reconcile field then
            ;3 sort by secondary key picked by user


     (lambda (x y)
            (let (
                (base-x  (get-account x))
                (base-y  (get-account y))
                (reconcile-x (get-reconcile-field x))
                (reconcile-y (get-reconcile-field y))
                (primary-x  (var-s p-key x column-vector account-types-to-reverse))
                (primary-y  (var-s p-key y column-vector account-types-to-reverse))
                (secondary-x  (var-s s-key x column-vector account-types-to-reverse))
                (secondary-y  (var-s s-key y column-vector account-types-to-reverse)))

                (if (not (string-ci=? base-x base-y))
                    (string-ci<=? base-x base-y)
                    (if (not (= reconcile-x reconcile-y ))
                        (< reconcile-x reconcile-y )
                        (if (not (comp-p1 primary-x primary-y))
                            (comp-p2 primary-x primary-y )
                            (if (not (comp-s1 secondary-x secondary-y))
                                (comp-s2 secondary-x secondary-y )
                                (string-ci<=? (get-description x) (get-description y)))
                    )
                ))
        ))

    ))

;;following part of original transaction.scm
;; The option-values of the sorting key multichoice option, for
;; which a subtotal should be enabled.
(define subtotal-enabled '(account-name
                           account-code
                           reconcile
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

(define (split-same-reconcile-p a b)
  (equal? (xaccSplitGetReconcile a) (xaccSplitGetReconcile b)))

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

(define (add-subheading-row data table width subheading-style)
  (let ((heading-cell (gnc:make-html-table-cell data)))
    (gnc:html-table-cell-set-colspan! heading-cell width)
    (gnc:html-table-append-row/markup!
     table
     subheading-style
     (list heading-cell))))

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

;; render an account subheading - column-vector determines what is displayed
(define (render-account-subheading
         split table width subheading-style column-vector)
  (let ((account (xaccSplitGetAccount split)))
    (add-subheading-row (gnc:make-html-text
                         (gnc:html-markup-anchor
                           (gnc:account-anchor-text account)
                           (account-namestring account
                                               (used-sort-account-code      column-vector)
                                               #t
                                               (used-sort-account-full-name column-vector))))
                        table width subheading-style)))

(define (render-corresponding-account-subheading
         split table width subheading-style column-vector)
  (let ((account (xaccSplitGetAccount (xaccSplitGetOtherSplit split))))
    (add-subheading-row (gnc:make-html-text
                         (gnc:html-markup-anchor
                          (if (not (null? account))
                           (gnc:account-anchor-text account)
                           "")
                           (account-namestring account
                                               (used-sort-account-code      column-vector)
                                               #t
                                               (used-sort-account-full-name column-vector))))
                        table width subheading-style)))

 (define (render-reconcile-subheading
         split table width subheading-style column-vector)
  (let* ((flag (string (xaccSplitGetReconcile split)))
        (reconcile-string
            (if (string-ci=? flag "y")
                    text-reconciled
            (if (string-ci=? flag "c")
                    text-cleared
          (if (string-ci=? flag "n")
                    text-not-reconciled
                    "t")))))
    (add-subheading-row (gnc:make-html-text
                ;(gnc:html-markup-b
                 (gnc:html-markup-h3
                    reconcile-string))
             table width subheading-style)))


(define (render-week-subheading split table width subheading-style column-vector)
  (add-subheading-row (gnc:date-get-week-year-string
               (gnc:timepair->date
            (gnc-transaction-get-date-posted
             (xaccSplitGetParent split))))
              table width subheading-style))

(define (render-month-subheading split table width subheading-style column-vector)
  (add-subheading-row (gnc:date-get-month-year-string
                      (gnc:timepair->date
                       (gnc-transaction-get-date-posted
                        (xaccSplitGetParent split))))
                     table width subheading-style))

(define (render-quarter-subheading split table width subheading-style column-vector)
  (add-subheading-row (gnc:date-get-quarter-year-string
                      (gnc:timepair->date
                       (gnc-transaction-get-date-posted
                        (xaccSplitGetParent split))))
                     table width subheading-style))

(define (render-year-subheading split table width subheading-style column-vector)
  (add-subheading-row (gnc:date-get-year-string
                      (gnc:timepair->date
                       (gnc-transaction-get-date-posted
                        (xaccSplitGetParent split))))
                      table width subheading-style))


(define (add-subtotal-row table width subtotal-string subtotal-collector
                          subtotal-style export?)
  (let ((currency-totals (subtotal-collector
                          'format gnc:make-gnc-monetary #f))
        (blanks (gnc:make-html-table-cell/size 1 (- width 1) #f)))
    (gnc:html-table-append-row/markup!
     table
     subtotal-style
     (if export?
      (append! (cons (gnc:make-html-table-cell/markup "total-label-cell" subtotal-string)
                     (gnc:html-make-empty-cells (- width 2)))
               (list (gnc:make-html-table-cell/markup
                      "total-number-cell"
                      (car currency-totals))))
     (list (gnc:make-html-table-cell/size/markup 1 (- width 1) "total-label-cell"
                                          subtotal-string)
           (gnc:make-html-table-cell/markup
            "total-number-cell"
             (car currency-totals)))))
    (for-each (lambda (currency)
                (gnc:html-table-append-row/markup!
                 table
                 subtotal-style
                 (append!
                  (if export?
                   (gnc:html-make-empty-cells (- width 1))
                   (list blanks))
                         (list (gnc:make-html-table-cell/markup
                                "total-number-cell" currency)))))
              (cdr currency-totals))))

(define (total-string str) (string-append (_ "Total For ") str))
(define (total-reconcile-string str) (string-append (_ "Total ") str))

(define (render-account-subtotal
         table width split total-collector subtotal-style column-vector export?)
    (add-subtotal-row table width
                      (total-string (account-namestring (xaccSplitGetAccount split)
                                                        (used-sort-account-code      column-vector)
                                                        #t
                                                        (used-sort-account-full-name column-vector)))
                      total-collector subtotal-style export?))

(define (render-corresponding-account-subtotal
         table width split total-collector subtotal-style column-vector export?)
    (add-subtotal-row table width
                      (total-string (account-namestring (xaccSplitGetAccount
                                                          (xaccSplitGetOtherSplit split))
                                                        (used-sort-account-code      column-vector)
                                                        #t
                                                        (used-sort-account-full-name column-vector)))
                    total-collector subtotal-style export?))


(define (render-reconcile-subtotal
         table width split total-collector subtotal-style column-vector export?)
     (let* ((flag (string (xaccSplitGetReconcile split)))
        (reconcile-string
            (if (string-ci=? flag "y")
                    text-reconciled
            (if (string-ci=? flag "c")
                    text-cleared
          (if (string-ci=? flag "n")
                    text-not-reconciled
                    "t")))))
    (add-subtotal-row table width
                      (total-reconcile-string reconcile-string)
                    total-collector subtotal-style export?)
    (set! amount-total-hash (make-hash-table))
    ))

(define (render-week-subtotal
     table width split total-collector subtotal-style column-vector export?)
  (let ((tm (gnc:timepair->date (gnc-transaction-get-date-posted
                 (xaccSplitGetParent split)))))
    (add-subtotal-row table width
              (total-string (gnc:date-get-week-year-string tm))
              total-collector subtotal-style export?)))

(define (render-month-subtotal
         table width split total-collector subtotal-style column-vector export?)
  (let ((tm (gnc:timepair->date (gnc-transaction-get-date-posted
                                 (xaccSplitGetParent split)))))
    (add-subtotal-row table width
                      (total-string (gnc:date-get-month-year-string tm))
                      total-collector subtotal-style export?)))


(define (render-quarter-subtotal
         table width split total-collector subtotal-style column-vector export?)
  (let ((tm (gnc:timepair->date (gnc-transaction-get-date-posted
                                 (xaccSplitGetParent split)))))
    (add-subtotal-row table width
                      (total-string (gnc:date-get-quarter-year-string tm))
                     total-collector subtotal-style export?)))

(define (render-year-subtotal
         table width split total-collector subtotal-style column-vector export?)
  (let ((tm (gnc:timepair->date (gnc-transaction-get-date-posted
                                 (xaccSplitGetParent split)))))
    (add-subtotal-row table width
                      (total-string (strftime "%Y" tm))
                      total-collector subtotal-style export?)))


(define (render-grand-total
         table width total-collector export?)
  (add-subtotal-row table width
                    (_ "Grand Total")
                    total-collector def:grand-total-style export?))

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
                                   gnc:pagename-display
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
(define (used-reconcile columns-used)
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

(define (num-columns-required columns-used)
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
      (set! col-req (- col-req 1)))))

(define (build-column-used options)
  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option options section name)))
  (let ((column-list (make-vector columns-used-size #f)))
    (if (opt-val gnc:pagename-display (N_ "Date"))
        (vector-set! column-list 0 #t))
    (if (opt-val gnc:pagename-display (N_ "Reconciled Date"))
        (vector-set! column-list 1 #t))
    (if (if (gnc:lookup-option options gnc:pagename-display (N_ "Num"))
            (opt-val gnc:pagename-display (N_ "Num"))
            (opt-val gnc:pagename-display (N_ "Num/Action")))
        (vector-set! column-list 2 #t))
    (if (opt-val gnc:pagename-display (N_ "Description"))
        (vector-set! column-list 3 #t))
    (if (opt-val gnc:pagename-display (N_ "Reconciled"))
        (vector-set! column-list 4 #t))
    (if (opt-val gnc:pagename-display (N_ "Other Account Name"))
        (vector-set! column-list 5 #t))
    (if (opt-val gnc:pagename-display (N_ "Shares"))
        (vector-set! column-list 6 #t))
    (if (opt-val gnc:pagename-display (N_ "Price"))
        (vector-set! column-list 7 #t))
    (let ((amount-setting
            (opt-val gnc:pagename-display (N_ "Amount"))))
      (if (eq? amount-setting 'single)
          (vector-set! column-list 8 #t))
      (if (eq? amount-setting 'double)
          (begin (vector-set! column-list 9 #t)
                 (vector-set! column-list 10 #t)))
                 )

    (let ((running-bal?
            (opt-val gnc:pagename-display (N_ "Running Balance"))))
    (if running-bal?
        (vector-set! column-list 11 #t)))
    (if (opt-val gnc:pagename-display  (N_ "Use Full Account Name"))
        (vector-set! column-list 12 #t))
    (if (opt-val gnc:pagename-display (N_ "Memo"))
        (vector-set! column-list 13 #t))
    (if (opt-val gnc:pagename-display (N_ "Account Code"))
        (vector-set! column-list 14 #t))
    (if (opt-val gnc:pagename-display (N_ "Other Account Code"))
        (vector-set! column-list 15 #t))
    (if (opt-val gnc:pagename-display (N_ "Use Full Other Account Name"))
        (vector-set! column-list 16 #t))
    (if (opt-val pagename-sorting (N_ "Show Account Code"))
        (vector-set! column-list 17 #t))
    (if (opt-val (N_ "Sorting") (N_ "Show Full Account Name"))
        (vector-set! column-list 18 #t))
    (if (opt-val gnc:pagename-display (N_ "Notes"))
        (vector-set! column-list 19 #t))
    column-list))

(define (make-heading-list column-vector options)
  (let ((heading-list '()))
    (if (used-date column-vector)
        (addto! heading-list (_ "Date")))
    (if (used-reconciled-date column-vector)
        (addto! heading-list (_ "Reconciled Date")))
    (if (used-num column-vector)
        (addto! heading-list (if (and (qof-book-use-split-action-for-num-field
                                                        (gnc-get-current-book))
                                      (if (gnc:lookup-option options
                                                    gnc:pagename-display
                                                    (N_ "Trans Number"))
                                          (gnc:option-value
                                            (gnc:lookup-option options
                                                    gnc:pagename-display
                                                    (N_ "Trans Number")))
                                          #f))
                                 (_ "Num/T-Num")
                                 (_ "&nbsp;&nbsp; Num &nbsp;&nbsp;"))))
    (if (used-description column-vector)
        (addto! heading-list (_ "Description")))
    (if (used-memo column-vector)
        (if (used-notes column-vector)
            (addto! heading-list (string-append (_ "Memo") "/" (_ "Notes")))
            (addto! heading-list (_ "Memo"))))
    (if (used-reconcile column-vector)
        (addto! heading-list (_ "  R  ")))
    (if (or (used-other-account-name column-vector) (used-other-account-code column-vector))
        (addto! heading-list (_ "Transfer from/to")))
    (if (used-shares column-vector)
        (addto! heading-list (_ "Shares")))
    (if (used-price column-vector)
        (addto! heading-list (_ "Price")))
    (if (used-amount-single column-vector)
        (addto! heading-list (_ "Amount")))
    ;; FIXME: Proper labels: what?
    (if (used-amount-double-positive column-vector)
        (addto! heading-list (_ "Debit")))
    (if (used-amount-double-negative column-vector)
        (addto! heading-list (_ "Credit")))
    (if (used-running-balance column-vector)
        (addto! heading-list (_ "Balance")))
    (reverse heading-list)))

(define (add-split-row table split column-vector options
                       row-style account-types-to-reverse transaction-row?)

  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option options section name)))


  (let* ((row-contents '())
     (dummy  (gnc:debug "split is originally" split))
         (parent (xaccSplitGetParent split))
         (account (xaccSplitGetAccount split))
         (account-type (xaccAccountGetType account))
         (currency (if (not (null? account))
                       (xaccAccountGetCommodity account)
                       (gnc-default-currency)))
     (report-currency (if (opt-val gnc:pagename-general optname-common-currency)
                   (opt-val gnc:pagename-general optname-currency)
                   currency))
         (damount (if (gnc:split-voided? split)
                     (xaccSplitVoidFormerAmount split)
                     (xaccSplitGetAmount split)))
     (trans-date (gnc-transaction-get-date-posted parent))
     (currency-frac (gnc-commodity-get-fraction report-currency))

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
    (split-value-coll             ; need "correct" sign for summations
            (if (member account-type account-types-to-reverse)
                (gnc:make-gnc-monetary currency (gnc-numeric-neg (gnc:gnc-monetary-amount split-value)))
                split-value))


               )

    (if (used-date column-vector)
        (addto! row-contents
                (if transaction-row?
                    (gnc:make-html-table-cell/markup "date-cell"
                        (gnc-print-date (gnc-transaction-get-date-posted parent)))
                    " ")))
    (if (used-reconciled-date column-vector)
        (addto! row-contents
                (gnc:make-html-table-cell/markup "date-cell"
            (let ((date (gnc-split-get-date-reconciled split)))
              (if (equal? date (cons 0 0))
                  " "
                  (gnc-print-date date))))))
    (if (used-num column-vector)
        (addto! row-contents
                (if transaction-row?
                    (if (qof-book-use-split-action-for-num-field
                                                        (gnc-get-current-book))
                        (let* ((num (gnc-get-num-action parent split))
                               (t-num (if (if (gnc:lookup-option options
                                                    gnc:pagename-display
                                                    (N_ "Trans Number"))
                                              (opt-val gnc:pagename-display
                                                    (N_ "Trans Number"))
                                              #f)
                                          (gnc-get-num-action parent #f)
                                          ""))
                               (num-string (if (equal? t-num "")
                                               num
                                               (string-append num "/" t-num))))
                              (gnc:make-html-table-cell/markup "text-cell"
                                   num-string))
                        (gnc:make-html-table-cell/markup "text-cell"
                            (gnc-get-num-action parent split)))
                    " ")))

    (if (used-description column-vector)
        (addto! row-contents
                (if transaction-row?
                    (gnc:make-html-table-cell/markup "text-cell"
                        (if description-titlecase?
                        (string-titlecase (xaccTransGetDescription parent))
                        (xaccTransGetDescription parent)))
                    " ")))

    (if (used-memo column-vector)
        (let ((memo (xaccSplitGetMemo split)))
          (if (and (equal? memo "") (used-notes column-vector))
              (addto! row-contents (xaccTransGetNotes parent))
              (addto! row-contents memo))))

    (if (used-reconcile column-vector)
       (addto! row-contents (string-append " " (string-upcase (string (xaccSplitGetReconcile split)) ) " ")))

    (if (or (used-other-account-name column-vector) (used-other-account-code column-vector))
       (addto! row-contents (account-namestring (xaccSplitGetAccount
                                                   (xaccSplitGetOtherSplit split))
                                                (used-other-account-code      column-vector)
                                                (used-other-account-name      column-vector)
                                                (used-other-account-full-name column-vector))))

    (if (used-shares column-vector)
        (addto! row-contents (xaccSplitGetAmount split)))
    (if (used-price column-vector)
        (addto!
         row-contents
         (gnc:make-gnc-monetary (xaccTransGetCurrency parent)
                                (xaccSplitGetSharePrice split))))
    (if (used-amount-single column-vector)
        (addto! row-contents
                (gnc:make-html-table-cell/markup "number-cell"
                                                 (gnc:html-transaction-anchor parent split-value))))
    (if (used-amount-double-positive column-vector)
        (if (gnc-numeric-positive-p (gnc:gnc-monetary-amount split-value))
            (addto! row-contents
                    (gnc:make-html-table-cell/markup "number-cell"
                                                     (gnc:html-transaction-anchor parent split-value)))
            (addto! row-contents " ")))
    (if (used-amount-double-negative column-vector)
        (if (gnc-numeric-negative-p (gnc:gnc-monetary-amount split-value))
            (addto! row-contents
                    (gnc:make-html-table-cell/markup
                     "number-cell" (gnc:html-transaction-anchor parent (gnc:monetary-neg split-value))))
            (addto! row-contents " ")))
    (if (used-running-balance column-vector)
      (begin
        (gnc:debug "split is " split)
        (gnc:debug "split get balance:" (xaccSplitGetBalance split))
        (hash-set! amount-total-hash report-currency  (gnc-numeric-add
                (gnc:gnc-monetary-amount split-value-coll)
                (hash-ref amount-total-hash report-currency (gnc-numeric-zero))  GNC-DENOM-AUTO GNC-RND-ROUND))
        (addto! row-contents
          (gnc:make-html-table-cell/markup
           "number-cell"
           (gnc:make-gnc-monetary currency
                        (hash-ref amount-total-hash report-currency (gnc-numeric-zero))  )))))

    (gnc:html-table-append-row/markup! table row-style
                                       (reverse row-contents))
    split-value-coll))


;;

(define date-sorting-types (list 'date 'exact-time 'register-order))

(define (recon-options-generator)
  (define gnc:*transaction-report-options* (gnc:new-options))
  (define (gnc:register-recon-option new-option)
    (gnc:register-option gnc:*transaction-report-options* new-option))

 ;; General options

;; To add gnctimeperiod-utilities comment out  old  period over which to report income
;; and add section 3
;;

;;  (gnc:options-add-date-interval!
 ;;  gnc:*transaction-report-options*
 ;;  gnc:pagename-general (N_ "Start Date") (N_ "End Date") "a")
;;

;;
 ;; step 3 of 4 to add gnctimeperiod-utilities
     ;add  select custom date or a specific period
     ; changed add-option to gnc:register-recon-option
(let ((periodoptions gnc:*transaction-report-options*))
    (gnc:register-recon-option
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
    ))
     ; add  custom date
    (gnc:options-add-date-interval!
     gnc:*transaction-report-options* the_tab
        custom-from-date custom-to-date "cb")

     ;  add pick year for specific period
    (gnc:register-recon-option
        (gnc:make-multichoice-option the_tab (N_ text-pick-year)
        "ce" (N_ "Pick the year for report") 'this-yr
        gnc:list-years
        ))
     ;  add pick specific period
    (gnc:register-recon-option
        (gnc:make-multichoice-option the_tab (N_ text-period)
        "cf" (N_ "Pick portion of the year for report") 'fullyear
        gnc:list-periods
        ))
    ; add pick specific last XX
    (gnc:register-recon-option
        (gnc:make-multichoice-option the_tab (N_ text-last)
        "cg" (N_ "Pick portion of the year for report") 'last_365
        gnc:list-lasts
        ))
      ; add pick specific month
    (gnc:register-recon-option
        (gnc:make-multichoice-option the_tab (N_ text-month)
        "ch" (N_ "Pick which month for report") '4
        gnc:list-months
        ))

  (gnc:register-recon-option
   (gnc:make-multichoice-option
    gnc:pagename-general (N_ "Style")
    "d" (N_ "Report style.")
    'single
    (list (vector 'multi-line
                  (N_ "Multi-Line")
                  (N_ "Display N lines."))
          (vector 'single
                  (N_ "Single")
                  (N_ "Display 1 line.")))))

  (gnc:register-recon-option
   (gnc:make-complex-boolean-option
    gnc:pagename-general optname-common-currency
    "e" (N_ "Convert all transactions into a common currency.") #f
    #f
    (lambda (x) (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         gnc:pagename-general
         optname-currency
         x))
    ))

  (gnc:options-add-currency!
   gnc:*transaction-report-options* gnc:pagename-general optname-currency "f")

  (gnc:register-recon-option
   (gnc:make-simple-boolean-option
    gnc:pagename-general optname-table-export
    "g" (N_ "Formats the table suitable for cut & paste exporting with extra cells.") #f))

  ;; Accounts options

  ;; account to do report on
  (gnc:register-recon-option
   (gnc:make-account-list-option
    gnc:pagename-accounts (N_ "Accounts")
    "a" (N_ "Report on these accounts.")
    ;; select, by default, no accounts! Selecting all accounts will
    ;; always imply an insanely long waiting time upon opening, and it
    ;; is almost never useful. So we instead display the normal error
    ;; message saying "Click here", and the user knows how to
    ;; continue.
    (lambda ()
      '())
    #f #t))

  (gnc:register-recon-option
   (gnc:make-account-list-option
    gnc:pagename-accounts (N_ "Filter By...")
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

  (gnc:register-recon-option
   (gnc:make-multichoice-option
    gnc:pagename-accounts (N_ "Filter Type")
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

  (gnc:register-recon-option
   (gnc:make-multichoice-option
    gnc:pagename-accounts optname-void-transactions
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

                   (vector 'date
                           (N_ "Date")
                           (N_ "Sort by date."))

                   (vector 'reconciled-date
                           (N_ "Reconciled Date")
                           (N_ "Sort by the Reconciled Date."))

                   (vector 'register-order
                           (N_ "Register Order")
                           (N_ "Sort as with the register."))

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

                   (vector 'number
                           (N_ "Number/Action")
                           (N_ "Sort by check number/action."))

                   (vector 't-number
                           (N_ "Transaction Number")
                           (N_ "Sort by transaction number."))

                   (vector 'memo
                           (N_ "Memo")
                           (N_ "Sort by memo.")))
             (list (vector 'none
                           (N_ "None")
                           (N_ "Do not sort."))

                   (vector 'date
                           (N_ "Date")
                           (N_ "Sort by date."))

                   (vector 'reconciled-date
                           (N_ "Reconciled Date")
                           (N_ "Sort by the Reconciled Date."))

                   (vector 'register-order
                           (N_ "Register Order")
                           (N_ "Sort as with the register."))

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

                   (vector 'number
                           (N_ "Number")
                           (N_ "Sort by check/transaction number."))

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

    ;; base sorting criterion
    (gnc:register-recon-option
     (gnc:make-simple-boolean-option
      pagename-sorting (N_ "Show Full Account Name")
      "a1"
      (N_ "Show the full account name for subtotals and subtitles?")
      #f))

    (gnc:register-recon-option
     (gnc:make-simple-boolean-option
      pagename-sorting (N_ "Show Account Code")
      "a2"
      (N_ "Show the account code for subtotals and subtitles?")
      #f))
    ;; Primary sorting criterion
    (gnc:register-recon-option
     (gnc:make-multichoice-option
      pagename-sorting optname-prime-sortkey
      "a3"
      (N_ "Sort by this criterion First.")
      'date
      key-choice-list
      ))
    (gnc:register-recon-option
     (gnc:make-multichoice-option
      pagename-sorting (N_ "Primary Sort Order")
      "e" (N_ "Order of Primary sorting.")
      'ascend
      ascending-choice-list))
    ;; Secondary sorting criterion
    (gnc:register-recon-option
     (gnc:make-multichoice-option
      pagename-sorting optname-sec-sortkey
      "f"
      (N_ "Sort by this criterion second.")
      'date
      key-choice-list
      ))

    (gnc:register-recon-option
     (gnc:make-multichoice-option
      pagename-sorting (N_ "Secondary Sort Order")
      "i" (N_ "Order of Secondary sorting.")
      'ascend
      ascending-choice-list))

;;for find
 (gnc:register-recon-option
     (gnc:make-simple-boolean-option
      pagename-sorting (N_ optname-find-min)
      "j1"
      (N_ "Only show amounts greater than or equal to")
     #f))

    (gnc:register-recon-option
        (gnc:make-number-range-option pagename-sorting (N_ "Min Amount")
        "j2" (N_ "Minimum amount to show")
            100.0   ;; default
        -900000.0       ;; lower bound
        4000000.0   ;; upper bound
            2.0     ;; number of decimals
           100.0    ;; step size
        ))


    (gnc:register-recon-option
     (gnc:make-simple-boolean-option
      pagename-sorting (N_ optname-find-max)
      "j3"
      (N_ "Only show entries less than or equal")
      #f))

    (gnc:register-recon-option
        (gnc:make-number-range-option pagename-sorting (N_ "Max Amount")
        "j4" (N_ "Maximum amount to show")
            20000.0  ;; default
          -800000.0  ;; lower bound
          5000000.0 ;; upper bound
            2.0     ;; number of decimals
          100.0     ;; step size
        ))

      ;;
    (gnc:register-recon-option
    (gnc:make-complex-boolean-option
     pagename-sorting (N_ optname-find-text?)
    "k1" (N_ "Only show transactions containing the string") #f
      #f
    (lambda (x) (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-sorting optname-find1-field x)
        (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         pagename-sorting optname-find1-text
         x))
    ))

    (gnc:register-recon-option
     (gnc:make-multichoice-option
      pagename-sorting  optname-find1-field
      "k2" (N_ "Select which field or category to use")
      'description
      list-findchoices))

    (gnc:register-recon-option
     (gnc:make-string-option
      pagename-sorting optname-find1-text
      "k3" (N_ "text to look for (all transactions have a space added at front so ' a' will include all which start with a ") (N_ "")))


;
;     (let ((periodoptions gnc:*transaction-report-options*))
    (gnc:register-recon-option
;        (gnc:make-multichoice-callback-option
        (gnc:make-multichoice-option
        pagename-sorting optname-find2-operand
        "k4" (N_ "Select which field or category to use")
        'none
        list-find2-operands))
;        list-findchoices #f
;        (lambda (x)
;        (gnc-option-db-set-option-selectable-by-name
;         gnc:*transaction-report-options*
 ;        pagename-sorting (N_ text-pick-year)
 ;        (if (equal? x 'customdates) #f #t))
;        (gnc-option-db-set-option-selectable-by-name
 ;         gnc:*transaction-report-options*
;        pagename-sorting (N_ text-period)
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


    (gnc:register-recon-option
     (gnc:make-multichoice-option
      pagename-sorting  optname-find2-field
      "k5" (N_ "Select which field or category to use")
      'description
      list-findchoices))

        (gnc:register-recon-option
     (gnc:make-string-option
      pagename-sorting optname-find2-text
      "k6" (N_ "text to look for  ") (N_ "")))

;end of section for find

 )


  ;; Display options

  (for-each
   (lambda (l)
     (gnc:register-recon-option
      (gnc:make-simple-boolean-option
       gnc:pagename-display (car l) (cadr l) (caddr l) (cadddr l))))
   ;; One list per option here with: option-name, sort-tag,
   ;; help-string, default-value
   (list
   (list (N_  opname-separate-credits-debits)  "a1"  (N_ "First display credits and then debits, if not selected then mix them together") #t)
   (list (N_  optname-show-cleared)            "a3"  (N_ "Display cleared items") #t)
   (list (N_  optname-show-not-reconciled)     "a4"  (N_ "Display not-reconciled items") #t)
   (list (N_ optname-show-reconciled)          "a5"  (N_ "Display reconciled items") #t)
   (list (N_  optname-only-reconciled-after-date?) "a7"  (N_ "Display only reconciled items starting with reconciled date selected below") #t)
 ;; note the pick date goes here for  optname-reconciled-from-date
    (list (N_ "Date")                         "b2"  (N_ "Display the date?") #t)
    (list (N_ "Reconciled Date")              "b3" (N_ "Display the reconciled date?") #f)
    (if (qof-book-use-split-action-for-num-field (gnc-get-current-book))
        (list (N_ "Num/Action")               "c"  (N_ "Display the check number?") #t)
        (list (N_ "Num")                      "c"  (N_ "Display the check number?") #t))
    (list (N_ "Description")                  "d1"  (N_ "Display the description?") #t)
  ;; note the "memo"  option in between here
    (list (N_ "Notes")                        "d2" (N_ "Display the notes if the memo is unavailable?") #f)
    (list (N_ "Reconciled")                   "e"  (N_ "Display reconciled flag?") #f)
    (list (N_ "Account Code")                 "g"  (N_ "Display the account code?") #f)
    (list (N_ "Other Account Name")           "h"  (N_ "Display the other account name?\
 (if this is a split transaction, this parameter is guessed).") #t)
    (list (N_ "Use Full Other Account Name")  "i"  (N_ "Display the full other account name?") #f)
    (list (N_ "Other Account Code")           "j"  (N_ "Display the other account code?") #f)
    (list (N_ "Shares")                       "k"  (N_ "Display the number of shares?") #f)
    (list (N_ "Price")                        "l"  (N_ "Display the shares price?") #f)
    ;; note the "Amount" multichoice option in between here
    (list (N_ "Running Balance")              "n"  (N_ "Display a running balance?") #t)
    (list (N_ "Totals")                       "o"  (N_ "Display the totals?") #t)
    ;; note the "sign reverse" multichoice option in between here
    (list optname-show-imbalance              "r"  opthelp-show-imbalance #t)
    (list optname-descript-titlecase            "s"  (N_ "Titlecase The First Letter in Each Word") #t)
      ))

(gnc:register-recon-option
     (gnc:make-date-option
      gnc:pagename-display (N_ optname-reconciled-from-date)
      "a8" (N_ "If above box is checked then no reconciled transactions prior to this date are shown")
      (lambda () (cons 'absolute (cons (- (current-time) (* 60 60 24 5)) 0))) ;set default date to be 5 days ago
      #f 'absolute #f ))

  (if (qof-book-use-split-action-for-num-field (gnc-get-current-book))
      (gnc:register-recon-option
       (gnc:make-simple-boolean-option
        gnc:pagename-display (N_ "Trans Number")
                                    "b5" (N_ "Display the trans number?") #f)))

  ;; Add an option to display the memo, and disable the notes option
  ;; when memos are not included.
  (gnc:register-recon-option
   (gnc:make-complex-boolean-option
    gnc:pagename-display (N_ "Memo")
    "d"  (N_ "Display the memo?") #f
    #f
    (lambda (x) (gnc-option-db-set-option-selectable-by-name
         gnc:*transaction-report-options*
         gnc:pagename-display
         (N_ "Notes")
         x))))

  (gnc:register-recon-option
   (gnc:make-multichoice-option
    gnc:pagename-display (N_ "Amount")
    "m" (N_ "Display the amount?")
    'single
    (list
     (vector 'none (N_ "None     ") (N_ "No amount display."))
     (vector 'single (N_ "Single") (N_ "Single Column Display."))
     (vector 'double (N_ "Double") (N_ "Two Column Display."))
     )))

  (gnc:register-recon-option
   (gnc:make-multichoice-option
    gnc:pagename-display (N_ "Sign Reverses")
    "p" (N_ "Reverse amount display for certain account types.")
    'none
    (list
     (vector 'none (N_ "None") (N_ "Don't change any displayed amounts."))
     (vector 'income-expense (N_ "Income and Expense")
             (N_ "Reverse amount display for Income and Expense Accounts."))
     (vector 'credit-accounts (N_ "Credit Accounts")
             (N_ "Reverse amount display for Liability, Payable, Equity, \
Credit Card, and Income accounts.")))))


  (gnc:options-set-default-section gnc:*transaction-report-options*
                                   gnc:pagename-general)

  gnc:*transaction-report-options*)


(define (display-date-interval begin end)
  (let ((begin-string (gnc-print-date begin))
        (end-string (gnc-print-date end)))
    (sprintf #f (_ "From %s To %s%s") begin-string end-string findtitle)))

(define (get-base-subtotal-style options)
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
(define (filter4display
                splits
                show-reconciled
                show-cleared
                show-not-reconciled
                only-reconciled-after-date?
                reconciled-from-date  )

  (define (displayfound? currentsplit )

    (let* (
       (show? #f)
       (flag (string (xaccSplitGetReconcile currentsplit))))

       (if (string-ci=? flag "y")
            (if show-reconciled
                (if only-reconciled-after-date?
                    (let* ((date (gnc-split-get-date-reconciled  currentsplit))
                           (date-string
                           ; (if (equal? date (cons 0 0))
                           ;     " "
                                (strftime "%Y%m%d" (gnc:timepair->date date))))
                      (set! show?  (string-ci>?  date-string reconciled-from-date)))
                    (set! show?  #t)))
            (if (string-ci=? flag "c")
                (if show-cleared
                    (set! show?  #t))
                (if (string-ci=? flag "n")
                   (if show-not-reconciled
                   (set! show?  #t))
            )))
            show?)
     )
     (filter displayfound? splits)
 )

(define (filtersplits-found splits account-types-to-reverse)

    (define (splitfound? currentsplit )
        ;  (if (not (null? splits))

        (let* (
            (parent (xaccSplitGetParent currentsplit))
            (descript (xaccTransGetDescription parent))

            (account (xaccSplitGetAccount currentsplit))
            (account-type (xaccAccountGetType account))
            (accountother (xaccSplitGetAccount (xaccSplitGetOtherSplit currentsplit)))
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
            (split-value-num  (gnc:gnc-numeric-num (gnc:gnc-monetary-amount split-value)))
                )
;;;;;
        (define (found-text? which-field text-to-find ); based on entries in find-field-number
        (case which-field
        ((10) ; 'description
            (string-contains (string-append " " (string-upcase (xaccTransGetDescription parent) ) " ")  text-to-find))
        (( 9) ; 'amount
            (string-contains (string-append " " (gnc:monetary->string split-value) " ")  text-to-find))
        ((13) ; 'memo
            (string-contains (string-append " " (string-upcase (xaccSplitGetMemo currentsplit) ) " ")  text-to-find))
        ((14) ; 'notes
            (string-contains (string-append " " (string-upcase (xaccTransGetNotes parent) ) " ")  text-to-find))
        (( 1) ; 'account-name
            (string-contains (string-append " " (string-upcase (gnc-account-get-full-name account) ) " ")  text-to-find))
        (( 2) ;'accountcodee
            (string-contains (string-append " " (string-upcase (xaccAccountGetCode account) ) " ")  text-to-find))

        ((7)  ; 'corresponding-acc-name
            (if accountother
            (string-contains (string-append " " (string-upcase (gnc-account-get-full-name accountother))  " ")  text-to-find))
           )
        ((8)  ; 'corresponding-acc-code
                  (if accountother
               (string-contains (string-append " " (string-upcase (xaccAccountGetCode accountother))  " ")  text-to-find))
            )
        ((16) ; 'memo/notes
            (or (string-contains (string-append " " (string-upcase (xaccSplitGetMemo currentsplit) ) " ")  text-to-find) ;memo
            (string-contains (string-append " " (string-upcase (xaccTransGetNotes parent) ) " ")  text-to-find))) ;notes
        ((15) ; 'any
            (or (string-contains (string-append " " (string-upcase (xaccTransGetDescription parent) ) " ")  text-to-find) ;description
            (string-contains (string-append " " (string-upcase (xaccSplitGetMemo currentsplit) ) " ")  text-to-find) ; memo
            (string-contains (string-append " " (string-upcase (xaccTransGetNotes parent) ) " ")  text-to-find) ;notes
            (if accountother (string-contains (string-append " " (string-upcase (gnc-account-get-full-name accountother) ) " ")  text-to-find)) ;other account-name
            (if accountother (string-contains (string-append " " (string-upcase (xaccAccountGetCode accountother) ) " ")  text-to-find)))) ; account-code
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


        (and
            (or (not find-text?)
                (if find-text?
                    (let* ((found1? (found-text? find1-field  find1-text)))
                        (if (equal? find2-operand 'none)
                                found1?
                        (if (equal? find2-operand 'and)
                                (and found1? (found-text? find2-field  find2-text))
                        (if (equal? find2-operand 'or)
                                (or found1? (found-text? find2-field  find2-text))
                        (if (equal? find2-operand 'not)
                                (and found1? (not (found-text? find2-field  find2-text)))
                                #f)))))
                                #f)
                )
            (or (not find-min?) (>= split-value-num find-min )) ; consolidated find is handled in different section
            (or (not find-max?) (<= split-value-num find-max ))
        )
        )
        )
        (filter splitfound? splits )
        )

    ;
(define (filtersplits direction splits account-types-to-reverse)

    (define (splittype? currentsplit )
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
            (split-value-num  (gnc:gnc-numeric-num (gnc:gnc-monetary-amount split-value)))
                )
            (direction split-value-num 0 )
        )
        )
        (filter splittype? splits )
        )


    ;
;;


;; ;;;;;;;;;;;;;;;;;;;;
;; Here comes the big function that builds the whole table.
(define (make-split-table splits options
                          base-subtotal-pred
                          reconcile-subtotal-pred
                          secondary-subtotal-pred
                          base-subheading-renderer
                          reconcile-subheading-renderer
                          secondary-subheading-renderer
                          base-subtotal-renderer
                          reconcile-subtotal-renderer
                          secondary-subtotal-renderer)

 (let ((work-to-do (length splits))
       (work-done 0)
       (used-columns (build-column-used options)))

  (define (transaction-report-multi-rows-p options)
    (eq? (gnc:option-value
          (gnc:lookup-option options gnc:pagename-general (N_ "Style")))
         'multi-line))

  (define (transaction-report-export-p options)
    (gnc:option-value
     (gnc:lookup-option options gnc:pagename-general
       optname-table-export)))

  (define (add-other-split-rows split table used-columns
                                row-style account-types-to-reverse)
    (define (other-rows-driver split parent table used-columns i)
      (let ((current (xaccTransGetSplit parent i)))
        (cond ((null? current) #f)
              ((equal? current split)
               (other-rows-driver split parent table used-columns (+ i 1)))
              (else (begin
                      (add-split-row table current used-columns options
                                     row-style account-types-to-reverse #f)
                      (other-rows-driver split parent table used-columns
                                         (+ i 1)))))))

    (other-rows-driver split (xaccSplitGetParent split)
                       table used-columns 0))

  (define (do-rows-with-subtotals splits
                                  table
                                  used-columns
                                  width
                                  multi-rows?
                                  odd-row?
                                  export?
                                  account-types-to-reverse
                                  base-subtotal-pred
                                  reconcile-subtotal-pred
                                  secondary-subtotal-pred
                                  base-subheading-renderer
                                  reconcile-subheading-renderer
                                  secondary-subheading-renderer
                                  base-subtotal-renderer
                                  reconcile-subtotal-renderer
                                  secondary-subtotal-renderer
                                  base-subtotal-collector
                                  reconcile-subtotal-collector
                                  secondary-subtotal-collector
                                  total-collector)

    (gnc:report-percent-done (* 100 (/ work-done work-to-do)))
    (set! work-done (+ 1 work-done))
    (if (null? splits)
        (begin
          (gnc:html-table-append-row/markup!
           table
           def:grand-total-style
           (list
            (gnc:make-html-table-cell/size
             1 width (gnc:make-html-text (gnc:html-markup-hr)))))

      (if (gnc:option-value (gnc:lookup-option options gnc:pagename-display "Totals"))
          (render-grand-total table width total-collector export?)))

        (let* ((current (car splits))
               (current-row-style (if multi-rows? def:normal-row-style
                                      (if odd-row? def:normal-row-style
                                          def:alternate-row-style)))
               (rest (cdr splits))
               (next (if (null? rest) #f
                         (car rest)))
               (split-value (add-split-row
                             table
                             current
                             used-columns
                 options
                             current-row-style
                             account-types-to-reverse
                             #t)))
          (if multi-rows?
              (add-other-split-rows
               current table used-columns def:alternate-row-style
               account-types-to-reverse))

          (base-subtotal-collector 'add
                                      (gnc:gnc-monetary-commodity
                                       split-value)
                                      (gnc:gnc-monetary-amount
                                       split-value))
          (reconcile-subtotal-collector 'add  ;;dbd fix me - add here for credit  and debit collectors
                                        (gnc:gnc-monetary-commodity
                                         split-value)
                                        (gnc:gnc-monetary-amount
                                         split-value))
          (secondary-subtotal-collector 'add
                                        (gnc:gnc-monetary-commodity
                                         split-value)
                                        (gnc:gnc-monetary-amount
                                         split-value))
          (total-collector 'add
                           (gnc:gnc-monetary-commodity split-value)
                           (gnc:gnc-monetary-amount split-value))

          (if (and base-subtotal-pred
                   (or (not next)
                       (and next
                            (not (base-subtotal-pred current next)))))
              (begin ;;base is changing to a new category, or we are done with the list
              ;;would check if there is a reconcile  but don't need to - there always is a reconcile
               ; 1. there is a reconcile so see if secondary, if so then print secondary total
                 (if secondary-subtotal-pred
                    (begin
                      (secondary-subtotal-renderer
                       table width current
                       secondary-subtotal-collector
                       def:secondary-subtotal-style used-columns export?)
                      (secondary-subtotal-collector 'reset #f #f)))
                      ;; now print out the reconcile sort total
                 (reconcile-subtotal-renderer table width current
                        reconcile-subtotal-collector   ;;dbd fix me
                        def:secondary-subtotal-style used-columns
                        export?)
                 (reconcile-subtotal-collector 'reset #f #f)
                      ;; now print out the base sort total
                 (base-subtotal-renderer table width current
                          base-subtotal-collector
                          def:base-subtotal-style used-columns
                          export?)
                 (base-subtotal-collector 'reset #f #f)
                 ; if new category
                 (if next
                    (begin
                      (base-subheading-renderer  ;  print primary heading
                       next table width def:base-subtotal-style used-columns)

                     (reconcile-subheading-renderer  ;  print reconcile heading
                         next table width def:secondary-subtotal-style used-columns)
                        ;if there is a secondary sort then print its heading
                     (if secondary-subtotal-pred
                          (secondary-subheading-renderer
                           next
                           table
                           width def:secondary-subtotal-style used-columns))))
              )
              ;  base is not changing - check if reconcile is changing
              (if (and reconcile-subtotal-pred
                       (or (not next)
                           (and next ;; if reconcile sort is changing to a new "category" such as cleared then print
                                (not (reconcile-subtotal-pred
                                      current next)))))
                  (begin   ; if so print secondary total
                    (if secondary-subtotal-pred
                      (begin
                        (secondary-subtotal-renderer
                            table width current
                            secondary-subtotal-collector
                            def:secondary-subtotal-style used-columns export?)
                        (secondary-subtotal-collector 'reset #f #f)))
                        ;; now print out the reconcile sort total
                    (reconcile-subtotal-renderer table width current
                         reconcile-subtotal-collector
                         def:secondary-subtotal-style used-columns
                         export?)
                    (reconcile-subtotal-collector 'reset #f #f)

                    (reconcile-subheading-renderer  ;  print reconcile heading
                         next table width def:secondary-subtotal-style used-columns)
                        ;if there is a secondary sort then print its heading
                    (if secondary-subtotal-pred
                          (secondary-subheading-renderer
                                next table
                                width def:secondary-subtotal-style used-columns))
                  )
                  ; reconcile is not changing yet , just check on secondary
                  (if (and secondary-subtotal-pred
                       (or (not next)
                           (and next ;; if secondary sort is changing to a new "category" such as new month then print
                                (not (secondary-subtotal-pred
                                      current next)))))
                    (begin (secondary-subtotal-renderer ;  print secondary total
                          table width current
                          secondary-subtotal-collector
                          def:secondary-subtotal-style used-columns export?)
                         (secondary-subtotal-collector 'reset #f #f)
                         (if next
                             (secondary-subheading-renderer ;  print secondary heading
                              next table width
                              def:secondary-subtotal-style used-columns))))
              )
           )

          (do-rows-with-subtotals rest
                                  table
                                  used-columns
                                  width
                                  multi-rows?
                                  (not odd-row?)
                                  export?
                                  account-types-to-reverse
                                  base-subtotal-pred
                                  reconcile-subtotal-pred
                                  secondary-subtotal-pred
                                  base-subheading-renderer
                                  reconcile-subheading-renderer
                                  secondary-subheading-renderer
                                  base-subtotal-renderer
                                  reconcile-subtotal-renderer
                                  secondary-subtotal-renderer
                                  base-subtotal-collector
                                  reconcile-subtotal-collector
                                  secondary-subtotal-collector
                                  total-collector))))

  (let* ((table (gnc:make-html-table))
         (width (num-columns-required used-columns))
         (multi-rows? (transaction-report-multi-rows-p options))
     (export? (transaction-report-export-p options))
         (account-types-to-reverse
          (get-account-types-to-reverse options)))

    (gnc:html-table-set-col-headers!
     table
     (make-heading-list used-columns options))


    ;;     (gnc:warn "Splits:" splits)
    (if (not (null? splits))
        (begin
          (set! amount-total-hash (make-hash-table))
          (if base-subheading-renderer
              (base-subheading-renderer
               (car splits) table width def:base-subtotal-style used-columns))
          (if reconcile-subheading-renderer
              (reconcile-subheading-renderer
               (car splits) table width def:secondary-subtotal-style used-columns))
          (if secondary-subheading-renderer
              (secondary-subheading-renderer
               (car splits) table width def:secondary-subtotal-style used-columns))

          (do-rows-with-subtotals splits table used-columns width
                                  multi-rows? #t
                                  export?
                                  account-types-to-reverse
                                  base-subtotal-pred
                                  reconcile-subtotal-pred
                                  secondary-subtotal-pred
                                  base-subheading-renderer
                                  reconcile-subheading-renderer
                                  secondary-subheading-renderer
                                  base-subtotal-renderer
                                  reconcile-subtotal-renderer
                                  secondary-subtotal-renderer
                                  (gnc:make-commodity-collector)
                                  (gnc:make-commodity-collector)
                                  (gnc:make-commodity-collector)
                                  (gnc:make-commodity-collector))))

    table)))

;;

;; ;;;;;;;;;;;;;;;;;;;;
;; Here comes the renderer function for this report.
(define (recon-renderer report-obj)

  (define options (gnc:report-options report-obj))

  (define (opt-val section name)
    (gnc:option-value
     (gnc:lookup-option options section name)))

  (define comp-funcs-assoc-list
    ;; Defines the different sorting keys, together with the
    ;; subtotal functions. Each entry: (cons
    ;; 'sorting-key-option-value (vector 'query-sorting-key
    ;; subtotal-function subtotal-renderer))
;;  (let* ((used-columns (build-column-used options))) ;; tpo: gives unbound variable options?
    (let* ((used-columns (build-column-used (gnc:report-options report-obj))))
      (list (cons 'account-name  (vector
                                  (list SPLIT-ACCT-FULLNAME)
                                  split-account-full-name-same-p
                                  render-account-subheading
                                  render-account-subtotal))
            (cons 'account-code  (vector
                                  (list SPLIT-ACCOUNT ACCOUNT-CODE-)
                                  split-account-code-same-p
                                  render-account-subheading
                                  render-account-subtotal))
            (cons 'exact-time    (vector
                                  (list SPLIT-TRANS TRANS-DATE-POSTED)
                                  #f #f #f))
            (cons 'date          (vector
                                  (list SPLIT-TRANS TRANS-DATE-POSTED)
                                  #f #f #f))
            (cons 'reconciled-date (vector
                                  (list SPLIT-DATE-RECONCILED)
                                  #f #f #f))
            (cons 'register-order (vector
                                  (list QUERY-DEFAULT-SORT)
                                  #f #f #f))
            (cons 'reconcile
                                 (vector
                                  (list #f)
                                  split-same-reconcile-p
                                  render-reconcile-subheading
                                  render-reconcile-subtotal))
            (cons 'corresponding-acc-name
                                 (vector
                                  (list SPLIT-CORR-ACCT-NAME)
                                  split-same-corr-account-full-name-p
                                  render-corresponding-account-subheading
                                  render-corresponding-account-subtotal))
            (cons 'corresponding-acc-code
                                 (vector
                                  (list SPLIT-CORR-ACCT-CODE)
                                  split-same-corr-account-code-p
                                  render-corresponding-account-subheading
                                  render-corresponding-account-subtotal))
            (cons 'amount        (vector (list SPLIT-VALUE) #f #f #f))
            (cons 'description   (vector (list SPLIT-TRANS TRANS-DESCRIPTION) #f #f #f))
            (if (qof-book-use-split-action-for-num-field (gnc-get-current-book))
                (cons 'number    (vector (list SPLIT-ACTION) #f #f #f))
                (cons 'number    (vector (list SPLIT-TRANS TRANS-NUM) #f #f #f)))
            (cons 't-number      (vector (list SPLIT-TRANS TRANS-NUM) #f #f #f))
            (cons 'memo          (vector (list SPLIT-MEMO) #f #f #f))
            (cons 'none          (vector '() #f #f #f)))))

  (define date-comp-funcs-assoc-list
    ;; Extra list for date option. Each entry: (cons
    ;; 'date-subtotal-option-value (vector subtotal-function
    ;; subtotal-renderer))
    (list
     (cons 'none (vector #f #f #f))
     (cons 'weekly (vector split-same-week-p render-week-subheading
               render-week-subtotal))
     (cons 'monthly (vector split-same-month-p render-month-subheading
                            render-month-subtotal))
     (cons 'quarterly (vector split-same-quarter-p render-quarter-subheading
                            render-quarter-subtotal))
     (cons 'yearly (vector split-same-year-p render-year-subheading
                           render-year-subtotal))))

  (define (get-subtotalstuff-helper
           name-sortkey name-subtotal name-date-subtotal
           comp-index date-index)
    ;; The value of the sorting-key multichoice option.
    (let ((sortkey  name-sortkey))
      (if (member sortkey date-sorting-types)
          ;; If sorting by date, look up the value of the
          ;; date-subtotalling multichoice option and return the
          ;; corresponding funcs in the assoc-list.
          (vector-ref
           (cdr (assq  name-date-subtotal
                      date-comp-funcs-assoc-list))
           date-index)
          ;; For everything else: 1. check whether sortkey has
          ;; subtotalling enabled at all, 2. check whether the
          ;; enable-subtotal boolean option is #t, 3. look up the
          ;; appropriate funcs in the assoc-list.
          (and (member sortkey subtotal-enabled)
               (and name-subtotal
                    (vector-ref
                     (cdr (assq sortkey comp-funcs-assoc-list))
                     comp-index))))))

  (define (get-query-sortkey sort-option-value)
    (vector-ref
     (cdr (assq sort-option-value comp-funcs-assoc-list))
     0))

  (define (get-subtotal-pred
           name-sortkey name-subtotal name-date-subtotal)
    (get-subtotalstuff-helper
     name-sortkey name-subtotal name-date-subtotal
     1 0))

  (define (get-subheading-renderer
           name-sortkey name-subtotal name-date-subtotal)
    (get-subtotalstuff-helper
     name-sortkey name-subtotal name-date-subtotal
     2 1))

  (define (get-subtotal-renderer
           name-sortkey name-subtotal name-date-subtotal)
    (get-subtotalstuff-helper
     name-sortkey name-subtotal name-date-subtotal
     3 2))

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
                (other-acct (xaccSplitGetAccount other)))
               (member other-acct account-list)))

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
    (cons 'number       11)
    (cons 't-number     12)
    (cons 'memo         13)
    )
  )
 (define (get-sort-type the-key)
        (if (equal? the-key 'amount)
            'amount
            'a-string)

    )


(define sort-list
    ;; List for sorting consolidated transactions. Each entry: (cons
    ;; 'type-of-comparison (vector get-first-variable compare-first-variable compare-first-if-normal-order
    ;;   compare-first-variable-if-reverse-sort get-second-variable compare-second-variable
    ;;   compare-second-standard-oder  compare-second-variable-reverse-order ))
    (list
    ;;      comparing                how to get variable   match      ascend            descend
    (cons 'a-string            (vector get-item-to-sort   string-ci=? string-ci<?     string-ci>? ))
    (cons 'amount               (vector get-item-to-sort     =           <               >       ))
    )
)

(define (sort-helper sort-option-value col-index)
    (vector-ref
     (cdr (assq sort-option-value sort-list))
     col-index))


;;

    (define find-field-number
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
    (cons 'number       11)
    (cons 't-number     12)
    (cons 'memo         13)
    (cons 'notes        14)
    (cons 'any          15)
    (cons 'memo/notes   16)
    (cons 'reconcile    17)
    )
  )
  (gnc:report-starting reportname)
  (let* ((document (gnc:make-html-document))
    (c_account_1 (opt-val gnc:pagename-accounts "Accounts"))
    (c_account_2 (opt-val gnc:pagename-accounts "Filter By..."))
    (filter-mode (opt-val gnc:pagename-accounts "Filter Type"))

    ;; step 4 of 4 needed for gnctimeperiod-utilities
;; the let needs to be a let*
;; may need to change op-value to get-option
        (whichperiod-val (opt-val the_tab text-whichperiod))

        (cust-start-date-tp     (gnc:timepair-start-day-time
                                    (gnc:date-option-absolute-time
                                    (opt-val the_tab
                                        custom-from-date))))

        (cust-end-date-tp       (gnc:timepair-end-day-time
                                    (gnc:date-option-absolute-time
                                    (opt-val the_tab
                                        custom-to-date))))
        (datelist               (gnc:get-dates
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


;; end of section 4 needed for using gnctimeperiod-utilities
;;
  ;      (begindate (gnc:timepair-start-day-time
 ;                   (gnc:date-option-absolute-time
  ;                   (opt-val gnc:pagename-general "Start Date"))))
  ;      (enddate (gnc:timepair-end-day-time
  ;                (gnc:date-option-absolute-time
  ;                 (opt-val gnc:pagename-general "End Date"))))
        (report-title (opt-val
                       gnc:pagename-general
                       gnc:optname-reportname))
        (base-key 'account-name )
        (base-order 'ascend )
        (primary-key   (opt-val pagename-sorting optname-prime-sortkey))
        (secondary-key (opt-val pagename-sorting optname-sec-sortkey))
        (primary-order (opt-val pagename-sorting "Primary Sort Order"))
        (secondary-order (opt-val pagename-sorting "Secondary Sort Order"))


    (void-status (opt-val gnc:pagename-accounts optname-void-transactions))
        (splits '())
        (splits-in '())
        (query (qof-query-create-for-splits)))

    (set! description-titlecase? (opt-val gnc:pagename-display optname-descript-titlecase))

 ;for find
    (set! find-text? (opt-val pagename-sorting optname-find-text?))
    (set! find1-field (cdr (assq (opt-val pagename-sorting optname-find1-field )  find-field-number  )))
    (set! find2-operand (opt-val pagename-sorting optname-find2-operand))
    (set! find1-text  (string-upcase (opt-val pagename-sorting optname-find1-text)))
    (set! find2-field (cdr (assq (opt-val pagename-sorting optname-find2-field )  find-field-number  )))
    (set! find2-text  (string-upcase (opt-val pagename-sorting optname-find2-text)))

    (set! find-min? (opt-val pagename-sorting optname-find-min))
    (set! find-max? (opt-val pagename-sorting optname-find-max))
    (set! find-min  (opt-val pagename-sorting "Min Amount"))
    (set! find-max  (opt-val pagename-sorting "Max Amount"))
    (set! comm-curr? (opt-val gnc:pagename-general optname-common-currency))
    (set! curr        (opt-val gnc:pagename-general optname-currency))

    ;;(gnc:warn "accts in recon-renderer:" c_account_1)
    ;;(gnc:warn "Report Account names:" (get-other-account-names c_account_1))

    (if (not (or (null? c_account_1) (and-map not c_account_1)))
        (begin
          (qof-query-set-book query (gnc-get-current-book))
          ;;(gnc:warn "query is:" query)
          (xaccQueryAddAccountMatch query
                                       c_account_1
                                       QOF-GUID-MATCH-ANY QOF-QUERY-AND)
          (xaccQueryAddDateMatchTS
           query #t begindate #t enddate QOF-QUERY-AND)
          (qof-query-set-sort-order query
                    (get-query-sortkey base-key)
                    (get-query-sortkey primary-key)
                    '())

          (qof-query-set-sort-increasing query
                                         (eq? base-order 'ascend)
                                         (eq? secondary-order 'ascend)
                                         #t)

      (case void-status
       ((non-void-only)
        (gnc:query-set-match-non-voids-only! query (gnc-get-current-book)))
       ((void-only)
        (gnc:query-set-match-voids-only! query (gnc-get-current-book)))
       (else #f))

          (set! splits (qof-query-run query))

          ;;(gnc:warn "Splits in recon-renderer:" splits)

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
        ;;(gnc:warn "Excluding Filter Accounts")
        (set! splits (filter (lambda (split)
                       (not (is-filter-member split c_account_2)))
                     splits))
        )
          )


 ;; for find option
    (set! do-find? (or find-text? find-min? find-max?)    )
    (set! findtitle "")
    (if do-find?
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

;;remove split entries user does not want to show
(if (not (null? splits))
(set! splits (filter4display
                splits
                (opt-val gnc:pagename-display optname-show-reconciled)
                (opt-val gnc:pagename-display optname-show-cleared)
                (opt-val gnc:pagename-display optname-show-not-reconciled)
                (opt-val gnc:pagename-display optname-only-reconciled-after-date?)
                (strftime "%Y%m%d" (gnc:timepair->date (gnc:timepair-start-day-time
                         (gnc:date-option-absolute-time
                                    (opt-val gnc:pagename-display optname-reconciled-from-date)))))
                ))
)

;; for find
(if (and (not (null? splits)) do-find?)
    (let(
        (account-types-to-reverse    (get-account-types-to-reverse options)))
        (set! splits (filtersplits-found splits account-types-to-reverse))))
    ))

;;

    (if  (not (null? splits))
        (begin
(let* (
        (account-types-to-reverse    (get-account-types-to-reverse options))
        (primary-comp-key  (cdr (assq primary-key  sort-key-number  )))
        (secondary-comp-key  (cdr (assq secondary-key  sort-key-number  )))
        (sort-type-p (get-sort-type primary-key))
        (sort-type-s (get-sort-type secondary-key))

        (var-p (sort-helper sort-type-p 0)) ;; dbd fix me
        (comp-p1 (sort-helper sort-type-p 1))
        (comp-p2 (if (equal? primary-order 'ascend)
                    (sort-helper sort-type-p 2)
                    (sort-helper sort-type-p 3)
                        ))
        (var-s (sort-helper sort-type-s 0)) ;; dbd fix me
        (comp-s1 (sort-helper sort-type-s 1))
       (comp-s2 (if (equal? secondary-order 'ascend)
                    (sort-helper sort-type-s 2)
                    (sort-helper sort-type-s 3)
                        ))
        (used-columns (build-column-used options))
            )
  (set! splits (sortreconcile splits primary-comp-key secondary-comp-key
                var-p comp-p1 comp-p2
                var-s comp-s1 comp-s2 used-columns account-types-to-reverse))
    )
   (if (opt-val gnc:pagename-display opname-separate-credits-debits)
        (let(
        (account-types-to-reverse    (get-account-types-to-reverse options)))
        (set! splits-in (filtersplits  > splits account-types-to-reverse))
        (set! splits     (filtersplits < splits account-types-to-reverse))

   ))
          (if (or (not (null? splits-in))  (not (null? splits)))
              (let ((table-in
                     (if (null? splits-in)
                        #f
                     (make-split-table
                      splits-in
                      options
                      (get-subtotal-pred 'account-name
                                         #t ; prime-subtotal
                                         #f)
                      (get-subtotal-pred 'reconcile
                                         #t ; reconcile-subtotal
                                         #f)
                      (get-subtotal-pred 'none
                                         #f
                                         'none)
                      (get-subheading-renderer 'account-name
                                                #t ; prime-subtotal
                                                #f)
                     (get-subheading-renderer  'reconcile
                                                #t ; reconcile-subtotal
                                                #f)
                      (get-subheading-renderer 'none
                                               #f
                                               'none)
                      (get-subtotal-renderer   'account-name
                                                #t ; prime-subtotal
                                                #f)
                      (get-subtotal-renderer   'reconcile
                                                #t ; prime-subtotal
                                                #f)
                      (get-subtotal-renderer   'none
                                               #f
                                               'none))
                        ))
                    (table
                     (if (null? splits)
                        #f
                     (make-split-table
                      splits
                      options
                      (get-subtotal-pred 'account-name
                                         #t ; prime-subtotal
                                         #f)
                      (get-subtotal-pred 'reconcile
                                         #t ; reconcile-subtotal
                                         #f)
                      (get-subtotal-pred 'none
                                         #f
                                         'none)
                      (get-subheading-renderer 'account-name
                                                #t ; prime-subtotal
                                                #f)
                     (get-subheading-renderer  'reconcile
                                                #t ; reconcile-subtotal
                                                #f)
                      (get-subheading-renderer 'none
                                               #f
                                               'none)
                      (get-subtotal-renderer   'account-name
                                                #t ; prime-subtotal
                                                #f)
                      (get-subtotal-renderer   'reconcile
                                                #t ; prime-subtotal
                                                #f)
                      (get-subtotal-renderer   'none
                                               #f
                                               'none))
                    )
                    ))

                (gnc:html-document-set-title! document
                                              report-title)
                (gnc:html-document-add-object!
                 document
                 (gnc:make-html-text
                  (gnc:html-markup-h3
                   (display-date-interval begindate enddate))))

                 (if (not (null? splits-in))
                     (begin
                        (gnc:html-document-add-object!
                                document
                                (gnc:make-html-text
                                (gnc:html-markup-h3
                                "  Credits ")))
                        (gnc:html-document-add-object!
                                document
                                table-in)
                        (gnc:html-table-append-ruler!
                                table-in
                                4
                        )
                        (gnc:html-document-add-object!
                                document
                                (gnc:make-html-text
                                (gnc:html-markup-h3
                                 "  Debits  ")))
                 ))
                 (if (not (null? splits))
                    (gnc:html-document-add-object!
                        document
                        table
                 ))
                (qof-query-destroy query)

;; show any imbalances if option choosen
    (if  (opt-val gnc:pagename-display optname-show-imbalance)
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
 )
;end for showing imbalance

              ;; error condition: no splits found
              (let ((p (gnc:make-html-text)))
                (gnc:html-text-append!
                 p
                 (gnc:html-markup-h2
                  (_ "No matching transactions found"))
                 (gnc:html-markup-p
                  (_ "No transactions were found that \
match the time interval and account selection specified \
in the Options panel.")))
                (gnc:html-document-add-object! document p)))
)
        ;; error condition: no accounts specified or no transactions found
 (if (not (or (null? c_account_1) (and-map not c_account_1)))
    ;; error condition: no splits found
              (let ((p (gnc:make-html-text)))
                (gnc:html-text-append!
                 p
                 (gnc:html-markup-h2
                  (_ "No matching transactions found"))
                 (gnc:html-markup-p
                  (_ "No transactions were found that \
match the time interval, account selection specified, \
and find requirements \
in the Options panel.")))
                (gnc:html-document-add-object! document p))
    ;; error condition: no accounts specified
        (gnc:html-document-add-object!
         document
     (gnc:html-make-no-account-warning
      report-title (gnc:report-id report-obj)))))

    (gnc:report-finished)
    document))

;; Define the report.
(gnc:define-report

 'version 1

 'name reportname
 'report-guid "2fe3b9833af044abb929a8dbda60620f"

 'options-generator recon-options-generator

 'renderer recon-renderer)
