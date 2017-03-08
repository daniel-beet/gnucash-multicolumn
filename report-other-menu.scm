;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; report-other-menu.scm: Add Other to report menu
;; 
;; By D.B. Doughty <dbdoughty at gmail.com>
;;  2017.01.26 - 2017.01.27
;;
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
;;
;; to link to this module add following line to your file.
;;(use-modules (gnucash report-other-menu))
;;

(define-module (gnucash report-other-menu)
   #:export (gnc:menuname-other ))
   
(use-modules (gnucash gettext))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gnome-utils))

(gnc:module-load "gnucash/gnome-utils" 0) ;;needed
(gnc:module-load "gnucash/report/report-system" 0) ; needed

(define gnc:menuname-other (N_ "_Other"))
(define other-menu
   (gnc:make-menu gnc:menuname-other (list gnc:menuname-reports)))

(gnc-add-scm-extension other-menu)
;; end


