(*
   PRINT POLY. Pretty-print polynomials from CSci 2041 Project 1.

     James Moen
     25 Sep 22
*)

(* PRINTF. Define various predefined printing functions. *)

open Printf ;;

(* POLY. A univariate polynomial, with nonzero integer coefficients and integer
   exponents greater than or equal to zero. *)

type poly =
  PolyEmpty |
  PolyTerm of int * int * poly ;;

(* PRINT POLY. Print POLY so it's allegedly easy to read. You need not know how
   this works. Maybe it will help with debugging. *)

let printPoly poly =

(* PRINTING POLY. Do all the work for PRINT POLY. *)

  let rec printingPoly poly =
    match poly
    with PolyEmpty ->
          () |

         PolyTerm (coef, expo, other) ->
          printf " %c %i x^%i"
            (if coef < 0 then '-' else '+')
            (abs coef) expo ;
          printingPoly other

(* Lost? This is PRINT POLY's body. *)

  in match poly
     with PolyEmpty ->
            printf "0\n" |

          PolyTerm(coef, expo, other) ->
            printf "%i x^%i" coef expo ;
            printingPoly other ;
            printf "\n" ;;


let polyList1 = [1; 1];;
let polyList2 = [10; 3; 5; 2; 3; 1];;
let polyList3 = [3; 3; 2; 1];;

let poly1 = makePoly polyList1;;
let poly2 = makePoly polyList2;;
let poly3 = makePoly polyList3;;

printPoly poly1;;
printPoly poly2;;
printPoly poly3;;