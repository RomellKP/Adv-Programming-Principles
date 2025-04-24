open List;;


type poly =
  PolyEmpty |
  PolyTerm of int * int * poly ;;

let isPolyOk p=
    let rec isPolyOking p =
        match p with 
        |PolyTerm (coef, expo, other) -> 
            if coef = 0
                then false
            else if expo < 0
                then false
            else
                match other with 
                |PolyEmpty -> true
                |PolyTerm(_, expo2, _) -> expo2 < expo && isPolyOking other
        |PolyEmpty -> true

    in isPolyOking p
;; 

        
    
exception MakePolyError


let makePoly l =
        let rec makePolying i =
            match i with
                | [] -> PolyEmpty
                |[x] -> raise MakePolyError
                |x :: y :: z -> PolyTerm (x, y, makePolying z)
    in makePolying l
            ;;




let polyAdd l r=
    (* Here l and r are well formed instances of the type poly. 
    Using the constructors PolyEmpty and PolyTerm, construct a new well formed polynomial 
    that represents the sum of l and r, and return it. *)
    let rec polyAdding l r=
        match(l, r) with
        (PolyTerm (lCoef, lExpo, lOther), 
        PolyTerm (rCoef, rExpo, rOther)) -> 
        if lExpo > rExpo 
            then polyAdding lOther r
        else if lExpo < rExpo
            then polyAdding l rOther
        else
            match ((lCoef + rCoef)) with
            |0 -> PolyEmpty
            |_ -> PolyTerm((lCoef + rCoef), lExpo, polyAdding lOther rOther)
    in polyAdding l r
;;



    

let polyMinus r=
    (* Here r is a well formed instance of the type poly. Using the constructors PolyEmpty 
    and PolyTerm, construct a new well formed polynomial that represents − r, and return it. *)
    let rec polyMinusing r =
        match r with
        |PolyTerm (coef, expo, other)->
            PolyTerm((-coef), (-expo), polyMinusing other)
        |PolyEmpty -> PolyEmpty
    

    in polyMinusing r 
;;

(* test cases:
0
10 x^3 + 5 x^2 + 3 x^1
3 x^5 + 9 x^4 + 2 x^3 - 4 x^1 + 2 x^0 *)