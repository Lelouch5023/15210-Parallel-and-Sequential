functor MkBruteForcePD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open P
  open Seq

  (* Remove this line when you're done. *)
  exception NotYetImplemented

  fun parenMatchSeq p =
      let
      fun pm (NONE,_) = NONE 
        | pm (SOME(0),CPAREN) = NONE
        | pm (SOME(c),CPAREN) = SOME(c-1)
        | pm (SOME(c),OPAREN) = SOME(c+1)
      in
      (iter (pm) (SOME(0)) (p)) = (SOME(0))
      end
(*
  fun parenMatchPar p = 
      let
      fun pm s = 
          case showt(s) of
            EMPTY => (0,0)
          | ELT OPAREN =>  (0,1)
          | ELT CPAREN => (1,0)
          | NODE (l,r) =>
            let
            val ((x1,y1),(x2,y2)) = par ((fn () => pm l),(fn () => pm r))
            in
            if y1 > x2
            then (x1,y2 + (y1-x2))
            else (x1 + (x2-y1),y2)
            end
       in
       pm p = (0,0)
       end
*)
  fun contigousElem parens = 
      let
      fun helperSub i j p = 
          if(i < length(p)-1)
          then if( i + j <= length(p))
               then (subseq p (i,j))::(helperSub (i) (j+1) (p))
               else helperSub (i+1) (1) (p)
          else []
      in
      fromList (helperSub 0 1 parens)
      end

  fun realPmatch sequ =
      case showl(sequ) of
        NIL => false
      | CONS(_,_)  => let
                       val first = nth sequ (0)
                       val last = nth sequ (length(sequ)-1)
                      in
                       if((first = OPAREN) andalso (last = CPAREN))
                       then true
                       else false
                       end
  
  fun parenDist (parens : paren seq) : int option =
      let
      val all = contigousElem parens
      val filterPMatch = filter realPmatch all
      val filterClose = filter (fn x => parenMatchSeq
                                        (subseq x (1,length(x)-2)))
                                filterPMatch
      val maxes = map (fn x => SOME(length(x))) filterClose
      in
      iter Option210.intMax NONE maxes
      end
  (* Task 4.3
     parenDist first gets all contingous substrings of parens with the
     function contiguousElem. Then it uses realPmatch to get any
     possible strings that could be matched as it if the string 
     has a open parenthesis as its first element and close parenthesis
     as its last parenthesis. Then filter out the any non matched strings
     by checking the substring that doesn't include the last and first
     element to see it is closed. Then we check to see the maximum length
     out of all the substrings left.  *)

end
