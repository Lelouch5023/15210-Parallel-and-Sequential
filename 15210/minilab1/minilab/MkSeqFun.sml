functor MkSeqFun (structure Seq : PARTIALSEQUENCE) : SEQFUN =
struct
  open Seq

  (* Remove this line when you're done *)
  exception NotYetImplemented
  exception BadHarmonics

  (* Task 5.3 *)

  fun allHarmonics n =
      if n <= 0 
      then raise BadHarmonics
      else
              let
              val origin =  tabulate(fn i => 1.0/(Real.fromInt(i)+2.0)) (n)
              val (fin,_) = iterh (fn (x,y) => x+y) 1.0 origin
              in
              fin
              end
      

  (* Task 5.4 *)
 (* helper function for groupedHarmonics it will essentially do the 
    bulk of the work that groupedHarmonics is supposed to do by grouping
    and splitting a given sequence s into k other sequences
    groupedHarmonics will feed this function the result of allHarmonics
    and then use enum to enumerate the sequence *)
  fun groupHelp k s =
      if length s <= k
      then singleton(s)
      else tabulate (fn 0 => take (s,k)
                      | x => nth (groupHelp k  (drop (s,k))) (x-1))
             (Real.ceil(Real.fromInt(length(s)) / Real.fromInt(k)))

  fun groupedHarmonics n k =
      let
      val origin = allHarmonics (n)
      val modified = groupHelp k origin
      in
      enum modified
      end

  (* Task 5.6 *)
  (* helper function It is used organize the code a little bit
     to make it more readable. Esssentially this converts each
     element (e,s) where e is a int and s is a sequence of reals
     into a nice looking string  *)
  fun printHelper (e,s) = 
      "(" ^ Int.toString(e) ^ ", " ^ Seq.toString Real.toString s ^ ")"

  fun printGroups G = print(Seq.toString printHelper G ^ "\n")
      
end
