functor MkPartialArraySequence (structure BareSeq : BARESEQUENCE)
                                  : PARTIALSEQUENCE =
struct
  open BareSeq

  (* Remove this line when you're done *)
  exception NotYetImplemented

  (* Task 5.1 *)

  fun rev s =
      let val n = length s
      in tabulate (fn i => nth s (n-1-i)) n
      end

  fun map f s =
      let val n = length s
      in tabulate (fn i =>f( nth s i))  n
      end

  fun enum s =
      let val n = length s
      in tabulate (fn i => (i, nth s i)) n
      end

  fun mapIdx f s =
      let val n = length s
      in tabulate (fn i => f(i,nth s i)) n
      end

  fun append (s, t) =
      let
      val first = length s
      val second = length t
      in tabulate (fn i => if i < first
                           then nth s i
                           else nth t (i-first))
                   (first + second)
      end

  fun iter f b s = 
      case showl(s) of
       NIL => b
     | CONS(x,rest) => 
                iter f (f(b,x)) rest

 (* helper function for iterh will proudce a sequence of all the 
    partial results except it will not calucate the last an final 
    result iterh will handle that  *)
  fun iterHelp f b s = case showl(s) of
                      NIL => singleton(b)
                    | CONS(x,rest) => 
                                  tabulate
                                  (fn 0 => b
                                    | i => nth (iterHelp f (f(b,x)) rest) 
                                                (i-1))
                                   (length(s))

  fun iterh f b s =
      case showl(s) of
       NIL => (empty(),b)
      | _ =>let
            val fin = iterHelp f b s
            in
            (fin, f(nth fin (length(fin)-1),nth s (length(s)-1)))
            end 

  fun toList s = 
      case showl(s) of
        NIL => nil
      | CONS(x,rest) => x::toList rest

  (* Task 5.5 *)

  fun toString f s = 
   "<" ^ String.concatWith(",")(toList(map f s)) ^ ">"




end 
