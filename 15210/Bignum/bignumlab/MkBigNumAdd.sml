functor MkBigNumAdd(structure U : BIGNUM_UTIL) : BIGNUM_ADD =
struct
  structure Util = U
  open Util
  open Seq

  (* Remove this line when you're done. *)
  exception WrongCarry

  infix 6 ++

  datatype carry = GEN | PROP | STOP

  fun x ++ y =
      let
      fun carrying (pass,PROP) = pass           
        | carrying (PROP,pass) = pass
        | carrying (_,GEN) = GEN
        | carrying (_,STOP) = STOP
      fun setup b1 b2 = 
          case (b1,b2) of
         (ZERO,ZERO) => STOP
       | (ZERO,ONE) => PROP
       | (ONE,ZERO) => PROP
       | (ONE,ONE) => GEN
       fun laststep car1 car2 = 
          case (car1,car2) of
          (GEN,GEN) => ONE
        | (STOP,STOP) => ZERO
        | (PROP,STOP) => ONE
        | (PROP,GEN) => ZERO
        | (STOP,GEN) => ONE
        | (GEN,STOP) => ZERO
        | (_,_) => raise WrongCarry
      (* this funcion assumes that the first argument is greater
         than the second argument  *)
      fun addfromcarry x1 y1 = 
          let
          val n = length y1
          val funin = (fn i => if(i >= n)
                               then setup (nth x1 i) (ZERO)
                               else
                                 setup (nth x1 i) (nth y1 i))
           val setSeq = tabulate funin (length x1)
           val (wholeSeq,last) = scan carrying STOP setSeq
           val fin = tabulate (fn i => 
                               laststep (nth setSeq i)
                                        (nth wholeSeq i))
                              (length x1)
           val _ = print(Int.toString(length(fin)))
           in
           if( last = GEN)
           then append (fin,(singleton(ONE)))
           else fin
           end
      in
      if( (length x) > (length y))
      then addfromcarry x y
      else addfromcarry y x
      end

  val add = op++
end
