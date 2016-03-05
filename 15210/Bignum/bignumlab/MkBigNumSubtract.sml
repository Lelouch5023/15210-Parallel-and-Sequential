functor MkBigNumSubtract(structure BNA : BIGNUM_ADD) : BIGNUM_SUBTRACT =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  (* Remove this line when you're done. *)
  exception NotYetImplemented

  infix 6 ++ --

  fun x ++ y = BNA.add (x, y)
  fun x -- y =
      let
      fun flip ZERO = ONE
        | flip ONE = ZERO
      val x1 = append(x,singleton(ZERO))
      val twoComY = append (y,tabulate(fn i => ZERO)(length(x)-length(y)+1))
      val inverseY = tabulate (fn i => flip(nth twoComY i)) (length twoComY)
      val inverseY2 = inverseY ++ singleton(ONE)
      val result = x ++ inverseY2
      val lengthfin = length result
      fun takeAway s =
          if(nth s (length(s)-1) = ONE)
          then s
          else takeAway (take(s,length(s)-1))
      val result2 = if(lengthfin > length(x))
                    then take(result,lengthfin-1)
                    else result
      val returning =takeAway(result2)
      in
      returning
      end

  val sub = op--
end
