functor MkBigNumMultiply(structure BNA : BIGNUM_ADD
                         structure BNS : BIGNUM_SUBTRACT
                         sharing BNA.Util = BNS.Util) : BIGNUM_MULTIPLY =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  (* Remove this line when you're done. *)
  exception DifferentSizes

  infix 6 ++ --
  infix 7 **

  fun x ++ y = BNA.add (x, y)
  fun x -- y = BNS.sub (x, y)
  fun x ** y =
      let
      fun multiply b1 b2 =
          let
          val (longer,other)  = if(length(b1) > length(b2))
                                then (b1,b2)
                                else (b2,b1)
          in
          case (showt(longer),showt(other)) of
           (EMPTY,EMPTY) => empty()
         | (EMPTY,ELT(_)) => empty()
         | (ELT(_),EMPTY) => empty()
         | (ELT(a),ELT(b)) => (case (a,b) of
                                 (ZERO,ZERO) => empty()
                               | (ONE,ONE) => singleton(ONE):bignum
                               | (ONE,ZERO) => empty()
                               | (ZERO,ONE) => empty())
         | (NODE(_,_),EMPTY) => empty()
         | (NODE(left,right),ELT(single)) => (if(single = ZERO)
                                      then  empty()
                                      else append(left,right))
         |(NODE(_,_),NODE(_,_)) => 
                let
                val (p,q) = (drop(longer,length(longer) div 2)
                            ,take(longer,length(longer) div 2))
                val totlength = length(p) + length(q)
                val (s,r) = if(length(other) <= length(q))
                            then (other,empty())
                            else (take(other,length(q))
                                 ,drop(other,length(q)))
                val pPlusq = p ++ q
                val rPluss = r ++ s
                val (pr,qs,pqTimesrs) = Primitives.par3(fn() => p ** r,
                                  fn() =>  q ** s,
                                  fn() =>  pPlusq ** rPluss)
                val psPlusrp = (pqTimesrs -- pr) -- qs
                val prshifted = append(tabulate(fn i => ZERO)(2 * length(q)),pr)
                val middshift = 
                         append(tabulate(fn i => ZERO)(length(q)),psPlusrp)
                in
                prshifted ++ middshift ++ qs
                end
          end
       in
       multiply(x)(y)
       end

  val mul = op**
end
