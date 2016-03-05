functor MkSeqUtil(structure Seq : SEQUENCE) : SEQ_UTIL =
struct
  structure Seq = Seq
  open Seq

  (* remove this line before submitting *)
  exception NotImplemented

  type 'a hist = ('a * int) seq

  fun histogram (cmp : 'a ord) (s : 'a seq) : 'a hist =
      let
      val indexed = map (fn (i,x) => (x,i)) (enum s)
      in
      map (fn (x,s2) => (x,length(s2))) (collect cmp indexed)
      end

  fun choose (hist : 'a hist) (p : real) : 'a =
      let
      val total = reduce (fn (x,y) => x+y) (0) (map(fn (x,tot) => tot) hist)
      fun cdf inx cumil =
          let
          val (elem,amount) = nth hist inx
          val currcumil = cumil + Real.fromInt(amount)
          val cdfpoint = currcumil / Real.fromInt(total)
          in
          if(cdfpoint >= p orelse inx >= length(hist))
          then elem
          else cdf (inx+1) (currcumil)
          end
       in
       cdf (0) (0.0)
       end
          
end
