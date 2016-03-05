functor MkBoruvkaSegmenter
  (structure Seq : SEQUENCE
   structure Rand : RANDOM210
   sharing Seq = Rand.Seq)
  : SEGMENTER =
struct
  structure Seq = Rand.Seq
  open Seq

  structure R = Rand
  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight

  (*This is minEdges given to us in lecture*)    
  fun minEdges(E,n) =
      let

          fun compareEdges ((u1,(v1,w1,L1)),(u2,(v2,w2,L2))) = 
                                                      Int.compare(w2,w1)

          val blankEdges = tabulate(fn i => (~1,~1,~1))(n)

          val sortedE = sort(compareEdges)(E)


          val ET = inject(sortedE)(blankEdges)
          val ETaddV = tabulate(fn i => (i,nth ET i))(length ET)
          val ETmodify = filter(fn (_,(x,_,_)) => not(x = ~1))(ETaddV)

      in
          ETmodify
      end

  fun removeEdges (credInput) ((u,v,w),L) =
      let
          val min = Int.min(nth credInput u,nth credInput v)
      in
          if(w > min orelse min < 0) 
          then false
          else true
      end

 (*This is just the minStarContract given to us 
   in lecture*)
  fun minStarContract ((E, n),random) =
      let
      (* 0 = heads, 1 = tails*)
          val (headsTails,rando) = Rand.flip(random)(n)
          val modifyE = map(fn ((u,v,w),L) => (u,(v,w,L)))(E)
          val minE = minEdges(modifyE,n)
   
       
          fun tailsToHeads (u,(v,w,L)) =
          if(nth headsTails u = 1 andalso nth headsTails v = 0)
          then (u,(v,w,L),true)
          else (u,(v,w,L),false)


          val punfiltered = 
                tabulate(fn i => tailsToHeads(nth minE i))(length minE)
          val P = filter(fn (_,_,x) => x)(punfiltered)
          val V' = filter(fn (u,(v,w,L),x) => not x)(punfiltered)

      in
          (V',P,rando)
      end
          
 (* Thi is MST given to us in lecture it will output
    a sequence where the index is represent of the vertex
    and the actual element is vertex where it got contracted to
    it will also return all the contracted edges.*)
 fun MST ((E,T,Pinput, n),random) creditSeq = 
     if(length E = 0)
     then (Pinput,T)
     else
      let
      
          fun compareCred((u1,v1),(u2,v2)) = 
                     Int.compare(nth creditSeq u2,nth creditSeq u1)

          val (V',PT,randomNext) = minStarContract((E,n),random)

        
          val pfirst = map(fn (u,(v,_,_),_) => (u,v))(PT)
          val plast = map(fn (u,_,_) => (u,u))(V')

        
          val P = append((pfirst),(plast))
          val P' = inject(P)(Pinput)
          val P'' = map(fn x => nth P' x)(P')

        (* This is to find the minimum credit value of all the
           satelite vertices*)
          val P'cred = 
                map(fn (u,v) => (v,nth creditSeq u))(sort(compareCred)(P))
          val minCreditSeq = inject(P'cred)(creditSeq)
          val T' = map(fn (u,(v,w,L),_) =>(u,v,w))(PT)

        (*This is to find the sum of all the weights from the 
          contracted vertices and to update the value
          from the minimum credit of all teh contracted verties*)
          val flipsT' = map(fn (u,v,w) => (v,w))(T')
          val collectEdges = collect ( Int.compare)(flipsT')
          val sumWeights = 
                         map(fn (x,xs) => (x,iter(op+)(0)(xs)))(collectEdges)
          val credsWithMin = 
                       map(fn (x,y) => (x,(nth minCreditSeq x) - y))(sumWeights)
          val newCredSeq = inject(credsWithMin)(minCreditSeq)

         (*this is to filter out all the uneeded vertices and to recursively
           call MST on that*)
         val Tfinal = append(T,T')
         val E' = map(fn ((u,v,w),L) => ((nth P' u,nth P' v,w),L))(E)
         val E'' = filter(fn ((u,v,w),L) => not(u = v))(E')
         val E''' = filter(removeEdges(newCredSeq))(E'')

      in
         MST((E''',Tfinal,P'',n),randomNext)(newCredSeq)
      end

 fun findSegments (E,n) initialCredit =
     let

         val initialRandom = Rand.fromInt(0)
         val E' = tabulate(fn i => (nth E i,i))(length E)

        (* all the initial values of the vertices along with 
           their credits.*)
         val credSeq = tabulate(fn i => initialCredit)(n)
         val E'' = filter(removeEdges(credSeq))(E')
         val Pinput = tabulate(fn i => i)(n)

         val (R,T) = MST((E',empty(),Pinput,n),initialRandom)(credSeq)
     in
         (R,T)
     end
      
      
      
      
end
