functor MkBridges(structure STSeq : ST_SEQUENCE) : BRIDGES =
struct
  structure Seq = STSeq.Seq
  open Seq

  type vertex = int
  type edge = vertex * vertex
  type edges = edge seq

  (* Remove these two lines before submittting. *)
  exception NotYetImplemented

  type ugraph = vertex seq seq

  fun makeGraph (E : edge seq) : ugraph = 
      let
      fun reverse (v1,v2) = (v2,v1)
      val reversed = tabulate(fn i => reverse(nth E i))(length E)
      val alledges = append(E,reversed)
      val result = map(fn (x,xs) => xs)(collect (Int.compare)(alledges))
      in
      result
      end

  fun findBridges (G : ugraph) : edges = 
      let(*returns DFS numbers along with potential bridges*)
      fun DFS(v1)((X,S,B,p),v2) = 
          if(STSeq.nth X v2) 
          then (X,S,B,p)
          else
             let
             val X' = STSeq.update(v2,true)(X)
             val B' = STSeq.update(v2,(v1,v2))(B)
             val (X'',S',B',p') = iter(DFS(v2))(X',S,B',p+1)(nth G v2)
             val exitState = STSeq.update(v2,(p,p'))(S')
             in
             (X'',exitState,B',p'+1)
             end
      (*makes sure to go through all vertices for the DFS*)
      fun checker ((check,state,bri,p),v) = 
          if(STSeq.nth check v)
          then (check,state,bri,p)
          else DFS(v)((check,state,bri,p+1),v)
      val initState = STSeq.fromSeq(tabulate(fn i => false)(length G))
      val initState1 = STSeq.fromSeq(tabulate(fn i => (~1,~1))(length G))
      val initState2 = STSeq.fromSeq(tabulate(fn i => (0,0))(length G))
      val (_,DFSnums,brid,_) = iter checker (initState,initState1,initState2,1) 
                               (tabulate (fn i => i)(length G))
      val brid2 = STSeq.toSeq(brid)
      val bridfilt = filter (fn (x,y) => x >= 0 andalso y >= 0)(brid2)
      fun isEmpty xs = 
          case showl(xs) of
            NIL => true
          | CONS(_,_) => false
      (*checks to see if v1 is a ancestor of v2*)
      fun checkBackE(v1)(v2) = 
          let
          val (a1,b1) = STSeq.nth DFSnums v1
          val (a2,b2) = STSeq.nth DFSnums v2
          in
          (a2 > (a1+1) andalso b2 < b1)
          end
      fun checkPandC (v1,v2) = 
          let
          val v1Neighbors = nth G v1
          val v2Neighbors = nth G v2
          val anyChild = filter(fn x => checkBackE(v1)(x))(v1Neighbors)
          val anyParent = filter(fn x => checkBackE(x)(v2))(v2Neighbors)
          in
          isEmpty(anyChild) andalso isEmpty(anyParent)
          end
      in
      filter(checkPandC)(bridfilt)
      end

     
          
      

end
