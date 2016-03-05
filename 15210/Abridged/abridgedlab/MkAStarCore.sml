functor MkAStarCore(structure Table : TABLE
                    structure PQ : PQUEUE
                      where type Key.t = real) : ASTAR =
struct
  structure Set = Table.Set
  structure Seq = Set.Seq

  type weight = real
  type vertex = Set.key
  type edge = vertex * vertex * weight
  type heuristic = vertex -> real

  (* Define this type yourself *)
  type graph = (vertex * weight) Seq.seq Table.table

  fun makeGraph (E : edge Seq.seq) : graph = 
      let
      val divi = Seq.map (fn (v1,v2,w) => (v1,(v2,w))) E
      in
      Table.collect(divi)
      end

  fun findPath h G (S, T) =
      let
      fun neighbors v = 
          case Table.find(G)(v) of
               NONE => Seq.empty()
             | SOME(xs) => xs
      fun dijkstra (X,Q) = 
      case PQ.deleteMin(Q) of
        (NONE,_) => NONE
      | (SOME(d,(total,v)),Q') => 
           if(Set.find(X)(v))
           then dijkstra(X,Q')
           else
               if(Set.find(T)(v))
               then SOME(v,d-total)
               else
                 let
                 val X' = Set.insert(v)(X)
                 fun relax (tempQ,(u,w)) = 
                    PQ.insert(d+w+h(u),(total+h(u),u))(tempQ)
                  val Q'' = Seq.iter (relax) (Q') (neighbors(v))
                 in
                 dijkstra (X',Q'')
                 end
       val seqT = Set.toSeq(S)
       val seqTW = Seq.map(fn x => (0.0+h(x),(h(x),x)))(seqT)
       val listTW = Seq.toList(seqTW)
       val initState = PQ.fromList(listTW) 
       in
       dijkstra(Set.empty(),initState)
       end


end
