functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  (* Remove the following when you're done! *)
  exception NotYetImplemented
  type nyi = unit

  (* Table.key defines our vertex type *)
  type vertex = key
  type edge = vertex * vertex

  (* You must define the following two types *)
  type graph = (vertex Seq.seq  Table.table*int)
  type asp = vertex Seq.seq Table.table

  fun makeGraph (E : edge seq) : graph =
      (Table.collect(E),length(E))

  fun numEdges ((g,numedges) : graph) : int =
      numedges
      

  fun numVertices (((g,numedges)) : graph) : int =
      let
      val grange =Set.fromSeq(Seq.flatten(Table.range g))
      val gdomain = Table.domain g
      in
      Set.size(Set.union(grange,gdomain))
      end
  

  fun outNeighbors ((g,numEdges) : graph) (v : vertex) : vertex seq =
      case (find g v) of
        NONE => Seq.empty()
      | SOME(vs) => vs
      

  fun makeASP (G : graph) (v : vertex) : asp =
      let
      (*merge does the same thing as it always does but when 
       it comes to a key that this the same from both A,B it will just
       combine the two sequences so that it is a vertex sequence sequence*)
      fun merge (A,B) = Table.merge (fn (a,b) => Seq.append(a,b)) (A,B)
      fun tag ((g,numEdges):graph)(F) = (*Tags where the vertices come from *)
          let
          fun tagN v' = Table.tabulate
                             (fn _ => Seq.singleton(v'))
                             (Set.fromSeq(outNeighbors (g,numEdges) (v')))
                              (*goes through all the neighbors and tags it
                                with the original vertex*)
          val tagged = Table.tabulate tagN F
          in
          Table.reduce merge (Table.empty()) tagged
          end
      fun BFS X F = 
           if(Table.size(F) = 0)
           then X
           else (*same code as from lecture*)
               let
               val X' = merge(X,F)
               val Ngh = tag (G)(Table.domain(F))
               val F' = Table.erase(Ngh,Table.domain X')
               in
               BFS X' F'
               end
      in(*the source vertex gets matched to an empty sequence this is 
           how we will now when to stop when searching for the certain
           shortest paths*)
      BFS (Table.empty())(Table.singleton(v,Seq.empty()))
      end
      

 (*this will go through and look at the last vertex and find it in A:ASP
    which will give the next vertex or vertices, then we go through and 
   do this again until we reach the source vertex.*)
  fun report (A : asp) (v : vertex) : vertex seq seq =
      let
      fun finding A x vs = 
          case Table.find(A)(x) of
            NONE => Seq.empty()
          | SOME(xs) =>
               case Seq.showl(xs) of
                    NIL => map (fn i => Seq.append(Seq.singleton(x),i)) vs
                  | CONS(_,_) =>
                     let
                     val vs' = map (fn i => Seq.append(Seq.singleton(x),i))
                                   (vs)
                     val branch = map (fn i => finding A i vs')
                                      (xs)
                     val result = Seq.flatten(branch)
                     in
                     Seq.flatten(branch)
                     end
      val start = Seq.singleton(Seq.singleton(v))
      in (*does the first iteration before putting into the function*)
      case Table.find A v of
        NONE => Seq.empty()
      | SOME(bs) => Seq.flatten(Seq.map
                                    (fn i => finding A i start)
                                    (bs))
      end
                      
      
end
