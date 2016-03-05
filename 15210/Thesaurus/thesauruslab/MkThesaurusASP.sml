functor MkThesaurusASP (ASP : ALL_SHORTEST_PATHS where type vertex = string)
  : THESAURUS =
struct
  structure Seq = ASP.Seq
  open Seq

  (* Remove the following two lines when you're done! *)
  exception NotYetImplemented
  type nyi = unit

  (* You must define the following type *)
  type thesaurus = ASP.graph

  fun make (S : (string * string seq) seq) : thesaurus =
      let (* turns S into the correct format then uses makeGraph*)
      fun matching (x,xs) = tabulate(fn i => (x,nth xs i)) (length(xs))
      val newSeq = flatten(map matching S)
      in
      ASP.makeGraph(newSeq)
      end

  fun numWords (T : thesaurus) : int =
      ASP.numVertices(T)

  fun synonyms (T : thesaurus) (w : string) : string seq =
      ASP.outNeighbors(T)(w)

  fun query (T : thesaurus) (w1 : string) : string ->  string seq seq =
      let
      val firststage = ASP.makeASP(T)(w1)
      in
      (fn y => ASP.report(firststage)(y))
      end
end
