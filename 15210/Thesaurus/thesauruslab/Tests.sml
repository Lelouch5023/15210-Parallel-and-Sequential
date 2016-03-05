structure Tests =
struct
  structure T = MkTreapTable(structure HashKey = IntElt)
  structure S = T.Seq
  open S

  type edge = int * int

  val testfile = "support/thesaurus.txt"

  (* A trivial test that has a graph containing 2 vertices and an edge *)
  val edgeseq = [(1,2)]
  val edgeseq2 = [(1,2),(1,3),(2,4),(3,4),(4,5),(5,6),(5,7)]
  val edgeseq3 = [(1,2),(1,3),(1,4),(1,5),(5,6),(2,6),(3,4),(4,6)]
  
  (* Tests *)
  
  val testsNum = [edgeseq,edgeseq2,edgeseq3];

  val testsOutNeighbors = [(edgeseq, 1),(edgeseq2,2),(edgeseq3,1)]

  val testsReport = [((edgeseq, 1), 2),((edgeseq2,1),7),((edgeseq3,1),4),
                     ((edgeseq3,1),6)]

  val testsNumWords =  [testfile]

  val testsSynonyms = [(testfile, "HELLO")]

  val testsQuery = [(testfile, ("GOOD", "BAD")),(testfile,("FUN","MISERY"))]
end
