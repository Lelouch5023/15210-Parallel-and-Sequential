structure Tester =
struct
  open ArraySequence

  structure Bridges : BRIDGES =
    MkBridges(structure STSeq = MkSTSequence(structure Seq = ArraySequence))

  functor MkAStar(structure Vtx : HASHKEY) : ASTAR =
    MkAStarCore(structure Table = MkTreapTable(structure HashKey = Vtx)
                structure PQ = MkSkewBinomialHeapPQ(structure OrdKey = RealElt))

  structure IntAStar : ASTAR =
    MkAStar(structure Vtx = IntElt)

  structure StringAStar : ASTAR =
    MkAStar(structure Vtx = StringElt)

  structure SetThis = IntAStar.Set

  val test = %[(1,2,1.0),(2,3,10.0),(2,4,1.0),(2,5,1.0),(4,5,1.0),(4,3,1.0)]
  val stateS = SetThis.singleton(1)
  val stateT = SetThis.singleton(3) 
  val testpath = IntAStar.makeGraph(test)
  val h = (fn i => 0.0)
  val init  = IntAStar.findPath h testpath
  


end
