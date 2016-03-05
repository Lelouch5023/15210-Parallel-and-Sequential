structure Tests =
struct
  (* testsMST: list of (E, n) pairs, where E is a list of edges (u,v,w) and
   *   n is the number of vertices
   *
   * note: sum of edge weights must be strictly less than the
   *   maximum integer (valOf Int.maxInt) *)
  val testsMST =
    [
      ([(0,2,10), (1,0,5)], 3),
      ([(0,1,10), (0,2,2),(0,3,5),(1,4,10),(2,1,10),(2,5,2),(4,5,2)],6)
    ]
end
