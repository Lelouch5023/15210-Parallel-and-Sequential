structure Tests =
struct

  val tests = List.map ArraySequence.% [
    [(1,1,2)],
    [(1,1,3),(2,1,4)],
    [(4,5,20),(1,3,5),(2,4,6),(8,7,11),(12,11,13),(10,10,14),(17,2,21)],
    [(12,11,13),(10,10,14),(17,2,21)],
    [(12,11,13),(10,10,14)],
    [(1,3,4),(3,2,11),(6,6,8),(7,4,10)],
    [(20,10000,90000)]
  ]

end
