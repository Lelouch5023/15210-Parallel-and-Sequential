structure Tests =
struct
  structure Seq = ArraySequence
  open Seq

  type point = int * int

  (* Here are a couple of test structures. ordSet1 is a test ordered set
   * (table), while points1 is a test collection of points in the plane.
   *
   * Note that for an ordered table test, you should just specify a sequence
   * of keys (since that's all that matters). *)

  val ordSet1 = % [5, 7, 2, 8, 9, 1]

  val testsFirst = [
    ordSet1
  ]
  val testsLast = [
    ordSet1
  ]
  val testsPrev = [
    (ordSet1, 8)
  ]
  val testsNext = [
    (ordSet1, 8)
  ]
  val testsJoin = [
    (ordSet1, % [100])
  ]
  val testsSplit = [
    (ordSet1, 7)
  ]
  val testsRange = [
    (ordSet1, (5,8))
  ]


  val points1 = % [(0,0),(1,2),(3,3),(4,4),(5,1)]
  val points2 = % [(1,2),(0,0),(13,5),(2,4),(3,7),(10,8),(8,12)]

  val testsCount = [
    (points1, ((1,3),(5,1)))
  , (points1, ((3,3),(6,1)))
  , (points2, ((2,12),(5,5)))
  , (points2, ((2,12),(5,6)))
  ]


end
