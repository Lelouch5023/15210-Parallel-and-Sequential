structure Tests =
struct
  val testsAdd : (IntInf.int * IntInf.int) list = [
    (4000,3334),
    (3,2),
    (123, 937),
    (10001,456432),
    (45454,12121),
    (0,0)
  ]

  val testsSub : (IntInf.int * IntInf.int) list = [
    (4000,3334),
    (3,2),
    (1024, 937)
  ]

  val testsMul : (IntInf.int * IntInf.int) list = [
    (4000,3334),
    (3,2),
    (123, 937),
    (4,8),
    (1,2),
    (64,0),
    (3,3),
    (5,5),
    (2,2),
    (1,1)
  ]

end
