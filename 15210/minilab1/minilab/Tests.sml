structure Tests =
struct
  structure Seq = BareArraySequence
  val % = Seq.fromList

  (********************************************)
  (*** Tests for MkPartialArraySequence.sml ***)
  (********************************************)

  (* Add test cases to each list, to test your implementation of that sequence
   * function, using the anonymous function in the comment (if applicable). The
   * type of each test case depends on the function; follow the examples
   * provided. Note that the '%'' function declared above converts a given list
   * into a sequence.
   *
   * For example, the first list testsMapIntDouble as it is given to you, runs
   *     map (fn x => 2*x) <1, 2, 3>
   *     map (fn x => 2*x) <2, 4>
   * against your implementation, and ours, and compares for equality.
   *)

  (* map (fn x => 2*x) S *)
  val testsMapIntDouble = [
    % [1, 2, 3],
    % [2, 4],
    % [0,1,3,4,100]
  ]

  (* enum S *)
  val testsEnumInt = [
    % [1, 2, 3],
    % [0,~1,3,2,1,1,1,1,11,1]
  ]

  (* mapIdx (fn (i,x) => i*x) S *)
  val testsMapIdxIntMult = [
    % [1, 2, 3],
    % [~2,0,3,2,5,0] 
  ]

  (* append (S, T) *)
  val testsAppendStr = [
    (% ["Hello"], % ["World", "!"]),
    (%[""],%["","","FIX IT"])
  ]

  (* iterh (fn (s,x) => s^(Int.toString x)) "" S *)
  val testsIterhIntJoin = [
    % [1, 2, 3],
    % [0,1,2,3,4,5,6,4,~1]
  ]

  (* iter (fn (s,x) => s*x) 1 S *)
  val testsIterIntMult = [
    % [1, 2, 3],
    % [~1,1,~2,~4]
  ]

  (* toList S *)
  val testsToListInt = [
    % [1, 2, 3]
  ]

  (* toString Int.toString S *)
  val testsToStringInt = [
    % [1, 2, 3]
  ]

  (* toString Bool.toString S *)
  val testsToStringBool = [
    % [true, false]
  ]

  (******************************)
  (*** Tests for MkSeqFun.sml ***)
  (******************************)

  (* List of n values *)
  val testsAllHarmonics = [5,6,7]

  (* List of (n,k) values *)
  val testsGroupedHarmonics = [(6,3),(5,2)]
end
