functor MkBSTOrderedTable (structure Tree : BSTREE
                           structure Seq : SEQUENCE) : ORD_TABLE =
struct

  structure Table = MkBSTTable(structure Tree = Tree
                               structure Seq = Seq)

  (* Include all the functionalities of the standard Table *)
  open Table

  (* This is defined after "open" so it doesn't get overwritten *)
  structure Key = Tree.Key
  type key = Key.t

  (* Remember, type 'a table = 'a Tree.bst *)

  (* Remove this line before submitting! *)
  exception NYI

  fun first (T : 'a table) : (key * 'a) option =
      let
          fun getToMin (TT:'a table)(output: (key * 'a) option) =
              case Tree.expose(TT) of
                NONE => output
              | SOME{left = L, key = k,value = v,...} => 
                                          getToMin(L)(SOME(k,v))
      in
          getToMin(T)(NONE)
      end
      

  fun last (T : 'a table) : (key * 'a) option =
      let
           fun getToMax (TT: 'a table)(output: (key*'a) option) = 
               case Tree.expose(TT) of
                 NONE => output
               | SOME{right = R,key = k,value = v,...} =>
                                             getToMax(R)(SOME(k,v))
      in
           getToMax(T)(NONE)
      end

  fun previous (T : 'a table) (k : key) : (key * 'a) option =
      let
        val (L2,_,_) =  Tree.splitAt(T,k) 
      in
     ( case Tree.expose(L2) of
          NONE => NONE 
        | SOME{key = k',value = v',...} => SOME(k',v')  )
      end

      

  fun next (T : 'a table) (k : key) : (key * 'a) option =
      let
         val (L2,_,R2) = Tree.splitAt(T,k)
      in
         case Tree.expose(R2) of
              NONE => NONE
            | SOME{key = k',value = v',...} => SOME(k',v')
      end

  fun join (L : 'a table, R : 'a table) : 'a table =
                                  Tree.join(L,R)

  fun split (T : 'a table, k : key) : 'a table * 'a option * 'a table =
                                   Tree.splitAt(T,k)

  fun getRange (T : 'a table) (low : key, high : key) : 'a table =
      let
         val (_,lowIn,rightSide) = split(T,low)
         val passTree = case lowIn of
                           NONE => rightSide
                         | SOME(v) => join(singleton(low,v),rightSide)


         val (leftSide,highIn,_) = split(passTree,high)
      in
      case highIn of
         NONE => leftSide
       | SOME(v) => join(leftSide,singleton(high,v))
     end

end
