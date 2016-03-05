functor MkRangeCount(structure OrdTable : ORD_TABLE) : RANGE_COUNT =
struct
  structure OrdTable = OrdTable
  open OrdTable

  (* Ordered table type: *)
  type 'a table = 'a table
  type 'a seq = 'a Seq.seq
  type point = Key.t * Key.t

  (* Use this to compare x- or y-values. *)
  val compareKey : (Key.t * Key.t) -> order = Key.compare


  (* Define this yourself *)
  type countTable = (Key.t table * Key.t) table 


  (*Our data structure will be a table of tables. The keys will be
    the x-values for each point from the point sequence given
    the values will be a table of all the y-value points that are to the
    left of the current x-valued point. Like the three sided query
    the table will house all the y-values that derive from the points
    that come to the left of the key x-value point*)
  fun makeCountTable (S : point seq) : countTable =
      let

         (*sort the points by the increasing x-value
           this way when we are entering in the table for our
           data structure it will add in the all the x-values
           that came before it*)
         fun comparePoints ((x1,_),(x2,_)) = compareKey(x1,x2)
         val sortedS = Seq.sort(comparePoints)(S)


         val xToYTable = fromSeq(sortedS)


         fun insertTable ((T,YTable),(x,y)) = 
             let
                  val YTable' = insert(fn (v,v') => v)((y,y))(YTable)
                  val T' = insert(fn (v,v') => v)((x,(YTable',y)))(T)
             in
                  (T',YTable')
             end
         
      (*So what this will do is it will keep track of all the y-values that 
        that came before and insert them into the other table that is passed
         *)
         val (finTable,_) = Seq.iter(insertTable)((empty(),empty()))(sortedS)
      
       in
          finTable
       end
      
               
   (* from the our data structure we can get all the y-values to the
      left of xLeft and the all the y-valuse that is to lthe left of
      xRight. Then we can use getRange to find the ranges of y that is
      inbetween yHI and yLo. Afterwards we just get the sizes and 
      subtract the two to get the number of points that is in the
      rectangle*)
  fun count (T : countTable)
                   ((xLeft, yHi) : point, (xRght, yLo) : point) : int  =
      
      let
           val SOME(_,(leftLine,y1)) = case (find(T)(xLeft)) of
                                     NONE => previous(T)(xLeft)
                                   | SOME(v) => SOME(xLeft,v)
           val leftYrange = getRange(leftLine)(yLo,yHi)
    
           val addition = case (compareKey(y1,yHi),compareKey(y1,yLo)) of
                            (GREATER,_) => 0
                          | (_,LESS) => 0
                          | (_,_) => 1


           val SOME(_,(rightLine,y2)) = case find(T)(xRght) of
                              NONE => previous(T)(xRght)
                            | SOME(v) => SOME(xRght,v)
           val rightYrange =  getRange(rightLine)(yLo,yHi)
      
      in
           size(rightYrange) - size(leftYrange) + addition
      end
       
end
