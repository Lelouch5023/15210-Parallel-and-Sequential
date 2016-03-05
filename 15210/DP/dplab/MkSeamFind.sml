functor MkSeamFind(structure Seq : SEQUENCE) : SEAMFIND =
struct
  structure Seq = Seq
  open Seq

  (* Remove when you're done! *)
  exception NotYetImplemented

  type pixel = { r : real, g : real, b : real }
  type image = { width : int, height : int, data : pixel seq seq }
  type gradient = real

  fun findSeam G =
      let
          fun minIndex ((i:int,m1:real),(j:int,m2:real)) = 
              if(m1 > m2) 
              then (j,m2) 
              else (i,m1)
          fun findMin (0,m) (mValues:real seq) = 
                 let
                    val (indexed,m2) = minIndex((0,nth mValues 0),
                                             (1,nth mValues 1))
                 in
                    (indexed,m+m2)
                 end
            | findMin (i,m) (mValues:real seq) = 
                   if( i = length(mValues)-1 )

                   then 
                       let
                           val (indexed,m2) = minIndex((i,nth mValues i),
                                                       (i-1,nth(mValues)(i-1)))
                        in
                           (indexed,m2+m)
                        end
                    else
                        let
                            val (indexed,m2) = minIndex((i-1,nth(mValues)(i-1)),
                                               minIndex((i,nth(mValues)(i)),
                                                       (i+1,nth(mValues)(i+1))))
                        in
                            (indexed,m+m2)
                        end

          fun findPMValue (previous,current) = 
              let
                 val nonIndexed = map(fn (i,x) => x)(previous)
                 val indexedC = mapIdx(fn (i,x) =>(i,x))(current)
                 val pass = map(fn x => findMin x nonIndexed)(indexedC)
               in
                 pass
               end
          val zeroRow = (tabulate(fn i => (i,0.0))(length(nth G 0)))
          val (first,last)  = iterh(findPMValue)(zeroRow)(G)
          val result = append(drop(first,1),singleton(last))
	  val reversedResult = rev(result)
          
          fun lowestSeam (i,seqM) = 
              let
                 val (j,_) = nth seqM i
              in
                 j
              end

          fun pickIndex ((ind1,(i,m1)),(ind2,(j,m2))) = if(m1 > m2)
                                then (ind2,(j,m2))
                                else (ind1,(i,m1))

        
          val initialValues = nth (nth reversedResult 0) 0
          val lastRow = nth reversedResult 0
          val lastRowIdx = mapIdx(fn (i,x) => (i,x))(lastRow)
          val (lowestIndex,(lowIn,lowCost)) = iter(pickIndex)((0,initialValues))
                                              (lastRowIdx)
          val (lowestSeamS,extra) =  
                            iterh(lowestSeam)(lowestIndex)
                                 (reversedResult)
      in
      rev(lowestSeamS)
      end
end





