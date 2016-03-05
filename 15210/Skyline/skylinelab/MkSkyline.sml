functor MkSkyline(structure S : SEQUENCE) : SKYLINE =
struct
  structure Seq = S
  open Primitives
  open Seq

  exception NotYetImplemented

  fun skyline (buildings : (int * int * int) seq) : (int * int) seq =
      let
        fun computeSkyline S =
            case showt S of
              EMPTY => singleton((0,0))
            | ELT (l,h,r) => fromList[(l,h),(r,0)]
            | NODE(L, R) => let
                            val (leftlist,rightlist) = 
                                 par(fn () => computeSkyline L
                                    ,fn () => computeSkyline R)
                            val leftlist2 = 
                                 map (fn (x,h1) => (x,h1,0)) leftlist
                            val rightlist2 =  
                                 map (fn (y,h2) => (y,h2,1)) rightlist
                            fun cmporder ((x1,_,_),(y1,_,_)) =
                                if(x1 = y1)
                                then EQUAL
                                else if(x1 > y1)
                                     then GREATER
                                     else LESS
                            val merged = merge
                                     cmporder
                                     leftlist2
                                     rightlist2
                            fun leftcmp ((x1,x2,x3),(y1,y2,y3)) = 
                                      case (y3) of
                                         1 => (x1,x2,x3)
                                       | _  => (y1,y2,y3)
                            fun rightcmp ((x1,x2,x3),(y1,y2,y3)) =   
                                   case (y3) of
                                      0 => (x1,x2,x3)
                                    | _  =>(y1,y2,y3)
                            val leftcmpsq =
                                     scani leftcmp (0,0,0)(merged)
                                           
                            val rightcmpsq =
                                     scani (rightcmp) (0,0,1)(merged)          
                            fun comping i =
                                let
                                val (point,height,wh)  = nth merged i
                                val (point2,height2,wh2) = 
                                            if(wh = 0)
                                            then nth rightcmpsq i
                                            else nth leftcmpsq i
                                in
                                if(height > height2 
                                   orelse i = 0 
                                   orelse i = length(merged) - 1
                                   orelse 
                                 (point,height,wh) = (point2,height2,wh2) ) 
                                then (point,height)
                                else let
                                     val (point3,height3,wh3) = 
                                                     if(wh = 0)
                                                     then nth leftcmpsq (i-1)
                                                     else nth  rightcmpsq (i-1)
                                     in 
                                     if((height2 <  height3))
                                     then (point,height2)
                                     else (point,~1)
                                     end
                                end   
                            in
                            filter (fn (_,height) => height >= 0)
                                    (tabulate comping (length(merged)))
                            end
      in
      computeSkyline buildings                            
      end
end
