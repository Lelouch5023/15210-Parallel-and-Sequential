functor MkDivideAndConquerPD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open Primitives
  open P
  open Seq

  (* Remove this line when you're done. *)


  exception NotYetImplemented

  fun parenMatchPar s = 
      let
      fun pm s = 
          case showt(s) of
            EMPTY => (0,0,0,0,0,0)
          | ELT OPAREN => (0,0,0,1,0,1)
          | ELT CPAREN => (1,0,0,0,0,1)
          | NODE(l,r) =>
            let
            val ((freeClL,cldistL,maxdistL,freeOpL,opdistL,lengthL)
                ,(freeClR,cldistR,maxdistR,freeOpR,opdistR,lengthR))
              = par ((fn () => pm l),(fn() => pm r))
            (* close parentheiss )*()*(\*    *)
            val maxdist = Int.max(maxdistL,maxdistR)
            val length = lengthL + lengthR
            in
            if(freeOpL = freeClR)(* this creates a matched string*)
            then
                 (freeClL,cldistL
                 ,Int.max(maxdist,2*freeClR + opdistL + cldistR)
                 ,freeOpR,opdistR,length)
            else if(freeOpL > freeClR)(* the left side takes all of right *)
                 then if(freeOpR = 0)
                      then (freeClL,cldistL
                           ,Int.max(maxdist,2*freeClR + opdistL + 
                                            cldistR + opdistR)
                           ,freeOpL - freeClR,lengthR+opdistL + freeClR
                           ,length)
                       else
                       (freeClL,cldistL
                       ,Int.max(maxdist,2*freeClR + opdistL + 
                                        cldistR + opdistR)
                       ,freeOpR + (freeOpL - freeClR)
                       ,2*freeClR + opdistL + cldistR + opdistR
                       ,length)
                 else 
                      if(freeClL = 0)
                      then (freeClR-freeOpL,lengthL + cldistR + freeOpL
                           ,Int.max(maxdist,2*freeOpL + cldistL 
                                            + cldistR + opdistL)
                           ,freeOpR,opdistR
                           ,length)
                      else
                      (freeClL + (freeClR - freeOpL),
                             2*freeOpL + opdistL + cldistL + cldistR
                      ,Int.max(maxdist,2*freeOpL + cldistL 
                                       + cldistR + opdistL)
                      ,freeOpR,opdistR
                      ,length)
            end
                          
      in
      pm s
      end

  fun parenDist (parens : paren seq) : int option =
      let
      val (_,_,max,_,_,_) = parenMatchPar(parens)
      in
      if(max = 0)
      then NONE
      else SOME(max)
      end

(*    parenDist just implements the function parenMatchPar with takes 
      parens and divides up the sequence and pass up information 
      on the sequence that tell us about the sequence. What parenMatchPar
      does it gives the (CloseParen,CloseDist,MaxDist,OpenParen,OpenDist,
      length). CloseParen represents how many close parenthesis there
      are that are not matched, CloseDist represents the distance of 
      close parenthesis. the Maxdist give the current max dist of matched 
      parenthesis. 
      Likewise OpenParen gives how many open parenthesis that are not
      matched with a closed parenthesis there are and OpenDist is the dist 
      of OpenParen parenthesis distance. Essentially this treats every 
      substring as a concatenation of two or more strings.
     
      string seen as "CPAREN*,MATCHEDSUBSTRINGS8,OPAREN*" where * representes
      the star operator in regular expressions.
      CloseParen
      represents  the first substring and OpenParen represents the second 
      substring that concatenates the first substring. The distance aspect 
      keeps  track of the distance of the two substrings. MaxDist is used
      to keep track of the largest matched string. Note that this could
      be a substring that is not counted as part of the OpenParen or 
      CloseParen. The length is there to have a way keep track of the
      length of the string in case that the string happens to be nothing
      but concatenated strings with no unmatched open and close parens. 
      
      So it takes OpensParens and CloseParens from both the left and right
      side whichever side makes sense sees the open and close parenthesis
      creates a matched string. It then checks to see if the created matched
      string is larger than the current max and changes the max if appropriate.
 *)



end
