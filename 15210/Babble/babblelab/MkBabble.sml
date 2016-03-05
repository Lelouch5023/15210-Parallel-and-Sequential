functor MkBabble
  (structure Rand : RANDOM210
   structure Data : KGRAM_DATA
   structure Util : SEQ_UTIL
   sharing Data.Seq = Util.Seq
   sharing Data.Seq = Rand.Seq)
  : BABBLE =
struct
  structure Seq = Data.Seq
  structure Rand = Rand
  structure Data = Data
  open Seq

  (* remove this line before submitting *)
  exception NotImplemented

  type sentence = Data.token Seq.seq

  (* Implement the following functions *)
  fun makeSentence data len seed =
      let
      val (firstRand,nextRand) = Rand.randomReal seed NONE 
      val starter = Data.chooseStarter data firstRand
      val k = Data.getK data
      fun repeater seed' passed total = 
          case (Data.lookupHist data passed) of
            NONE => total
          | SOME(v) => let
                   val (rando,nextSeed) = Rand.randomReal seed' NONE
                   val nextTok = Util.choose v rando
                   val newTotal = append(total,singleton(nextTok))
                   val newPassed = drop(newTotal,(length(newTotal)-(k-1)))
                   in
                   if(length(newTotal) >= len)
                   then newTotal
                   else repeater nextSeed newPassed newTotal
                   end
       in
       repeater nextRand starter starter
       end

  fun makeParagraph data numSentences (low, high) seed =
      let
      val (randLengths,seed2) = 
                 Rand.randomIntSeq (seed) (SOME(low,high)) (numSentences)
      val (randInts,seed3) = 
                    (Rand.randomIntSeq (seed2) (NONE) (numSentences))
      val (randSeeds) = map (fn x => Rand.fromInt(x))
                            (randInts)
      val makeSent = tabulate 
                     (fn i => makeSentence (data) 
                                           (nth randLengths i)
                                           (nth randSeeds i))
                      (numSentences)
      in
      makeSent
      end
end
