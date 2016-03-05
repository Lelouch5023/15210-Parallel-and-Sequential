functor MkKGramData
  (structure Util : SEQ_UTIL
   structure Tok : TOKEN
   structure Table : TABLE where type Key.t = Tok.token Tok.Seq.seq
   sharing Table.Seq = Util.Seq
   sharing Table.Seq = Tok.Seq)
  : KGRAM_DATA =
struct
  structure Seq = Util.Seq
  structure Tok = Tok
  open Seq

  (* remove this line before submitting *)
  exception NotImplemented

  type 'a hist = ('a * int) seq
  type token = Tok.token
  type kgram = token seq

  (* redefine this type *)
  type kgramdata = (int seq) Table.table * token seq * int * (token seq) hist

  (* implement the following functions *)
  fun makeData (corpus : string) (k : int) =
      let
      val tokenized = Tok.tokenize corpus
      val contsubseq = tabulate (fn i => 
                             let  
                             val elem = subseq tokenized (i,k-1)
                             in
                             (elem,i)
                             end)
                            (length(tokenized)-(k-1)+1)
      val cmping = collate Tok.compare
      val collected = collect cmping contsubseq
      val histo = Util.histogram cmping (map (fn (x,i) => x) contsubseq)
      in
      (Table.fromSeq(collected),tokenized,k,histo)
      end

  fun lookupHist ((tab,corpus,k,_) : kgramdata) (gram : kgram) =
      case (Table.find tab gram) of
       (NONE) => NONE
      |(SOME(v)) => let
                    val tokenseq = tabulate
                                   (fn i =>
                                       if((nth v i)+(k-1) < length(corpus))
                                       then(SOME(nth corpus ((nth v i) + (k-1))))
                                            else NONE)
                                    (length(v))
                     val filtered = map(fn SOME(wat) => wat)
                                    (filter (fn NONE => false | SOME(x) => true)
                                           tokenseq)
                     in
                     case showl(filtered) of
                      NIL => NONE
                     | _ => SOME(Util.histogram Tok.compare filtered)
                     end
      

  fun getK ((_,_,k,_) : kgramdata) = k
    

  fun chooseStarter ((_,_,_,histo) : kgramdata) (r : real) =
      Util.choose histo r
end
