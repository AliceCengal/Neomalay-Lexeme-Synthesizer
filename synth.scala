trait Phoneme {}

object B extends Phoneme {}; object P extends Phoneme {}
object T extends Phoneme {}; object D extends Phoneme {}

object C extends Phoneme {}; object J extends Phoneme {}
object K extends Phoneme {}; object G extends Phoneme {}

object M extends Phoneme {}; object N extends Phoneme {}
object NY extends Phoneme {}; object NG extends Phoneme {}

object S extends Phoneme {}; object H extends Phoneme {}
object L extends Phoneme {}; object R extends Phoneme {}

object W extends Phoneme {}; object Y extends Phoneme {}
object NULLF extends Phoneme {}

object A extends Phoneme {}; object E extends Phoneme {}
object I extends Phoneme {}; object U extends Phoneme {}
object O extends Phoneme

object Register {
  val vowels = Seq(A, E, I, U)
  val semivowels = Seq(W, Y)
  val voiced = Seq(B, D, J, G)
  val voiceless = Seq(P, T, C, K)
  val plosives = voiced ++ voiceless
  val nasals = Seq(M, N, NY, NG)
  val fricatives = Seq(S, H, L, R)
  val consonants = plosives ++ fricatives ++ nasals ++ semivowels
  val consonantsWithNull = consonants :+ NULLF
  
  val laminals = Seq(C, J, NY)
}

object Phonotactics {
  import Register._
  
  def OnsetHomorganicity(
    c1: Phoneme, v1: Phoneme, c2: Phoneme, 
    c3: Phoneme, v2: Phoneme, c4: Phoneme): Boolean =
    (c1,c3) != (B,P) && (c1,c3) != (B,P) &&
    (c1,c3) != (C,J) && (c1,c3) != (J,C) &&
    (c1,c3) != (K,G) && (c1,c3) != (G,K)
  
  def CodaConsonants(
    c1: Phoneme, v1: Phoneme, c2: Phoneme, 
    c3: Phoneme, v2: Phoneme, c4: Phoneme): Boolean = 
    !voiced.contains(c2) && !voiced.contains(c4) &&
    !laminals.contains(c2) && !laminals.contains(c4)
  
  def FirstCoda(
    c1: Phoneme, v1: Phoneme, c2: Phoneme, 
    c3: Phoneme, v2: Phoneme, c4: Phoneme): Boolean =
    c2 != H && !voiceless.contains(c2) && 
    (c2 != S || voiceless.contains(c3))
  
  def NasalCoda(
    c1: Phoneme, v1: Phoneme, c2: Phoneme, 
    c3: Phoneme, v2: Phoneme, c4: Phoneme): Boolean =
    !nasals.contains(c2) || 
    ( plosives.contains(c3) || c3 == S ) &&
    ( (c2,c3) == (M,B) || (c2,c3) == (M,P) ||
    (c2,c3) == (N,T) || (c2,c3) == (N,D) ||
    (c2,c3) == (N,C) || (c2,c3) == (N,J) || (c2,c3) == (NG,S) 
    || (c2,c3) == (NG,K) || (c2,c3) == (NG,G))
  
  def RCoda(
    c1: Phoneme, v1: Phoneme, c2: Phoneme, 
    c3: Phoneme, v2: Phoneme, c4: Phoneme): Boolean =
    c2 != R  && c2 != L //|| ( c3 != H && c3 != W && c3 != J )
  
  def NoGemination(
    c1: Phoneme, v1: Phoneme, c2: Phoneme, 
    c3: Phoneme, v2: Phoneme, c4: Phoneme): Boolean =
    c2 != c3
  
  def NoEinSecondSyll(
    c1: Phoneme, v1: Phoneme, c2: Phoneme, 
    c3: Phoneme, v2: Phoneme, c4: Phoneme): Boolean = 
    { v2 != E }
  
  def NoSemivowelInFirstSyll(
    c1: Phoneme, v1: Phoneme, c2: Phoneme, 
    c3: Phoneme, v2: Phoneme, c4: Phoneme): Boolean = 
    { c2 != W && c2 != Y }
  
  def VowelSemivowelHarmony(
    c1: Phoneme, v1: Phoneme, c2: Phoneme, 
    c3: Phoneme, v2: Phoneme, c4: Phoneme): Boolean =
    (v2,c4) != (I,Y) && (v2,c4) != (U,W)
  
  def EHarmony(
    c1: Phoneme, v1: Phoneme, c2: Phoneme, 
    c3: Phoneme, v2: Phoneme, c4: Phoneme): Boolean =
    v1 != E || 
    (!semivowels.contains(c1) && !semivowels.contains(c2))
  
  def LekiuExclusivity(
    c1: Phoneme, v1: Phoneme, c2: Phoneme, 
    c3: Phoneme, v2: Phoneme, c4: Phoneme): Boolean =
    (v2,c4) != (I,W) || v1 == E
  
  /*
  def NoSemivowelAfterE(
    c1: Phoneme, v1: Phoneme, c2: Phoneme, 
    c3: Phoneme, v2: Phoneme, c4: Phoneme): Boolean =
  */
}

trait Glypher {
  def glyph(f: Phoneme): String
  def word(seq: Seq[Phoneme]): String =
    seq.map{ glyph(_) }
     .reduce{ _ + _ }
}

object StandardGlyph extends Glypher {
  def glyph(f: Phoneme) = f match {
    case B => "b";  case P => "p";  case T => "t";    case D => "d"
    case C => "c";  case J => "j";  case K => "k";    case G => "g"
    case M => "m";  case N => "n";  case NY => "ny";  case NG => "ng"
    case S => "s";  case H => "h";  case L => "l";    case R => "r"
    case W => "w";  case Y => "y";  case NULLF => ""
    case A => "a";  case E => "e";  case I => "i";    case U => "u"
    case O => "o";
  }
}

object NaturalGlyph extends StandardGlyph {
  import Register._
  
  val (C1, V1, C2, C3, V2, C4) = (0, 1, 2, 3, 4, 5)
  
  def transform(seq: Seq[Phoneme]): Seq[Phoneme] = {
    val word = seq.toArray
    val consonants = plosives ++ fricatives ++ nasals
  
    if ( word() )
  }
  
  def word(seq: Seq[Phoneme]): String = super.word(transform(seq))
}

object WordQuad {
  def apply(wordlist: Seq[String]): Seq[String] = {
    val padded = wordlist.map {_.padTo(10, " ").mkString}
    padded.grouped(8)
          .map(_.reduce(_ + _))
          .toSeq
  }
}

trait Generator extends Iterator[Seq[Phoneme]]

class ShuffleGen extends Generator {
  import Register._
  
  val rng = new util.Random
  
  def hasNext = true
  
  def next() = 
    Seq(
      sample(consonantsWithNull),
      sample(vowels),
      sample(consonantsWithNull),
      sample(consonants),
      sample(vowels),
      sample(consonantsWithNull))
  
  def sample(seq: Seq[Phoneme]) =
    seq(rng.nextInt(seq.size))
}

class LexemeSynth {
  import Register._
  import Phonotactics._
  
  def trigraphtest(): Unit = {
    val trigraphs = for {
      v1 <- vowels
      c <- consonants
      v2 <- vowels
      if (v1 != E && v2 != E)
    } yield { Seq(v1, c, v2) }
    
    val words = trigraphs.map{ StandardGlyph.word(_) }
    WordQuad(words).foreach{println(_)}
  }
  
  def simpleSynth = {
    for {
      c1 <- consonantsWithNull.iterator
      v1 <- vowels.iterator
      c2 <- consonantsWithNull.iterator
      c3 <- consonants.iterator
      v2 <- vowels.iterator
      c4 <- consonantsWithNull.iterator
      if NoEinSecondSyll(c1, v1, c2, c3, v2, c4)
      if NoSemivowelInFirstSyll(c1, v1, c2, c3, v2, c4)
      if CodaConsonants(c1, v1, c2, c3, v2, c4)
      if FirstCoda(c1, v1, c2, c3, v2, c4)
      if NasalCoda(c1, v1, c2, c3, v2, c4)
      if VowelSemivowelHarmony(c1, v1, c2, c3, v2, c4)
      if NoGemination(c1, v1, c2, c3, v2, c4)
      if RCoda(c1, v1, c2, c3, v2, c4)
      if EHarmony(c1, v1, c2, c3, v2, c4)
      if LekiuExclusivity(c1, v1, c2, c3, v2, c4)
      if OnsetHomorganicity(c1, v1, c2, c3, v2, c4)
    } yield Seq(c1, v1, c2, c3, v2, c4)
  }
  
  def genSynth = {
    for {
      Seq(c1, v1, c2, c3, v2, c4) <- new ShuffleGen
      if NoEinSecondSyll(c1, v1, c2, c3, v2, c4)
      if NoSemivowelInFirstSyll(c1, v1, c2, c3, v2, c4)
      if CodaConsonants(c1, v1, c2, c3, v2, c4)
      if FirstCoda(c1, v1, c2, c3, v2, c4)
      if NasalCoda(c1, v1, c2, c3, v2, c4)
      if VowelSemivowelHarmony(c1, v1, c2, c3, v2, c4)
      if NoGemination(c1, v1, c2, c3, v2, c4)
      if RCoda(c1, v1, c2, c3, v2, c4)
      if EHarmony(c1, v1, c2, c3, v2, c4)
      if LekiuExclusivity(c1, v1, c2, c3, v2, c4)
      if OnsetHomorganicity(c1, v1, c2, c3, v2, c4)
    } yield Seq(c1, v1, c2, c3, v2, c4)
  }
  
  def synth = genSynth
  
  def countLexemes(g: Iterator[_]): Int =
    g.foldLeft(0) { (n,_) => n+1 }
}

println("Synth")

val s = new LexemeSynth
//s.trigraphtest()
val words = s.synth.take(400).toSeq.map{StandardGlyph.word(_)}
WordQuad(words).foreach{println(_)}
//print("Total lexemes generated: "); println(s.countLexemes(s.simpleSynth))
println("Done")
