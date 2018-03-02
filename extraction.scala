/////// Importation des library ////////////:
import scala.collection.mutable.ArrayBuffer
import java.io._
import scala.io._

//////// definition methode //////////////////
class Meth {
  def printArray[K](array:Array[K]) = array.mkString("(" , ", " , ")")
}

////////// récupération du jeu de données sur la page wikipedia
object datahtml extends App {
  val html = scala.io.Source.fromURL("https://fr.wikipedia.org/wiki/Liste_des_%C3%89tats_du_monde_par_continent")
  val wiki = html.mkString
  val writer = new PrintWriter(new File("datawiki.txt"))//// enregistrement de la page html en txt, ainsi l'objet suivant etats mondiaux tourne plus vite
  writer.write(wiki)
  writer.close()
  //println(wiki)
}

object Etatsmondiaux extends App
{
  val wiki = Source.fromFile("datawiki.txt", "UTF-8").mkString
  val wiki2 =wiki.split("ul>") /// séparer le document txt en plusieurs éléements et coupe devant les tableaux qui nous intéressent
  val lw2 = wiki2.length
  //println(wiki2(4)) : point de verification de découpage des données
  //println(wiki2.mkString("<", ",", ">"))
  //println(lw2) // nombre d'éléments dans mon array wiki2

  /// je ne garde que les tableaux dont la structure est semblable :
  var wiki3 = Array(wiki2(5),wiki2(7),wiki2(9),wiki2(11),wiki2(39),wiki2(61),wiki2(63),wiki2(65),wiki2(67),wiki2(89),wiki2(91))
  // println(wiki3.mkString("<", ",", ">")) point de vérification que seuls les tableaux voulus ont été gardés
  //println(wiki3(0))// : vérification d'un élément de wiki3
  val lw3 = wiki3.length
  // println(lw3)
  val lw3bis= lw3-1
  //println(lw3bis)

  var Pays = new ArrayBuffer[String]() // création d'une array pour récupérer les noms des pays
  var Lien = new ArrayBuffer [String]() // création d'une array pour récupérer les liens html des pays

  // le but est de construire une matrice du type ((nom pays,lien html), (nom pays,lien html), ...)
  // avec la fonction zip je peux associée la liste de nom avec la liste de lien

  for (i <- 0 to lw3bis) {
    ///////// D'abord récupérer le nom du pays donc découper les lignes de chaque tableau pour isoler nom du pays////////
    val ipays = wiki3(i).mkString // lors de l'activation de la boucle remplacer 0 par i
    // println(ipays)
    val IDpays = ipays.split("<li><span class=\"datasortkey\" data-sort-value=\"")
    //println(IDpays.mkString("<", ",", ">"))
    val ipays2 = IDpays.mkString
    val IDpays2 = ipays2.split("</a></span></li>")
    //println(IDpays2.mkString("<", ",", ">"))
    // maintenant le début de chaque ligne de tableau commence par le nom du pays
    //println(IDpays2(2))
    // donc maintenant besoin de split derrière le nom du pays et de sauvegarder le nom du pays
    val lipd = IDpays2.length - 1
    //println(lipd)

    for (i <- 0 until lipd) {

      val idp3 = IDpays2(i).mkString
      // println(idp3)
      // extraction du nom
      val ids = idp3.indexOfSlice("\"><span class=\"")
      // lors de l'activation de la boucle remplacer 0 par i
      //println (ids)
      val np = idp3.slice(0, ids).replace("\n", "")
      // println(np)
      Pays += np

      // extraction du lien
      val idl = idp3.indexOfSlice("Fichier:Flag")
      val lp = idp3.drop(idl)
      //println(lp)
      val idl2 = lp.indexOfSlice(" href=\"")
      val lp2 = lp.drop(idl2)
      val idl3 = lp2.indexOfSlice("wiki")
      val lp3 = lp2.drop(idl3)
      //println(lp3)
      val idl4 = lp3.indexOfSlice("\"")
      val lp4 = lp3.slice(0, idl4)
      // println(lp4)
      Lien += lp4

    }
  }
//println(Pays)

//println(Lien)

  val jdd = Pays.zip(Lien) /// lie les 2 listes
  // println(jdd)
  val jddmap= jdd.toMap /// transforme le tuple en map
  // println(jddmap)
  // println(jddmap.size)
  //  val mapjdd = Map(Pays -> Lien)
  //  println(mapjdd)


  for ((k,v) <- jddmap) printf("%s\n", k) /// imprimer lle champ clé de la map

  println("choisissez un pays de la liste")

  var inputpays = StdIn.readLine()
  val url= jddmap(inputpays)
  //  println(url)
  val pagewikihtml=Source.fromURL("https://fr.wikipedia.org/"++url,"UTF-8")
  val pagewikipays = pagewikihtml.mkString
  //println(pagewikipays)

  // maintenant chercher capital et population dans page wiki pays

  // ça ne fonctionne pas pour tous les pays à cause du manque d'homogénéité sur les pages wiki des pays

  // le problème apparaît quand il y a plusieurs capitale (administrative, legislative et judiciaire)

  /// !!!!!!  fonctionne avec Algerie France ...  !!!!!!

  val indicedevantcapitale = pagewikipays.indexOfSlice("Capitale</a></th>\n<td><a href=\"/wiki/")
  //println(indicedevantcapitale)
  val pagewikipaysreduite = pagewikipays.drop(indicedevantcapitale)
  val indicederrierecapitale =  pagewikipaysreduite.indexOfSlice("</a>\n<p")
  val capitaleisolement = pagewikipaysreduite.slice(0,indicederrierecapitale)
  //  println(capitaleisolement)

  val indicenomcapitale = capitaleisolement.indexOfSlice("title=\"")
  val nomcapitale = capitaleisolement.drop(indicenomcapitale)
  //  println(nomcapitale)
  val nomcapitale2 = nomcapitale.split("\"")
  println (s"The capital of $inputpays is :" )
  println(nomcapitale2(1))

  val indicedevantpopulation = pagewikipays.indexOfSlice(">Population totale</a> <span style=\"font-weight:normal;\">")
  //println(indicedevantpopulation)

  //("<sup id")
  val psupreduite = pagewikipays.slice(indicedevantpopulation,indicedevantpopulation+1000)
  val indicenowrap = psupreduite.indexOf("nowrap\">")
  val indicesup= psupreduite.indexOfSlice("<sup id=")
  val psupreduite2 = psupreduite.slice(indicenowrap+8,indicesup)
  val poptotale = psupreduite2.replaceAll("&#160;"," ")
  println (s"La population totale est de $poptotale habitants.")

}





