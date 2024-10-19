import com.sun.source.tree.ImportTree


trait  ArbolHuffman {
  case class HojaHuff(caracter: Char, peso: Int) extends ArbolHuffman

  case class RamaHuff(nodoIzq: ArbolHuff, nodoDer: ArbolHuff) extends ArbolHuff


  trait CodigoHuff {

    //Devuelve el peso del arbolHuff
    def peso(arbol: ArbolHuff): Int = arbol match
      case HojaHuff(caracter, pesoH) => pesoH
      case RamaHuff(nodoIzq: ArbolHuff, nodoDer: ArbolHuff) => peso(nodoIzq) + peso(nodoDer)
        throw new IllegalArgumentException(s"Tipo no esperado: ${arbol.getClass}")

    def caracteres(arbol: ArbolHuff): List[Char] =
      def AuxCaracter(arbol: ArbolHuff, lista: List[Char]): List[Char] = arbol match
        case HojaHuff(caracter, pesoH) => caracter :: lista
      //case RamaHuff(nodoIzq: ArbolHuff, nodoDer: ArbolHuff) => lista:::cadena

      AuxCaracter(arbol, List())


  }

  object suma extends CodigoHuff

  val arbol: ArbolHuff = suma.RamaHuff(suma.HojaHuff('a', 1), suma.HojaHuff('b', 2))
  suma.peso(arbol)
}



