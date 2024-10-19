trait CodigoHuff {


  //Devuelve el peso del arbolHuff
  def peso(arbol:ArbolHuff):Int = arbol  match
      case HojaHuff(caracter, pesoH)=> pesoH
      case RamaHuff(nodoIzq:ArbolHuff,nodoDer:ArbolHuff,pesoR:Int,cadena) => peso(nodoIzq) +peso(nodoDer)
        throw new IllegalArgumentException(s"Tipo no esperado: ${arbol.getClass}")
  def caracteres(arbol: ArbolHuff): List[Char] =
    def AuxCaracter(arbol:ArbolHuff, lista:List[Char]):List[Char] = arbol match
      case HojaHuff(caracter, pesoH) => caracter::lista
      case RamaHuff(nodoIzq: ArbolHuff, nodoDer: ArbolHuff, pesoR: Int, cadena) => lista:::cadena
    
    AuxCaracter(arbol, List())

  
}
