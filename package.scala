package object ConjuntosF {
 /*
 Funcion caracteristica
 */
 type Conj = Int => Boolean

 /*
  Funcion que verifica si un numero pertenece o no a un conjunto
  */
 def pertenece(elem: Int, s: Conj): Boolean = s(elem)

 /**
  * FUNCIONES BÁSCIAS SOBRE CONJUNTOS
  */

 /*
  * Devuelve el conjunto con un solo elemento
  */
 def conjuntoUnitario(elem: Int): Conj = {
  (x: Int) => x == elem
 }

 /*
  * Devuelve la union de dos conjuntos
  * el conjunto de todos los elemntos que estan en ´s´o en ´t´
  */
 def union(s: Conj, t: Conj): Conj = {
  (x: Int) => s(x) || t(x)
 }

 /*
  * Devuelve la interseccion de dos conjuntos,
  * el conjunto de todos los elementos que estan en ´s´y en ´t´
  */
 def interseccion(s: Conj, t: Conj): Conj = {
  (x: Int) => s(x) && t(x)
 }

 /*
  * Devuelve la diferencia de dos conjuntos,
  * el conjunto de todos los elementos de ´s´ que no están en ´t´
  */
 def dif(s: Conj, t: Conj): Conj = {
  (x: Int) => s(x) && !t(x)
 }

 /*
  * Devuelve el subconjunto de elementos de ´s´ para los cuales ´p´ se cumple
  */
 def filtrar(s: Conj, p: Int => Boolean): Conj = {
  interseccion(s, p)
 }

 /**
  * CONSULTAS Y TRANSFORMACIONES DE CONJUNTOS
  */
 /*
 Los limites para ´para_todo´ y ´existe´ son +/- 1000
  */
 val limite = 1000

 /*
 Calcula si todos los elementos de ´s´ que estan en los limites satisfacen ´p´
  */
 def paratodo(s: Conj, p: Int => Boolean): Boolean = {
  def paratodoF(elem: Int): Boolean = {
   if (s(elem) && !p(elem)) false
   else if (elem > limite || elem < -limite) true
   else paratodoF(elem - 1)
  }

  paratodoF(limite)
 }

 /*
 Calcula si existe algun elemento de ´s´dentro de los limites que satisfaga ´p´
  */
 def existe(s: Conj, p: Int => Boolean): Boolean = {
  !paratodo(s, (elem: Int) => !pertenece(elem, p))
 }

 /*
 Devuelve el conjunto transformado aplicando ´f´ a cada elemento de ´s´
  */
 def map(s: Conj, f: Int => Int): Conj = {
  (x => existe(s, (y: Int) => f(y) == x))
 }
}
