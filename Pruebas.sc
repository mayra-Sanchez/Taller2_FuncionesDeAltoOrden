import ConjuntosF.{Conj, conjuntoUnitario, dif, existe, filtrar, interseccion, map, paratodo, pertenece, union}

/**
 * Despliega el contenido de un Conj
 */
def conjComoCadena( s : Conj): String = {
  val xs = for (i <- 0 to 10 if pertenece(i,s)) yield i
  xs.mkString("{",",","}")
}

val s1 = conjuntoUnitario(1)
val s2 = conjuntoUnitario(2)
val s3 = conjuntoUnitario(3)
val s4 = conjuntoUnitario(4)
val s5 = conjuntoUnitario(5)
val s6 = union (s1 , s3)
val s7 = union (s2 , s3)
val s8 = union (s3 , s4)
val s9 = union (s4 , s5)
val s10 = union (s3, s5)
val s11 = interseccion(s6,s7)
val s12 = interseccion(s7,s8)
val s13 = interseccion(s8,s9)
val s14 = interseccion(s9,s10)
val s15 = interseccion(s10,s6)
val s16 = dif(s6,s7)
val s17 = dif(s7,s8)
val s18 = dif(s8,s9)
val s19 = dif(s9,s10)
val s20 = dif(s10,s6)

conjComoCadena(s1)
conjComoCadena(s2)
conjComoCadena(s3)
conjComoCadena(s4)
conjComoCadena(s5)
conjComoCadena(s6)
conjComoCadena(s7)
conjComoCadena(s8)
conjComoCadena(s9)
conjComoCadena(s10)
conjComoCadena(s11)
conjComoCadena(s12)
conjComoCadena(s13)
conjComoCadena(s14)
conjComoCadena(s15)
conjComoCadena(s16)
conjComoCadena(s17)
conjComoCadena(s18)
conjComoCadena(s19)
conjComoCadena(s20)

pertenece (1, s1)
assert(pertenece (1, s1), "Conjunto_Unitario")

// Prueba de uniones
pertenece( 1 , s6 )
pertenece( 2 , s6 )
!pertenece( 3 , s7 )
pertenece( 1 , s7 )
pertenece( 3 , s8 )
!pertenece( 2 , s8 )
pertenece( 2 , s9 )
pertenece( 3 , s9 )
!pertenece( 1 , s10 )

// Prueba de intersecciones
pertenece( 1 , s11 )
!pertenece( 2 , s11 )
!pertenece( 3 , s12 )
pertenece( 2 , s12 )
!pertenece( 1 , s13 )
pertenece( 3 , s13 )
!pertenece( 3 , s14 )
pertenece( 3 , s15 )
!pertenece( 1 , s15 )

// Prueba de diferenciacion
pertenece( 1 , s16 )
!pertenece( 2 , s17 )
!pertenece( 3 , s18 )
pertenece( 2 , s19 )
!pertenece( 1 , s20 )
pertenece( 3 , s16 )
!pertenece( 3 , s17 )
pertenece( 3 , s18 )
!pertenece( 1 , s20 )

/**
 * Prueba de filtros
 */
val s21 = filtrar ( union( s6 , s7 ) , ( x : Int ) => ( x % 2) == 0)
conjComoCadena ( s21 )
!pertenece( 1 , s21 )
!pertenece( 3 , s21 )
pertenece( 2 , s21 )

/**
 * Prueba de para_todo y existe
 */
!paratodo( union( s6 , s7 ) , ( x : Int ) => ( x % 2) == 0 )
existe( union( s6 , s7 ) , ( x : Int ) => ( x % 2) == 0 )
paratodo( s7 , ( x : Int ) => ( x % 2) == 1 )

/**
 * Prueba de map
 */
val s22 = map( union ( s4 , s5 ) , ( x : Int ) => x * x )
conjComoCadena ( s22 )
!pertenece( 2 , s22 )
!pertenece( 3 , s22 )
!pertenece( 5 , s22 )
!pertenece( 6 , s22 )
!pertenece( 7 , s22 )
!pertenece( 8 , s22 )
pertenece( 1 , s22 )
pertenece( 4 , s22 )
pertenece( 9 , s22 )