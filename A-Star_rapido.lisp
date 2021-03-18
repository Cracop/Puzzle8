; 2 | 8 | 3        Estado inicial
; 1 | 6 | 4        0 = Espacio Vacio
; 7 | 0 | 5
; La representación de lo de arriba
;El 0 al inicio es solo para que las posiciones empiecen desde el 1 q
;El número del final me dice la posición de la casilla vacía
; (0 2 8 3  1 6 4  7 0 5 posV)
; (0 1 2 3  4 5 6  7 8 9|7)
;Entonces, 
;mover a la izquierda sería restarle 1
;mover a la derecha es sumar 1
;mover arriba es restarle 3
;mover hacia abajo es sumarle 3
;Por esto a los nodos se le va agregar un valor 
;que indique la posición del vació, para facilitarnos la vida al momento de mover
;Como es A*, el valor de cada nodo es así
;;f(n)=g(n)+h(n)
(setq estadoFinal '(A 1 2 3 8 0 4 7 6 5 5))
;(setq estadoFinal '(A 1 2 3 4 5 6 7 8 0 9))

;(setq estadoPrueba '(A 1 3 4 8 6 2 7 0 5 8));en 5 nodos
;(setq estadoPrueba '(A 2 8 1 0 4 3 7 6 5 4));27 nodos
;(setq estadoPrueba '(A 2 8 1 4 6 3 0 7 5 7)); 87 nodos
;(setq estadoPrueba '(A 5 6 7 4 0 8 3 2 1 5)); nodos

;(setq estadoPrueba '(A 2 8 3 1 0 4 7 6 5 5)); 5 nodos
;(setq estadoPrueba '(A 2 8 3 1 6 4 7 0 5 8)) ;6 nodos
;(setq estadoPrueba '(A 8 3 5 2 0 6 7 1 4 5)) ; 348 nodos
;(setq estadoPrueba '(A 1 3 4 8 0 5 7 2 6 5)) ; 12 nodos
;(setq estadoPrueba '(A 1 3 4 8 6 2 0 7 5 7)) ; 6 nodos
;(setq estadoPrueba '(A 3 6 4 0 1 2 8 7 5 4)) ; 45 nodos
(setq estadoPrueba '(A 4 2 5 0 1 3 7 6 8 4)) ;2493 nodos
;(setq estadoPrueba (list 'A '0 '1 '3 '4 '2 '5 '7 '8 '6 '1)); Llegó a 75mil nodos y no
;(setq estadoPrueba '(A 2 1 3 8 0 4 6 7 5 5)); 2187 nodos
;(setq estadoPrueba '(A 5 6 7 4 0 8 3 2 1 5)); Casi 31mil nodos y no
;Nodo es (A p1 p2 p3 p4 p5 p6 p7 p8 p9 posV)


(defun funcionHamming (estadoActual)
	(setq costo 0) ;Creo la variable costo
    (dotimes (i 9) ;Voy a iterar sobre la lista completa
        (if (not (equal (nth i estadoActual) 0)) ;Si la posición actual no es el vacío
            (cond ((equal (nth i estadoActual) (nth i estadoFinal)) ;Comparo las posiciones
                    NIL) ;Si son iguales no hago nada
                  (T (incf costo)) ;Si son diferentes, aumento el costo
            ) ;Cierro el cond
        ) ;Cierro el if
    ) ;Ciero el dotimes
    (return-from funcionHamming costo) ;Regreso
) ;Acaba la función

;;Defino la estructura con la cual guardo los estados (Es como una clase)
(defstruct nodo
	estado ;Contiene la lista que representa a la matriz (como se ve el tablero)
	gValor ;Es donde guardo el resultado de g(n), nivel
	fValor ;Es donde guardo el resultado de f(n)
	padre ;El nodo padre
	operador

)

;(defun set_valorH(nodoActual) ;Funcion que obtiene el valor h para el nodo
;	(setf h (funcionHamming nodoActual))
;)

(defun initNodo(estadoTemp) ;Inicializo un nodo con sus valores
	(setq nuevoNodo (make-nodo :estado estadoTemp
		:gValor 0 ;Inicia con nivel 0
		:fValor 9999 ;Inicio como muy costoso, porque no sé que tanto valga
		:padre NIL ;Inicia huérfano
		:operador NIL))
	(return-from initNodo nuevoNodo)
)

(defun hacerCambio(estado nuevaPos viejaPos);Con esto puedo mover el 0

	(setf copia (copy-list estado)) ;Copio el nodo que pase
	(rotatef (nth nuevaPos copia) (nth viejaPos copia)) ;Intercambio la posición del 0
	(fill copia nuevaPos :start 10 :end 11) ;modifico en la copia la nueva posición del 0
	(return-from hacerCambio copia) ;Regreso la copia

)


(defun generar_sucesores2 (nodoActual)
	(setq estado (nodo-estado nodoActual))
	(setq operadorPadre (nodo-operador nodoActual))
	;Los nuevos indices
	;(setq posVacio (car (reverse estado)))
    (setq posVacio (car (last estado)))
	(setq arriba (- posVacio 3))
	(setq abajo (+ posVacio 3))
	(setf izquierda (- posVacio 1))
	(setf derecha (+ posVacio 1))
	(setq gValorPadre (nodo-gValor nodoActual))
	
	(if (and (> arriba 0) (not(eq operadorPadre 'abajo)));Si me puedo mover hacia arriba
		(progn 
            (setq hijoArriba (hacerCambio estado arriba posVacio))
            (if 
                (revisaPosibilididad hijoArriba (append abierto cerrado))
                (progn 
                    (setq nodoArriba (initNodo hijoArriba))
			        (setf (nodo-gValor nodoArriba) (+ 1 gValorPadre))
			        (setf (nodo-fValor nodoArriba) (+ (nodo-gValor nodoArriba) (funcionHamming hijoArriba)))
			        (setf (nodo-padre nodoArriba) nodoActual)
			        (setf (nodo-operador nodoArriba) 'arriba)
                    (insertarAbierto nodoArriba)
                    ;(setf abierto (push nodoArriba abierto))
                    
                    
                )
            )
		)
	)
    
	(if (and (< abajo 10) (not(eq operadorPadre 'arriba)));Si me puedo mover hacia abajo
		(progn 
            (setq hijoAbajo (hacerCambio estado abajo posVacio))
            (if 
                (revisaPosibilididad hijoAbajo (append abierto cerrado))
                (progn 
                    (setq nodoAbajo (initNodo hijoAbajo))
			        (setf (nodo-gValor nodoAbajo) (+ 1 gValorPadre))
			        (setf (nodo-fValor nodoAbajo) (+ (nodo-gValor nodoAbajo) (funcionHamming hijoAbajo)))
			        (setf (nodo-padre nodoAbajo) nodoActual)
			        (setf (nodo-operador nodoAbajo) 'abajo)
                    (insertarAbierto nodoAbajo)
                    ;(setf abierto (push nodoAbajo abierto))
                    
                )
            )
		)
	)
	(if (and (/= (mod izquierda 3) 0) (not(eq operadorPadre 'der)));Si me puedo mover hacia la izquierda
		(progn 
            (setq hijoIzq (hacerCambio estado izquierda posVacio))
            (if 
                (revisaPosibilididad hijoIzq (append abierto cerrado))
                (progn 
                    (setq nodoIzq (initNodo hijoIzq))
			        (setf (nodo-gValor nodoIzq) (+ 1 gValorPadre))
                    (setf (nodo-fValor nodoIzq) (+ (nodo-gValor nodoIzq) (funcionHamming hijoIzq)))
                    (setf (nodo-padre nodoIzq) nodoActual)
			        (setf (nodo-operador nodoIzq) 'Izq)
                    (insertarAbierto nodoIzq)
                    ;(setf abierto (push nodoIzq abierto))
                    
                )
            )
		)
	)
	(if (and (/= (mod posVacio 3) 0) (not(eq operadorPadre 'izq))) ;Si me puedo mover hacia la derecha
		(progn 
            (setq hijoDer (hacerCambio estado derecha posVacio))

            (if 
                (revisaPosibilididad hijoDer (append abierto cerrado))
                (progn 
                    (setq nodoDer (initNodo hijoDer))
			        (setf (nodo-gValor nodoDer) (+ 1 gValorPadre))
			        (setf (nodo-fValor nodoDer) (+ (nodo-gValor nodoDer) (funcionHamming hijoDer)))
			        (setf (nodo-padre nodoDer) nodoActual)
			        (setf (nodo-operador nodoDer) 'Der)
                    (insertarAbierto nodoDer)
                    ;(setf abierto (push nodoDer abierto))
                    
                )
            )
		)
	)
)



(defun generar_sucesores (nodoActual)
	(setq estado (nodo-estado nodoActual))
	(setq operadorPadre (nodo-operador nodoActual))
	;Los nuevos indices
	;(setq posVacio (car (reverse estado)))
    (setq posVacio (car (last estado)))
	(setq arriba (- posVacio 3))
	(setq abajo (+ posVacio 3))
	(setf izquierda (- posVacio 1))
	(setf derecha (+ posVacio 1))
	(setq gValorPadre (nodo-gValor nodoActual))
	
	(if (and (> arriba 0) (not(eq operadorPadre 'abajo)));Si me puedo mover hacia arriba
		(progn 
            (setq hijoArriba (hacerCambio estado arriba posVacio))
            (if 
                (revisaPosibilididad hijoArriba cerrado)
                (progn 
                    (setq nodoArriba (initNodo hijoArriba))
			        (setf (nodo-gValor nodoArriba) (+ 1 gValorPadre))
			        (setf (nodo-fValor nodoArriba) (+ (nodo-gValor nodoArriba) (funcionHamming hijoArriba)))
			        (setf (nodo-padre nodoArriba) nodoActual)
			        (setf (nodo-operador nodoArriba) 'arriba)
                    ;(insertarAbierto nodoArriba)
                    (enqueue abierto nodoArriba (nodo-fValor nodoArriba)) 
                    ;(setf abierto (push nodoArriba abierto))
                    
                    
                )
            )
		)
	)
    
	(if (and (< abajo 10) (not(eq operadorPadre 'arriba)));Si me puedo mover hacia abajo
		(progn 
            (setq hijoAbajo (hacerCambio estado abajo posVacio))
            (if 
                (revisaPosibilididad hijoAbajo cerrado)
                (progn 
                    (setq nodoAbajo (initNodo hijoAbajo))
			        (setf (nodo-gValor nodoAbajo) (+ 1 gValorPadre))
			        (setf (nodo-fValor nodoAbajo) (+ (nodo-gValor nodoAbajo) (funcionHamming hijoAbajo)))
			        (setf (nodo-padre nodoAbajo) nodoActual)
			        (setf (nodo-operador nodoAbajo) 'abajo)
                    ;(insertarAbierto nodoAbajo)
                    (enqueue abierto nodoAbajo (nodo-fValor nodoAbajo)) 
                    ;(setf abierto (push nodoAbajo abierto))
                    
                )
            )
		)
	)
	(if (and (/= (mod izquierda 3) 0) (not(eq operadorPadre 'der)));Si me puedo mover hacia la izquierda
		(progn 
            (setq hijoIzq (hacerCambio estado izquierda posVacio))
            (if 
                (revisaPosibilididad hijoIzq cerrado)
                (progn 
                    (setq nodoIzq (initNodo hijoIzq))
			        (setf (nodo-gValor nodoIzq) (+ 1 gValorPadre))
                    (setf (nodo-fValor nodoIzq) (+ (nodo-gValor nodoIzq) (funcionHamming hijoIzq)))
                    (setf (nodo-padre nodoIzq) nodoActual)
			        (setf (nodo-operador nodoIzq) 'Izq)
                    ;(insertarAbierto nodoIzq)
                    (enqueue abierto nodoIzq (nodo-fValor nodoIzq)) 
                    ;(setf abierto (push nodoIzq abierto))
                    
                )
            )
		)
	)
	(if (and (/= (mod posVacio 3) 0) (not(eq operadorPadre 'izq))) ;Si me puedo mover hacia la derecha
		(progn 
            (setq hijoDer (hacerCambio estado derecha posVacio))

            (if 
                (revisaPosibilididad hijoDer cerrado)
                (progn 
                    (setq nodoDer (initNodo hijoDer))
			        (setf (nodo-gValor nodoDer) (+ 1 gValorPadre))
			        (setf (nodo-fValor nodoDer) (+ (nodo-gValor nodoDer) (funcionHamming hijoDer)))
			        (setf (nodo-padre nodoDer) nodoActual)
			        (setf (nodo-operador nodoDer) 'Der)
                    ;(insertarAbierto nodoDer)
                    (enqueue abierto nodoDer (nodo-fValor nodoDer)) 
                    ;(setf abierto (push nodoDer abierto))
                    
                )
            )
		)
	)
)


(defun revisaPosibilididad2 (estado nodos)
    (setq cont 0)
    (if (null nodos) (return-from revisaPosibilididad t))

    (loop while (not (null (nth cont nodos))) do
        (if (equal estado (nodo-estado (nth cont nodos)))
            (return-from revisaPosibilididad NIL)
        )
        (incf cont)
    )
    (return-from revisaPosibilididad t)
)

(defun revisaPosibilididad (estado nodos)
    (if (gethash estado nodos)
    (return-from revisaPosibilididad nil)
    (return-from revisaPosibilididad t))
)

(defun mejorOpcion (abierta) ;Me va a regresar el mejor estado de entre los de abierto
	(setf min (initNodo (nodo-estado (nth 0 abierto)))) ;Obtengo el estado del nodo 
	(setf (nodo-fValor min) 9999) ;Pongo el minimo valor como algo muy alto, porque mejor es nada
	(block abierto_loop ;Un block evalua una serie de formas y regresa el resultado de la forma que de return 
		(loop for n in abierto do ;Itero sobre cada elemento en abierto, e es un estado
			(if(< (nodo-fValor n) (nodo-fValor min)) ;Voy comparando el valorF del estado, con el minimo que tengo
					(setf min (copy-nodo n)) () ) ;Si es menor, min ahora es e 
                    ;(if (prueba) (casoTrue) (casoFalse))
		)
		(return-from abierto_loop min) ;Regreso el min
	)
)

(defun insertarAbierto (nodo)
    (setf abierto (push nodo abierto))
)



(defun eliminarElemento(e nlista);Quito el elemento de la lista, le paso el nodo completo
	(setq m_list nil) ;Inicio una lista vacía
	(loop for nodo in nlista do ;Itero sobre cada nodo de nlista
		(progn ;Sirve pare ejecutar una serie de formas
			(if(not(equal (nodo-estado nodo) (nodo-estado e))) ;Si los estados no son lo  
				(
					push nodo m_list ;agrego el nodo a la lista
				)
			)
		)
		collect m_list ;voy guardando en m_list lo que voy agregando
	)
	(return-from eliminarElemento m_list) ;regreso la lista
)

(defun a_Estrella2 (nodoInicial)
	(loop while (and (not(null abierto)) (< nodosExpandidos 5000))do;Mientras abierto siga con algún elemento
		(progn 
			(setq actual (mejorOpcion abierto));Obtengo la mejor opción basada en f
            (setq actual (dequeue abierto) )
			(if (equal (nodo-estado actual) estadoMeta) ;Checo si es la meta
				(return-from a_Estrella (imprimeRuta actual nodosExpandidos)) ;Regreso la impresion
			)
			(setf hijos (generar_sucesores actual));Creo la lista de hijos
			(push actual cerrado)
            ;(print cerrado)
            ;Paso a cerrado el que acabo de expander
			(setf abierto (eliminarElemento actual abierto)) ;Lo elimino de abierto
            ;(print abierto)
			(incf nodosExpandidos)
			;(print nodosExpandidos) <=========
		)
	)
	(return-from a_Estrella 'fracaso) ;Esto es si nunca hubo resultado
)
(defun a_Estrella (nodoInicial)
	;(if (not(sePuede nodoInicial estadoMeta))
	;	(return-from a_Estrella 'No_se_puede_resolver)
	;)
	(if (not (es_resoluble nodoInicial estadoMeta))
		(progn
			(return-from a_Estrella "Irresoluble")
		)
		
	)
	(loop while (and (/= (QUEUE-SIZE abierto) 0) (< nodosExpandidos 475000))do;Mientras abierto siga con algún elemento
		(progn 
			;(setq actual (mejorOpcion abierto));Obtengo la mejor opción basada en f
            (setq actual (dequeue abierto) )
			(if (equal (nodo-estado actual) estadoMeta) ;Checo si es la meta
				(return-from a_Estrella (imprimeRuta actual nodosExpandidos)) ;Regreso la impresion
			)
			( if (< (nodo-gValor actual) 50)
            (progn 
            (setf hijos (generar_sucesores actual));Creo la lista de hijos
            (setf (gethash (nodo-estado actual) cerrado) actual)
            (incf nodosExpandidos)
            ;(print (nodo-gValor actual))
			(print nodosExpandidos)
            ))

		)
	)
	(return-from a_Estrella 'fracaso) ;Esto es si nunca hubo resultado
)

(defun imprimeRuta(actual nodosExpandidos)
	(setf ruta nil)
	(loop while(not(eq actual NIL)) do
		(progn
			(if (not (null (nodo-operador actual)))
                (push (nodo-operador actual) ruta)
            )
            
			;(push (nodo-estado actual) ruta)
			(setf actual (nodo-padre actual))
		)
	)
	(push nodosExpandidos ruta)
	;(reverse ruta)
	ruta
)

(defun solver (nodoInicial)
    (setq nodosExpandidos 0)
    (setq estadoMeta '(A 1 2 3 8 0 4 7 6 5 5))
	;(setq estadoMeta '(A 1 2 3 4 5 6 7 8 0 9))
    (setq ruta (a_Estrella nodoInicial))
    ruta
)

(defun es_resoluble (inicio meta)
	;;funcion que revisa el numero de inversiones en los estados inicial y final para ver si el puzzle es resoluble
	(setf lista_pares_inicial nil)
	(setf lista_pares_meta nil)
	(dotimes(cont 9);;inversiones del estado inicial
		(progn
			(if(not(eq (nth cont inicio) 0))
				(progn
					(setf cont1 (+ 1 cont))
					(loop while(<= cont1 8) do
						(progn
							(if(not(eq(nth cont1 inicio) 0))
								(progn
									(push (list (nth cont inicio) 
													(nth cont1 inicio)) 
														lista_pares_inicial)
								)
							)
							(setf cont1 (+ 1 cont1))
						)
					)

				)
			)
		)
	)
	(dotimes(cont 9);;inversiones del estado meta
		(progn
			(if(not(eq (nth cont meta) 0))
				(progn
					(setf cont1 (+ 1 cont))
					(loop while(<= cont1 8) do
						(progn
							(if(not(eq(nth cont1 meta) 0))
								(progn
									(push (list (nth cont meta) 
													(nth cont1 meta)) 
														lista_pares_meta)
								)
							)
							(setf cont1 (+ 1 cont1))
						)
					)
				)
			)
		)
	)
	(setf en_comun 0);;compara inversiones de los dos estados
	(loop for x in lista_pares_inicial do

		(loop for y in lista_pares_meta do
			(progn
				(if(and (eq (first x) (first y)) (eq (second x)(second y)) )
					(setf en_comun (+ 1 en_comun))
				)
			)
		)
	)
	(setf no_inversiones 0)	
	
	(setf no_inversiones (- (list-length lista_pares_inicial) en_comun))
	
	(if (evenp no_inversiones ) (return-from es_resoluble T);;si es par el num de inversiones, resuelve, si no no
						(return-from es_resoluble NIL))
)
;;=====================================================================
(load 'Heaps.lisp)
(defvar nodoInicial (initNodo estadoPrueba))
(defvar cerrado NIL)
;(setq table (make-hash-table :test 'equal))
(defparameter abierto (make-instance 'priority-queue))
(defparameter cerrado (make-hash-table :test 'equal))
(enqueue abierto nodoInicial 0) 
;(print (dequeue abierto)) ;=> 'test
(print (solver estadoPrueba))
;(print (es_resoluble estadoPrueba estadoFinal))


