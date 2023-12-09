(define (domain rutas-clase)
    (:requirements :typing :equality :negative-preconditions)
    (:types
        movil cargable sitio tren fabrica
    )

    (:predicates
        (en ?obj - movil ?loc - sitio) ; el objeto esta en la localizacion
        (conectada ?loc1 - sitio ?loc2 - sitio) ; loc1 y loc2 estan conectadas
        (cargado ?p - cargable ?obj - movil) ; p esta cargado en el movil obj
        (contiene ?p - cargable ?loc - sitio) ; p esta contenido en el sitio loc
        (procesado ?p - cargable) ; p ha sido procesado
        (eliminado ?p - cargable) ; p ha sido eliminado
        (puerto ?loc - sitio) ; loc es un sitio que corresponde con puerto
        (fabrica ?loc - sitio) ; loc es un sitio que corresponde con puerto
        (almacen ?loc - sitio) ; loc es un sitio que corresponde con puerto
        (espacio_tren ?obj - movil ?tn - tren) ; obj movil tiene la capacidad tn de espacio
        (contador_tren ?t1 - tren ?t2 - tren) ; el vagon t2 es el siguiente al vagon t1
        (espacio_max_tn ?obj - movil ?tn - tren) ; obj movil tiene una maxima capacidad tn de espacio
        (espacio_fabrica ?loc - sitio ?fn - fabrica) ; loc sitio tiene la capacidad fn de espacio
        (contador_fabrica ?f1 - fabrica ?f2 - fabrica) ; la fabrica f2 es la siguiente a la fabrica f1 
        (espacio_max_fn ?loc - sitio ?fn - fabrica) ; loc sitio tiene una maxima capacidad fn de espacio
    )

    (:action mover
        :parameters ( ?obj - movil ?loc1 - sitio ?loc2 - sitio)
        :precondition (
            and
            (en ?obj ?loc1)
            (conectada ?loc1 ?loc2)
        )
        :effect (and
            (en ?obj ?loc2)
            (not (en ?obj ?loc1))
        )
    )

    (:action cargar
        :parameters ( ?obj - movil ?p - cargable ?loc - sitio ?t1 - tren ?t2 - tren)
        :precondition (and
            (en ?obj ?loc)
            (puerto ?loc)
            (contiene ?p ?loc)
            (not(procesado ?p))
            (espacio_tren ?obj ?t1)
            (contador_tren ?t1 ?t2)
            (not(espacio_max_tn ?obj ?t1))
        )
        :effect (and
            (cargado ?p ?obj)
            (not (contiene ?p ?loc))
            (espacio_tren ?obj ?t2)
            (not(espacio_tren ?obj ?t1))
        )
    )

    (:action procesar
        :parameters ( ?obj - movil ?p - cargable ?loc - sitio ?t1 - tren ?t2 - tren ?f1 - fabrica ?f2 - fabrica)
        :precondition (and
            (en ?obj ?loc)
            (fabrica ?loc)
            (contiene ?p ?loc)
            (not(eliminado ?p))
            (espacio_tren ?obj ?t1)
            (contador_tren ?t1 ?t2)
            (espacio_fabrica ?loc ?f2)
            (contador_fabrica ?f1 ?f2)
        )
        :effect (and
            (cargado ?p ?obj)
            (not (contiene ?p ?loc))
            (espacio_tren ?obj ?t2)
            (not(espacio_tren ?obj ?t1))
            (espacio_fabrica ?loc ?f1)
            (not (espacio_fabrica ?loc ?f2))
        )
    )

    (:action descargar
        :parameters ( ?obj - movil ?p - cargable ?loc - sitio ?t1 - tren ?t2 - tren ?f1 - fabrica ?f2 - fabrica)
        :precondition (and
            (en ?obj ?loc)
            (not(puerto ?loc))
            (cargado ?p ?obj)
            (espacio_tren ?obj ?t2)
            (contador_tren ?t1 ?t2)
            (espacio_fabrica ?loc ?f1)
            (contador_fabrica ?f1 ?f2)
            (not(espacio_max_fn ?loc ?f1))
        )
        :effect (and
            (not (cargado ?p ?obj))
            (contiene ?p ?loc)
            (espacio_tren ?obj ?t1)
            (not (espacio_tren ?obj ?t2))
            (espacio_fabrica ?loc ?f2)
            (not(espacio_fabrica ?loc ?f1))
            (when(fabrica ?loc)
                (procesado ?p)
            )
        )
    )

    (:action simplificar
        :parameters ( ?p - cargable ?loc - sitio ?f1 - fabrica ?f2 - fabrica)
        :precondition (and
            (almacen ?loc)
            (contiene ?p ?loc)
            (contador_fabrica ?f1 ?f2)
            (procesado ?p)
            (not(eliminado ?p))
        )
        :effect (and
            (eliminado ?p)
            (espacio_fabrica ?loc ?f1)
            (not (espacio_fabrica ?loc ?f2))
        )
    )
)