(define (problem rutas)
    (:domain rutas-clase)
    (:objects
        tren - movil
        puerto almacen fabrica1 fabrica2 - sitio
        paquete1 paquete2 paquete3 paquete4 paquete5 paquete6 paquete7 paquete8 - cargable
        espacio_fabrica1 espacio_fabrica2 espacio_fabrica3 - fabrica
        espacio_vagon1 espacio_vagon2 espacio_vagon3 espacio_vagon4 - tren
    )
    (:init
        ; gestion de sitios
        (en tren almacen)
        (almacen almacen)
        (puerto puerto)
        (fabrica fabrica1)
        (fabrica fabrica2)
        (conectada almacen puerto)
        (conectada puerto fabrica2)
        (conectada fabrica2 fabrica1)
        (conectada fabrica1 almacen)

        ; gestion de cargables
        (contiene paquete1 puerto)
        (contiene paquete2 puerto)
        (contiene paquete3 puerto)
        (contiene paquete4 puerto)
        (contiene paquete5 puerto)
        (contiene paquete6 puerto)
        (contiene paquete7 puerto)
        (contiene paquete8 puerto)

        ; gestion de espacio de almacen tren y fabrica
        (contador_tren espacio_vagon1 espacio_vagon2)
        (contador_tren espacio_vagon2 espacio_vagon3)
        (contador_tren espacio_vagon3 espacio_vagon4)
        (contador_fabrica espacio_fabrica1 espacio_fabrica2)
        (contador_fabrica espacio_fabrica2 espacio_fabrica3)
        (espacio_tren tren espacio_vagon1)
        (espacio_max_tn tren espacio_vagon4)
        (espacio_fabrica almacen espacio_fabrica1)
        (espacio_fabrica fabrica1 espacio_fabrica1)
        (espacio_fabrica fabrica2 espacio_fabrica1)
        (espacio_max_fn almacen espacio_fabrica3)
        (espacio_max_fn fabrica1 espacio_fabrica2)
        (espacio_max_fn fabrica2 espacio_fabrica3)

    )
    (:goal
        ; el procesamiento se realizara uno a uno ya que se ha realizado de forma incremental
        (and
            (procesado paquete1)
            (procesado paquete2)
            (procesado paquete3)
            (procesado paquete4)
            (procesado paquete5)
            (procesado paquete6)
            (procesado paquete7)
            (procesado paquete8)
            (contiene paquete1 almacen)
            (contiene paquete2 almacen)
            (contiene paquete3 almacen)
            (contiene paquete4 almacen)
            (contiene paquete5 almacen)
            (contiene paquete6 almacen)
            (contiene paquete7 almacen)
            (contiene paquete8 almacen)
            (eliminado paquete1)
            (eliminado paquete2)
            (eliminado paquete3)
            (eliminado paquete4)
            (eliminado paquete5)
            (eliminado paquete6)
            (eliminado paquete7)
            (eliminado paquete8)
        )
    )
)