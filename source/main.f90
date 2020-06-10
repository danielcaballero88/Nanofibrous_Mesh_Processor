program hello
    ! ----------
    use class_configuracion
    use procesador
    ! ----------
    implicit none
    character(len=120) :: nomarch_configfile
    type(configuracion) :: conf
    integer :: istat
    integer :: i
    ! ----------

    nomarch_configfile = 'ConfigurationFile.txt'
    call conf%leer(nomarch_configfile)
    call conf%imprimir()

    call procesar_instruccion(conf, 1)
    call procesar_instruccion(conf, 2)
    call procesar_instruccion(conf, 3)
    call procesar_instruccion(conf, 4)

    ! ----------
end program

