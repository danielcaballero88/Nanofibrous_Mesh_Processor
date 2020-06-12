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

    call procesar_instrucciones(conf)

    ! ----------
end program

