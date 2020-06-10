program hello
    ! ----------
    use class_archivo
    use class_instruccion
    use class_configuracion
    ! ----------
    implicit none
    type(configuracion) :: conf
    integer :: istat
    integer :: i
    ! ----------

    call conf%leer('ConfigurationFile.txt')

    i = 2

    ! ----------
end program

