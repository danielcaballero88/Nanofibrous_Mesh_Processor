program hello
    ! ----------
    use class_archivo
    use class_instruccion
    ! ----------
    implicit none
    type(archivo) :: arch
    type(instruccion) :: instr
    integer :: istat
    integer :: i
    ! ----------

    istat = arch%abrir('ConfigurationFile.txt', 'old')

!    istat = read_from_configfile(instr, arch, 1)
    istat = instr%leer(arch, 1)


    istat = arch%cerrar()

    ! ----------
end program

