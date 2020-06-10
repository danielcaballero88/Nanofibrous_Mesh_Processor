! ==========
module class_configuracion
    ! ==========
    use class_archivo
    use class_instruccion
    implicit none
    !private
    ! ==========

    ! ----------
    type, public :: configuracion
        ! -----
        character(len=120) :: nomarch ! nombre del archivo donde esta la configuracion
        type(archivo) :: arch ! idem pero el objeto archivo
        type(instrucciones) :: instr
        ! -----
    contains
        ! -----
        procedure :: leer => read_configfile
        procedure :: imprimir => imprimir
        ! -----
    end type configuracion
    ! ----------

    ! ==========
    contains
    ! ==========

    ! ----------
    subroutine read_configfile(this, nomarch)
        ! -----
        implicit none
        class(configuracion), intent(inout) :: this
        character(len=120), intent(in) :: nomarch
        integer :: istatus
        ! -----

        this%nomarch = nomarch
        istatus = this%arch%abrir(this%nomarch, 'old')

        istatus = this%instr%leer(this%arch, '* Numero de acciones')

        ! -----
    end subroutine read_configfile
    ! ----------

    ! ----------
    subroutine imprimir(this)
        ! -----
        implicit none
        class(configuracion), intent(in) :: this
        integer :: i
        ! -----
        write(6,*) "---"
        write(6,*) "Instrucciones"
        write(6,*) "n: ", this%instr%num
        write(6,*) "labels: ", this%instr%ilista
        write(6,*) "---"
        do i=1,this%instr%num
            write(6,*) get_str_label(this%instr%ilista(i))
            write(6,*) this%instr%lista(i)%tipo
        end do
        ! -----
    end subroutine
    ! ----------

    ! ==========
end module class_configuracion
! ==========
