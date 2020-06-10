! ==========
module class_configuracion
    ! ==========
    use class_archivo
    use class_instruccion
    implicit none
    private
    ! ==========

    ! ----------
    type, public :: configuracion
        ! -----
        character(len=120) :: nomarch ! nombre del archivo donde esta la configuracion
        type(archivo) :: arch ! idem pero el objeto archivo
        type(instrucciones) :: instr
        integer :: num_parcon
        real(8), allocatable :: parcon(:)
        ! -----
    contains
        ! -----
        procedure :: leer => read_configfile
        procedure :: get_tipo
        procedure :: get_instr
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

        ! abrir archivo
        this%nomarch = nomarch
        istatus = this%arch%abrir(this%nomarch, 'old')

        ! leer instrucciones
        istatus = this%instr%leer(this%arch, '* Numero de acciones')

        ! leer parametros constitutivos
        istatus = this%arch%fsif('* Parametros Constitutivos', .true.)
        read(this%arch%fid, *) this%num_parcon
        allocate( this%parcon(this%num_parcon) )
        read(this%arch%fid, *) this%parcon

        ! cerrar archivo
        istatus = this%arch%cerrar()
        ! -----
    end subroutine read_configfile
    ! ----------

    ! ----------
    function get_tipo(this,i) result(tipo)
        implicit none
        class(configuracion), intent(in) :: this
        integer, intent(in) :: i
        character(len=120) :: tipo
        !
        tipo = this%instr%lista(i)%tipo
        !
    end function get_tipo
    ! ----------

    ! ----------
    function get_instr(this,i) result(instr)
        implicit none
        class(configuracion), intent(in) :: this
        integer, intent(in) :: i
        type(instruccion) :: instr
        !
        instr = this%instr%lista(i)
        !
    end function get_instr
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
        write(6,*) "---"
        write(6,*) "Parametros constitutivos"
        write(6,*) this%num_parcon
        write(6,*) this%parcon
        write(6,*) "---"
        ! -----
    end subroutine
    ! ----------

    ! ==========
end module class_configuracion
! ==========
