! ==========
module class_instruccion
! Modulo dedicado a opciones de seteo del problema: case parameters
    ! ==========
    use class_archivo
    implicit none
    private
    ! ==========

    type string
        character(len=120) :: s
    end type

    ! ----------
    type, public :: instruccion
        ! -----
!        private ! no se puede acceder a las variables
        ! -----
        ! variables generales
        character(len=120) :: tipo
        integer :: opcion_mallas
        character(len=120) :: nomarch_mallas
        integer :: num_mallas
        type(string), allocatable :: lista_nomarchs_mallas(:) ! lista de nombres de archivos de mallas
        ! variables interseccion
        integer :: num_pasos_intsec
        logical :: periodic_intsec
        ! variables equilibrar
        integer :: num_pasos_vibrac
        integer, allocatable :: lista_niters_vibrac(:)
        real(8), allocatable :: lista_drmags_vibrac(:)
        real(8) :: fza_ref ! referencia de fuerza para calcular desplazamientos
        real(8) :: fza_tol ! tolerancia de fuerza para calcular si un nodo esta en equilibrio
        integer :: opcion_Fs ! opcion para el F macro (=1 es un solo F, =2 es una lista de valores de F)
        character(len=120) :: nomarch_Fs ! nombre de archivo con lista de deformaciones
        integer :: num_Fs
        real(8), allocatable :: lista_Fs(:,:,:) ! shape=(2,2,num_Fs)
        ! variables traccion (algunas ya estan)
        real(8) :: dtime, dot_F11, dot_F22, F11_fin
        integer :: opcion_save ! para guardar archivos de mallas deformadas (0=no, 1=si)
        real(8) :: dF11_save
        integer :: num_F11_saves
        real(8), allocatable :: lista_F11_tosave(:) ! shape=(num_F11_saves)
        logical, allocatable :: lista_F11_ifsaved(:) ! shape=(num_F11_saves)
        logical, allocatable :: completed_F11_saves(:) ! shape=(num_mallas)
        ! variables uniaxial (usa la mayoria de los metodos previos)
        real(8) :: ten22
        ! -----
    contains
        ! -----
        procedure :: leer => read_from_configfile
        ! -----
    end type
    ! ----------

    ! ==========
contains
    ! ==========

    ! ----------
    ! HUB
    function read_from_configfile(this, arch, ilabel) result(istat)
        ! -----
        ! funcion para leer los parametros de una instruccion cualquiera a partir de configfile
        implicit none
        class(instruccion), intent(inout) :: this
        type(archivo), intent(in) :: arch ! intent(in) porque solo se lee
        integer, intent(in) :: ilabel
        ! -----
        integer :: istat
        ! -----

        istat = arch%fsif( get_str_label(ilabel), .true.)
        read(arch%fid, *) this%tipo
        select case (trim(this%tipo))
            case ("Intersectar")
                istat = read_from_configfile_intersectar(this, arch)
            case ("Simplificar")
                istat = read_from_configfile_simplificar(this, arch)
            case ("Equilibrar")
                istat = read_from_configfile_equilibrar(this, arch)
            case ("Traccion")
                istat = read_from_configfile_traccion(this, arch)
            case ("Uniaxial")
                istat = read_from_configfile_uniaxial(this, arch)
            case default
                write(*,*) "Error, tipo de instruccion desconocido"
                stop
        end select

        istat = 0 ! todo ok

        ! -----
    end function read_from_configfile
    ! ----------

    ! ----------
    function read_from_configfile_intersectar(this, arch) result(istat)
        ! -----
        ! leer los parametros propios del metodo intersectar
        ! ya se leyo el tipo de instruccion, restan los demas parametros
        ! -----
        implicit none
        ! -----
        class(instruccion), intent(inout) :: this
        type(archivo), intent(in) :: arch
        integer :: istat
        ! -----

        ! ---
        read(arch%fid, *) this%opcion_mallas, this%nomarch_mallas
        call get_lista_mallas(this)
        read(arch%fid, *) this%num_pasos_intsec
        read(arch%fid, *) this%periodic_intsec
        ! -
        istat = 0 ! todo ok
        ! ---

        ! -----
    end function read_from_configfile_intersectar
    ! ----------

    ! ----------
    function read_from_configfile_simplificar(this, arch) result(istat)
        ! -----
        ! leer los parametros propios del metodo simplificar
        ! ya se leyo el tipo de instruccion, restan los demas parametros
        implicit none
        ! -----
        class(instruccion), intent(inout) :: this
        type(archivo), intent(in) :: arch
        integer :: istat
        ! -----

        read(arch%fid, *) this%opcion_mallas, this%nomarch_mallas
        call get_lista_mallas(this)
        !
        istat = 0 ! todo ok

        ! -----
    end function read_from_configfile_simplificar
    ! ----------

    ! ----------
    function read_from_configfile_equilibrar(this, arch) result(istat)
        ! -----
        ! leer los parametros propios del metodo equilibrar
        ! ya se leyo el tipo de instruccion, restan los demas parametros
        ! -----
        implicit none
        ! -----
        class(instruccion), intent(inout) :: this
        type(archivo), intent(in) :: arch
        integer :: istat
        ! -----

        ! leo las mallas a procesar
        read(arch%fid, *) this%opcion_mallas, this%nomarch_mallas
        call get_lista_mallas(this)
        ! leo parametros de solver
        read(arch%fid, *) this%num_pasos_vibrac
        if (this%num_pasos_vibrac > 0) then ! si es =0 entonces no hay pasos de equilibrio, se calcula el afin y nada mas
            allocate( this%lista_niters_vibrac(this%num_pasos_vibrac) )
            allocate( this%lista_drmags_vibrac(this%num_pasos_vibrac) )
        end if
        read(arch%fid, *) this%lista_niters_vibrac
        read(arch%fid, *) this%lista_drmags_vibrac
        read(arch%fid, *) this%fza_ref, this%fza_tol
        ! leo deformaciones macro a equilibrar
        read(arch%fid, *) this%opcion_Fs
        read(arch%fid, *) this%nomarch_Fs
        call get_lista_Fs(this)
        !
        istat = 0 ! todo ok

        ! -----
    end function read_from_configfile_equilibrar
    ! ----------

    ! ----------
    function read_from_configfile_traccion(this, arch) result(istat)
        ! -----
        ! leer los parametros propios del metodo traccion
        ! ya se leyo el tipo de instruccion, restan los demas parametros
        ! -----
        implicit none
        ! -----
        class(instruccion), intent(inout) :: this
        type(archivo), intent(in) :: arch
        integer :: istat
        ! -----

        read(arch%fid, *) this%opcion_mallas, this%nomarch_mallas
        call get_lista_mallas(this)
        ! leo parametros de solver
        read(arch%fid, *) this%num_pasos_vibrac
        if (this%num_pasos_vibrac > 0) then ! si es =0 entonces no hay pasos de equilibrio, se calcula el afin y nada mas
            allocate( this%lista_niters_vibrac(this%num_pasos_vibrac) )
            allocate( this%lista_drmags_vibrac(this%num_pasos_vibrac) )
        end if
        read(arch%fid, *) this%lista_niters_vibrac
        read(arch%fid, *) this%lista_drmags_vibrac
        read(arch%fid, *) this%fza_ref, this%fza_tol
        ! leo parametros temporales de la traccion
        read(arch%fid, *) this%dtime, this%dot_F11, this%dot_F22, this%F11_fin
        read(arch%fid, *) this%opcion_save, this%dF11_save
        call assemble_saves(this)
        !
        istat = 0 ! todo ok

        ! -----
    end function read_from_configfile_traccion
    ! ----------

    ! ----------
    function read_from_configfile_uniaxial(self, arch) result(istat)
        ! -----
        ! leer los parametros propios del metodo uniaxial
        ! ya se leyo el tipo de instruccion, restan los demas parametros
        ! -----
        implicit none
        ! -----
        class(instruccion), intent(inout) :: self
        type(archivo), intent(in) :: arch
        integer :: istat
        ! -----

        ! leo las mallas a procesar
        read(arch%fid, *) self%opcion_mallas, self%nomarch_mallas
        call get_lista_mallas(self)
        ! leo parametros de solver
        read(arch%fid, *) self%num_pasos_vibrac
        if (self%num_pasos_vibrac > 0) then ! si es =0 entonces no hay pasos de equilibrio, se calcula el afin y nada mas
            allocate( self%lista_niters_vibrac(self%num_pasos_vibrac) )
            allocate( self%lista_drmags_vibrac(self%num_pasos_vibrac) )
        end if
        read(arch%fid, *) self%lista_niters_vibrac
        read(arch%fid, *) self%lista_drmags_vibrac
        read(arch%fid, *) self%fza_ref, self%fza_tol
        ! leo parametros temporales de la traccion
        read(arch%fid, *) self%dtime, self%dot_F11, self%ten22, self%F11_fin
        read(arch%fid, *) self%opcion_save, self%dF11_save
        call assemble_saves(self)
        !
        istat = 0 ! todo ok

        ! -----
    end function read_from_configfile_uniaxial
    ! ----------

    ! ----------
    ! LOCAL
    subroutine get_lista_mallas(this)
        ! -----
        implicit none
        ! -----
        class(instruccion), intent(inout) :: this
        ! -----
        type(archivo) :: arch_mallas
        integer :: i
        integer :: istatus
        ! -----

        if (this%opcion_mallas==1) then ! si opcion=1, entonces tengo una sola malla
            this%num_mallas = 1
            allocate( this%lista_nomarchs_mallas(this%num_mallas) )
            this%lista_nomarchs_mallas(1)%s = this%nomarch_mallas
        else if (this%opcion_mallas==2) then ! si opcion_mallas=2, entonces se lee una lista de mallas
            istatus = arch_mallas%abrir(this%nomarch_mallas, 'old')
            ! -
                read(arch_mallas%fid, *) this%num_mallas
                allocate( this%lista_nomarchs_mallas(this%num_mallas) )
                do i=1,this%num_mallas
                    read(arch_mallas%fid, '(A)') this%lista_nomarchs_mallas(i)%s
                end do
            ! -
            istatus = arch_mallas%cerrar()
        else
            write(*,*) "Error, opcion debe ser 1 o 2 y es: ", this%opcion_mallas
            stop
        end if

        ! -----
    end subroutine get_lista_mallas
    ! ----------

    ! ----------
    ! LOCAL
    subroutine get_lista_Fs(this)
        ! -----
        implicit none
        ! -----
        class(instruccion), intent(inout) :: this
        ! -----
        type(archivo) :: arch_Fs
        integer :: i
        integer :: istat
        ! -----

        if (this%opcion_Fs==1) then ! si opcion=1, entonces tengo un solo F dado en lugar de this%arch_Fs
            this%num_Fs = 1
            allocate( this%lista_Fs(2,2,this%num_Fs) )
            read(this%nomarch_Fs, *) this%lista_Fs(:,:,1) ! leo los valores de F de la strin this%arch_Fs
        else if (this%opcion_Fs==2) then ! si opcion=2, entonces se lee una lista de Fs del archivo dado en line
            istat = arch_Fs%abrir(this%nomarch_Fs, 'old')
            ! -
                read(arch_Fs%fid, *) this%num_Fs
                allocate( this%lista_Fs(2,2,this%num_Fs) )
                do i=1,this%num_Fs
                    read(arch_Fs%fid, *) this%lista_Fs(:,:,i)
                end do
            ! -
            close(unit=arch_Fs%fid)
        else
            write(*,*) "Error, opcion debe ser 1 o 2 y es: ", this%opcion_Fs
            stop
        end if

        ! -----
    end subroutine get_lista_Fs
    ! ----------

    ! ----------
    ! LOCAL
    subroutine assemble_saves(this)
        ! -----
        implicit none
        ! -----
        class(instruccion), intent(inout) :: this
        ! -----
        integer :: i
        ! -----

        if (this%opcion_save==1) then ! preparo una lista de saves
            this%num_F11_saves = int( (this%F11_fin-1.d0) / this%dF11_save ) + 1
            allocate( this%lista_F11_tosave(this%num_F11_saves) )
            allocate( this%lista_F11_ifsaved(this%num_F11_saves) )
            do i=1,this%num_F11_saves
                this%lista_F11_tosave(i) = 1.d0 + dfloat(i-1)*this%dF11_save
                this%lista_F11_ifsaved(i) = .false.
            end do
            allocate ( this%completed_F11_saves(this%num_mallas) )
            this%completed_F11_saves = .false.
        else
            this%num_F11_saves = 0
            allocate ( this%completed_F11_saves(this%num_mallas) )
            this%completed_F11_saves = .true.
        end if

        ! -----
    end subroutine
    ! ----------

    ! ----------
    ! LOCAL STATIC
    pure function get_str_label(ilabel) result(slabel)
        ! -----
        ! Given an integer label 'ilabel' returns a strin label "* <ilabel>"
        ! IMPORTANT: slabel has len=3, so ilabel can be max 9
        ! -----
        implicit none
        integer, intent(in) :: ilabel
        character(len=3) :: slabel
        ! -----
        ! ---
        WRITE(slabel,'(A2,I1)') "* ", ilabel
        ! ---
        ! -----
    end function
    ! ----------

! ==========================================================================
end module class_instruccion
! ==========================================================================
