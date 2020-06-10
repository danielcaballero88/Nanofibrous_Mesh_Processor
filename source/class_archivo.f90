! ==========
module class_archivo
    ! ==========
    implicit none
    private
    ! ==========

    public :: get_file_unit

    type, public :: archivo
        character(len=120) :: nombre
        integer :: fid
        logical :: abierto = .false.
    contains
        procedure :: abrir => abrir_archivo
        procedure :: fsif => fsif
        procedure :: get_line => get_line
        procedure :: cerrar => cerrar_archivo
    end type archivo

    ! ==========
contains
    ! ==========

    ! ----------
    function abrir_archivo(this, nombre, str_status) result(istat)
        ! -----
        implicit none
        class(archivo), intent(inout) :: this
        character(len=*), intent(in) :: nombre
        character(len=*), intent(in) :: str_status
        integer :: istat
        ! -----

        ! --- Chequeo que este abierto el archivo
        if (this%abierto) then
            write(6,*) "WARNING, intentando abrir archivo ya abierto"
            istat = 1
        else
            this%nombre = nombre
            this%fid = get_file_unit()
            open(file=this%nombre, unit=this%fid, status=str_status, iostat=istat)
            this%abierto = .true.
            istat = 0
        end if
        ! -----
    end function abrir_archivo
    ! ----------

    ! ----------
    function cerrar_archivo(this) result(istat)
        ! -----
        implicit none
        class(archivo), intent(inout) :: this
        integer :: istat
        ! -----
        close(unit=this%fid, iostat=istat)
        this%abierto = .false.
        ! -----
    end function cerrar_archivo
    ! ----------

    ! ----------
    function fsif(this, target_str, mandatory) result(istat)
        ! -----
        implicit none
        class(archivo), intent(in) :: this
        character(len=*), intent(in) :: target_str
        logical, intent(in) :: mandatory
        integer :: istat
        ! -----

        istat = FindStringInFile(target_str, this%fid, mandatory)

        ! -----
    end function
    ! ----------

    ! ----------
    function get_line(this) result(line)
        ! -----
        implicit none
        class(archivo), intent(in) :: this
        character(len=50) :: line
        ! -----
        read(this%fid, '(A)') line
        ! -----
    end function
    ! ----------

    ! ----------
    function get_file_unit() result(lu)
        ! -----
        ! get_file_unit returns a unit number that is not in use
        ! -----
        !integer, intent(in) :: lu_max ! valor maximo posible para buscar units
        integer :: lu, checkIOSTAT
        logical :: checkOPENED
        integer, parameter :: m = 99
        ! -----
        do lu = m,1,-1
            inquire(unit=lu, opened=checkOPENED, iostat=checkIOSTAT)
            if (checkIOSTAT.ne.0) cycle
            if (.NOT.checkOPENED) exit
        end do
        ! -----

        ! -----
    end function get_file_unit
    ! ----------

    ! ----------
    FUNCTION Upper_Case(string)
        ! -----
        ! The Upper_Case function converts the lower case characters of a string to upper case one.
        ! Use this function in order to achieve case-insensitive: all character variables used
        ! are pre-processed by Uppper_Case function before these variables are used. So the users
        ! can call functions without pey attention of case of the keywords passed to the functions.
        ! -----
        IMPLICIT NONE
        ! -----
        CHARACTER(LEN=*), INTENT(IN):: string     ! string to be converted
        CHARACTER(LEN=LEN(string))::   Upper_Case ! converted string
        INTEGER::                      n1         ! characters counter
        ! -----

        Upper_Case = string
        DO n1=1,LEN(string)
            SELECT CASE(ICHAR(string(n1:n1)))
                CASE(97:122)
                    Upper_Case(n1:n1)=CHAR(ICHAR(string(n1:n1))-32) ! Upper case conversion
            END SELECT
        ENDDO

        ! -----
    END FUNCTION Upper_Case
    ! ----------

    ! ----------
    FUNCTION FindStringInFile(Str, ioUnit, Mandatory) RESULT (iError)
        ! -----
        ! Busca un String en un archivo (STR), sino lo encuentra rebovina el archivo
        ! y pone iError < 0 como indicador de no hallazgo
        ! Str: String to find, ioUnit: Unit assigned to Input File; iError: Error Status variable
        ! -----
        IMPLICIT NONE
        ! Parameters
        LOGICAL,PARAMETER  :: Segui=.True.
        ! Arguments
        CHARACTER(*), INTENT(IN) :: Str
        INTEGER, INTENT (IN) :: ioUnit
        LOGICAL, OPTIONAL, INTENT(IN) :: Mandatory
        ! Locals
        LOGICAL :: MandatoryL
        CHARACTER(LEN=120) :: DummyString
        CHARACTER(LEN=120) :: upStr
        INTEGER :: iError
        INTEGER :: Leng
        ! -----

        ! ---
        IF ( PRESENT(Mandatory) ) THEN
            MandatoryL = Mandatory
        ELSE
            MandatoryL = .FALSE.
        END IF
        ! ---
        iError=0
        Leng = LEN_TRIM(Str)
        upStr = Upper_Case(Str)       ! Line added by NB. Converts Str to Upper Case string
        ! ---
        REWIND(ioUnit)
        Search_Loop: DO WHILE (segui)
            READ(ioUnit,'(1A120)',IOSTAT=iError) DummyString ! iError es 0 si lee con exito, >0 si hay error y <0 si es end of file.
            DummyString = Upper_Case(DummyString)   ! line added by NB
            !       if (iError==59) backspace(ioUnit)
            IF (iError.lt.0) THEN
                REWIND (ioUnit)
                EXIT Search_Loop
            END IF
            IF ( DummyString(1:1)    /=    '*'      ) CYCLE Search_Loop
            IF ( DummyString(1:Leng) == upStr(1:Leng) ) EXIT Search_Loop
        END DO Search_Loop
        ! ---
        IF (MandatoryL) THEN
            IF ( .NOT. ( iError == 0 ) ) THEN
                WRITE(*,*) upStr, 'NOT FOUND'
                STOP
            END IF
        END IF
        ! ---

        ! -----
    END FUNCTION FindStringInFile
    ! ----------

end module class_archivo
! ==========
