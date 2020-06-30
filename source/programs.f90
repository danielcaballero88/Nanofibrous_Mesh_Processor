module programs

use Aux
USE class_malla_completa, ONLY : MallaCom
use class_mallita, only : MallaSim

contains

! ==========================================================================
subroutine main_intersectar(filename_malla_in, npasadas, periodicidad)
    use class_malla_completa
    implicit none
    CHARACTER(LEN=120), intent(in) :: filename_malla_in
    integer, intent(in) :: npasadas
    logical, intent(in) :: periodicidad
    character(len=120) :: filename_malla_out
    TYPE(MallaCom) :: MC, MC2
    integer :: i
    integer :: iStat1, iStat2

    write(*,*) "Leer malla, intersectar fibras y reescribir:"
    CALL leer_malla(MC, filename_malla_in)
    ! Hago la interseccion muchas veces porque cada vez tengo la limitacion de no cortar al mismo segmento dos veces
    i = 0
    write(*,*) "Intersectando fibras"
    iStat1 = 0
    iStat2 = 0
    write(*,*) mc%nsegs
    DO WHILE (.true.)
        i = i+1
        WRITE(*,'(I4)', ADVANCE='no') i
        CALL intersectar_fibras(MC, MC2, .FALSE., periodicidad, iStat1) ! dentro de la misma capa
        MC = MC2
        CALL intersectar_fibras(MC, MC2, .TRUE., periodicidad, iStat2) ! con capas adyacentes
        MC = MC2
        write(*,*) mc%nsegs
        IF ( (iStat1 == 1).AND.(iStat2 == 1) )  EXIT
        if (i==npasadas) exit
    END DO
    write(*,*)

    write(*,*) "Escribiendo malla intersectada"
    filename_malla_out = "_i"
    call modify_txt_filename(filename_malla_in, filename_malla_out)
    CALL escribir_malla(mc, filename_malla_out)
    write(*,*) "Malla intersectada OK"

end subroutine main_intersectar
! ==========================================================================


! ==========================================================================
subroutine main_simplificar(filename_malla_in, nparamcon, paramcon)
    use class_malla_completa
    use class_mallita
    implicit none
    CHARACTER(LEN=120), intent(in) :: filename_malla_in ! nombre original de la malla (as deposited)
    integer, intent(in) :: nparamcon
    real(8), intent(in) :: paramcon(nparamcon)
    character(len=120) :: filename_malla_in2, filename_malla_out
    type(MallaCom) :: mc
    type(MallaSim) :: ms

    write(*,*) "Leer malla intersectada y generar malla simplificada:"
    ! El archivo de malla que leo debe haber sido intersectado, por eso,
    ! Le agrego al nombre de malla el identificador de que ya fue intersectada
    filename_malla_in2 = "_i"
    call modify_txt_filename(filename_malla_in, filename_malla_in2)
    call leer_malla(mc, filename_malla_in2)
    call Desde_MallaCom(mc, ms, nparamcon, paramcon)

    write(*,*) "Escribiendo mallita"
    ! la malla que escribo va a tener identificador "_i_s"
    filename_malla_out = "_s"
    call modify_txt_filename(filename_malla_in2, filename_malla_out)
    CALL escribir_mallita(ms, filename_malla_out)
    write(*,*) "Malla simplificada OK"

end subroutine main_simplificar
! ==========================================================================

! ==========================================================================
subroutine main_equilibrar  (filename_malla_in, &
                            nparcon, parcon, &
                            Fmacro, num_pasos, lista_veces, lista_drmags, fzaref, fzatol, &
                            str_num_output_opt)
    ! Calcula el equilibrio elastico de una malla dado un tensor F macroscopico (Fmacro)
    ! ----------
    use class_mallita
    implicit none
    ! ----------
    CHARACTER(LEN=120), intent(in) :: filename_malla_in
    integer, intent(in) :: nparcon
    real(8), intent(in) :: parcon(nparcon)
    real(8), intent(in) :: Fmacro(2,2)
    integer, intent(in) :: num_pasos
    integer, intent(in) :: lista_veces(num_pasos)
    real(8), intent(in) :: lista_drmags(num_pasos)
    real(8), intent(in) :: fzaref
    real(8), intent(in) :: fzatol
    character(len=*), intent(in), optional :: str_num_output_opt
    ! ----------
    character(len=120) :: filename_malla_in2, filename_malla_out, aux_string
    type(MallaSim) :: ms
    integer :: n
    ! ----------

    ! La malla que leo debe haber sido intersectada y simplificada
    filename_malla_in2 = "_i_s" ! por eso busco el archivo con estos identificadores
    call modify_txt_filename(filename_malla_in, filename_malla_in2)
    write(*,*) "Calculando equilibrio"
    call leer_mallita(ms, filename_malla_in2, nparcon, parcon)
    n = ms%nnods

    call calcular_equilibrio(ms, num_pasos, lista_veces, lista_drmags, fzaref, fzatol, Fmacro)

    filename_malla_out = "_e"
    call modify_txt_filename(filename_malla_in2, filename_malla_out)
    if (present(str_num_output_opt)) then
        aux_string = trim(str_num_output_opt)
        call modify_txt_filename(filename_malla_out, aux_string)
        filename_malla_out = aux_string
    end if
    call escribir_mallita(ms, filename_malla_out)
    write(*,*) "Equilibrio calculado OK"

    ! ----------
end subroutine main_equilibrar
! ==========================================================================


! ==========================================================================
subroutine main_traccion(filename_malla_in, &
                        nparcon, parcon, &
                        num_pasos, lista_veces, lista_drmags, fzaref, fzatol, &
                        dtime, dotF11, dotF22, F11fin, &
                        filename_curva, &
                        opcion_save, nsaves, lista_saves_F)
    ! Simula un ensayo de traccion con un esquema explicito
    ! imponiendo tasas de deformacion axial y transversal
    ! ----------
    use class_mallita
    implicit none
    ! ----------
    CHARACTER(LEN=120), intent(in) :: filename_malla_in
    integer, intent(in) :: nparcon
    real(8), intent(in) :: parcon(nparcon)
    integer, intent(in) :: num_pasos
    integer, intent(in) :: lista_veces(num_pasos)
    real(8), intent(in) :: lista_drmags(num_pasos)
    real(8), intent(in) :: fzaref
    real(8), intent(in) :: fzatol
    real(8), intent(in) :: dtime
    real(8), intent(in) :: dotF11
    real(8), intent(in) :: dotF22
    real(8), intent(in) :: F11fin
    CHARACTER(LEN=120), intent(in) :: filename_curva
    integer, intent(in) :: opcion_save
    integer, intent(in) :: nsaves
    real(8), intent(in) :: lista_saves_F(nsaves)
    ! ----------
    character(len=120) :: filename_malla_in2, filename_malla_out, filename_curva2
    real(8) :: F11ini
    integer :: fid_curva
    real(8) :: time
    real(8) :: Fmacro(2,2)
    type(MallaSim) :: ms
    logical, allocatable :: lista_saves_if(:)
    logical :: listo_saves
    integer :: isave
    ! ----------

    write(*,*) "Empezando Traccion"

    ! Preparo la lista de saves si es que hay
    if (opcion_save==1) then
        allocate( lista_saves_if(nsaves) )
        lista_saves_if = .false.
        listo_saves = .false.
    else
        listo_saves = .true. ! no habra saves
    end if

    write(*,*) "Leyendo Malla:"

    ! agrego identificador de malla intersectada y simplificada
    filename_malla_in2 = "_i_s"
    call modify_txt_filename(filename_malla_in, filename_malla_in2)
    ! defino el nombre del archivo de la curva constitutiva
    ! lo hago agregando la nombre del archivo de la malla un identificador:
    filename_curva2 = "_c"
    call modify_txt_filename(filename_malla_in, filename_curva2)
    ! Leo la malla
    write(*,*) "archivo: ", filename_malla_in2
    call leer_mallita(ms, filename_malla_in2, nparcon, parcon)


    if (ms%status_deformed) then
        ! Si la malla esta previamente deformada, empiezo a trabajar desde alli
        Fmacro = ms%Fmacro
        F11ini = Fmacro(1,1)
        lista_saves_if = (F11ini > lista_saves_F)
        isave = count(lista_saves_if) + 1
        ! Abro un archivo viejo para continuar la curva constitutiva
        fid_curva = get_file_unit()
        open(unit=fid_curva, file=trim(filename_curva2), status="old", position="append", action="write")
    else
        ! Si la malla esta virgen, empiezo desde deformacion nula
        Fmacro = reshape(source=[1.d0, 0.d0, 0.d0, 1.d0], shape=shape(Fmacro))
        isave = 1
        ! Abro un archivo nuevo para escribir la curva constitutiva
        fid_curva = get_file_unit()
        open(unit=fid_curva, file=trim(filename_curva2), status="replace")
    end if

    ! Comienzo esquema temporal
    time = 0.d0
    do while ( Fmacro(1,1) .le. F11fin )
        ! Calculo el equilibrio de la malla para el Fmacro dado en este paso de tiempo
        call calcular_equilibrio(ms, num_pasos, lista_veces, lista_drmags, fzaref, fzatol, Fmacro)
        ! Guardo en archivo la informacion constitutiva para este step
        write(*,"(2E20.8E4)") Fmacro(1,1), ms%Tmacro(1,1)
        write(fid_curva,"(8E20.8E4)") Fmacro, ms%Tmacro
        ! Calculo plasticidad y/o rotura de fibras
        call calcular_plasticidad_rotura(ms, dtime)
        ! ---
        ! Me fijo si guardo la malla o no
        if (.not. listo_saves) then
            if ( dabs( Fmacro(1,1) - lista_saves_F(isave) ) < dotF11*dtime ) then
                write(filename_malla_out,"(A6, I4.4)") "_trac_", isave
                call modify_txt_filename(filename_malla_in2, filename_malla_out)
                call escribir_mallita(ms, filename_malla_out)
                isave = isave + 1
                if (isave > nsaves) listo_saves = .true.
            end if
        end if
        ! ---
        ! Incremento tiempo y deformacion para siguiente paso
        Fmacro(1,1) = Fmacro(1,1) + dotF11*dtime
        Fmacro(2,2) = Fmacro(2,2) + dotF22*dtime
        time = time + dtime
    end do
    ! Cierro el archivo donde guarda la curva constitutiva
    close(unit=fid_curva)

    ! ----------
end subroutine main_traccion
! ==========================================================================


! ==========================================================================
subroutine main_uniaxial(filename_malla_in, &
                        nparcon, parcon, &
                        num_pasos, lista_veces, lista_drmags, fzaref, fzatol, &
                        dtime, dotF11, T22, F11fin, &
                        filename_curva, &
                        opcion_save, nsaves, lista_saves_F)
    ! Simula un ensayo de traccion con un esquema explicito
    ! imponiendo tasas de deformacion axial y transversal
    ! ----------
    use class_mallita
    implicit none
    ! ----------
    CHARACTER(LEN=120), intent(in) :: filename_malla_in
    integer, intent(in) :: nparcon
    real(8), intent(in) :: parcon(nparcon)
    integer, intent(in) :: num_pasos
    integer, intent(in) :: lista_veces(num_pasos)
    real(8), intent(in) :: lista_drmags(num_pasos)
    real(8), intent(in) :: fzaref
    real(8), intent(in) :: fzatol
    real(8), intent(in) :: dtime
    real(8), intent(in) :: dotF11
    real(8), intent(in) :: T22 ! tension contra la cual contraer (en uniaxial verdadero seria cero)
    real(8), intent(in) :: F11fin
    CHARACTER(LEN=120), intent(in) :: filename_curva
    integer, intent(in) :: opcion_save
    integer, intent(in) :: nsaves
    real(8), intent(in) :: lista_saves_F(nsaves)
    ! ----------
    character(len=120) :: filename_malla_in2, filename_malla_out, filename_curva2
    real(8) :: F11ini
    integer :: fid_curva
    real(8) :: time
    real(8) :: Fmacro(2,2)
    type(MallaSim) :: ms
    logical, allocatable :: lista_saves_if(:)
    logical :: listo_saves = .false.
    integer :: isave
    ! ----------
    integer :: k, maxk=100
    real(8) :: dF22 = 0.01d0
    ! ----------

    write(*,*) "Empezando Uniaxial"

    ! Preparo la lista de saves si es que hay
    if (opcion_save==1) then
        allocate( lista_saves_if(nsaves) )
        lista_saves_if = .false.
        listo_saves = .false.
    else
        listo_saves = .true. ! no habra saves
    end if

    write(*,*) "Leyendo Malla:"
    ! Agrego identificador de malla intersectada y simplificada
    filename_malla_in2 = "_i_s"
    call modify_txt_filename(filename_malla_in, filename_malla_in2)
    ! Defino el nombre del archivo de la curva constitutiva
    ! (lo hago agregando la nombre del archivo de la malla un identificador)
    filename_curva2 = "_c"
    call modify_txt_filename(filename_malla_in, filename_curva2)
    ! Leo la malla
    write(*,*) "archivo: ", filename_malla_in2
    call leer_mallita(ms, filename_malla_in2, nparcon, parcon)

    if (ms%status_deformed) then
        ! Si la malla esta previamente deformada, empiezo a trabajar desde alli
        Fmacro = ms%Fmacro
        F11ini = Fmacro(1,1)
        lista_saves_if = (F11ini > lista_saves_F)
        isave = count(lista_saves_if) + 1
        ! Abro un archivo viejo para continuar la curva constitutiva
        fid_curva = get_file_unit()
        open(unit=fid_curva, file=trim(filename_curva2), status="old", position="append", action="write")
    else
        ! Si la malla esta virgen, empiezo desde deformacion nula
        Fmacro = reshape(source=[1.d0, 0.d0, 0.d0, 1.d0], shape=shape(Fmacro))
        isave = 1
        ! Abro un archivo nuevo para escribir la curva constitutiva
        fid_curva = get_file_unit()
        open(unit=fid_curva, file=trim(filename_curva2), status="replace")
    end if

    ! Comienzo esquema temporal
    time = 0.d0
    do while ( Fmacro(1,1) .le. F11fin )
        ! Calculo el equilibrio de la malla para el Fmacro dado en este paso de tiempo y F22 de la iteracion anterior
        do k=1,maxk
            call calcular_equilibrio(ms, num_pasos, lista_veces, lista_drmags, fzaref, fzatol, Fmacro)
            if (ms%Tmacro(2,2)>T22) then
                Fmacro(2,2) = Fmacro(2,2) - dF22
!            else if (ms%Tmacro(2,2) < T22) then
!                Fmacro(2,2) = Fmacro(2,2) + dF22
!                exit
            else
                exit
            end if
        end do
        ! Guardo en archivo la informacion constitutiva para este step
        write(*,"(4E20.8E4)") Fmacro(1,1), ms%Tmacro(1,1), Fmacro(2,2), ms%Tmacro(2,2)
        write(fid_curva,"(8E20.8E4)") Fmacro, ms%Tmacro
        ! Calculo plasticidad y/o rotura de fibras
        call calcular_plasticidad_rotura(ms, dtime)
        ! Me fijo si guardo la malla o no
        if (.not. listo_saves) then
            if ( dabs( Fmacro(1,1) - lista_saves_F(isave) ) < dotF11*dtime ) then
                write(filename_malla_out,"(A6, I4.4)") "_uaxi_", isave
                call modify_txt_filename(filename_malla_in2, filename_malla_out)
                call escribir_mallita(ms, filename_malla_out)
                isave = isave + 1
                if (isave > nsaves) listo_saves = .true.
            end if
        end if
        ! Incremento tiempo y deformacion para siguiente paso
        Fmacro(1,1) = Fmacro(1,1) + dotF11*dtime
        time = time + dtime
    end do
    ! Cierro el archivo donde guarda la curva constitutiva
    close(unit=fid_curva)

    ! ----------
end subroutine main_uniaxial
! ==========================================================================

end module
