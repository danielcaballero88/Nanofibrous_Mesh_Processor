! ==========
module procesador
    ! ==========
    use class_configuracion
    use class_instruccion, only : instruccion
    use programs
    implicit none
!    private
    ! ==========

    ! ==========
contains
    ! ==========

    ! ----------
    subroutine procesar_instruccion(conf, i)
        ! -----
        implicit none
        type(configuracion), intent(in) :: conf
        integer, intent(in) :: i
        ! -----

        select case ( trim(conf%instr%lista(i)%tipo) )
            case ("Intersectar")
                call procesar_interseccion(conf, i)
            case ("Simplificar")
                call procesar_simplificar(conf, i)
            case ("Equilibrar")
                call procesar_equilibrar(conf, i)
            case ("Traccion")
                call procesar_traccion(conf, i)
!            case ("Uniaxial")
            case default
                write(*,*) "Error, tipo de instruccion desconocido"
                stop
        end select

        ! -----
    end subroutine procesar_instruccion
    ! ----------

    ! ----------
    subroutine procesar_interseccion(conf, i)
        ! -----
        ! procesar la interseccion de 1 o varias mallas acorde a instr
        ! -----
        implicit none
        type(configuracion), intent(in) :: conf
        integer, intent(in) :: i
        type(instruccion) :: instr
        integer :: j
        ! -----

        instr = conf%instr%lista(i)
        do j=1,instr%num_mallas
            write(6,*) 'Procesar interseccion'
            write(6,*) 'Malla: ', trim(instr%lista_nomarchs_mallas(j)%s)
            write(6,*) 'num_pasos_intsec: ', instr%num_pasos_intsec
            write(6,*) 'periodic_intsec: ', instr%periodic_intsec
            call main_intersectar(instr%lista_nomarchs_mallas(j)%s, &
                                  instr%num_pasos_intsec, &
                                  instr%periodic_intsec)
        end do

        ! -----
    end subroutine procesar_interseccion
    ! ----------

    ! ----------
    subroutine procesar_simplificar(conf, i)
        ! -----
        ! procesar metodo simplificar de 1 o varias mallas acorde a instr
        ! -----
        implicit none
        type(configuracion), intent(in) :: conf
        integer, intent(in) :: i
        type(instruccion) :: instr
        integer :: j
        ! -----

        instr = conf%instr%lista(i)
        do j=1,instr%num_mallas
            write(6,*) '---'
            write(6,*) 'Procesar simplificar'
            write(6,*) 'Malla: ', trim(instr%lista_nomarchs_mallas(j)%s)
            call main_simplificar(instr%lista_nomarchs_mallas(j)%s, &
                                  conf%num_parcon, &
                                  conf%parcon)
            write(6,*) '---'
        end do

        ! -----
    end subroutine procesar_simplificar
    ! ----------

    ! ----------
    subroutine procesar_equilibrar(conf, i)
        ! -----
        ! procesar metodo equilibrar de 1 o varias mallas acorde a instr
        ! -----
        implicit none
        type(configuracion), intent(in) :: conf
        integer, intent(in) :: i
        type(instruccion) :: instr
        integer :: j, k
        character(len=5) :: str_Fk
        ! -----

        instr = conf%instr%lista(i)
        do j=1,instr%num_mallas
            write(6,*) '---'
            write(6,*) 'Procesar equilibrar'
            write(6,*) 'Malla: ', trim(instr%lista_nomarchs_mallas(j)%s)
            do k=1,instr%num_Fs
                write(str_Fk, '(A2, I3.3)') '_e', k ! 7.7 indica que el campo es de 7 y se usan como minimo 7, entonces imprime los ceros
                call main_equilibrar(instr%lista_nomarchs_mallas(j)%s, &
                                     conf%num_parcon, &
                                     conf%parcon, &
                                     instr%lista_Fs(:,:,k), &
                                     instr%num_pasos_vibrac, &
                                     instr%lista_niters_vibrac, &
                                     instr%lista_drmags_vibrac, &
                                     instr%fza_ref, &
                                     instr%fza_tol, &
                                     str_Fk)
            end do
            write(6,*) '---'
        end do

        ! -----
    end subroutine procesar_equilibrar
    ! ----------

    ! ----------
    subroutine procesar_traccion(conf, i)
        ! -----
        ! procesar metodo equilibrar de 1 o varias mallas acorde a instr
        ! -----
        implicit none
        type(configuracion), intent(in) :: conf
        integer, intent(in) :: i
        type(instruccion) :: instr
        integer :: j, k
        character(len=120) :: aux_string
        ! -----

        aux_string = ' ' ! esta para mantener compatibilidad con main_traccion, en un futuro se va
        instr = conf%instr%lista(i)
        do j=1,instr%num_mallas
            write(6,*) '---'
            write(6,*) 'Procesar traccion'
            write(6,*) 'Malla: ', trim(instr%lista_nomarchs_mallas(j)%s)
            call main_traccion(instr%lista_nomarchs_mallas(j)%s, &
                               conf%num_parcon, &
                               conf%parcon, &
                               instr%num_pasos_vibrac, &
                               instr%lista_niters_vibrac, &
                               instr%lista_drmags_vibrac, &
                               instr%fza_ref, &
                               instr%fza_tol, &
                               instr%dtime, &
                               instr%dot_F11, &
                               instr%dot_F22, &
                               instr%F11_fin, &
                               aux_string, &
                               instr%opcion_save, &
                               instr%dF11_save)
            write(6,*) '---'
        end do

        ! -----
    end subroutine procesar_traccion
    ! ----------

    ! ==========
end module procesador
! ==========
