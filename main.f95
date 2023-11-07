! Dupla Raquel Maciel e Yuri Mateus
subroutine grava_dados(n, x, y, arq)
    implicit none
    integer, intent(in) :: N
    real, intent(in) :: x(N), y(N)
    character(len=*) :: arq
    integer :: i, fu
    open(action='write', file=arq, newunit=fu, status='replace')
    do i = 1, N
        write(fu, *) x(i), y(i)
    end do
    close(fu)
end subroutine grava_dados

subroutine plota_resultados(arq)
    character(len=*) :: arq
    call execute_command_line('gnuplot -p ' // arq)
end subroutine plota_resultados

program integral_plotter
    implicit none
    integer :: i
    real :: A, B
    character(len=*), parameter :: OUT_FILE = 'data.txt' ! Output file.
    character(len=*), parameter :: PLT_FILE = 'plot.plt' ! Gnuplot file.
    integer, parameter :: N = 30
    real :: x(N)
    real :: y(N)
    A = 0.0
    B = 3.14159

    do i = 1, N
        x(i) = real(i)
        y(i) = f(x(i))
    end do

    call grava_dados(N, x, y, OUT_FILE)
    call plota_resultados(PLT_FILE)
    PRINT *, "Integral definida de", A, " até ", B, ": ", integral(A, B, n=1000)
    
contains

    FUNCTION roundsUp(x, places) RESULT(x2)
        REAL, INTENT(IN) :: x
        INTEGER, INTENT(IN) :: places
        REAL :: x2, pow 
        pow = 10.0 ** places
        x2 = floor(x * pow) / pow
    END FUNCTION roundsUp

    FUNCTION f(x) RESULT(y)
        REAL, INTENT(IN) :: x
        REAL :: y
        ! Trocar a função a ser calculada aqui
        INTRINSIC :: cos
        y = x ** 3
    END FUNCTION f

    FUNCTION integral(a, b, n) RESULT(integral_result)
        REAL, INTENT(IN) :: a, b
        INTEGER, INTENT(IN) :: n
        REAL :: integral_result
        REAL :: h, area
        INTEGER :: i

        h = (b - a) / REAL(n)

        area = 0.5 * (f(a) + f(b))
        
        DO i = 0, n - 1
            area = area + f(a + REAL(i) * h)
        END DO
        
        integral_result = roundsUp(h * area, 2)
    END FUNCTION integral

end program integral_plotter
