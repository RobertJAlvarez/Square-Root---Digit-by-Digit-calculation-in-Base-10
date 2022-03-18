PROGRAM sqrt_test
  IMPLICIT NONE
  INTEGER, PARAMETER :: INT_QD = SELECTED_INT_KIND(18)  ! Use 64 bits
  INTEGER, PARAMETER :: REAL_DBL = SELECTED_REAL_KIND(p=15) ! Use 64 bits
  REAL(REAL_DBL) :: num, quot_result, S1, S2
  INTEGER :: i

  DO i=1,25
    CALL RANDOM_NUMBER(num)
    WRITE(*,'(4(A15,A3))') 'Num','','SQRT(num)','','SQR(num):','','Diff:',''
    S1 = SQRT(num)
    S2 = SQR(num)
    WRITE(*,'(4(F15.13,A3))') num,'', S1,'', S2,'', ABS(S2-S1),''
  END DO

  CONTAINS

  REAL(REAL_DBL) FUNCTION SQR(pass_num)
    IMPLICIT NONE
    REAL(REAL_DBL), INTENT(IN) :: pass_num
    REAL(REAL_DBL) :: real_part, quot ! Decimal part of pass_num -- quotient
    INTEGER(INT_QD) :: divi, rem      ! Divisor -- remainder
    INTEGER :: natural_part           ! Whole numbers in pass_num
    INTEGER :: n_dig_pairs            ! Save number of pair for integers
    INTEGER :: new_quot_dig           ! Save new quotient digit
    INTEGER :: i, two_dig             ! Dummy variable -- next two digits to add to rem, 
    INTEGER :: nat_r                  ! Save natural_part in reverse pair, Ex: 1234 -> 3412

    natural_part = int(pass_num)
    real_part = pass_num - natural_part

    divi = 0
    quot = 0.0D0
    rem = 0
    n_dig_pairs = 0
    ! If pass_num is greater than 1
    IF (natural_part /= 0) THEN
      ! Make pair
      nat_r = 0
      DO WHILE (natural_part > 0)
        nat_r = nat_r*100 + MOD(natural_part,100)
        natural_part = natural_part/100
        n_dig_pairs = n_dig_pairs + 1
      END DO
      ! Calculate sqrt root
      DO i=1, n_dig_pairs
        two_dig = MOD(nat_r,100)                        ! Get first two digits of natural_part
        nat_r = nat_r/100                               ! Delete those first two digits
        rem = rem*100 + two_dig                         ! Add next 2 digits in the dividend
        new_quot_dig = new_divi(divi,rem)               ! Find new digit for quotient
        quot = quot*10.0D0 + new_quot_dig               ! Update quotient
        rem = rem - ((divi+new_quot_dig)*new_quot_dig)  ! Perform division
        divi = (divi + 2*new_quot_dig)*10               ! Update dividend
      END DO
    END IF
    ! Real part is always calculated for maximum precision
    DO i=n_dig_pairs+1, 14
      real_part = real_part*100
      two_dig = int(real_part)
      real_part = real_part - two_dig
      rem = rem*100 + two_dig                     ! Add next 2 digits in the dividend
      new_quot_dig = new_divi(divi,rem)               ! Find new digit for quotient
      quot = quot*10.0D0 + new_quot_dig               ! Update quotient
      rem = rem - ((divi+new_quot_dig)*new_quot_dig)  ! Perform division
      divi = (divi + 2*new_quot_dig)*10               ! Update dividend
    END DO

    DO i=n_dig_pairs+1, 14
      quot = quot * 0.1D0
    END DO

    SQR = quot
  END FUNCTION SQR

  INTEGER FUNCTION new_divi(divisor,dividend)
    IMPLICIT NONE
    INTEGER(INT_QD), INTENT(IN) :: divisor, dividend
    INTEGER :: low, high, mid, best_dig
    INTEGER(INT_QD) :: temp

    low = 0
    high = 9
    DO WHILE (low <= high)
      mid = (high + low)/2
      temp = (divisor + mid)*mid
      IF (temp == dividend) THEN
        best_dig = mid
        EXIT
      ELSE IF (temp < dividend) THEN
        best_dig = mid
        low = mid + 1
      ELSE
        high = mid - 1
      END IF
    END DO
    new_divi = best_dig
  END FUNCTION new_divi
END PROGRAM sqrt_test
