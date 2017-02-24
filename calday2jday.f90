!***********************************************************************************************************************************
!
! Program modified by Robert Green from GREG2DOY -  David G. Simpson - NASA Goddard Space Flight Center
!
!  Language:     ANSI Standard Fortran-90
!
!  Version:      1.00b  (October 25, 2004)
!
!  Description:  This program converts a date on the Gregorian or Julian calendars to a day of year.
!
!  Note:         Array GREGORIAN_START defines the end dates of the Julian calendar and start dates of the Gregorian calendar.
!                Set the parameter GREGORIAN_CHOICE to indicate the desired start date of the Gregorian calendar, as listed in
!                array GREGORIAN_START.
!***********************************************************************************************************************************
!  Main program
!***********************************************************************************************************************************

      PROGRAM GREG2DOY

      IMPLICIT NONE

      TYPE :: DATE_TYPE
         INTEGER :: YEAR_J                                                          ! year of end of Julian calendar
         INTEGER :: MONTH_J                                                         ! month of end of Julian calendar
         INTEGER :: DAY_J                                                           ! day of end of Julian calendar
         INTEGER :: YEAR_G                                                          ! year of start of Gregorian calendar
         INTEGER :: MONTH_G                                                         ! month of start of Gregorian calendar
         INTEGER :: DAY_G                                                           ! day of start of Gregorian calendar
         INTEGER :: NDAYS                                                           ! number of days dropped from calendar at switch
      END TYPE DATE_TYPE

      INTEGER :: D                                                                  ! day of month (+ fraction)
      INTEGER :: DOY
      INTEGER :: K
      INTEGER :: M                                                                  ! month (1-12)
      INTEGER :: Y                                                                  ! year
      LOGICAL :: GREGORIAN_FLAG                                                     ! .TRUE. for Gregorian date, .FALSE. for Julian
      LOGICAL :: LEAP

      TYPE (DATE_TYPE), DIMENSION (3) :: GREGORIAN_START =   &
         (/ DATE_TYPE (1582, 10,  4, 1582, 10, 15, 10),      &                      ! 1: Decree by Pope Gregory XIII
            DATE_TYPE (1752,  9,  2, 1752,  9, 14, 11),      &                      ! 2: Great Britain
            DATE_TYPE (1918,  1, 31, 1918,  2, 14, 13)  /)                          ! 3: Russia

      INTEGER, PARAMETER :: GREGORIAN_CHOICE = 1                                    ! set to 1 for 1582 date, 2 for 1752 date, etc.

      LOGICAL :: GREGORIAN



!-----------------------------------------------------------------------------------------------------------------------------------
!  Main program code
!-----------------------------------------------------------------------------------------------------------------------------------

! Read in the calendar date from standard input
      character(len=5) :: Yarg 
      character(len=5) :: Marg
      character(len=5) :: Darg
      integer :: num_args
      num_args = command_argument_count()
      if (.NOT. num_args .EQ. 3) then
         write(*,*) 'calday2jday expects 3 arguments: year month day'
         call exit(1)
      endif
      call get_command_argument(1,Yarg)
      call get_command_argument(2,Marg)
      call get_command_argument(3,Darg)
      read(Yarg,'(I10)') Y
      read(Marg,'(I10)') M
      read(Darg,'(I10)') D

!      WRITE (UNIT=*, FMT='(A)', ADVANCE='NO') ' Enter month (1-12):  '              ! prompt for month
!      READ (*,*) Y, M, D
!      READ (UNIT=*, FMT=*) M

!      WRITE (UNIT=*, FMT='(A)', ADVANCE='NO') ' Enter day:  '                       ! prompt for day of month
!      READ (UNIT=*, FMT=*) D

!      WRITE (UNIT=*, FMT='(A)', ADVANCE='NO') ' Enter year:  '                      ! prompt for year
!      READ (UNIT=*, FMT=*) Y

      GREGORIAN_FLAG = GREGORIAN(Y, M, INT(D), GREGORIAN_START(GREGORIAN_CHOICE))   ! test for Gregorian calendar

      LEAP = .FALSE.
      IF (MOD(Y,4) .EQ. 0) LEAP = .TRUE.

      IF (GREGORIAN_FLAG) THEN
         IF (MOD(Y,100) .EQ. 0) LEAP = .FALSE.
         IF (MOD(Y,400) .EQ. 0) LEAP = .TRUE.
      END IF

! Checking if there are to many months
      if (M .GT. 12) then
         write(*,*) "Invalid date"
         call exit(2)
      endif 

      IF (LEAP) THEN
         K = 1
      ELSE
         K = 2
      END IF

! Checking if the day of the month is possible
      if (M .eq. 1) then
          if (D .GT. 31) then
             write(*,*) "Invalid date"
             call exit(3)
          endif 
      else if (M .eq. 2) then
          if (LEAP) then
             if (D .GT. 29) then
                write(*,*) "Invalid date"
                call exit(3)
             endif 
          else 
             if (D .GT. 28) then
                write(*,*) "Invalid date"
                call exit(3)
             endif 
          endif
      else if (M .eq. 3) then
          if (D .GT. 31) then
             write(*,*) "Invalid date"
             call exit(3)
          endif 
      else if (M .eq. 4) then
          if (D .GT. 30) then
             write(*,*) "Invalid date"
             call exit(3)
          endif 
      else if (M .eq. 5) then
          if (D .GT. 31) then
             write(*,*) "Invalid date"
             call exit(3)
          endif 
      else if (M .eq. 6) then
          if (D .GT. 30) then
             write(*,*) "Invalid date"
             call exit(3)
          endif 
      else if (M .eq. 7) then
          if (D .GT. 31) then
             write(*,*) "Invalid date"
             call exit(3)
          endif 
      else if (M .eq. 8) then
          if (D .GT. 31) then
             write(*,*) "Invalid date"
             call exit(3)
          endif 
      else if (M .eq. 9) then
          if (D .GT. 30) then
             write(*,*) "Invalid date"
             call exit(3)
          endif 
      else if (M .eq. 10) then
          if (D .GT. 31) then
             write(*,*) "Invalid date"
             call exit(3)
          endif 
      else if (M .eq. 11) then
          if (D .GT. 30) then
             write(*,*) "Invalid date"
             call exit(3)
          endif 
      else if (M .eq. 12) then
          if (D .GT. 31) then
             write(*,*) "Invalid date"
             call exit(3)
          endif 
      endif

      DOY = ((275*M)/9) - K*((M+9)/12) + D - 30

! Checking if there are too many days in the year
      if (LEAP) then
          if (DOY .GT. 366) then
             write(*,*) "Invalid date"
             call exit(4)
          endif 
      else
          if (DOY .GT. 365) then
             write(*,*) "Invalid date"
             call exit(5)
          endif 
      endif


      IF (GREGORIAN_FLAG .AND. (Y .EQ. GREGORIAN_START(GREGORIAN_CHOICE)%YEAR_G)) THEN
         DOY = DOY - GREGORIAN_START(GREGORIAN_CHOICE)%NDAYS
      END IF

      IF (.NOT. GREGORIAN_FLAG) THEN                                                ! print msg if Julian calendar in effect
         WRITE (UNIT=*, FMT='(/,A)') ' Julian calendar.'
      END IF

      WRITE (UNIT=*, FMT='(I4,A,I3.3)') Y,'-',DOY                        ! print result

      END PROGRAM GREG2DOY






!***********************************************************************************************************************************
!  GREGORIAN
!
!  This function determines whether a given date is in the Gregorian calendar (return value of .TRUE.) or on the Julian calendar
!  (return value of .FALSE.).
!***********************************************************************************************************************************

      FUNCTION GREGORIAN (YEAR, MONTH, DAY, GREG_START) RESULT (GREG_FLAG)

      IMPLICIT NONE

      TYPE :: DATE_TYPE
         INTEGER :: YEAR_J                                                          ! year of end of Julian calendar
         INTEGER :: MONTH_J                                                         ! month of end of Julian calendar
         INTEGER :: DAY_J                                                           ! day of end of Julian calendar
         INTEGER :: YEAR_G                                                          ! year of start of Gregorian calendar
         INTEGER :: MONTH_G                                                         ! month of start of Gregorian calendar
         INTEGER :: DAY_G                                                           ! day of start of Gregorian calendar
         INTEGER :: NDAYS                                                           ! number of days dropped from calendar at switch
      END TYPE DATE_TYPE

      INTEGER, INTENT(IN) :: YEAR                                                   ! input year
      INTEGER, INTENT(IN) :: MONTH                                                  ! input month
      INTEGER, INTENT(IN) :: DAY                                                    ! input day of month
      TYPE (DATE_TYPE), INTENT(IN) :: GREG_START                                    ! contains Julian stop/Gregorian start dates

      LOGICAL :: GREG_FLAG                                                          ! result flag (.TRUE. for Gregorian)

      INTEGER :: CALTYPE = 0                                                        ! 0=unknown, 1=Julian, 2=Gregorian


      IF (YEAR .LT. GREG_START%YEAR_J) THEN                                         ! if year before end of Julian calendar..
         CALTYPE = 1                                                                ! ..then this is a Julian date
      ELSE IF (YEAR .EQ. GREG_START%YEAR_J) THEN                                    ! if this is the last year of the Julian cal..
         IF (MONTH .LT. GREG_START%MONTH_J) THEN                                    ! ..then if this is before the ending month..
            CALTYPE = 1                                                             ! ..then this is a Julian date
         ELSE IF (MONTH .EQ. GREG_START%MONTH_J) THEN                               ! if this is the ending month..
            IF (DAY .LE. GREG_START%DAY_J) THEN                                     ! ..then if this is before/at the ending date..
               CALTYPE = 1                                                          ! ..then this is a Julian date
            END IF
         END IF
      END IF

      IF (YEAR .GT. GREG_START%YEAR_G) THEN                                         ! if year after start of Gregorian calendar..
         CALTYPE = 2                                                                ! ..then this is a Gregorian date
      ELSE IF (YEAR .EQ. GREG_START%YEAR_G) THEN                                    ! if this is the first year of the Greg. cal..
         IF (MONTH .GT. GREG_START%MONTH_G) THEN                                    ! ..then if this is after the starting month..
            CALTYPE = 2                                                             ! ..then this is a Gregorian date
         ELSE IF (MONTH .EQ. GREG_START%MONTH_G) THEN                               ! if this is the starting month..
            IF (DAY .GE. GREG_START%DAY_G) THEN                                     ! ..then if this is at/after the starting date..
               CALTYPE = 2                                                          ! ..then this is a Gregorian date
            END IF
         END IF
      END IF

      SELECT CASE (CALTYPE)                                                         ! check calendar type
         CASE (0)                                                                   ! if unknown, we have an invalid date
            WRITE (UNIT=*, FMT='(A)') ' No such date.'                              ! print error message
            STOP                                                                    ! stop program
         CASE (1)                                                                   ! if Julian date..
            GREG_FLAG = .FALSE.                                                     ! ..set return value to .false.
         CASE (2)                                                                   ! if Gregorian date..
            GREG_FLAG = .TRUE.                                                      ! ..set return value to .true.
      END SELECT

      END FUNCTION GREGORIAN
