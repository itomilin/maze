! 2018 Find all ways in maze.
program lab_4
    use Environment
    use io
    use OMP_LIB                                          ! Include OMP library.

    implicit none
    character(:), allocatable :: input_file
    character(:), allocatable :: output_file

    integer, parameter :: CODE                   = 6
    integer, parameter :: WALL                   = 9608  ! █
    integer, parameter :: ROAD_LINE_HORIZONTAL   = 9552  ! ═
    integer, parameter :: ROAD_LINE_VERTICAL     = 9553  ! ║
    integer, parameter :: ROAD_ANGLE_RIGHT_ABOVE = 9559  ! ╗
    integer, parameter :: ROAD_ANGLE_RIGHT_BELOW = 9565  ! ╝
    integer, parameter :: ROAD_ANGLE_LEFT_ABOVE  = 9556  ! ╔
    integer, parameter :: ROAD_ANGLE_LEFT_BELOW  = 9562  ! ╚
    integer, parameter :: PATH_S                 = 9633  ! □
    integer, parameter :: FINISH                 = 9711
    integer, parameter :: SPACE                  = 32

     ! Left number is size of maze, without borders.
    integer, parameter :: SIZE_LABIRINT          = 5 + 2
    integer(16)        :: c                      = 0
    integer            :: x                      = 2
    integer            :: y                      = 2

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!ARRAYS_OMP!!!!!!!!!!!!!!!!!!!!!!!!
    integer(16)            :: i = 1
!    integer(16), parameter :: N = 100000000
    integer                :: number_threads = 0
!    integer                :: thread_num = 0
    integer                :: sum_c = 0
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!ARRAYS_OMP!!!!!!!!!!!!!!!!!!!!!!!!
    character(kind=CH_) :: Lab(SIZE_LABIRINT, SIZE_LABIRINT)

    type Path
        character(kind=CH_), allocatable :: CopyLab(:,:)
        type(Path), pointer              :: next => Null()
    end type Path

    type(Path), pointer :: head => Null()

    type container
        character(kind=CH_) :: Lab(SIZE_LABIRINT, SIZE_LABIRINT)
        integer             :: X_coordinate = 0
        integer             :: Y_coordinate = 0
    end type container

    type(container), allocatable :: Labirints(:)

    ! Intel Core i7-5650U 2C/4T
    ! Get own cores. Divisuon by 2, exclude HT tech.

    !$ number_threads = omp_get_num_procs() / 2

    allocate(Labirints(number_threads))
    print *, "Count of cores: ", number_threads

    input_file  = "./data/maze5x5.txt"
    output_file = "./bin/output.txt"
    c = 0
    sum_c = 0

    if (number_threads == 1) then
        Call DrawLabirint(input_file)
        Labirints(1)%Lab          = Lab
        Labirints(1)%X_coordinate = 2
        Labirints(1)%Y_coordinate = 2
    else if(number_threads == 2) then
        Call DrawLabirint(input_file)
        Lab(2, 2:3) = Draw("LEFT", "RIGHT")
        Labirints(1)%Lab          = Lab
        Labirints(1)%X_coordinate = 2
        Labirints(1)%Y_coordinate = 3

        Call DrawLabirint(input_file)
        Lab(2:3, 2) = Draw("UP", "DOWN")
        Labirints(2)%Lab          = Lab
        Labirints(2)%X_coordinate = 3
        Labirints(2)%Y_coordinate = 2
    else if(number_threads == 4) then
        Call DrawLabirint(input_file)
        Lab(2, 2:4) = Draw("LEFT", "RIGHT")
        Labirints(1)%Lab          = Lab
        Labirints(1)%X_coordinate = 2
        Labirints(1)%Y_coordinate = 4

        Call DrawLabirint(input_file)
        Lab(2:4, 2) = Draw("UP", "DOWN")
        Labirints(2)%Lab          = Lab
        Labirints(2)%X_coordinate = 4
        Labirints(2)%Y_coordinate = 2

        Call DrawLabirint(input_file)
        Lab(2, 2:3) = Draw("LEFT", "RIGHT")
        Lab(3, 3) = Draw("UP", "DOWN")
        Labirints(3)%Lab          = Lab
        Labirints(3)%X_coordinate = 3
        Labirints(3)%Y_coordinate = 3

        Call DrawLabirint(input_file)
        Lab(2:3, 2) = Draw("UP", "DOWN")
        Lab(3, 3) = Draw("LEFT", "RIGHT")
        Labirints(4)%Lab          = Lab
        Labirints(4)%X_coordinate = 3
        Labirints(4)%Y_coordinate = 3
    end if

    !$OMP PARALLEL DO firstprivate(c)
    do i = 1, number_threads
        Call Labirint(Labirints(i)%Lab, "UP", Labirints(i)%X_coordinate, Labirints(i)%Y_coordinate, c)
        sum_c = sum_c + c
    end do
    !$OMP END PARALLEL DO

    print *, "Ways: ", sum_c

    contains
    recursive subroutine Labirint(Lab, from, x, y, c)
        character(kind=CH_), intent(inout) :: Lab(:, :)
        character(*),        intent(in)    :: from
        integer,             intent(in)    :: x
        integer,             intent(in)    :: y
        integer(16),         intent(inout) :: c

        if (Lab(x, y) == Char(FINISH, CH_)) then
           c = c + 1
        else
           if (Lab(x, y + 1) == Char(SPACE, CH_) .or. Lab(x, y + 1) == Char(FINISH, CH_)) then ! Left.
              Lab(x, y) = Draw(from, "RIGHT")
              call Labirint(Lab, "LEFT", x, y + 1, c)
           end if

           if (Lab(x + 1, y) == Char(SPACE, CH_) .or. Lab(x + 1, y) == Char(FINISH, CH_)) then ! Down.
              Lab(x, y) = Draw(from, "DOWN")
              call Labirint(Lab, "UP", x + 1, y, c)
           end if

           if (Lab(x - 1, y) == Char(SPACE, CH_) .or. Lab(x - 1, y) == Char(FINISH, CH_)) then ! Up.
              Lab(x, y) = Draw(from, "UP")
              call Labirint(Lab, "DOWN", x - 1, y, c)
           end if

           if (Lab(x, y - 1) == Char(SPACE, CH_) .or. Lab(x, y - 1) == Char(FINISH, CH_)) then ! Left.
              Lab(x, y) = Draw(from, "LEFT")
              call Labirint(Lab, "RIGHT", x, y - 1, c)
           end if
           Lab(x, y) = Char(SPACE, CH_)
        end if
    end subroutine Labirint

    function Draw(from, to) result(line)
        character(*), intent(in) :: from
        character(*), intent(in) :: to
        character(kind=CH_)      :: line

        if ( from == "LEFT"       .and. to == "RIGHT" ) then
           line = Char(ROAD_LINE_HORIZONTAL, CH_)
        else if ( from == "LEFT"  .and. to == "UP"    ) then
           line = Char(ROAD_ANGLE_RIGHT_BELOW, CH_)
        else if ( from == "LEFT"  .and. to == "DOWN"  ) then
           line = Char(ROAD_ANGLE_RIGHT_ABOVE, CH_)
        else if ( from == "UP"    .and. to == "DOWN"  ) then
           line = Char(ROAD_LINE_VERTICAL, CH_)
        else if ( from == "UP"    .and. to == "RIGHT" ) then
           line = Char(ROAD_ANGLE_LEFT_BELOW, CH_)
        else if ( from == "UP"    .and. to == "LEFT"  ) then
           line = Char(ROAD_ANGLE_RIGHT_BELOW, CH_)
        else if ( from == "DOWN"  .and. to == "UP"    ) then
           line = Char(ROAD_LINE_VERTICAL, CH_)
        else if ( from == "DOWN"  .and. to == "LEFT"  ) then
           line = Char(ROAD_ANGLE_RIGHT_ABOVE, CH_)
        else if ( from == "DOWN"  .and. to == "RIGHT" ) then
           line = Char(ROAD_ANGLE_LEFT_ABOVE, CH_)
        else if ( from == "RIGHT" .and. to == "DOWN"  ) then
           line = Char(ROAD_ANGLE_LEFT_ABOVE, CH_)
        else if ( from == "RIGHT" .and. to == "LEFT"  ) then
           line = Char(ROAD_LINE_HORIZONTAL, CH_)
        else if ( from == "RIGHT" .and. to == "UP"    ) then
           line = Char(ROAD_ANGLE_LEFT_BELOW, CH_)
        end if
    end function Draw

!    recursive subroutine Add(current, Lab)
!        type(Path), pointer, intent(inout) :: current
!        character(kind=CH_), intent(inout) :: Lab(:, :)
!
!        if (.not. associated(current)) then
!           allocate(current)
!           current%CopyLab = Lab
!        else
!           call Add(current%next, Lab)
!        end if
!    end subroutine Add

!    recursive subroutine OutputVarsOfPath(current, listName, pos, num)
!        type(Path), pointer, intent(inout) :: current
!        character(*), intent(in)           :: listName
!        character(*), intent(in)           :: pos
!        character(:), allocatable          :: format
!        integer                            :: in = 0, out = 0
!        integer, intent(in)                :: num
!        integer                            :: i
!
!        if (associated(current)) then
!           format = "("//SIZE_LABIRINT//"A1)"
!           !write (*, format) (current%CopyLab(i, :), i = 1, SIZE_LABIRINT)
!           open (file = output_file, position = pos, encoding = E_,  newunit = out)
!              write(out, "(1A)") listName//num
!              write (out, format) (current%CopyLab(i, :), i = 1, SIZE_LABIRINT)
!           close (out)
!           call OutputVarsOfPath(current%next, "Var № ", "append", num + 1)
!        end if
!    end subroutine OutputVarsOfPath

    subroutine DrawLabirint(input_file)
        character(*), intent(in)  :: input_file
        integer                   :: i
        character(:), allocatable :: format
        integer                   :: in = 0, out = 0

        open (file = input_file, encoding=E_, newunit = in)
           !read (in, *) !
           format = "("//SIZE_LABIRINT//"A1)"
           read (in, format) (Lab(i, :), i = 1, SIZE_LABIRINT)
        close (in)

!        open (CODE, encoding=E_) ! для правильной отрисовки символов на консоле
!           write (*, format) (Lab(i, :), i = 1, SIZE_LABIRINT)
!        close (CODE)
    end subroutine DrawLabirint
end program lab_4

