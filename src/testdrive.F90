! This file is part of test-drive.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!# Enable support for quadruple precision
#ifndef WITH_QP
#define WITH_QP 0
#endif

!# Enable support for extended double precision
#ifndef WITH_XDP
#define WITH_XDP 0
#endif

!> Provides a light-weight procedural testing framework for Fortran projects.
!>
!> Testsuites are defined by a [[collect_interface]] returning a set of
!> [[unittest_type]] objects. To create a new test use the [[new_unittest]]
!> constructor, which requires a test identifier and a procedure with a
!> [[test_interface]] compatible signature. The error status is communicated
!> by the allocation status of an [[error_type]].
!>
!> The necessary boilerplate code to setup the test entry point is just
!>
!>```fortran
!>program tester
!>  use, intrinsic :: iso_fortran_env, only : error_unit
!>  use testdrive, only : run_testsuite, new_testsuite, testsuite_type
!>  use test_suite1, only : collect_suite1
!>  use test_suite2, only : collect_suite2
!>  implicit none
!>  integer :: stat, is
!>  type(testsuite_type), allocatable :: testsuites(:)
!>  character(len=*), parameter :: fmt = '("#", *(1x, a))'
!>
!>  stat = 0
!>
!>  testsuites = [ &
!>    new_testsuite("suite1", collect_suite1), &
!>    new_testsuite("suite2", collect_suite2) &
!>    ]
!>
!>  do is = 1, size(testsuites)
!>    write(error_unit, fmt) "Testing:", testsuites(is)%name
!>    call run_testsuite(testsuites(is)%collect, error_unit, stat)
!>  end do
!>
!>  if (stat > 0) then
!>    write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
!>    error stop
!>  end if
!>
!>end program tester
!>```
!>
!> Every test is defined in a separate module using a ``collect`` function, which
!> is exported and added to the ``testsuites`` array in the test runner.
!> All test have a simple interface with just an allocatable [[error_type]] as
!> output to provide the test results.
!>
!>```fortran
!>module test_suite1
!>  use testdrive, only : new_unittest, unittest_type, error_type, check
!>  implicit none
!>  private
!>
!>  public :: collect_suite1
!>
!>contains
!>
!>!> Collect all exported unit tests
!>subroutine collect_suite1(testsuite)
!>  !> Collection of tests
!>  type(unittest_type), allocatable, intent(out) :: testsuite(:)
!>
!>  testsuite = [ &
!>    new_unittest("valid", test_valid), &
!>    new_unittest("invalid", test_invalid, should_fail=.true.) &
!>    ]
!>
!>end subroutine collect_suite1
!>
!>subroutine test_valid(error)
!>  type(error_type), allocatable, intent(out) :: error
!>  ! ...
!>end subroutine test_valid
!>
!>subroutine test_invalid(error)
!>  type(error_type), allocatable, intent(out) :: error
!>  ! ...
!>end subroutine test_invalid
!>
!>end module test_suite1
!>```
!>
!> For an example setup checkout the ``test/`` directory in this project.
module testdrive
  use, intrinsic :: iso_fortran_env, only : error_unit
  implicit none
  private

  public :: run_testsuite, run_selected, new_unittest, new_testsuite
  public :: select_test, select_suite
  public :: unittest_type, testsuite_type, error_type
  public :: check, test_failed, skip_test
  public :: test_interface, collect_interface
  public :: get_argument, get_variable, to_string
  public :: junit_output, junit_header
  public :: init_color_output


  !> Single precision real numbers
  integer, parameter :: sp = selected_real_kind(6)

  !> Double precision real numbers
  integer, parameter :: dp = selected_real_kind(15)

#if WITH_XDP
  !> Extended double precision real numbers
  integer, parameter :: xdp = selected_real_kind(18)
#endif

#if WITH_QP
  !> Quadruple precision real numbers
  integer, parameter :: qp = selected_real_kind(33)
#endif

  !> Char length for integers
  integer, parameter :: i1 = selected_int_kind(2)

  !> Short length for integers
  integer, parameter :: i2 = selected_int_kind(4)

  !> Length of default integers
  integer, parameter :: i4 = selected_int_kind(9)

  !> Long length for integers
  integer, parameter :: i8 = selected_int_kind(18)

  !> Error code for success
  integer, parameter :: success = 0

  !> Error code for failure
  integer, parameter :: fatal = 1

  !> Error code for skipped test
  integer, parameter :: skipped = 77


  !> Error message
  type :: error_type

    !> Error code
    integer :: stat = success

    !> Payload of the error
    character(len=:), allocatable :: message

  contains

    !> Escalate uncaught errors
    final :: escalate_error

  end type error_type


  interface check
    module procedure :: check_stat
    module procedure :: check_logical
    module procedure :: check_float_sp
    module procedure :: check_float_dp
#if WITH_XDP
    module procedure :: check_float_xdp
#endif
#if WITH_QP
    module procedure :: check_float_qp
#endif
    module procedure :: check_float_exceptional_sp
    module procedure :: check_float_exceptional_dp
#if WITH_XDP
    module procedure :: check_float_exceptional_xdp
#endif
#if WITH_QP
    module procedure :: check_float_exceptional_qp
#endif
    module procedure :: check_complex_sp
    module procedure :: check_complex_dp
#if WITH_XDP
    module procedure :: check_complex_xdp
#endif
#if WITH_QP
    module procedure :: check_complex_qp
#endif
    module procedure :: check_complex_exceptional_sp
    module procedure :: check_complex_exceptional_dp
#if WITH_XDP
    module procedure :: check_complex_exceptional_xdp
#endif
#if WITH_QP
    module procedure :: check_complex_exceptional_qp
#endif
    module procedure :: check_int_i1
    module procedure :: check_int_i2
    module procedure :: check_int_i4
    module procedure :: check_int_i8
    module procedure :: check_bool
    module procedure :: check_string
  end interface check


  interface to_string
    module procedure :: integer_i1_to_string
    module procedure :: integer_i2_to_string
    module procedure :: integer_i4_to_string
    module procedure :: integer_i8_to_string
    module procedure :: real_sp_to_string
    module procedure :: real_dp_to_string
#if WITH_XDP
    module procedure :: real_xdp_to_string
#endif
#if WITH_QP
    module procedure :: real_qp_to_string
#endif
    module procedure :: complex_sp_to_string
    module procedure :: complex_dp_to_string
#if WITH_XDP
    module procedure :: complex_xdp_to_string
#endif
#if WITH_QP
    module procedure :: complex_qp_to_string
#endif
  end interface to_string


  !> Implementation of check for not a number value, in case a compiler does not
  !> provide the IEEE intrinsic ``ieee_is_nan`` (currently this is Intel oneAPI on MacOS)
  interface is_nan
    module procedure :: is_nan_sp
    module procedure :: is_nan_dp
#if WITH_XDP
    module procedure :: is_nan_xdp
#endif
#if WITH_QP
    module procedure :: is_nan_qp
#endif
  end interface is_nan


  abstract interface
    !> Entry point for tests
    subroutine test_interface(error)
      import :: error_type

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

    end subroutine test_interface
  end interface


  !> Declaration of a unit test
  type :: unittest_type

    !> Name of the test
    character(len=:), allocatable :: name

    !> Entry point of the test
    procedure(test_interface), pointer, nopass :: test => null()

    !> Whether test is supposed to fail
    logical :: should_fail = .false.

  end type unittest_type


  abstract interface
    !> Collect all tests
    subroutine collect_interface(testsuite)
      import :: unittest_type

      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

    end subroutine collect_interface
  end interface


  !> Collection of unit tests
  type :: testsuite_type

    !> Name of the testsuite
    character(len=:), allocatable :: name

    !> Entry point of the test
    procedure(collect_interface), pointer, nopass :: collect => null()

  end type testsuite_type


  !> Output JUnit.xml for discovering unit tests by other tools
  type :: junit_output
    !> XML output string (initial block)
    character(len=:), allocatable :: xml_start
    !> XML output string (current block)
    character(len=:), allocatable :: xml_block
    !> XML output string (final block)
    character(len=:), allocatable :: xml_final
    !> Unique identifier
    integer :: uid = 0
    !> Timestamp
    character(len=19) :: timestamp = '1970-01-01T00:00:00'
    !> Hostname
    character(len=:), allocatable :: hostname
    !> Package name
    character(len=:), allocatable :: package
    !> Testsuite name
    character(len=:), allocatable :: testsuite
    !> Number of tests
    integer :: tests = 0
    !> Number of failures
    integer :: failures = 0
    !> Number of errors
    integer :: errors = 0
    !> Number of skipped tests
    integer :: skipped = 0
    !> Running time
    real(sp) :: time = 0.0_sp
  end type junit_output


  !> Container for terminal escape code
  type :: color_code
    !> Style descriptor
    integer(i1) :: style = -1_i1
    !> Background color descriptor
    integer(i1) :: bg = -1_i1
    !> Foreground color descriptor
    integer(i1) :: fg = -1_i1
  end type color_code

  interface operator(+)
    module procedure :: add_color
  end interface operator(+)

  interface operator(//)
    module procedure :: concat_color_left
    module procedure :: concat_color_right
  end interface operator(//)


  !> Colorizer class for handling colorful output in the terminal
  type, public :: color_output

    type(color_code) :: &
      reset = color_code(), &
      bold = color_code(), &
      dim = color_code(), &
      italic = color_code(), &
      underline = color_code(), &
      blink = color_code(), &
      reverse = color_code(), &
      hidden = color_code()

    type(color_code) :: &
      black = color_code(), &
      red = color_code(), &
      green = color_code(), &
      yellow = color_code(), &
      blue = color_code(), &
      magenta = color_code(), &
      cyan = color_code(), &
      white = color_code()

    type(color_code) :: &
      bg_black = color_code(), &
      bg_red = color_code(), &
      bg_green = color_code(), &
      bg_yellow = color_code(), &
      bg_blue = color_code(), &
      bg_magenta = color_code(), &
      bg_cyan = color_code(), &
      bg_white = color_code()
  end type color_output

  interface color_output
    module procedure :: new_color_output
  end interface color_output

  type(color_output), protected :: color

  character(len=*), parameter :: fmt = '(1x, *(1x, a))'
  character(len=*), parameter :: newline = new_line("a")


contains


  !> Driver for testsuite
  recursive subroutine run_testsuite(collect, unit, stat, parallel, junit)

    !> Collect tests
    procedure(collect_interface) :: collect

    !> Unit for IO
    integer, intent(in) :: unit

    !> Number of failed tests
    integer, intent(inout) :: stat

    !> Run the tests in parallel
    logical, intent(in), optional :: parallel

    !> Produce junit output
    type(junit_output), intent(inout), optional :: junit

    type(unittest_type), allocatable :: testsuite(:)
    integer :: it
    logical :: parallel_

    parallel_ = .true.
    if(present(parallel)) parallel_ = parallel

    call collect(testsuite)

    call junit_push_suite(junit, "testdrive")

    !$omp parallel do schedule(dynamic) shared(testsuite, unit) reduction(+:stat) &
    !$omp if (parallel_)
    do it = 1, size(testsuite)
      !$omp critical(testdrive_testsuite)
      write(unit, '(1x, 4(1x, a))') &
        & "Starting", (color%blue)//testsuite(it)%name//color%reset, &
        & color%dim//"..."//color%reset, &
        & color%bold//"(" // color%cyan//to_string(it)//color%bold // &
        & "/" // color%cyan//to_string(size(testsuite))//color%bold // ")"//color%reset
      !$omp end critical(testdrive_testsuite)
      call run_unittest(testsuite(it), unit, stat, junit)
    end do

    call junit_pop_suite(junit)

  end subroutine run_testsuite


  !> Driver for selective testing
  recursive subroutine run_selected(collect, name, unit, stat, junit)

    !> Collect tests
    procedure(collect_interface) :: collect

    !> Name of the selected test
    character(len=*), intent(in) :: name

    !> Unit for IO
    integer, intent(in) :: unit

    !> Number of failed tests
    integer, intent(inout) :: stat

    !> Produce junit output
    type(junit_output), intent(inout), optional :: junit

    type(unittest_type), allocatable :: testsuite(:)
    integer :: it

    call collect(testsuite)

    call junit_push_suite(junit, "testdrive")

    it = select_test(testsuite, name)

    if (it > 0 .and. it <= size(testsuite)) then
      call run_unittest(testsuite(it), unit, stat, junit)
    else
      write(unit, fmt) "Available tests:"
      do it = 1, size(testsuite)
        write(unit, fmt) "-", testsuite(it)%name
      end do
      stat = -huge(it)
    end if

    call junit_pop_suite(junit)

  end subroutine run_selected


  !> Run a selected unit test
  recursive subroutine run_unittest(test, unit, stat, junit)

    !> Unit test
    type(unittest_type), intent(in) :: test

    !> Unit for IO
    integer, intent(in) :: unit

    !> Number of failed tests
    integer, intent(inout) :: stat

    !> Produce junit output
    type(junit_output), intent(inout), optional :: junit

    type(error_type), allocatable :: error
    character(len=:), allocatable :: message

    call test%test(error)
    if (.not.test_skipped(error)) then
      if (allocated(error) .neqv. test%should_fail) stat = stat + 1
    end if
    call junit_push_test(junit, test, error, 0.0_sp)
    call make_output(message, test, error)
    !$omp critical(testdrive_testsuite)
    write(unit, '(a)') message
    !$omp end critical(testdrive_testsuite)
    if (allocated(error)) then
      call clear_error(error)
    end if

  end subroutine run_unittest


  pure function test_skipped(error) result(is_skipped)

    !> Error handling
    type(error_type), intent(in), optional :: error

    !> Test was skipped
    logical :: is_skipped

    is_skipped = .false.
    if (present(error)) then
      is_skipped = error%stat == skipped
    end if

  end function test_skipped


  !> Create output message for test (this procedure is pure and therefore cannot launch tests)
  pure subroutine make_output(output, test, error)

    !> Output message for display
    character(len=:), allocatable, intent(out) :: output

    !> Unit test
    type(unittest_type), intent(in) :: test

    !> Error handling
    type(error_type), intent(in), optional :: error

    character(len=:), allocatable :: label
    type(color_code) :: label_color

    if (test_skipped(error)) then
      label_color = color%yellow + color%bold
      label = "SKIPPED"
    else if (present(error) .neqv. test%should_fail) then
      if (test%should_fail) then
        label_color = color%magenta + color%bold
        label = "UNEXPECTED PASS"
      else
        label_color = color%red + color%bold
        label = "FAILED"
      end if
    else
      if (test%should_fail) then
        label_color = color%cyan + color%bold
        label = "EXPECTED FAIL"
      else
        label_color = color%green + color%bold
        label = "PASSED"
      end if
    end if
    output = "       " // color%dim//"..."//color%reset // " " // &
      & color%blue//test%name//color%reset // &
      & " "//color%bold//"["//label_color//label//color%bold//"]"//color%reset
    if (present(error)) then
      output = output // newline // "  "//color%bold//"Message:"//color%reset//" " // error%message
    end if
  end subroutine make_output


  !> Initialize output for JUnit.xml
  pure subroutine junit_header(junit, package)

    !> JUnit output
    type(junit_output), intent(inout), optional :: junit

    !> Package name
    character(len=*), intent(in) :: package

    if (.not.present(junit)) return

    junit%xml_start = &
      & '<?xml version="1.0" encoding="UTF-8"?>' // newline // &
      & '<testsuites' // newline // &
      & ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' // newline // & 
      & ' xsi:noNamespaceSchemaLocation="JUnit.xsd"' // newline // &
      & '>' // newline
    junit%xml_block = ''
    junit%xml_final = &
      & '</testsuites>'

    junit%hostname = 'localhost'
    junit%package = package

  end subroutine junit_header

  !> Register a test suite in JUnit.xml
  subroutine junit_push_suite(junit, name)

    !> JUnit output
    type(junit_output), intent(inout), optional :: junit

    !> Name of the test suite
    character(len=*), intent(in) :: name

    if (.not.present(junit)) return

    junit%timestamp = get_timestamp()
    junit%testsuite = name
    junit%uid = junit%uid + 1

  end subroutine junit_push_suite

  !> Finalize a test suite in JUnit.xml
  subroutine junit_pop_suite(junit)

    !> JUnit output
    type(junit_output), intent(inout), optional :: junit

    if (.not.present(junit)) return

    junit%xml_start = &
      & junit%xml_start // &
      & '  <testsuite' // newline // &
      & '   name="'//junit%testsuite//'"' // newline // &
      & '   package="'//junit%package//'"' // newline // &
      & '   id="'//to_string(junit%uid)//'"' // newline // &
      & '   timestamp="'//junit%timestamp//'"' // newline // &
      & '   hostname="'//junit%hostname//'"' // newline // &
      & '   tests="'//to_string(junit%tests)//'"' // newline // &
      & '   failures="'//to_string(junit%failures)//'"' // newline // &
      & '   errors="'//to_string(junit%errors)//'"' // newline // &
      & '   skipped="'//to_string(junit%skipped)//'"' // newline // &
      & '   time="'//to_string(junit%time)//'"' // newline // &
      & '  >' // newline // &
      & '  <properties>' // newline // &
      & '  </properties>' // newline // &
      & junit%xml_block // newline // &
      & '  </testsuite>' // newline

    junit%xml_block = ''
    junit%tests = 0
    junit%failures = 0
    junit%errors = 0
    junit%skipped = 0
    junit%time = 0.0_sp

    call junit_write(junit)

  end subroutine junit_pop_suite

  !> Register a new unit test
  subroutine junit_push_test(junit, test, error, time)

    !> JUnit output
    type(junit_output), intent(inout), optional :: junit

    !> Unit test
    type(unittest_type), intent(in) :: test

    !> Error handling
    type(error_type), intent(in), optional :: error

    !> Running time
    real(sp), intent(in) :: time

    if (.not.present(junit)) return

    !$omp critical(testdrive_junit)
    junit%tests = junit%tests + 1
    junit%time = junit%time + time

    junit%xml_block = &
      & junit%xml_block // &
      & '   <testcase' // newline // &
      & '    name="'//test%name//'"' // newline // &
      & '    classname="'//junit%testsuite//'"' // newline // &
      & '    time="'//to_string(time)//'"' // newline // &
      & '   >' // newline

    if (test_skipped(error)) then
      junit%xml_block = &
        & junit%xml_block // &
        & '    <skipped/>' // newline
      junit%skipped = junit%skipped + 1
    elseif (present(error)) then
      if (test%should_fail) then
        junit%xml_block = &
          & junit%xml_block // &
          & '    <system-out>' // newline // &
          & '     "'//error%message//'"' // newline // &
          & '    </system-out>' // newline
      else
        junit%xml_block = &
          & junit%xml_block // &
          & '    <failure' // newline // &
          & '     message="'//error%message//'"' // newline // &
          & '     type="AssertionError"' // newline // &
          & '    />' // newline
        junit%failures = junit%failures + 1
      end if
    else
      if (test%should_fail) then
        junit%xml_block = &
          & junit%xml_block // &
          & '    <failure' // newline // &
          & '     message="Unexpected pass"' // newline // &
          & '     type="AssertionError"' // newline // &
          & '    />' // newline
        junit%failures = junit%failures + 1
      else
        junit%xml_block = &
          & junit%xml_block // &
          & '    <system-out>' // newline // &
          & '     "Test passed successfully"' // newline // &
          & '    </system-out>' // newline
      end if
    end if

    junit%xml_block = &
      & junit%xml_block // &
      & '   </testcase>' // newline   
    !$omp end critical(testdrive_junit)

  end subroutine junit_push_test


  !> Write results to JUnit.xml
  subroutine junit_write(junit)

    !> JUnit output
    type(junit_output), intent(inout), optional :: junit

    integer :: io

    if (.not.present(junit)) return
    open( &
      & newunit=io, &
      & file='JUnit'//junit%package//'.xml', &
      & status='replace', &
      & action='write')
    write(io, '(a)') junit%xml_start // junit%xml_final
    close(io)

  end subroutine junit_write


  !> Create ISO 8601 formatted timestamp
  function get_timestamp() result(timestamp)

    !> ISO 8601 formatted timestamp
    character(len=19) :: timestamp

    character(len=8) :: date
    character(len=10) :: time

    call date_and_time(date=date, time=time)

    timestamp = date(1:4) // "-" // date(5:6) // "-" // date(7:8) // "T" // &
      & time(1:2) // ":" // time(3:4) // ":" // time(5:6)

  end function get_timestamp


  !> Select a unit test from all available tests
  function select_test(tests, name) result(pos)

    !> Name identifying the test suite
    character(len=*), intent(in) :: name

    !> Available unit tests
    type(unittest_type) :: tests(:)

    !> Selected test suite
    integer :: pos

    integer :: it

    pos = 0
    do it = 1, size(tests)
      if (name == tests(it)%name) then
        pos = it
        exit
      end if
    end do

  end function select_test


  !> Select a test suite from all available suites
  function select_suite(suites, name) result(pos)

    !> Name identifying the test suite
    character(len=*), intent(in) :: name

    !> Available test suites
    type(testsuite_type) :: suites(:)

    !> Selected test suite
    integer :: pos

    integer :: it

    pos = 0
    do it = 1, size(suites)
      if (name == suites(it)%name) then
        pos = it
        exit
      end if
    end do

  end function select_suite


  !> Register a new unit test
  function new_unittest(name, test, should_fail) result(self)

    !> Name of the test
    character(len=*), intent(in) :: name

    !> Entry point for the test
    procedure(test_interface) :: test

    !> Whether test is supposed to error or not
    logical, intent(in), optional :: should_fail

    !> Newly registered test
    type(unittest_type) :: self

    self%name = name
    self%test => test
    if (present(should_fail)) self%should_fail = should_fail

  end function new_unittest


  !> Register a new testsuite
  function new_testsuite(name, collect) result(self)

    !> Name of the testsuite
    character(len=*), intent(in) :: name

    !> Entry point to collect tests
    procedure(collect_interface) :: collect

    !> Newly registered testsuite
    type(testsuite_type) :: self

    self%name = name
    self%collect => collect

  end function new_testsuite


  subroutine check_stat(error, stat, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Status of operation
    integer, intent(in) :: stat

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (stat /= success) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, "Non-zero exit code encountered", more)
      end if
    end if

  end subroutine check_stat


  subroutine check_logical(error, expression, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Result of logical operator
    logical, intent(in) :: expression

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (.not.expression) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, "Condition not fullfilled", more)
      end if
    end if

  end subroutine check_logical


  subroutine check_float_dp(error, actual, expected, message, more, thr, rel)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    real(dp), intent(in) :: actual

    !> Expected floating point value
    real(dp), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    !> Allowed threshold for matching floating point values
    real(dp), intent(in), optional :: thr

    !> Check for relative errors instead
    logical, intent(in), optional :: rel

    logical :: relative
    real(dp) :: diff, threshold

    call check(error, actual, message, more)
    if (allocated(error)) return

    if (present(thr)) then
      threshold = thr
    else
      threshold = epsilon(expected)
    end if

    if (present(rel)) then
      relative = rel
    else
      relative = .false.
    end if

    if (relative) then
      diff = abs(actual - expected) / abs(expected)
    else
      diff = abs(actual - expected)
    end if

    if (diff > threshold) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        if (relative) then
          call test_failed(error, &
            "Floating point value mismatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(int(diff*100))//"%)", &
            more)
        else
          call test_failed(error, &
            "Floating point value mismatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(diff)//")", &
            more)
        end if
      end if
    end if

  end subroutine check_float_dp


  subroutine check_float_exceptional_dp(error, actual, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    real(dp), intent(in) :: actual

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (is_nan(actual)) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, "Exceptional value 'not a number' found", more)
      end if
    end if

  end subroutine check_float_exceptional_dp


  subroutine check_float_sp(error, actual, expected, message, more, thr, rel)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    real(sp), intent(in) :: actual

    !> Expected floating point value
    real(sp), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    !> Allowed threshold for matching floating point values
    real(sp), intent(in), optional :: thr

    !> Check for relative errors instead
    logical, intent(in), optional :: rel

    logical :: relative
    real(sp) :: diff, threshold

    call check(error, actual, message, more)
    if (allocated(error)) return

    if (present(thr)) then
      threshold = thr
    else
      threshold = epsilon(expected)
    end if

    if (present(rel)) then
      relative = rel
    else
      relative = .false.
    end if

    if (relative) then
      diff = abs(actual - expected) / abs(expected)
    else
      diff = abs(actual - expected)
    end if

    if (diff > threshold) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        if (relative) then
          call test_failed(error, &
            "Floating point value mismatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(int(diff*100))//"%)", &
            more)
        else
          call test_failed(error, &
            "Floating point value mismatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(diff)//")", &
            more)
        end if
      end if
    end if

  end subroutine check_float_sp


  subroutine check_float_exceptional_sp(error, actual, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    real(sp), intent(in) :: actual

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (is_nan(actual)) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, "Exceptional value 'not a number' found", more)
      end if
    end if

  end subroutine check_float_exceptional_sp


#if WITH_XDP
  subroutine check_float_xdp(error, actual, expected, message, more, thr, rel)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    real(xdp), intent(in) :: actual

    !> Expected floating point value
    real(xdp), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    !> Allowed threshold for matching floating point values
    real(xdp), intent(in), optional :: thr

    !> Check for relative errors instead
    logical, intent(in), optional :: rel

    logical :: relative
    real(xdp) :: diff, threshold

    call check(error, actual, message, more)
    if (allocated(error)) return

    if (present(thr)) then
      threshold = thr
    else
      threshold = epsilon(expected)
    end if

    if (present(rel)) then
      relative = rel
    else
      relative = .false.
    end if

    if (relative) then
      diff = abs(actual - expected) / abs(expected)
    else
      diff = abs(actual - expected)
    end if

    if (diff > threshold) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        if (relative) then
          call test_failed(error, &
            "Floating point value mismatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(int(diff*100))//"%)", &
            more)
        else
          call test_failed(error, &
            "Floating point value mismatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(diff)//")", &
            more)
        end if
      end if
    end if

  end subroutine check_float_xdp


  subroutine check_float_exceptional_xdp(error, actual, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    real(xdp), intent(in) :: actual

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (is_nan(actual)) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, "Exceptional value 'not a number' found", more)
      end if
    end if

  end subroutine check_float_exceptional_xdp
#endif


#if WITH_QP
  subroutine check_float_qp(error, actual, expected, message, more, thr, rel)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    real(qp), intent(in) :: actual

    !> Expected floating point value
    real(qp), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    !> Allowed threshold for matching floating point values
    real(qp), intent(in), optional :: thr

    !> Check for relative errors instead
    logical, intent(in), optional :: rel

    logical :: relative
    real(qp) :: diff, threshold

    call check(error, actual, message, more)
    if (allocated(error)) return

    if (present(thr)) then
      threshold = thr
    else
      threshold = epsilon(expected)
    end if

    if (present(rel)) then
      relative = rel
    else
      relative = .false.
    end if

    if (relative) then
      diff = abs(actual - expected) / abs(expected)
    else
      diff = abs(actual - expected)
    end if

    if (diff > threshold) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        if (relative) then
          call test_failed(error, &
            "Floating point value mismatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(int(diff*100))//"%)", &
            more)
        else
          call test_failed(error, &
            "Floating point value mismatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(diff)//")", &
            more)
        end if
      end if
    end if

  end subroutine check_float_qp


  subroutine check_float_exceptional_qp(error, actual, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    real(qp), intent(in) :: actual

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (is_nan(actual)) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, "Exceptional value 'not a number' found", more)
      end if
    end if

  end subroutine check_float_exceptional_qp
#endif


  subroutine check_complex_dp(error, actual, expected, message, more, thr, rel)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    complex(dp), intent(in) :: actual

    !> Expected floating point value
    complex(dp), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    !> Allowed threshold for matching floating point values
    real(dp), intent(in), optional :: thr

    !> Check for relative errors instead
    logical, intent(in), optional :: rel

    logical :: relative
    real(dp) :: diff, threshold

    call check(error, actual, message, more)
    if (allocated(error)) return

    if (present(thr)) then
      threshold = thr
    else
      threshold = epsilon(abs(expected))
    end if

    if (present(rel)) then
      relative = rel
    else
      relative = .false.
    end if

    if (relative) then
      diff = abs(actual - expected) / abs(expected)
    else
      diff = abs(actual - expected)
    end if

    if (diff > threshold) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        if (relative) then
          call test_failed(error, &
            "Floating point value mismatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(int(diff*100))//"%)", &
            more)
        else
          call test_failed(error, &
            "Floating point value mismatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(diff)//")", &
            more)
        end if
      end if
    end if

  end subroutine check_complex_dp


  subroutine check_complex_exceptional_dp(error, actual, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    complex(dp), intent(in) :: actual

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (is_nan(real(actual)) .or. is_nan(aimag(actual))) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, "Exceptional value 'not a number' found", more)
      end if
    end if

  end subroutine check_complex_exceptional_dp


  subroutine check_complex_sp(error, actual, expected, message, more, thr, rel)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    complex(sp), intent(in) :: actual

    !> Expected floating point value
    complex(sp), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    !> Allowed threshold for matching floating point values
    real(sp), intent(in), optional :: thr

    !> Check for relative errors instead
    logical, intent(in), optional :: rel

    logical :: relative
    real(sp) :: diff, threshold

    call check(error, actual, message, more)
    if (allocated(error)) return

    if (present(thr)) then
      threshold = thr
    else
      threshold = epsilon(abs(expected))
    end if

    if (present(rel)) then
      relative = rel
    else
      relative = .false.
    end if

    if (relative) then
      diff = abs(actual - expected) / abs(expected)
    else
      diff = abs(actual - expected)
    end if

    if (diff > threshold) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        if (relative) then
          call test_failed(error, &
            "Floating point value mismatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(int(diff*100))//"%)", &
            more)
        else
          call test_failed(error, &
            "Floating point value mismatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(diff)//")", &
            more)
        end if
      end if
    end if

  end subroutine check_complex_sp


  subroutine check_complex_exceptional_sp(error, actual, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    complex(sp), intent(in) :: actual

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (is_nan(real(actual)) .or. is_nan(aimag(actual))) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, "Exceptional value 'not a number' found", more)
      end if
    end if

  end subroutine check_complex_exceptional_sp


#if WITH_XDP
  subroutine check_complex_xdp(error, actual, expected, message, more, thr, rel)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    complex(xdp), intent(in) :: actual

    !> Expected floating point value
    complex(xdp), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    !> Allowed threshold for matching floating point values
    real(xdp), intent(in), optional :: thr

    !> Check for relative errors instead
    logical, intent(in), optional :: rel

    logical :: relative
    real(xdp) :: diff, threshold

    call check(error, actual, message, more)
    if (allocated(error)) return

    if (present(thr)) then
      threshold = thr
    else
      threshold = epsilon(abs(expected))
    end if

    if (present(rel)) then
      relative = rel
    else
      relative = .false.
    end if

    if (relative) then
      diff = abs(actual - expected) / abs(expected)
    else
      diff = abs(actual - expected)
    end if

    if (diff > threshold) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        if (relative) then
          call test_failed(error, &
            "Floating point value mismatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(int(diff*100))//"%)", &
            more)
        else
          call test_failed(error, &
            "Floating point value mismatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(diff)//")", &
            more)
        end if
      end if
    end if

  end subroutine check_complex_xdp


  subroutine check_complex_exceptional_xdp(error, actual, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    complex(xdp), intent(in) :: actual

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (is_nan(real(actual)) .or. is_nan(aimag(actual))) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, "Exceptional value 'not a number' found", more)
      end if
    end if

  end subroutine check_complex_exceptional_xdp
#endif


#if WITH_QP
  subroutine check_complex_qp(error, actual, expected, message, more, thr, rel)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    complex(qp), intent(in) :: actual

    !> Expected floating point value
    complex(qp), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    !> Allowed threshold for matching floating point values
    real(qp), intent(in), optional :: thr

    !> Check for relative errors instead
    logical, intent(in), optional :: rel

    logical :: relative
    real(qp) :: diff, threshold

    call check(error, actual, message, more)
    if (allocated(error)) return

    if (present(thr)) then
      threshold = thr
    else
      threshold = epsilon(abs(expected))
    end if

    if (present(rel)) then
      relative = rel
    else
      relative = .false.
    end if

    if (relative) then
      diff = abs(actual - expected) / abs(expected)
    else
      diff = abs(actual - expected)
    end if

    if (diff > threshold) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        if (relative) then
          call test_failed(error, &
            "Floating point value mismatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(int(diff*100))//"%)", &
            more)
        else
          call test_failed(error, &
            "Floating point value mismatch", &
            "expected "//to_string(expected)//" but got "//to_string(actual)//" "//&
            "(difference: "//to_string(diff)//")", &
            more)
        end if
      end if
    end if

  end subroutine check_complex_qp


  subroutine check_complex_exceptional_qp(error, actual, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found floating point value
    complex(qp), intent(in) :: actual

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (is_nan(real(actual)) .or. is_nan(aimag(actual))) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, "Exceptional value 'not a number' found", more)
      end if
    end if

  end subroutine check_complex_exceptional_qp
#endif


  subroutine check_int_i1(error, actual, expected, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found integer value
    integer(i1), intent(in) :: actual

    !> Expected integer value
    integer(i1), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (expected /= actual) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, &
          "Integer value mismatch", &
          "expected "//to_string(expected)//" but got "//to_string(actual), &
          more)
      end if
    end if

  end subroutine check_int_i1


  subroutine check_int_i2(error, actual, expected, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found integer value
    integer(i2), intent(in) :: actual

    !> Expected integer value
    integer(i2), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (expected /= actual) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, &
          "Integer value mismatch", &
          "expected "//to_string(expected)//" but got "//to_string(actual), &
          more)
      end if
    end if

  end subroutine check_int_i2


  subroutine check_int_i4(error, actual, expected, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found integer value
    integer(i4), intent(in) :: actual

    !> Expected integer value
    integer(i4), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (expected /= actual) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, &
          "Integer value mismatch", &
          "expected "//to_string(expected)//" but got "//to_string(actual), &
          more)
      end if
    end if

  end subroutine check_int_i4


  subroutine check_int_i8(error, actual, expected, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found integer value
    integer(i8), intent(in) :: actual

    !> Expected integer value
    integer(i8), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (expected /= actual) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, &
          "Integer value mismatch", &
          "expected "//to_string(expected)//" but got "//to_string(actual), &
          more)
      end if
    end if

  end subroutine check_int_i8


  subroutine check_bool(error, actual, expected, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found boolean value
    logical, intent(in) :: actual

    !> Expected boolean value
    logical, intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (expected .neqv. actual) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, &
          "Logical value mismatch", &
          "expected "//merge("T", "F", expected)//" but got "//merge("T", "F", actual), &
          more)
      end if
    end if

  end subroutine check_bool


  subroutine check_string(error, actual, expected, message, more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> Found boolean value
    character(len=*), intent(in) :: actual

    !> Expected boolean value
    character(len=*), intent(in) :: expected

    !> A detailed message describing the error
    character(len=*), intent(in), optional :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    if (expected /= actual) then
      if (present(message)) then
        call test_failed(error, message, more)
      else
        call test_failed(error, &
          "Character value mismatch", &
          "expected '"//expected//"' but got '"//actual//"'", &
          more)
      end if
    end if

  end subroutine check_string


  subroutine test_failed(error, message, more, and_more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> A detailed message describing the error
    character(len=*), intent(in) :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    !> Another line of error message
    character(len=*), intent(in), optional :: and_more

    character(len=*), parameter :: skip = newline // repeat(" ", 11)

    allocate(error)
    error%stat = fatal

    error%message = message
    if (present(more)) then
      error%message = error%message // skip // more
    end if
    if (present(and_more)) then
      error%message = error%message // skip // and_more
    end if

  end subroutine test_failed


  !> A test is skipped because certain requirements are not met to run the actual test
  subroutine skip_test(error, message, more, and_more)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    !> A detailed message describing the error
    character(len=*), intent(in) :: message

    !> Another line of error message
    character(len=*), intent(in), optional :: more

    !> Another line of error message
    character(len=*), intent(in), optional :: and_more

    call test_failed(error, message, more, and_more)
    error%stat = skipped

  end subroutine skip_test


  !> Obtain the command line argument at a given index
  subroutine get_argument(idx, arg)

    !> Index of command line argument, range [0:command_argument_count()]
    integer, intent(in) :: idx

    !> Command line argument
    character(len=:), allocatable, intent(out) :: arg

    integer :: length, stat

    call get_command_argument(idx, length=length, status=stat)
    if (stat /= success) return

    allocate(character(len=length) :: arg, stat=stat)
    if (stat /= success) return

    if (length > 0) then
      call get_command_argument(idx, arg, status=stat)
      if (stat /= success) deallocate(arg)
    end if

  end subroutine get_argument


  !> Obtain the value of an environment variable
  subroutine get_variable(var, val)

    !> Name of variable
    character(len=*), intent(in) :: var

    !> Value of variable
    character(len=:), allocatable, intent(out) :: val

    integer :: length, stat

    call get_environment_variable(var, length=length, status=stat)
    if (stat /= success) return

    allocate(character(len=length) :: val, stat=stat)
    if (stat /= success) return

    if (length > 0) then
      call get_environment_variable(var, val, status=stat)
      if (stat /= success) deallocate(val)
    end if

  end subroutine get_variable


  pure function integer_i1_to_string(val) result(string)
    integer, parameter :: ik = i1
    !> Integer value to create string from
    integer(ik), intent(in) :: val
    !> String representation of integer
    character(len=:), allocatable :: string

    integer, parameter :: buffer_len = range(val)+2
    character(len=buffer_len) :: buffer
    integer :: pos
    integer(ik) :: n
    character(len=1), parameter :: numbers(-9:0) = &
      ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

    if (val == 0_ik) then
      string = numbers(0)
      return
    end if

    n = sign(val, -1_ik)
    buffer = ""
    pos = buffer_len + 1
    do while (n < 0_ik)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_ik))
      n = n/10_ik
    end do

    if (val < 0_ik) then
      pos = pos - 1
      buffer(pos:pos) = '-'
    end if

    string = buffer(pos:)
  end function integer_i1_to_string


  pure function integer_i2_to_string(val) result(string)
    integer, parameter :: ik = i2
    !> Integer value to create string from
    integer(ik), intent(in) :: val
    !> String representation of integer
    character(len=:), allocatable :: string

    integer, parameter :: buffer_len = range(val)+2
    character(len=buffer_len) :: buffer
    integer :: pos
    integer(ik) :: n
    character(len=1), parameter :: numbers(-9:0) = &
      ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

    if (val == 0_ik) then
      string = numbers(0)
      return
    end if

    n = sign(val, -1_ik)
    buffer = ""
    pos = buffer_len + 1
    do while (n < 0_ik)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_ik))
      n = n/10_ik
    end do

    if (val < 0_ik) then
      pos = pos - 1
      buffer(pos:pos) = '-'
    end if

    string = buffer(pos:)
  end function integer_i2_to_string


  pure function integer_i4_to_string(val) result(string)
    integer, parameter :: ik = i4
    !> Integer value to create string from
    integer(ik), intent(in) :: val
    !> String representation of integer
    character(len=:), allocatable :: string

    integer, parameter :: buffer_len = range(val)+2
    character(len=buffer_len) :: buffer
    integer :: pos
    integer(ik) :: n
    character(len=1), parameter :: numbers(-9:0) = &
      ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

    if (val == 0_ik) then
      string = numbers(0)
      return
    end if

    n = sign(val, -1_ik)
    buffer = ""
    pos = buffer_len + 1
    do while (n < 0_ik)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_ik))
      n = n/10_ik
    end do

    if (val < 0_ik) then
      pos = pos - 1
      buffer(pos:pos) = '-'
    end if

    string = buffer(pos:)
  end function integer_i4_to_string


  pure function integer_i8_to_string(val) result(string)
    integer, parameter :: ik = i8
    !> Integer value to create string from
    integer(ik), intent(in) :: val
    !> String representation of integer
    character(len=:), allocatable :: string

    integer, parameter :: buffer_len = range(val)+2
    character(len=buffer_len) :: buffer
    integer :: pos
    integer(ik) :: n
    character(len=1), parameter :: numbers(-9:0) = &
      ["9", "8", "7", "6", "5", "4", "3", "2", "1", "0"]

    if (val == 0_ik) then
      string = numbers(0)
      return
    end if

    n = sign(val, -1_ik)
    buffer = ""
    pos = buffer_len + 1
    do while (n < 0_ik)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_ik))
      n = n/10_ik
    end do

    if (val < 0_ik) then
      pos = pos - 1
      buffer(pos:pos) = '-'
    end if

    string = buffer(pos:)
  end function integer_i8_to_string


  pure function real_sp_to_string(val) result(string)
    real(sp), intent(in) :: val
    character(len=:), allocatable :: string
    integer, parameter :: buffer_len = 128
    character(len=buffer_len) :: buffer

    write(buffer, '(g0)') val
    string = trim(buffer)

  end function real_sp_to_string


  pure function real_dp_to_string(val) result(string)
    real(dp), intent(in) :: val
    character(len=:), allocatable :: string
    integer, parameter :: buffer_len = 128
    character(len=buffer_len) :: buffer

    write(buffer, '(g0)') val
    string = trim(buffer)

  end function real_dp_to_string


#if WITH_XDP
  pure function real_xdp_to_string(val) result(string)
    real(xdp), intent(in) :: val
    character(len=:), allocatable :: string
    integer, parameter :: buffer_len = 128
    character(len=buffer_len) :: buffer

    write(buffer, '(g0)') val
    string = trim(buffer)

  end function real_xdp_to_string
#endif


#if WITH_QP
  pure function real_qp_to_string(val) result(string)
    real(qp), intent(in) :: val
    character(len=:), allocatable :: string
    integer, parameter :: buffer_len = 128
    character(len=buffer_len) :: buffer

    write(buffer, '(g0)') val
    string = trim(buffer)

  end function real_qp_to_string
#endif


  pure function complex_sp_to_string(val) result(string)
    complex(sp), intent(in) :: val
    character(len=:), allocatable :: string

    string = "("//to_string(real(val))//", "//to_string(aimag(val))//")"

  end function complex_sp_to_string


  pure function complex_dp_to_string(val) result(string)
    complex(dp), intent(in) :: val
    character(len=:), allocatable :: string

    string = "("//to_string(real(val))//", "//to_string(aimag(val))//")"

  end function complex_dp_to_string


#if WITH_XDP
  pure function complex_xdp_to_string(val) result(string)
    complex(xdp), intent(in) :: val
    character(len=:), allocatable :: string

    string = "("//to_string(real(val))//", "//to_string(aimag(val))//")"

  end function complex_xdp_to_string
#endif


#if WITH_QP
  pure function complex_qp_to_string(val) result(string)
    complex(qp), intent(in) :: val
    character(len=:), allocatable :: string

    string = "("//to_string(real(val))//", "//to_string(aimag(val))//")"

  end function complex_qp_to_string
#endif


  !> Clear error type after it has been handled.
  subroutine clear_error(error)

    !> Error handling
    type(error_type), intent(inout) :: error

    if (error%stat /= success) then
      error%stat = success
    end if

    if (allocated(error%message)) then
      deallocate(error%message)
    end if

  end subroutine clear_error


  !> Finalizer of the error type, in case the error is not correctly cleared it will
  !> be escalated at runtime in a fatal way
  subroutine escalate_error(error)

    !> Error handling
    type(error_type), intent(inout) :: error

    if (error%stat /= success) then
      write(error_unit, '(a)') "[Fatal] Uncaught error"
      if (allocated(error%message)) then
        write(error_unit, '(a, 1x, i0, *(1x, a))') &
          "Code:", error%stat, "Message:", error%message
      end if
      error stop
    end if

  end subroutine escalate_error


  !> Determine whether a value is not a number without requiring IEEE arithmetic support
  elemental function is_nan_sp(val) result(is_nan)
    !> Value to check
    real(sp), intent(in) :: val
    !> Value is not a number
    logical :: is_nan

    is_nan = .not.((val <= huge(val) .and. val >= -huge(val)) .or. abs(val) > huge(val))
  end function is_nan_sp

  !> Determine whether a value is not a number without requiring IEEE arithmetic support
  elemental function is_nan_dp(val) result(is_nan)
    !> Value to check
    real(dp), intent(in) :: val
    !> Value is not a number
    logical :: is_nan

    is_nan = .not.((val <= huge(val) .and. val >= -huge(val)) .or. abs(val) > huge(val))
  end function is_nan_dp

#if WITH_XDP
  !> Determine whether a value is not a number without requiring IEEE arithmetic support
  elemental function is_nan_xdp(val) result(is_nan)
    !> Value to check
    real(xdp), intent(in) :: val
    !> Value is not a number
    logical :: is_nan

    is_nan = .not.((val <= huge(val) .and. val >= -huge(val)) .or. abs(val) > huge(val))
  end function is_nan_xdp
#endif

#if WITH_QP
  !> Determine whether a value is not a number without requiring IEEE arithmetic support
  elemental function is_nan_qp(val) result(is_nan)
    !> Value to check
    real(qp), intent(in) :: val
    !> Value is not a number
    logical :: is_nan

    is_nan = .not.((val <= huge(val) .and. val >= -huge(val)) .or. abs(val) > huge(val))
  end function is_nan_qp
#endif

  !> Initialize color output
  subroutine init_color_output(use_color)
    !> Enable color output
    logical, intent(in) :: use_color

    color = new_color_output(use_color)
  end subroutine init_color_output

  !> Create a new colorizer object
  function new_color_output(use_color) result(new)
    !> Enable color output
    logical, intent(in) :: use_color
    !> New instance of the colorizer
    type(color_output) :: new

    type(color_code), parameter :: &
      reset = color_code(style=0_i1), &
      bold = color_code(style=1_i1), &
      dim = color_code(style=2_i1), &
      italic = color_code(style=3_i1), &
      underline = color_code(style=4_i1), &
      blink = color_code(style=5_i1), &
      reverse = color_code(style=7_i1), &
      hidden = color_code(style=8_i1)

    type(color_code), parameter :: &
      black = color_code(fg=0_i1), &
      red = color_code(fg=1_i1), &
      green = color_code(fg=2_i1), &
      yellow = color_code(fg=3_i1), &
      blue = color_code(fg=4_i1), &
      magenta = color_code(fg=5_i1), &
      cyan = color_code(fg=6_i1), &
      white = color_code(fg=7_i1)

    type(color_code), parameter :: &
      bg_black = color_code(bg=0_i1), &
      bg_red = color_code(bg=1_i1), &
      bg_green = color_code(bg=2_i1), &
      bg_yellow = color_code(bg=3_i1), &
      bg_blue = color_code(bg=4_i1), &
      bg_magenta = color_code(bg=5_i1), &
      bg_cyan = color_code(bg=6_i1), &
      bg_white = color_code(bg=7_i1)

    if (use_color) then
      new%reset = reset
      new%bold = bold
      new%dim = dim
      new%italic = italic
      new%underline = underline
      new%blink = blink
      new%reverse = reverse
      new%hidden = hidden
      new%black = black
      new%red = red
      new%green = green
      new%yellow = yellow
      new%blue = blue
      new%magenta = magenta
      new%cyan = cyan
      new%white = white
      new%bg_black = bg_black
      new%bg_red = bg_red
      new%bg_green = bg_green
      new%bg_yellow = bg_yellow
      new%bg_blue = bg_blue
      new%bg_magenta = bg_magenta
      new%bg_cyan = bg_cyan
      new%bg_white = bg_white
    end if
  end function new_color_output

  !> Add two escape sequences, attributes in the right value override the left value ones.
  pure function add_color(lval, rval) result(code)
    !> First escape code
    type(color_code), intent(in) :: lval
    !> Second escape code
    type(color_code), intent(in) :: rval
    !> Combined escape code
    type(color_code) :: code
  
    code = color_code( &
      style=merge(rval%style, lval%style, rval%style >= 0), &
      fg=merge(rval%fg, lval%fg, rval%fg >= 0), &
      bg=merge(rval%bg, lval%bg, rval%bg >= 0))
  end function add_color
  
  !> Concatenate an escape code with a string and turn it into an actual escape sequence
  pure function concat_color_left(lval, code) result(str)
    !> String to add the escape code to
    character(len=*), intent(in) :: lval
    !> Escape sequence
    type(color_code), intent(in) :: code
    !> Concatenated string
    character(len=:), allocatable :: str
  
    str = lval // escape_color(code)
  end function concat_color_left
  
  !> Concatenate an escape code with a string and turn it into an actual escape sequence
  pure function concat_color_right(code, rval) result(str)
    !> String to add the escape code to
    character(len=*), intent(in) :: rval
    !> Escape sequence
    type(color_code), intent(in) :: code
    !> Concatenated string
    character(len=:), allocatable :: str
  
    str = escape_color(code) // rval
  end function concat_color_right
  
  !> Transform a color code into an actual ANSI escape sequence
  pure function escape_color(code) result(str)
    !> Color code to be used
    type(color_code), intent(in) :: code
    !> ANSI escape sequence representing the color code
    character(len=:), allocatable :: str
    character, parameter :: chars(0:9) = &
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
  
    if (anycolor(code)) then
      str = achar(27) // "[0"  ! Always reset the style
      if (code%style > 0 .and. code%style < 10) str = str // ";" // chars(code%style)
      if (code%fg >= 0 .and. code%fg < 10) str = str // ";3" // chars(code%fg)
      if (code%bg >= 0 .and. code%bg < 10) str = str // ";4" // chars(code%bg)
      str = str // "m"
    else
      str = ""
    end if
  end function escape_color
  
  !> Check whether the code describes any color or is just a stub
  pure function anycolor(code)
    !> Escape sequence
    type(color_code), intent(in) :: code
    !> Any color / style is active
    logical :: anycolor
  
    anycolor = code%fg >= 0 .or. code%bg >= 0 .or. code%style >= 0
  end function anycolor

end module testdrive
