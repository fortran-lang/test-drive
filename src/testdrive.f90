! This file is part of test-drive.
! SPDX-Identifier: Apache-2.0
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

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
!>   use, intrinsic :: iso_fortran_env, only : error_unit
!>   use testdrive, only : run_testsuite, new_testsuite, testsuite_type
!>   use test_suite1, only : collect_suite1
!>   use test_suite2, only : collect_suite2
!>   implicit none
!>   integer :: stat, is
!>   type(testsuite_type), allocatable :: testsuites(:)
!>   character(len=*), parameter :: fmt = '("#", *(1x, a))'
!>
!>   stat = 0
!>
!>   testsuites = [ &
!>      & new_testsuite("suite1", collect_suite1), &
!>      & new_testsuite("suite2", collect_suite2) &
!>      & ]
!>
!>   do is = 1, size(testsuites)
!>      write(error_unit, fmt) "Testing:", testsuites(is)%name
!>      call run_testsuite(testsuites(is)%collect, error_unit, stat)
!>   end do
!>
!>   if (stat > 0) then
!>      write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
!>      error stop
!>   end if
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
!>   use testdrive, only : new_unittest, unittest_type, error_type, check
!>   implicit none
!>   private
!>
!>   public :: collect_suite1
!>
!>contains
!>
!>!> Collect all exported unit tests
!>subroutine collect_suite1(testsuite)
!>   !> Collection of tests
!>   type(unittest_type), allocatable, intent(out) :: testsuite(:)
!>
!>   testsuite = [ &
!>      & new_unittest("valid", test_valid), &
!>      & new_unittest("invalid", test_invalid, should_fail=.true.) &
!>      & ]
!>
!>end subroutine collect_suite1
!>
!>subroutine test_valid(error)
!>   type(error_type), allocatable, intent(out) :: error
!>   ! ...
!>end subroutine test_valid
!>
!>subroutine test_invalid(error)
!>   type(error_type), allocatable, intent(out) :: error
!>   ! ...
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
   public :: check, test_failed
   public :: test_interface, collect_interface
   public :: get_argument, get_variable


   !> Single precision real numbers
   integer, parameter :: sp = selected_real_kind(6)

   !> Double precision real numbers
   integer, parameter :: dp = selected_real_kind(15)

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
      module procedure :: check_int_i1
      module procedure :: check_int_i2
      module procedure :: check_int_i4
      module procedure :: check_int_i8
      module procedure :: check_bool
      module procedure :: check_string
   end interface check


   interface ch
      module procedure :: integer_i1_to_char
      module procedure :: integer_i2_to_char
      module procedure :: integer_i4_to_char
      module procedure :: integer_i8_to_char
      module procedure :: real_sp_to_char
      module procedure :: real_dp_to_char
   end interface ch


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


   character(len=*), parameter :: fmt = '(1x, *(1x, a))'
   character(len=*), parameter :: indent = repeat(" ", 5) // repeat(".", 3)


contains


!> Driver for testsuite
subroutine run_testsuite(collect, unit, stat)

   !> Collect tests
   procedure(collect_interface) :: collect

   !> Unit for IO
   integer, intent(in) :: unit

   !> Number of failed tests
   integer, intent(inout) :: stat

   type(unittest_type), allocatable :: testsuite(:)
   integer :: it

   call collect(testsuite)

   !$omp parallel do shared(testsuite, unit) reduction(+:stat)
   do it = 1, size(testsuite)
      !$omp critical(testdrive_testsuite)
      write(unit, '(1x, 3(1x, a), 1x, "(", i0, "/", i0, ")")') &
         & "Starting", testsuite(it)%name, "...", it, size(testsuite)
      !$omp end critical(testdrive_testsuite)
      call run_unittest(testsuite(it), unit, stat)
   end do

end subroutine run_testsuite


!> Driver for selective testing
subroutine run_selected(collect, name, unit, stat)

   !> Collect tests
   procedure(collect_interface) :: collect

   !> Name of the selected test
   character(len=*), intent(in) :: name

   !> Unit for IO
   integer, intent(in) :: unit

   !> Number of failed tests
   integer, intent(inout) :: stat

   type(unittest_type), allocatable :: testsuite(:)
   integer :: it

   call collect(testsuite)

   it = select_test(testsuite, name)

   if (it > 0 .and. it <= size(testsuite)) then
      call run_unittest(testsuite(it), unit, stat)
   else
      write(unit, fmt) "Available tests:"
      do it = 1, size(testsuite)
         write(unit, fmt) "-", testsuite(it)%name
      end do
      stat = -huge(it)
   end if

end subroutine run_selected


!> Run a selected unit test
subroutine run_unittest(test, unit, stat)

   !> Unit test
   type(unittest_type), intent(in) :: test

   !> Unit for IO
   integer, intent(in) :: unit

   !> Number of failed tests
   integer, intent(inout) :: stat

   type(error_type), allocatable :: error

   call test%test(error)
   !$omp critical(testdrive_testsuite)
   if (allocated(error) .neqv. test%should_fail) then
      if (test%should_fail) then
         write(unit, fmt) indent, test%name, "[UNEXPECTED PASS]"
      else
         write(unit, fmt) indent, test%name, "[FAILED]"
      end if
      stat = stat + 1
   else
      if (test%should_fail) then
         write(unit, fmt) indent, test%name, "[EXPECTED FAIL]"
      else
         write(unit, fmt) indent, test%name, "[PASSED]"
      end if
   end if
   if (allocated(error)) then
      write(unit, fmt) "Message:", error%message
      call clear_error(error)
   end if
   !$omp end critical(testdrive_testsuite)

end subroutine run_unittest


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
      diff = abs(actual - expected) / expected
   else
      diff = abs(actual - expected)
   end if

   if (diff > threshold) then
      if (present(message)) then
         call test_failed(error, message, more)
      else
         if (relative) then
            call test_failed(error, &
               "Floating point value missmatch", &
               "expected "//ch(expected)//" but got "//ch(actual)//" "//&
               "(difference: "//ch(int(diff*100))//"%)", &
               more)
         else
            call test_failed(error, &
               "Floating point value missmatch", &
               "expected "//ch(expected)//" but got "//ch(actual)//" "//&
               "(difference: "//ch(diff)//")", &
               more)
         end if
      end if
   end if

end subroutine check_float_dp


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
      diff = abs(actual - expected) / expected
   else
      diff = abs(actual - expected)
   end if

   if (diff > threshold) then
      if (present(message)) then
         call test_failed(error, message, more)
      else
         if (relative) then
            call test_failed(error, &
               "Floating point value missmatch", &
               "expected "//ch(expected)//" but got "//ch(actual)//" "//&
               "(difference: "//ch(int(diff*100))//"%)", &
               more)
         else
            call test_failed(error, &
               "Floating point value missmatch", &
               "expected "//ch(expected)//" but got "//ch(actual)//" "//&
               "(difference: "//ch(diff)//")", &
               more)
         end if
      end if
   end if

end subroutine check_float_sp


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
            "Integer value missmatch", &
            "expected "//ch(expected)//" but got "//ch(actual), &
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
            "Integer value missmatch", &
            "expected "//ch(expected)//" but got "//ch(actual), &
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
            "Integer value missmatch", &
            "expected "//ch(expected)//" but got "//ch(actual), &
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
            "Integer value missmatch", &
            "expected "//ch(expected)//" but got "//ch(actual), &
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
            "Logical value missmatch", &
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
            "Character value missmatch", &
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

   character(len=*), parameter :: skip = new_line("a") // repeat(" ", 11)

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


!> Obtain the command line argument at a given index
subroutine get_argument(idx, arg)

   !> Index of command line argument, range [0:command_argument_count()]
   integer, intent(in) :: idx

   !> Command line argument
   character(len=:), allocatable, intent(out) :: arg

   integer :: length, stat

   call get_command_argument(idx, length=length, status=stat)
   if (stat /= success) then
      return
   endif

   allocate(character(len=length) :: arg, stat=stat)
   if (stat /= success) then
      return
   endif

   if (length > 0) then
      call get_command_argument(idx, arg, status=stat)
      if (stat /= success) then
         deallocate(arg)
         return
      end if
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
   if (stat /= success) then
      return
   endif

   allocate(character(len=length) :: val, stat=stat)
   if (stat /= success) then
      return
   endif

   if (length > 0) then
      call get_environment_variable(var, val, status=stat)
      if (stat /= success) then
         deallocate(val)
         return
      end if
   end if

end subroutine get_variable


pure function integer_i1_to_char(val) result(string)
   integer(i1), intent(in) :: val
   character(len=:), allocatable :: string
   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer(i1) :: n
   character(len=1), parameter :: numbers(0:9) = &
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

   if (val == 0_i1) then
      string = numbers(0)
      return
   end if

   n = abs(val)
   buffer = ""

   pos = buffer_len + 1
   do while (n > 0_i1)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_i1))
      n = n/10_i1
   end do
   if (val < 0_i1) then
      pos = pos - 1
      buffer(pos:pos) = "-"
   end if

   string = buffer(pos:)
end function integer_i1_to_char


pure function integer_i2_to_char(val) result(string)
   integer(i2), intent(in) :: val
   character(len=:), allocatable :: string
   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer(i2) :: n
   character(len=1), parameter :: numbers(0:9) = &
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

   if (val == 0_i2) then
      string = numbers(0)
      return
   end if

   n = abs(val)
   buffer = ""

   pos = buffer_len + 1
   do while (n > 0_i2)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_i1))
      n = n/10_i2
   end do
   if (val < 0_i2) then
      pos = pos - 1
      buffer(pos:pos) = "-"
   end if

   string = buffer(pos:)
end function integer_i2_to_char


pure function integer_i4_to_char(val) result(string)
   integer(i4), intent(in) :: val
   character(len=:), allocatable :: string
   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer(i4) :: n
   character(len=1), parameter :: numbers(0:9) = &
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

   if (val == 0_i4) then
      string = numbers(0)
      return
   end if

   n = abs(val)
   buffer = ""

   pos = buffer_len + 1
   do while (n > 0_i4)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_i1))
      n = n/10_i4
   end do
   if (val < 0_i4) then
      pos = pos - 1
      buffer(pos:pos) = "-"
   end if

   string = buffer(pos:)
end function integer_i4_to_char


pure function integer_i8_to_char(val) result(string)
   integer(i8), intent(in) :: val
   character(len=:), allocatable :: string
   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer(i8) :: n
   character(len=1), parameter :: numbers(0:9) = &
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

   if (val == 0_i8) then
      string = numbers(0)
      return
   end if

   n = abs(val)
   buffer = ""

   pos = buffer_len + 1
   do while (n > 0_i8)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10_i1))
      n = n/10_i8
   end do
   if (val < 0_i8) then
      pos = pos - 1
      buffer(pos:pos) = "-"
   end if

   string = buffer(pos:)
end function integer_i8_to_char


pure function real_sp_to_char(val) result(string)
   real(sp), intent(in) :: val
   character(len=:), allocatable :: string
   integer, parameter :: buffer_len = 128
   character(len=buffer_len) :: buffer

   write(buffer, '(g0)') val
   string = trim(buffer)

end function real_sp_to_char


pure function real_dp_to_char(val) result(string)
   real(dp), intent(in) :: val
   character(len=:), allocatable :: string
   integer, parameter :: buffer_len = 128
   character(len=buffer_len) :: buffer

   write(buffer, '(g0)') val
   string = trim(buffer)

end function real_dp_to_char


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


end module testdrive
