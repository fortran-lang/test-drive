# The simple testing framework

This project offers a lightweight, procedural unit testing framework
based on nothing but standard Fortran.


## Usage

Testsuites are defined by a ``collect_interface`` returning a set of
``unittest_type`` objects. To create a new test use the ``new_unittest``
constructor, which requires a test identifier and a procedure with a
``test_interface`` compatible signature. The error status is communicated
by the allocation status of an ``error_type``.

The necessary boilerplate code to setup the test entry point is just

```fortran
program tester
   use, intrinsic :: iso_fortran_env, only : error_unit
   use testdrive, only : run_testsuite, new_testsuite, testsuite_type
   use test_suite1, only : collect_suite1
   use test_suite2, only : collect_suite2
   implicit none
   integer :: stat, is
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: fmt = '("#", *(1x, a))'

   stat = 0

   testsuites = [ &
      new_testsuite("suite1", collect_suite1), &
      new_testsuite("suite2", collect_suite2) &
      ]

   do is = 1, size(testsuites)
      write(error_unit, fmt) "Testing:", testsuites(is)%name
      call run_testsuite(testsuites(is)%collect, error_unit, stat)
   end do

   if (stat > 0) then
      write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
      error stop
   end if

end program tester
```

Every test is defined in a separate module using a ``collect`` function, which
is exported and added to the ``testsuites`` array in the test runner.
All test have a simple interface with just an allocatable ``error_type`` as
output to provide the test results.

```fortran
module test_suite1
   use testdrive, only : new_unittest, unittest_type, error_type, check
   implicit none
   private

   public :: collect_suite1

contains

!> Collect all exported unit tests
subroutine collect_suite1(testsuite)
   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      new_unittest("valid", test_valid), &
      new_unittest("invalid", test_invalid, should_fail=.true.) &
      ]

end subroutine collect_suite1

subroutine test_valid(error)
   type(error_type), allocatable, intent(out) :: error
   ! ...
end subroutine test_valid

subroutine test_invalid(error)
   type(error_type), allocatable, intent(out) :: error
   ! ...
end subroutine test_invalid

end module test_suite1
```


## License

Licensed under the Apache License, Version 2.0 (the “License”);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an *“as is” basis*,
*without warranties or conditions of any kind*, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in this project by you, as defined in the
Apache-2.0 license, shall be licensed as above, without any additional
terms or conditions.
