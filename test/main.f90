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

!> Driver for unit testing
program tester
  use, intrinsic :: iso_fortran_env, only : error_unit
  use testdrive, only : run_testsuite, new_testsuite, testsuite_type, &
    & select_suite, run_selected, get_argument, &
    & junitxml_open_file, junitxml_close_file, &
    & junitxml_write_testsuite_opening_tag, junitxml_write_testsuite_closing_tag
  use test_check, only : collect_check
  use test_select, only : collect_select
  implicit none
  integer :: stat, is
  character(len=:), allocatable :: suite_name, test_name
  type(testsuite_type), allocatable :: testsuites(:)
  character(len=*), parameter :: fmt = '("#", *(1x, a))'

  stat = 0

  testsuites = [ &
    new_testsuite("check", collect_check), &
    new_testsuite("select", collect_select) &
    ]

  call get_argument(1, suite_name)
  call get_argument(2, test_name)

  call junitxml_open_file()

  if (allocated(suite_name)) then
    is = select_suite(testsuites, suite_name)
    if (is > 0 .and. is <= size(testsuites)) then
      if (allocated(test_name)) then
        write(error_unit, fmt) "Suite:", testsuites(is)%name
        call junitxml_write_testsuite_opening_tag(testsuites(is)%name, is)
        call run_selected(testsuites(is)%collect, test_name, error_unit, stat)
        call junitxml_write_testsuite_closing_tag()
        if (stat < 0) then
          error stop 1
        end if
      else
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call junitxml_write_testsuite_opening_tag(testsuites(is)%name, is)
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
        call junitxml_write_testsuite_closing_tag()
      end if
    else
      write(error_unit, fmt) "Available testsuites"
      do is = 1, size(testsuites)
        write(error_unit, fmt) "-", testsuites(is)%name
      end do
      error stop 1
    end if
  else
    do is = 1, size(testsuites)
      write(error_unit, fmt) "Testing:", testsuites(is)%name
      call junitxml_write_testsuite_opening_tag(testsuites(is)%name, is)
      call run_testsuite(testsuites(is)%collect, error_unit, stat)
      call junitxml_write_testsuite_closing_tag()
    end do
  end if

  if (stat > 0) then
    write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
    error stop 1
  end if

  call junitxml_close_file()

end program tester
