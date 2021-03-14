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

module test_check
   use testdrive, only : new_unittest, unittest_type, error_type, check
   implicit none
   private

   public :: collect_check


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

contains


!> Collect all exported unit tests
subroutine collect_check(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      new_unittest("success", test_success), &
      new_unittest("failure", test_failure, should_fail=.true.), &
      new_unittest("failure-with-more", test_failure_with_more, should_fail=.true.), &
      new_unittest("real-single-abs", test_rsp_abs), &
      new_unittest("real-single-rel", test_rsp_rel), &
      new_unittest("real-single-abs-fail", test_rsp_abs_fail, should_fail=.true.), &
      new_unittest("real-single-rel-fail", test_rsp_rel_fail, should_fail=.true.), &
      new_unittest("real-double-abs", test_rdp_abs), &
      new_unittest("real-double-rel", test_rdp_rel), &
      new_unittest("real-double-abs-fail", test_rdp_abs_fail, should_fail=.true.), &
      new_unittest("real-double-rel-fail", test_rdp_rel_fail, should_fail=.true.), &
      new_unittest("integer-char", test_i1), &
      new_unittest("integer-char-fail", test_i1_fail, should_fail=.true.), &
      new_unittest("integer-short", test_i2), &
      new_unittest("integer-short-fail", test_i2_fail, should_fail=.true.), &
      new_unittest("integer-default", test_i4), &
      new_unittest("integer-default-fail", test_i4_fail, should_fail=.true.), &
      new_unittest("integer-long", test_i8), &
      new_unittest("integer-long-fail", test_i8_fail, should_fail=.true.), &
      new_unittest("logical-default", test_l4), &
      new_unittest("logical-default-fail", test_l4_fail, should_fail=.true.) &
      ]

end subroutine collect_check


subroutine test_success(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check(error, 0)

end subroutine test_success


subroutine test_failure(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check(error, 7)

end subroutine test_failure


subroutine test_failure_with_more(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check(error, 3, more="with an additional descriptive message here")

end subroutine test_failure_with_more


subroutine test_rsp_abs(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(sp) :: val

   val = 3.3_sp

   call check(error, val, 3.3_sp)

end subroutine test_rsp_abs


subroutine test_rsp_rel(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(sp) :: val

   val = 3.3_sp

   call check(error, val, 3.3_sp, rel=.true.)

end subroutine test_rsp_rel


subroutine test_rsp_abs_fail(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(sp) :: val

   val = 1.0_sp

   call check(error, val, 2.0_sp)

end subroutine test_rsp_abs_fail


subroutine test_rsp_rel_fail(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(sp) :: val

   val = 1.0_sp

   call check(error, val, 1.5_sp, rel=.true.)

end subroutine test_rsp_rel_fail


subroutine test_rdp_abs(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(dp) :: val

   val = 3.3_dp

   call check(error, val, 3.3_dp)

end subroutine test_rdp_abs


subroutine test_rdp_rel(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(dp) :: val

   val = 3.3_dp

   call check(error, val, 3.3_dp, rel=.true.)

end subroutine test_rdp_rel


subroutine test_rdp_abs_fail(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(dp) :: val

   val = 1.0_dp

   call check(error, val, 2.0_dp)

end subroutine test_rdp_abs_fail


subroutine test_rdp_rel_fail(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(dp) :: val

   val = 1.0_dp

   call check(error, val, 1.5_dp, rel=.true.)

end subroutine test_rdp_rel_fail


subroutine test_i1(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer(i1) :: val

   val = 3_i1

   call check(error, val, 3_i1)

end subroutine test_i1


subroutine test_i1_fail(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer(i1) :: val

   val = 3_i1

   call check(error, val, 4_i1)

end subroutine test_i1_fail


subroutine test_i2(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer(i2) :: val

   val = 3_i2

   call check(error, val, 3_i2)

end subroutine test_i2


subroutine test_i2_fail(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer(i2) :: val

   val = 3_i2

   call check(error, val, 4_i2)

end subroutine test_i2_fail


subroutine test_i4(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer(i4) :: val

   val = 3_i4

   call check(error, val, 3_i4)

end subroutine test_i4


subroutine test_i4_fail(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer(i4) :: val

   val = 3_i4

   call check(error, val, 4_i4)

end subroutine test_i4_fail


subroutine test_i8(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer(i8) :: val

   val = 3_i8

   call check(error, val, 3_i8)

end subroutine test_i8


subroutine test_i8_fail(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer(i8) :: val

   val = 3_i8

   call check(error, val, 4_i8)

end subroutine test_i8_fail


subroutine test_l4(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check(error, .true., .true.)
   if (allocated(error)) return

   call check(error, .false., .false.)

end subroutine test_l4


subroutine test_l4_fail(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check(error, .true., .false.)

end subroutine test_l4_fail


end module test_check
