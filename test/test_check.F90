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

module test_check
  use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan
  use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test, to_string
  implicit none
  private

  public :: collect_check


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

contains


  !> Collect all exported unit tests
  subroutine collect_check(testsuite)

    !> Collection of tests
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
      new_unittest("success", test_success), &
      new_unittest("failure", test_failure, should_fail=.true.), &
      new_unittest("failure-message", test_failure_message, should_fail=.true.), &
      new_unittest("failure-with-more", test_failure_with_more, should_fail=.true.), &
      new_unittest("skipped", test_skipped), &
      new_unittest("expression", test_expression), &
      new_unittest("expression-fail", test_expression_fail, should_fail=.true.), &
      new_unittest("expression-message", test_expression_message, should_fail=.true.), &
      new_unittest("expression-with-more", test_expression_with_more, should_fail=.true.), &
      new_unittest("real-single-abs", test_rsp_abs), &
      new_unittest("real-single-rel", test_rsp_rel), &
      new_unittest("real-single-nan", test_rsp_nan, should_fail=.true.), &
      new_unittest("real-single-abs-fail", test_rsp_abs_fail, should_fail=.true.), &
      new_unittest("real-single-rel-fail", test_rsp_rel_fail, should_fail=.true.), &
      new_unittest("real-single-abs-message", test_rsp_abs_message, should_fail=.true.), &
      new_unittest("real-single-nan-message", test_rsp_nan_message, should_fail=.true.), &
      new_unittest("real-double-abs", test_rdp_abs), &
      new_unittest("real-double-rel", test_rdp_rel), &
      new_unittest("real-double-nan", test_rdp_nan, should_fail=.true.), &
      new_unittest("real-double-abs-fail", test_rdp_abs_fail, should_fail=.true.), &
      new_unittest("real-double-rel-fail", test_rdp_rel_fail, should_fail=.true.), &
      new_unittest("real-double-abs-message", test_rdp_abs_message, should_fail=.true.), &
      new_unittest("real-double-nan-message", test_rdp_nan_message, should_fail=.true.), &
      new_unittest("real-xdouble-abs", test_rxdp_abs), &
      new_unittest("real-xdouble-rel", test_rxdp_rel), &
      new_unittest("real-xdouble-nan", test_rxdp_nan, should_fail=.true.), &
      new_unittest("real-xdouble-abs-fail", test_rxdp_abs_fail, should_fail=.true.), &
      new_unittest("real-xdouble-rel-fail", test_rxdp_rel_fail, should_fail=.true.), &
      new_unittest("real-xdouble-abs-message", test_rxdp_abs_message, should_fail=.true.), &
      new_unittest("real-xdouble-nan-message", test_rxdp_nan_message, should_fail=.true.), &
      new_unittest("real-quadruple-abs", test_rqp_abs), &
      new_unittest("real-quadruple-rel", test_rqp_rel), &
      new_unittest("real-quadruple-nan", test_rqp_nan, should_fail=.true.), &
      new_unittest("real-quadruple-abs-fail", test_rqp_abs_fail, should_fail=.true.), &
      new_unittest("real-quadruple-rel-fail", test_rqp_rel_fail, should_fail=.true.), &
      new_unittest("real-quadruple-abs-message", test_rqp_abs_message, should_fail=.true.), &
      new_unittest("real-quadruple-nan-message", test_rqp_nan_message, should_fail=.true.), &
      new_unittest("complex-single-abs", test_csp_abs), &
      new_unittest("complex-single-rel", test_csp_rel), &
      new_unittest("complex-single-nan", test_csp_nan, should_fail=.true.), &
      new_unittest("complex-single-abs-fail", test_csp_abs_fail, should_fail=.true.), &
      new_unittest("complex-single-rel-fail", test_csp_rel_fail, should_fail=.true.), &
      new_unittest("complex-single-abs-message", test_csp_abs_message, should_fail=.true.), &
      new_unittest("complex-single-nan-message", test_csp_nan_message, should_fail=.true.), &
      new_unittest("complex-double-abs", test_cdp_abs), &
      new_unittest("complex-double-rel", test_cdp_rel), &
      new_unittest("complex-double-nan", test_cdp_nan, should_fail=.true.), &
      new_unittest("complex-double-abs-fail", test_cdp_abs_fail, should_fail=.true.), &
      new_unittest("complex-double-rel-fail", test_cdp_rel_fail, should_fail=.true.), &
      new_unittest("complex-double-abs-message", test_cdp_abs_message, should_fail=.true.), &
      new_unittest("complex-double-nan-message", test_cdp_nan_message, should_fail=.true.), &
      new_unittest("complex-xdouble-abs", test_cxdp_abs), &
      new_unittest("complex-xdouble-rel", test_cxdp_rel), &
      new_unittest("complex-xdouble-nan", test_cxdp_nan, should_fail=.true.), &
      new_unittest("complex-xdouble-abs-fail", test_cxdp_abs_fail, should_fail=.true.), &
      new_unittest("complex-xdouble-rel-fail", test_cxdp_rel_fail, should_fail=.true.), &
      new_unittest("complex-xdouble-abs-message", test_cxdp_abs_message, should_fail=.true.), &
      new_unittest("complex-xdouble-nan-message", test_cxdp_nan_message, should_fail=.true.), &
      new_unittest("complex-quadruple-abs", test_cqp_abs), &
      new_unittest("complex-quadruple-rel", test_cqp_rel), &
      new_unittest("complex-quadruple-nan", test_cqp_nan, should_fail=.true.), &
      new_unittest("complex-quadruple-abs-fail", test_cqp_abs_fail, should_fail=.true.), &
      new_unittest("complex-quadruple-rel-fail", test_cqp_rel_fail, should_fail=.true.), &
      new_unittest("complex-quadruple-abs-message", test_cqp_abs_message, should_fail=.true.), &
      new_unittest("complex-quadruple-nan-message", test_cqp_nan_message, should_fail=.true.), &
      new_unittest("integer-char", test_i1), &
      new_unittest("integer-char-fail", test_i1_fail, should_fail=.true.), &
      new_unittest("integer-char-message", test_i1_message, should_fail=.true.), &
      new_unittest("integer-char-with-more", test_i1_with_more, should_fail=.true.), &
      new_unittest("integer-short", test_i2), &
      new_unittest("integer-short-fail", test_i2_fail, should_fail=.true.), &
      new_unittest("integer-short-message", test_i2_message, should_fail=.true.), &
      new_unittest("integer-short-with-more", test_i2_with_more, should_fail=.true.), &
      new_unittest("integer-default", test_i4), &
      new_unittest("integer-default-fail", test_i4_fail, should_fail=.true.), &
      new_unittest("integer-default-message", test_i4_message, should_fail=.true.), &
      new_unittest("integer-default-with-more", test_i4_with_more, should_fail=.true.), &
      new_unittest("integer-long", test_i8), &
      new_unittest("integer-long-fail", test_i8_fail, should_fail=.true.), &
      new_unittest("integer-long-message", test_i8_message, should_fail=.true.), &
      new_unittest("integer-long-with-more", test_i8_with_more, should_fail=.true.), &
      new_unittest("logical-default-true", test_l4_true), &
      new_unittest("logical-default-false", test_l4_false), &
      new_unittest("logical-default-fail", test_l4_fail, should_fail=.true.), &
      new_unittest("logical-default-message", test_l4_message, should_fail=.true.), &
      new_unittest("logical-default-with-more", test_l4_with_more, should_fail=.true.), &
      new_unittest("character", test_char), &
      new_unittest("character-fail", test_char_fail, should_fail=.true.), &
      new_unittest("character-message", test_char_message, should_fail=.true.), &
      new_unittest("character-with-more", test_char_with_more, should_fail=.true.), &
      new_unittest("character-with-more", test_char_with_more, should_fail=.true.), &
      new_unittest("string-i1", test_string_i1), &
      new_unittest("string-i2", test_string_i2), &
      new_unittest("string-i4", test_string_i4), &
      new_unittest("string-i8", test_string_i8) &
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


  subroutine test_failure_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    call check(error, 4, "Custom message describing the error")

  end subroutine test_failure_message


  subroutine test_failure_with_more(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    call check(error, 3, more="with an additional descriptive message here")

  end subroutine test_failure_with_more


  subroutine test_skipped(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    call skip_test(error, "This test is always skipped")

  end subroutine test_skipped


  subroutine test_expression(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    call check(error, index("info!", "!") > 0)

  end subroutine test_expression


  subroutine test_expression_fail(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    call check(error, index("info!", "?") > 0)

  end subroutine test_expression_fail


  subroutine test_expression_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    call check(error, index("info!", "!") == 0, 'index("info!", "!") == 0')

  end subroutine test_expression_message


  subroutine test_expression_with_more(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    call check(error, index("info!", "?") /= 0, more='index("info!", "?")')

  end subroutine test_expression_with_more


  subroutine test_rsp_abs(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    real(sp) :: val

    val = 3.3_sp

    call check(error, val, 3.3_sp, thr=sqrt(epsilon(val)))

  end subroutine test_rsp_abs


  subroutine test_rsp_nan(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    real(sp) :: val

    val = ieee_value(val, ieee_quiet_nan)

    call check(error, val, 3.3_sp, rel=.true.)

  end subroutine test_rsp_nan


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


  subroutine test_rsp_abs_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    real(sp) :: val

    val = 1.0_sp

    call check(error, val, 1.5_sp, message="Actual value is not 1.5")

  end subroutine test_rsp_abs_message


  subroutine test_rsp_nan_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    real(sp) :: val

    val = ieee_value(val, ieee_quiet_nan)

    call check(error, val, message="Actual value is not a number")

  end subroutine test_rsp_nan_message


  subroutine test_rdp_abs(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    real(dp) :: val

    val = 3.3_dp

    call check(error, val, 3.3_dp, thr=sqrt(epsilon(val)))

  end subroutine test_rdp_abs


  subroutine test_rdp_rel(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    real(dp) :: val

    val = 3.3_dp

    call check(error, val, 3.3_dp, rel=.true.)

  end subroutine test_rdp_rel


  subroutine test_rdp_nan(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    real(dp) :: val

    val = ieee_value(val, ieee_quiet_nan)

    call check(error, val, 3.3_dp, rel=.true.)

  end subroutine test_rdp_nan


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


  subroutine test_rdp_abs_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    real(dp) :: val

    val = 1.0_dp

    call check(error, val, 1.5_dp, message="Actual value is not 1.5")

  end subroutine test_rdp_abs_message


  subroutine test_rdp_nan_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    real(dp) :: val

    val = ieee_value(val, ieee_quiet_nan)

    call check(error, val, message="Actual value is not a number")

  end subroutine test_rdp_nan_message


  subroutine test_rxdp_abs(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_XDP
    real(xdp) :: val

    val = 3.3_xdp

    call check(error, val, 3.3_xdp, thr=sqrt(epsilon(val)))
#else
    call skip_test(error, "Extended double precision is not enabled")
#endif

  end subroutine test_rxdp_abs


  subroutine test_rxdp_rel(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_XDP
    real(xdp) :: val

    val = 3.3_xdp

    call check(error, val, 3.3_xdp, rel=.true.)
#else
    call skip_test(error, "Extended double precision is not enabled")
#endif

  end subroutine test_rxdp_rel


  subroutine test_rxdp_nan(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_XDP
    real(xdp) :: val

    val = ieee_value(val, ieee_quiet_nan)

    call check(error, val, 3.3_xdp, rel=.true.)
#else
    call skip_test(error, "Extended double precision is not enabled")
#endif

  end subroutine test_rxdp_nan


  subroutine test_rxdp_abs_fail(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_XDP
    real(xdp) :: val

    val = 1.0_xdp

    call check(error, val, 2.0_xdp)
#else
    call skip_test(error, "Extended double precision is not enabled")
#endif

  end subroutine test_rxdp_abs_fail


  subroutine test_rxdp_rel_fail(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_XDP
    real(xdp) :: val

    val = 1.0_xdp

    call check(error, val, 1.5_xdp, rel=.true.)
#else
    call skip_test(error, "Extended double precision is not enabled")
#endif

  end subroutine test_rxdp_rel_fail


  subroutine test_rxdp_abs_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_XDP
    real(xdp) :: val

    val = 1.0_xdp

    call check(error, val, 1.5_xdp, message="Actual value is not 1.5")
#else
    call skip_test(error, "Extended double precision is not enabled")
#endif

  end subroutine test_rxdp_abs_message


  subroutine test_rxdp_nan_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_XDP
    real(xdp) :: val

    val = ieee_value(val, ieee_quiet_nan)

    call check(error, val, message="Actual value is not a number")
#else
    call skip_test(error, "Extended double precision is not enabled")
#endif

  end subroutine test_rxdp_nan_message


  subroutine test_rqp_abs(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_QP
    real(qp) :: val

    val = 3.3_qp

    call check(error, val, 3.3_qp, thr=sqrt(epsilon(val)))
#else
    call skip_test(error, "Quadruple precision is not enabled")
#endif

  end subroutine test_rqp_abs


  subroutine test_rqp_rel(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_QP
    real(qp) :: val

    val = 3.3_qp

    call check(error, val, 3.3_qp, rel=.true.)
#else
    call skip_test(error, "Quadruple precision is not enabled")
#endif

  end subroutine test_rqp_rel


  subroutine test_rqp_nan(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_QP
    real(qp) :: val

    val = ieee_value(val, ieee_quiet_nan)

    call check(error, val, 3.3_qp, rel=.true.)
#else
    call skip_test(error, "Quadruple precision is not enabled")
#endif

  end subroutine test_rqp_nan


  subroutine test_rqp_abs_fail(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_QP
    real(qp) :: val

    val = 1.0_qp

    call check(error, val, 2.0_qp)
#else
    call skip_test(error, "Quadruple precision is not enabled")
#endif

  end subroutine test_rqp_abs_fail


  subroutine test_rqp_rel_fail(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_QP
    real(qp) :: val

    val = 1.0_qp

    call check(error, val, 1.5_qp, rel=.true.)
#else
    call skip_test(error, "Quadruple precision is not enabled")
#endif

  end subroutine test_rqp_rel_fail


  subroutine test_rqp_abs_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_QP
    real(qp) :: val

    val = 1.0_qp

    call check(error, val, 1.5_qp, message="Actual value is not 1.5")
#else
    call skip_test(error, "Quadruple precision is not enabled")
#endif

  end subroutine test_rqp_abs_message


  subroutine test_rqp_nan_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_QP
    real(qp) :: val

    val = ieee_value(val, ieee_quiet_nan)

    call check(error, val, message="Actual value is not a number")
#else
    call skip_test(error, "Quadruple precision is not enabled")
#endif

  end subroutine test_rqp_nan_message


  subroutine test_csp_abs(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    complex(sp) :: val

    val = cmplx(3.3_sp, 1.0_sp, sp)

    call check(error, val, cmplx(3.3_sp, 1.0_sp, sp), thr=sqrt(epsilon(abs(val))))

  end subroutine test_csp_abs


  subroutine test_csp_nan(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    complex(sp) :: val

    val = cmplx(ieee_value(0.0_sp, ieee_quiet_nan), &
      & ieee_value(0.0_sp, ieee_quiet_nan), sp)

    call check(error, val, cmplx(3.3_sp, 1.0_sp, sp), rel=.true.)

  end subroutine test_csp_nan


  subroutine test_csp_rel(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    complex(sp) :: val

    val = cmplx(3.3_sp, 1.0_sp, sp)

    call check(error, val, cmplx(3.3_sp, 1.0_sp, sp), rel=.true.)

  end subroutine test_csp_rel


  subroutine test_csp_abs_fail(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    complex(sp) :: val

    val = cmplx(1.0_sp, 2.0_sp, sp)

    call check(error, val, cmplx(2.0_sp, 1.0_sp, sp))

  end subroutine test_csp_abs_fail


  subroutine test_csp_rel_fail(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    complex(sp) :: val

    val = cmplx(1.0_sp, 1.5_sp, sp)

    call check(error, val, cmplx(1.5_sp, 1.0_sp, sp), rel=.true.)

  end subroutine test_csp_rel_fail


  subroutine test_csp_abs_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    complex(sp) :: val

    val = cmplx(1.0_sp, 1.5_sp, sp)

    call check(error, val, cmplx(1.5_sp, 1.0_sp, sp), message="Actual value is not 1.5+1.0i")

  end subroutine test_csp_abs_message


  subroutine test_csp_nan_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    complex(sp) :: val

    val = cmplx(ieee_value(0.0_sp, ieee_quiet_nan), 0.0_sp, sp)

    call check(error, val, message="Actual value is not a number")

  end subroutine test_csp_nan_message


  subroutine test_cdp_abs(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    complex(dp) :: val

    val = cmplx(3.3_dp, 1.0_dp, dp)

    call check(error, val, cmplx(3.3_dp, 1.0_dp, dp), thr=sqrt(epsilon(real(val))))

  end subroutine test_cdp_abs


  subroutine test_cdp_rel(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    complex(dp) :: val

    val = cmplx(3.3_dp, 1.0_dp, dp)

    call check(error, val, cmplx(3.3_dp, 1.0_dp, dp), rel=.true.)

  end subroutine test_cdp_rel


  subroutine test_cdp_nan(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    complex(dp) :: val

    val = cmplx(ieee_value(0.0_dp, ieee_quiet_nan), 0.0_dp, dp)

    call check(error, val, cmplx(3.3_dp, 1.0_dp, dp), rel=.true.)

  end subroutine test_cdp_nan


  subroutine test_cdp_abs_fail(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    complex(dp) :: val

    val = cmplx(1.0_dp, 2.0_dp, dp)

    call check(error, val, cmplx(2.0_dp, 1.0_dp, dp))

  end subroutine test_cdp_abs_fail


  subroutine test_cdp_rel_fail(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    complex(dp) :: val

    val = cmplx(1.0_dp, 1.5_dp, dp)

    call check(error, val, cmplx(1.5_dp, 1.0_dp, dp), rel=.true.)

  end subroutine test_cdp_rel_fail


  subroutine test_cdp_abs_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    complex(dp) :: val

    val = cmplx(1.0_dp, 1.5_dp, dp)

    call check(error, val, cmplx(1.5_dp, 1.0_dp, dp), message="Actual value is not 1.5+1.0i")

  end subroutine test_cdp_abs_message


  subroutine test_cdp_nan_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    complex(dp) :: val

    val = cmplx(ieee_value(0.0_dp, ieee_quiet_nan), 0.0_dp, dp)

    call check(error, val, message="Actual value is not a number")

  end subroutine test_cdp_nan_message


  subroutine test_cxdp_abs(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_XDP
    complex(xdp) :: val

    val = cmplx(3.3_xdp, 1.0_xdp, xdp)

    call check(error, val, cmplx(3.3_xdp, 1.0_xdp, xdp), thr=sqrt(epsilon(real(val))))
#else
    call skip_test(error, "Extended double precision is not enabled")
#endif

  end subroutine test_cxdp_abs


  subroutine test_cxdp_rel(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_XDP
    complex(xdp) :: val

    val = cmplx(3.3_xdp, 1.0_xdp, xdp)

    call check(error, val, cmplx(3.3_xdp, 1.0_xdp, xdp), rel=.true.)
#else
    call skip_test(error, "Extended double precision is not enabled")
#endif

  end subroutine test_cxdp_rel


  subroutine test_cxdp_nan(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_XDP
    complex(xdp) :: val

    val = cmplx(ieee_value(0.0_xdp, ieee_quiet_nan), 0.0_xdp, xdp)

    call check(error, val, cmplx(3.3_xdp, 1.0_xdp, xdp), rel=.true.)
#else
    call skip_test(error, "Extended double precision is not enabled")
#endif

  end subroutine test_cxdp_nan


  subroutine test_cxdp_abs_fail(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_XDP
    complex(xdp) :: val

    val = cmplx(1.0_xdp, 2.0_xdp, xdp)

    call check(error, val, cmplx(2.0_xdp, 1.0_xdp, xdp))
#else
    call skip_test(error, "Extended double precision is not enabled")
#endif

  end subroutine test_cxdp_abs_fail


  subroutine test_cxdp_rel_fail(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_XDP
    complex(xdp) :: val

    val = cmplx(1.0_xdp, 1.5_xdp, xdp)

    call check(error, val, cmplx(1.5_xdp, 1.0_xdp, xdp), rel=.true.)
#else
    call skip_test(error, "Extended double precision is not enabled")
#endif

  end subroutine test_cxdp_rel_fail


  subroutine test_cxdp_abs_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_XDP
    complex(xdp) :: val

    val = cmplx(1.0_xdp, 1.5_xdp, xdp)

    call check(error, val, cmplx(1.5_xdp, 1.0_xdp, xdp), message="Actual value is not 1.5+1.0i")
#else
    call skip_test(error, "Extended double precision is not enabled")
#endif

  end subroutine test_cxdp_abs_message


  subroutine test_cxdp_nan_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_XDP
    complex(xdp) :: val

    val = cmplx(ieee_value(0.0_xdp, ieee_quiet_nan), 0.0_xdp, xdp)

    call check(error, val, message="Actual value is not a number")
#else
    call skip_test(error, "Extended double precision is not enabled")
#endif

  end subroutine test_cxdp_nan_message


  subroutine test_cqp_abs(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_QP
    complex(qp) :: val

    val = cmplx(3.3_qp, 1.0_qp, qp)

    call check(error, val, cmplx(3.3_qp, 1.0_qp, qp), thr=sqrt(epsilon(real(val))))
#else
    call skip_test(error, "Quadruple precision is not enabled")
#endif

  end subroutine test_cqp_abs


  subroutine test_cqp_rel(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_QP
    complex(qp) :: val

    val = cmplx(3.3_qp, 1.0_qp, qp)

    call check(error, val, cmplx(3.3_qp, 1.0_qp, qp), rel=.true.)
#else
    call skip_test(error, "Quadruple precision is not enabled")
#endif

  end subroutine test_cqp_rel


  subroutine test_cqp_nan(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_QP
    complex(qp) :: val

    val = cmplx(ieee_value(0.0_qp, ieee_quiet_nan), 0.0_qp, qp)

    call check(error, val, cmplx(3.3_qp, 1.0_qp, qp), rel=.true.)
#else
    call skip_test(error, "Quadruple precision is not enabled")
#endif

  end subroutine test_cqp_nan


  subroutine test_cqp_abs_fail(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_QP
    complex(qp) :: val

    val = cmplx(1.0_qp, 2.0_qp, qp)

    call check(error, val, cmplx(2.0_qp, 1.0_qp, qp))
#else
    call skip_test(error, "Quadruple precision is not enabled")
#endif

  end subroutine test_cqp_abs_fail


  subroutine test_cqp_rel_fail(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_QP
    complex(qp) :: val

    val = cmplx(1.0_qp, 1.5_qp, qp)

    call check(error, val, cmplx(1.5_qp, 1.0_qp, qp), rel=.true.)
#else
    call skip_test(error, "Quadruple precision is not enabled")
#endif

  end subroutine test_cqp_rel_fail


  subroutine test_cqp_abs_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_QP
    complex(qp) :: val

    val = cmplx(1.0_qp, 1.5_qp, qp)

    call check(error, val, cmplx(1.5_qp, 1.0_qp, qp), message="Actual value is not 1.5+1.0i")
#else
    call skip_test(error, "Quadruple precision is not enabled")
#endif

  end subroutine test_cqp_abs_message


  subroutine test_cqp_nan_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

#if WITH_QP
    complex(qp) :: val

    val = cmplx(ieee_value(0.0_qp, ieee_quiet_nan), 0.0_qp, qp)

    call check(error, val, message="Actual value is not a number")
#else
    call skip_test(error, "Quadruple precision is not enabled")
#endif

  end subroutine test_cqp_nan_message


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

    call check(error, val, -4_i1)

  end subroutine test_i1_fail


  subroutine test_i1_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer(i1) :: val

    val = -3_i1

    call check(error, val, 7_i1, "Actual value is not seven")

  end subroutine test_i1_message


  subroutine test_i1_with_more(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer(i1) :: val

    val = 0_i1

    call check(error, val, 3_i1, more="with an additional descriptive message here")

  end subroutine test_i1_with_more


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

    call check(error, val, -4_i2)

  end subroutine test_i2_fail


  subroutine test_i2_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer(i2) :: val

    val = -3_i2

    call check(error, val, 7_i2, "Actual value is not seven")

  end subroutine test_i2_message


  subroutine test_i2_with_more(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer(i2) :: val

    val = 0_i2

    call check(error, val, 3_i2, more="with an additional descriptive message here")

  end subroutine test_i2_with_more


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

    call check(error, val, -4_i4)

  end subroutine test_i4_fail


  subroutine test_i4_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer(i4) :: val

    val = -3_i4

    call check(error, val, 7_i4, "Actual value is not seven")

  end subroutine test_i4_message


  subroutine test_i4_with_more(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer(i4) :: val

    val = 0_i4

    call check(error, val, 3_i4, more="with an additional descriptive message here")

  end subroutine test_i4_with_more


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

    call check(error, val, -4_i8)

  end subroutine test_i8_fail


  subroutine test_i8_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer(i8) :: val

    val = -3_i8

    call check(error, val, 7_i8, "Actual value is not seven")

  end subroutine test_i8_message


  subroutine test_i8_with_more(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer(i8) :: val

    val = 0_i8

    call check(error, val, 3_i8, more="with an additional descriptive message here")

  end subroutine test_i8_with_more


  subroutine test_l4_true(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    call check(error, .true., .true.)

  end subroutine test_l4_true


  subroutine test_l4_false(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    call check(error, .false., .false.)

  end subroutine test_l4_false


  subroutine test_l4_fail(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    call check(error, .true., .false.)

  end subroutine test_l4_fail


  subroutine test_l4_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    call check(error, .false., .true., "Logical value is not true")

  end subroutine test_l4_message


  subroutine test_l4_with_more(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    call check(error, .true., .false., more="with an additional descriptive message")

  end subroutine test_l4_with_more


  subroutine test_char(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    character(len=:), allocatable :: val

    val = "positive"

    call check(error, val, "positive")

  end subroutine test_char


  subroutine test_char_fail(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    character(len=:), allocatable :: val

    val = "positive"

    call check(error, val, "negative")

  end subroutine test_char_fail


  subroutine test_char_message(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    character(len=:), allocatable :: val

    val = "positive"

    call check(error, val, "negative", "Character string should be negative")

  end subroutine test_char_message


  subroutine test_char_with_more(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    character(len=:), allocatable :: val

    val = "positive"

    call check(error, val, "negative", more="with an additional descriptive message")

  end subroutine test_char_with_more


  subroutine test_string_i1(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    call check(error, to_string(-huge(1_i1) - 1_i1), "-128")
  end subroutine test_string_i1


  subroutine test_string_i2(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    call check(error, to_string(-huge(1_i2) - 1_i2), "-32768")
  end subroutine test_string_i2


  subroutine test_string_i4(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    call check(error, to_string(-huge(1_i4) - 1_i4), "-2147483648")
  end subroutine test_string_i4


  subroutine test_string_i8(error)

    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    call check(error, to_string(-huge(1_i8) - 1_i8), "-9223372036854775808")
  end subroutine test_string_i8


end module test_check
