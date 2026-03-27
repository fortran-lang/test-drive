# This file is part of test-drive.
# SPDX-Identifier: Apache-2.0 OR MIT
#
# Licensed under either of Apache License, Version 2.0 or MIT license
# at your option; you may not use this file except in compliance with
# the License.
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

option(BUILD_SHARED_LIBS "Whether the libraries built should be shared" FALSE)
option(TESTDRIVE_BUILD_TESTING "Enable testing for this project" ON)

set(
  module-dir
  "${PROJECT_NAME}/${CMAKE_Fortran_COMPILER_ID}-${CMAKE_Fortran_COMPILER_VERSION}"
)
set(module-dir "${module-dir}" PARENT_SCOPE)

# Set build type as CMake does not provide defaults
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(
    CMAKE_BUILD_TYPE "RelWithDebInfo"
    CACHE STRING "Build type to be used."
    FORCE
  )
  message(
    STATUS
    "Setting build type to '${CMAKE_BUILD_TYPE}' as none was specified."
  )
endif()

if(NOT DEFINED TESTDRIVE_WITH_QP AND DEFINED WITH_QP)
  set(TESTDRIVE_WITH_QP ${WITH_QP})
endif()
if(NOT DEFINED TESTDRIVE_WITH_XDP AND DEFINED WITH_XDP)
  set(TESTDRIVE_WITH_XDP ${WITH_XDP})
endif()
if(NOT DEFINED TESTDRIVE_WITH_IEEE_IS_NAN AND DEFINED WITH_IEEE_IS_NAN)
  set(TESTDRIVE_WITH_IEEE_IS_NAN ${WITH_IEEE_IS_NAN})
endif()

include(CheckFortranSourceCompiles)
if(NOT DEFINED TESTDRIVE_WITH_QP)
  check_fortran_source_compiles(
    "integer, parameter :: qp = selected_real_kind(33); complex(qp) :: x; end"
    TESTDRIVE_WITH_QP
    SRC_EXT "F90"
  )
  set(TESTDRIVE_WITH_QP ${TESTDRIVE_WITH_QP} PARENT_SCOPE)
endif()
if(NOT DEFINED TESTDRIVE_WITH_XDP)
  check_fortran_source_compiles(
    "
    integer, parameter :: xdp = merge(-1, selected_real_kind(18), selected_real_kind(18) == selected_real_kind(33))
    complex(xdp) :: x
    end
    "
    TESTDRIVE_WITH_XDP
    SRC_EXT "F90"
  )
  set(TESTDRIVE_WITH_XDP ${TESTDRIVE_WITH_XDP} PARENT_SCOPE)
endif()
if(NOT DEFINED TESTDRIVE_WITH_IEEE_IS_NAN)
  check_fortran_source_compiles(
    "use, intrinsic :: ieee_arithmetic, only : ieee_is_nan; logical :: x; x = ieee_is_nan(0.0); end"
    TESTDRIVE_WITH_IEEE_IS_NAN
    SRC_EXT "F90"
  )
  set(TESTDRIVE_WITH_IEEE_IS_NAN ${TESTDRIVE_WITH_IEEE_IS_NAN} PARENT_SCOPE)
endif()

include(CMakePackageConfigHelpers)
configure_package_config_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/template-config.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config.cmake"
  INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
)
write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake"
  VERSION "${PROJECT_VERSION}"
  COMPATIBILITY SameMinorVersion
)
install(
  FILES
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
)

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/template.pc"
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pc"
  @ONLY
)
install(
  FILES
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pc"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/pkgconfig"
)
