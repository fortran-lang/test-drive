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

# Unit testing
set(
  tests
  "check"
  "select"
)
set(
  test-srcs
  "main.f90"
)
foreach(t IN LISTS tests)
  string(MAKE_C_IDENTIFIER ${t} t) 
  list(APPEND test-srcs "test_${t}.F90")
endforeach()

add_executable(
  "${PROJECT_NAME}-tester"
  "${test-srcs}"
)
target_link_libraries(
  "${PROJECT_NAME}-tester"
  PRIVATE
  "${PROJECT_NAME}-lib"
)
target_compile_definitions(
  "${PROJECT_NAME}-tester"
  PRIVATE
  "WITH_QP=$<BOOL:${WITH_QP}>"
  "WITH_XDP=$<BOOL:${WITH_XDP}>"
)

add_test("${PROJECT_NAME}/all-tests" "${PROJECT_NAME}-tester")

foreach(t IN LISTS tests)
  add_test("${PROJECT_NAME}/${t}" "${PROJECT_NAME}-tester" "${t}")
endforeach()
