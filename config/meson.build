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

os = host_machine.system()
fc = meson.get_compiler('fortran')
fc_id = fc.get_id()
ca = [] # compile args
la = [] # link args

if fc_id == 'gcc'
  ca += [
    '-ffree-line-length-none',
    '-fbacktrace',
  ]
  if os == 'windows'
    la += ['-Wl,--allow-multiple-definition']
  endif
elif fc_id == 'intel-cl' or fc_id == 'intel-llvm-cl'
  ca += [
    '/traceback',
    '/fpp',
  ]
elif fc_id == 'intel' or fc_id == 'intel-llvm'
  ca += [
    '-traceback',
  ]
elif fc_id == 'pgi' or fc_id == 'nvidia_hpc'
  ca += [
    '-Mbackslash',
    '-Mallocatable=03',
    '-traceback',
  ]
elif fc_id == 'flang'
  ca += [
    '-Mbackslash',
    '-Mallocatable=03',
  ]
endif

add_project_arguments(fc.get_supported_arguments(ca), language: 'fortran')
add_project_link_arguments(fc.get_supported_arguments(la), language: 'fortran')

if get_option('openmp')
  omp_dep = dependency('openmp')
  lib_deps += omp_dep
endif

if get_option('qp').auto()
  with_qp = fc.compiles('''
    integer, parameter :: qp = selected_real_kind(33)
    complex(qp) :: x
    end
  ''')
else
  with_qp = get_option('qp').allowed()
endif

if get_option('xdp').auto()
  with_xdp = fc.compiles('''
    integer, parameter :: xdp = &
      & merge(-1, selected_real_kind(18), &
      & selected_real_kind(18) == selected_real_kind(33))
    complex(xdp) :: x
    end
  ''')
else
  with_xdp = get_option('xdp').allowed()
endif

add_project_arguments(
  '-DWITH_QP=@0@'.format(with_qp.to_int()),
  '-DWITH_XDP=@0@'.format(with_xdp.to_int()),
  language: 'fortran',
)
