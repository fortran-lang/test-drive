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

project(
  'test-drive',
  'fortran',
  version: '0.5.0',
  license: 'Apache-2.0 OR MIT',
  meson_version: '>=0.53',
  default_options: [
    'buildtype=debugoptimized',
    'default_library=both',
  ],
)
install = not (meson.is_subproject() and get_option('default_library') == 'static')

# General configuration information
lib_deps = []
subdir('config')

# Collect source of the project
srcs = []
subdir('src')

# MCTC library target
testdrive_lib = library(
  meson.project_name(),
  sources: srcs,
  version: meson.project_version(),
  dependencies: lib_deps,
  install: install,
)

# Export dependency for other projects and test suite
testdrive_inc = testdrive_lib.private_dir_include()
testdrive_dep = declare_dependency(
  link_with: testdrive_lib,
  include_directories: testdrive_inc,
  dependencies: lib_deps,
)

# Package the license files
testdrive_lic = files(
  'LICENSE-Apache',
  'LICENSE-MIT',
)

if install
  # Distribute the license files in share/licenses/<name>
  install_data(
    testdrive_lic,
    install_dir: get_option('datadir')/'licenses'/meson.project_name()
  )

  module_id = meson.project_name() / fc_id + '-' + fc.version()
  meson.add_install_script(
    'config'/'install-mod.py',
    get_option('includedir') / module_id,
  )

  pkg = import('pkgconfig')
  pkg.generate(
    testdrive_lib,
    description: 'The simple testing framework',
    subdirs: ['', module_id],
  )

  cmake_data = configuration_data({
    'PROJECT_NAME': meson.project_name(),
    'PROJECT_VERSION': meson.project_version(),
    'PACKAGE_INIT': 'cmake_minimum_required(VERSION 3.9...3.31 FATAL_ERROR)',
    'CMAKE_SIZEOF_VOID_P': '8',
    'CMAKE_INSTALL_PREFIX': get_option('prefix'),
    'CMAKE_INSTALL_INCLUDEDIR': get_option('includedir'),
    'CMAKE_INSTALL_LIBDIR': get_option('libdir'),
    'TESTDRIVE_WITH_QP': with_qp ? 'ON' : 'OFF',
    'TESTDRIVE_WITH_XDP': with_xdp ? 'ON' : 'OFF',
    'TESTDRIVE_WITH_IEEE_IS_NAN': with_ieee_is_nan ? 'ON' : 'OFF',
    'module-dir': module_id,
  })
  cmake_files = [
    configure_file(
      input: files('config'/'template-config.cmake'),
      output: meson.project_name() + '-config.cmake',
      configuration: cmake_data,
    ),
    configure_file(
      input: files('config'/'template-version.cmake'),
      output: meson.project_name() + '-config-version.cmake',
      configuration: cmake_data,
    ),
    configure_file(
      input: files('config'/'template-targets.cmake'),
      output: meson.project_name() + '-targets.cmake',
      configuration: cmake_data,
    ),
  ]
  install_data(
    cmake_files,
    install_dir: get_option('libdir')/'cmake'/meson.project_name()
  )
endif

# add the testsuite
if get_option('testing').auto() ? not meson.is_subproject() : get_option('testing').enabled()
  subdir('test')
endif
