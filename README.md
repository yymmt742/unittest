<a name="readme-top"></a>

<!-- PROJECT LOGO -->
<br />
<div align="center">
<h3 align="center">unittest</h3>
  <p align="center">
    Fortran unittest
  </p>
</div>

<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>

## About The Project

[![CI](https://github.com/yymmt742/unittest/actions/workflows/ci.yml/badge.svg)]()

This project provides a unit testing framework for Fortran.
Integration with the project is possible via cmake.

[![Screen Shot][product-screenshot1]]()

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- GETTING STARTED -->
## Getting Started
### Prerequisites

[![fypp][fypp]][fypp-url]
[![cmake][cmake]][cmake-url]
[![fortran][fortran-shield]][fortran-url]

* fypp
* fortran compiler
  gfortran >= 9.4.0
* cmake >= 3.14

### Installation

1. Clone the repo
   ```sh
   git clone https://github.com/yymmt742/unittest
   ```
2. Build fortran library
   ```sh
   cmake -B build
   cmake --build build
   cmake --install build
   ```

The project's own tests are enabled by default when building this repository
directly. Set `-DBUILD_TESTING=OFF` to build only the library. When this project
is included from another CMake project, its tests are disabled by default; set
`-DUNITTEST_BUILD_TESTING=ON` to opt in.

To use an installed copy from another CMake project:

```cmake
find_package(unittest CONFIG REQUIRED)
target_link_libraries(my_target PRIVATE unittest::unittest)
```

For an in-tree dependency, add this repository with `add_subdirectory(...)` and
link the same `unittest::unittest` target. FYPP is required when building from
source; it is not needed when consuming an installed package.

`UNITTEST_MAXIMUM_RANK` controls the maximum rank generated for the general
array assertions. Raising it adds many type and rank combinations, which
increases compile time and module size. The matrix-specific eye and symmetry
assertions remain rank two.

For a smaller API and faster build, configure with
`-DUNITTEST_XD_ONLY=ON`. This keeps scalar overloads and the available XD
array procedures, and binds them under the usual generic names such as
`assert` and `assert_allclose`. Array data must be passed as a contiguous
rank-one sequence together with its shape; rank-two-and-higher direct overloads
are omitted. In this mode the XD implementations currently cover logical
`assert`, `true`/`false`, `equal`/`not_equal`, `almost_equal`/`not_almost_equal`,
the ordered comparisons, `is_zero`, `is_eye`, `is_symmetric`, and real/complex
`allclose`/`not_allclose`. In a regular build, these array methods are exposed
with an `_xd` suffix (for example, `assert_equal_xd`); in XD-only mode they are
included in their regular generic names. Debug builds always generate the full
interface set so the complete test suite is available.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- USAGE EXAMPLES -->
## Usage

The following simple test code,

```f90
program main
  use mod_unittest
  implicit none
  type(unittest) :: u
  call u%init('Simple unittests')
  call u%assert(1 + 2 == 3)
  call u%assert(3 + 4 == 8)
  call u%finish_and_terminate()
end program main
```
returns the following results

[![Screen Shot][product-screenshot0]]()

In addition to the standard group of assertion methods,
several useful methods are available to provide powerful support for numerical software development.

| subroutine                                    | arguments                                                      |
| --------------------                          | -------------------------------------------------------------- |
| assert                                        | expr (*logical*)                                               |
| assert_true / assert_false                    | expr (*logical*)                                               |
| assert_equal / assert_not_equal               | a, b (*logical*, *integer*)                                    |
| assert_almost_equal / assert_not_almost_equal | a, b (*real*, *complex*), place=7 (*integer*)                  |
| assert_less / assert_greater_equal            | a, b (*integer*, *real*, *complex*)                            |
| assert_greater / assert_less_equal            | a, b (*integer*, *real*, *complex*)                            |
| assert_allclose / assert_not_allclose         | a, b (*integer*, *real*, *complex*), rtol=1E-5 (*real*),  atol=1E-8 (*real*) |
| assert_is_zero / assert_is_not_zero           | a (*integer*, *real*, *complex*)                               |
| assert_is_eye / assert_is_not_eye             | a(:,:) (*integer*, *real*, *complex*)                          |

For some assertion methods, especially large arrays,
a simplified error heatmap is available when the test fails.

[![Screen Shot][product-screenshot2]]()

Initialize `unittest` with `tap_support=.true.` to output test results in
[tap](https://testanything.org/) format.

[![Screen Shot][product-screenshot3]]()

For detail, see [api-reference](https://yymmt742.github.io/unittest).

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE.txt` for more information.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- CONTACT -->
## Contact

YYMMT742 - yymmt@kuchem.kyoto-u.ac.jp

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- MARKDOWN LINKS & IMAGES -->
[product-screenshot0]: assets/snap_00.png
[product-screenshot1]: assets/snap_01.png
[product-screenshot2]: assets/snap_02.png
[product-screenshot3]: assets/snap_03.png
[fypp]: https://img.shields.io/badge/fypp-064F8C?style=for-the-badge
[fypp-url]: https://fypp.readthedocs.io/en/stable/index.html
[cmake]: https://img.shields.io/badge/Cmake-064F8C?style=for-the-badge&logo=cmake&logoColor=EEEEEE
[cmake-url]: https://cmake.org/
[fortran-shield]: https://img.shields.io/badge/Fortran-734F96?style=for-the-badge&logo=fortran&logoColor=FFFFFF
[fortran-url]: https://fortran-lang.org/
