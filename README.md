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
* cmake >= 3.9

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
  call u%assert(1 + 2 == 3, "test 1 :: 1 + 2 == 3")
  call u%assert(3 + 4 == 8, "test 2 :: 3 + 4 == 8")
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
[fypp]: https://img.shields.io/badge/fypp-064F8C?style=for-the-badge
[fypp-url]: https://fypp.readthedocs.io/en/stable/index.html
[cmake]: https://img.shields.io/badge/Cmake-064F8C?style=for-the-badge&logo=cmake&logoColor=EEEEEE
[cmake-url]: https://cmake.org/
[fortran-shield]: https://img.shields.io/badge/Fortran-734F96?style=for-the-badge&logo=fortran&logoColor=FFFFFF
[fortran-url]: https://fortran-lang.org/

