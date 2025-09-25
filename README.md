# bluecatpowR
Repository holding R code and utils for Blue Cat team



## Installation

To install this package use devtools
```
devtools::install_github("https://github.com/VUKOZ-OEL/bluecatpowR")
```

## Adding new functions

Feel free to add new functionality as needed.
To do so just add new ***.R*** file into ***bluecatpowR/R*** directory containing your code.
All functions **must be** commented in Roxygen2 style to be added to package !
```
#' Add together two numbers
#'
#' @param x A number.
#' @param y A number.
#' @return A number.
#' @examples
#' add(1, 1)
#' add(10, 1)
add <- function(x, y) {
  x + y
}
```
Once you commit your changes into main branch automatic documentetion & namespace generation is triggered.
When finished (see progress in Actions tab) you can reinstall package. 

If added functionality rely on another package in **DESCRIPTION** file modify section:
```
Imports: dependency
```