## Utilities

Email utilities in this folder are all structured as {box} modules. Any functions
exported with the `@export` roxygen tag are available for use in other modules
and in the main source code. 

Box modules can be used like library, so `library(dplyr)` can be replaced with
`box::use(dplyr)` and calls to functions in the {dplyr} namespace are then done
by using `dplyr$mutate()` rather than `dplyr::mutate()`. So, for the utilities
section, you could do:

```r
box::use(src/utils/email)
email$send_email()
```

Full functionality of the {box} package is 
[available on its website](https://klmr.me/box/index.html). The modules and
functions are fully documented within their relevant files using full {roxygen2}
documentation style.
