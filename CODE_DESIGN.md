# CODE DESIGN

## Style notes
- Use `package::function()` where possible
- Inside `tidyverse` functions use `.data$var` to indicate variables inside the data
and `!!var` to indicate variables external to the data.


1. data/internal.R

Any changes here have to be sourced before they're available to the rest of the
package.
