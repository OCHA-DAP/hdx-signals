## Images

Functions to support generation of images for inclusion in campaigns. Used in the
`plot()`, `plot2()`, and `map()` functions for each indicator.

### Create and save images

`create_images()` is a utility function that
takes in a set of data and a function to create images for each signal, which
is then mapped across rows of the input data frame, generating, saving, and
uploading images to Mailchimp.

[`save_image.R`](save_image.R) is used to help save out images as PNG files and
then sends those up to the Mailchimp API.

### Other support functions

Other support functions for plotting in `{ggplot2}` are stored in the other two folders.
These are used in the indicator scripts themselves to support image creation in R, 
`maps` for specific mapping functions and `plots` for all general plotting utilities.

### {gghdx}

The thematic backbone for all plotting in HDX Signals is the
[{`gghdx`}](https://github.com/OCHA-DAP/gghdx) package, which provides general color scheme,
font styling, and plotting defaults.