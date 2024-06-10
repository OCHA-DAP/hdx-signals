## Images

Functions to support generation of images for inclusion in campaigns. Used in the
`plot()`, `plot2()`, and `map()` functions for each indicator.

### [`create_images.R`](create_images.R)

Similar to `generate_signals()`, `create_images()` is a utility function that
takes in a set of data and a function to create images for each signal, which
is then mapped across rows of the input data frame, generating, saving, and
uploading images to Mailchimp.

[`save_image.R`](save_image.R) is used to help save out images as PNG files and
then sends those up to the Mailchimp API.

### Other support functions

Other support functions for mapping are stored in the other two folders. These are
used in the indicator scripts themselves to support image creation in R, 
[maps](maps/README.md) for specific mapping functions and [plots](plot/README.md)
for all plot utilities.
