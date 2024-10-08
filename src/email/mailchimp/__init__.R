#' @export
box::use(
  ./audience[...],
  ./base_api[...],
  ./campaigns[...],
  ./custom_segmentation[...],
  ./delete[...],
  ./folders[...],
  ./images[...],
  ./segments[...],
  ./tags[...],
  ./templates[...]
)

if (is.null(box::name())) {
  box::use(./`__tests__`)
}
