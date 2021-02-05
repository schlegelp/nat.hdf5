#' Inspect Hdf5 file
#'
#' @details This function inspects an Hdf5 file and - if it complies with the
#'  known schema - returns info on the contained neurons.
#'
#' @export
#' @param f Path to file.
#' @param inspect.neurons Whether to run a detailed inspection of the neurons.
#'  If \code{FALSE} will only list the contained neurons. If this is \code{TRUE}
#'  the returned data will include info on whether a neuron contains meshes,
#'  dotprops and/or skeletons. Note that this is rather expensive and may take
#'  a while for files with thousands of neurons.
#' @param inspect.annotations Whether to run a detailed inspection of
#'  annotations associated with each neuron. Note that this is rather expensive
#'  and may take a while for files with thousands of neurons.
#' @seealso \code{\link{write.neuron.hdf5}}, \code{\link{read.neuron.hdf5}}
#'
#' @examples
#' \dontrun{
#' # Parse basic file info
#' info=inspect.hdf5('path/to/file.h5')
#' # `info` contains format specs and the IDs of neurons
#' info$format_spec
#' info$neurons
#' # Parse full info (slow!)
#' info.full=inspect.hdf5('path/to/file.h5', parse.neurons=T, parse.annotations=T)
#' # Does the neuron with given ID have a skeleton?
#' info$`42541231235`$skeleton
#' # What annotations are associated with that neuron
#' info$`42541231235`$annotations
#' }
inspect.hdf5 <- function(f,
                         inspect.neurons=F,
                         inspect.annotations=F, ...) {
  # Open the file
  file.h5 <- H5File$new(f, mode = "r")

  # Check if this is a neuron file
  if (!file.h5$attr_exists('format_spec')){
    stop("File ", f, " is missing format specifier")
  }

  # Root info about format
  info = list(format_spec = h5attr(file.h5, 'format_spec'),
              format_url = h5attr(file.h5, 'format_url'))

  if (inspect.neurons){
    # Each group represents a neuron
    grps = names(file.h5)
    neurons = list()
    # Open each group to check it's content
    for (n in grps){
      # Get the group
      g = file.h5[[n]]
      # Check for content
      contents = names(g)
      this <- list(skeleton="skeleton" %in% contents,
                   mesh="mesh" %in% contents,
                   dotprops="dotprops" %in% contents)
      if (inspect.annotations){
        if (!"annotations" %in% contents){
          this <- c(this, annnotations=NULL)
        } else {
          this <- c(this, annnotations=names(g[['annotations']]))
        }
      }
      # Keep a list of lists
      neurons <- append(neurons, list(this))
    }
    # Use the group names (which are the IDs) as names
    names(neurons) <- grps
    info <- append(info, list(neurons=neurons))
  } else {
    # If no detailled inspections, just track the group names (which are IDs)
    info <- append(info, list(neurons=names(file.h5)))
  }
  info
}
