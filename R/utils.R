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
#' # Which neurons have skeletons
#' info$skeletons
#' # What annotations are associated with which neurons
#' info$annotations$connectors
#' }
inspect.hdf5 <- function(f,
                         inspect.neurons=F,
                         inspect.annotations=F, ...) {
  # Open the file
  file.h5 <- hdf5r::H5File$new(f, mode = "r")

  # Check if this is a neuron file
  if (!file.h5$attr_exists('format_spec')){
    stop("File ", f, " is missing format specifier")
  }

  # Root info about format
  info = list(format_spec = hdf5r::h5attr(file.h5, 'format_spec'),
              format_url = hdf5r::h5attr(file.h5, 'format_url'),
              neurons = names(file.h5))

  if (inspect.neurons){
    # Each group represents a neuron
    grps = names(file.h5)
    info$skeletons = list()
    info$dotprops = list()
    info$meshes = list()

    if (inspect.annotations){
      info$annotations = list()
    }

    # Open each group to check it's content
    for (n in grps){
      # Get the group
      g = file.h5[[n]]
      # Check for content
      contents = names(g)

      if ("skeleton" %in% contents){
        info$skeletons = c(info$skeletons, n)
      }
      if ("meshes" %in% contents){
        info$meshes = c(info$meshes, n)
      }
      if ("dotprops" %in% contents){
        info$dotprops = c(info$dotprops, n)
      }

      if (inspect.annotations){
        if ("annotations" %in% contents){
          this_an = names(g[['annotations']])
          for (an in this_an){
            info$annotations[[an]] = c(info$annotations[[an]], n)
          }
        }
      }
    }
    # For reasons I don't yet understand, we have to unlist at the end
    info$skeletons = unlist(info$skeletons)
    info$dotprops = unlist(info$dotprops)
    info$meshes = unlist(info$meshes)
  }

  file.h5$close_all()
  info
}
