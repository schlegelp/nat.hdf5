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
  if(!file.exists(f)){
    stop("File does not exist: ", f)
  }

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


#' Remove neurons from a Hdf5 file
#'
#' @details This function will remove given neurons from an Hdf5 file.
#'   Note that due to the way Hdf5 works, deleting neurons will not reduce the
#'   file size. It does make makes that space available for use for future
#'   data though.
#'
#' @export
#' @param f Path to Hdf5 file.
#' @param which An id or list thereof flagging neurons for removal. IDs that
#'   don't exist in the file are silently ignored.
#' @param skeletons Boolean determining whether to remove skeleton
#'   representations for given neurons.
#' @param dotprops Boolean determining whether to remove dotprops
#'   representations for given neurons.
#' @param meshes Boolean determining whether to remove mesh representations for
#'   given neurons.
#' @param annotations Whether to remove annotations for given neuron.
#' @seealso \code{\link{write.neurons.hdf5}}, \code{\link{read.neurons.hdf5}},
#'   \code{\link{inspect.hdf5}}
drop.neurons.hdf5 <- function(f,
                              which,
                              skeletons=TRUE,
                              dotprops=TRUE,
                              meshes=TRUE,
                              annotations=TRUE) {
  if(!file.exists(f)){
    stop("File does not exist: ", f)
  }

  # Get info for file
  info = inspect.hdf5(f, inspect.neurons=F, inspect.annotations=F)

  # Force to str
  which = as.character(which)
  # Intersect with the neurons that are actually available
  which = intersect(which, info$neurons)
  # Complain if none left
  if (length(which) == 0) {
    stop("None of the neurons flagged for deleting appear to be in the Hdf5 file")
  }

  # Open file for read/write
  file.h5 <- hdf5r::H5File$new(f, mode = "r+")

  # Go over each neuron
  for (n in which){
    # Get this neuron's group
    grp = file.h5[[n]]

    # Get the datasets in this group
    data = names(grp)
    # Unlink datasets
    if (skeletons & "skeleton" %in% data){
      grp$link_delete("skeleton")
      data = setdiff(data, "skeleton")
    }
    if (dotprops & "dotprops" %in% data){
      grp$link_delete("dotprops")
      data = setdiff(data, "dotprops")
    }
    if (meshes & "mesh" %in% data){
      grp$link_delete("mesh")
      data = setdiff(data, "mesh")
    }
    if (annotations & "annotations" %in% data){
      grp$link_delete("annotations")
      data = setdiff(data, "annotations")
    }

    # Close the group
    grp$close()

    # If no more data, also unlink the group itself
    if (length(data) == 0){
      file.h5$link_delete(n)
    }
  }
}
