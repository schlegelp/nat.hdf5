#' Write neurons to a Hdf5 file
#'
#' @details This function will write \code{neurons}, \code{dotprops} and
#'   \code{meshes} objects to a Hdf5 file. Inside the file, all representations
#'   for a given neuron are put into the same "group" (=folder). Critically, to
#'   uniquely identify neurons that group's name will be the neuron's ID.
#'   Consequently, neurons without where `names(x)`, `x$id` (or `x$skid` for
#'   CATMAID neurons) is `NULL` will throw an error!
#'
#' @export
#' @param x A neuron (skeleton, mesh or dotprops) or a neuronlist.
#' @param file Path to Hdf5 file to write to. Will be created if it does not
#'   exists. If file exists, we will (by default) append `n` to existing data
#'   in that file.
#' @param serialized Whether to write serialized bytes streams of the neurons.
#'  This allows for much faster reading of the data back into `nat`.
#' @param raw Whether to write neurons as raw data. This is required for reading
#'  the data from tools other than `nat`.
#' @param annotations A string or list thereof flagging additional dataframes to
#'   be saved alongside the representations. For example:
#'   - `"connectors"`: exports the connector table associated with the neuron
#'   - `c("connectors", "predictions")`: looks for two dataframes
#'   - `NULL` or `FALSE`: no annotations are exported (default)
#'   Non-existing annotations are silently skipped!
#' @param format A string specifying which format to use.
#'   By default use "latest" (currently "v1"). Note that we don't allow mixing
#'   format specs in the same HDF5 file. So if you want to write to a file which
#'   already contains data in a given format, you have to use that format.
#' @param append If `file` exists, this determines whether we append data to
#'   that file (default) or if we overwrite the entire file.
#' @param overwrite.neurons Determines what happens if the representation
#'   (i.e. skeleton, dotprops or mesh) for a given neuron already exists in
#'   the file. Set to `TRUE` to silently overwrite existing data. Set to `FALSE`
#'   (default) to stop with an error.
#' @param force.id As laid out above, we require a unique (!) ID for each
#'   neuron. If `force.id=T` any neuron without an ID will be assigned a newly
#'   generated UUID.
#' @seealso \code{\link{read.neuron.hdf5}}, \code{\link{inspect.hdf5}}
#'
#' @examples
#' \dontrun{
#' library(nat)
#' n=write.neurons.hdf5(kcs20, '/path/to/hdf5_file.h5')
#' }
write.neurons.hdf5 <- function(x,
                               file,
                               annotations=NULL,
                               serialized=TRUE,
                               raw=FALSE,
                               format=c('latest', 'v1'),
                               append=TRUE,
                               force.id=FALSE,
                               overwrite.neurons=FALSE) {
  format = match.arg(format)

  # Find the writer for given format
  writer = switch(format,
                  latest=write.neurons.hdf5.v1,
                  v1=write.neurons.hdf5.v1)

  writer(x=x, file=file,
         annotations=annotations,
         append=append,
         overwrite.neurons=overwrite.neurons,
         raw=raw,
         serialized=serialized,
         force.id=force.id)
}


# hidden
# Writes version 1 of the schema
#' @rdname write.neurons.hdf5
write.neurons.hdf5.v1 <- function(x,
                                  file,
                                  annotations=NULL,
                                  append=TRUE,
                                  overwrite.neurons=FALSE,
                                  serialized=TRUE,
                                  raw=FALSE,
                                  force.id=force.id) {
  # Make sure either `raw` or `serialized` are TRUE
  if (!raw & !serialized){
    stop("`raw` and `serialized` must not both be FALSE!")
  }

  # Force `n` to a neuron list we can iterate over
  if (!nat::is.neuronlist(x)){
    x = nat::neuronlist()
  }

  # Before we get started, make sure that each neuron has an ID:
  # For each neuron ...
  for (i in 1:length(x)){
    # ..if there is no name for this neuron in the neuronlist...
    if (is.null(names(x)[[i]])){
      # ... use $skid or ...
      if (!is.null(x[[i]]$skid)){
        names(x)[[i]] <- x[[i]]$skid
      # ... use $id ...
      } else if (!is.null(x[[i]]$id)){
        names(x)[[i]] <- x[[i]]$id
      # ... assign an ID ....
      } else if (force.id){
        names(x)[[i]] <- uuid::UUIDgenerate()
      # ... or throw a tantrum.
      } else {
        stop("At least one neuron without a unique ID (neither `names(x)`, ",
             "`$skid` nor `$id`) You have to either assign ID(s) manually or ",
             "set `force.id=T`")
      }
    }
  }

  # Double check that there aren't any duplicate IDs
  if (any(duplicated(names(x)))){
    stop("Duplicate ID(s) found: ", names(x)[duplicated(names)])
  }

  if (append){
    # If file exists, we need to check if existing data has compatible format
    if (file.exists(file)){
      info = inspect.hdf5(file, inspect.neurons=F, inspect.annotations=F)
      if ('format_spec' %in% info){
        fs = info[['format_spec']]
        if (fs != 'navis_hdf5_v1'){
          stop('file ', file, ' appears to contain data written in a format ',
               'incompatible to the version 1 schema: ', fs)
        }
      }
    }
    # Creates new file if not exists, else open in append mode
    file.h5 = hdf5r::H5File$new(file, mode="a")
  } else {
    # Creates new file if not exists, else truncates (i.e. overwrites) existing file
    file.h5 = hdf5r::H5File$new(file, mode="w")
  }

  # Write root info to file
  hdf5r::h5attr(file.h5, "format_spec") <- "navis_hdf5_v1"
  hdf5r::h5attr(file.h5, "format_url") <- "https://github.com/schlegelp/navis"

  # Go over each neuron and save it
  pb <- progress::progress_bar$new(total = length(x))
  for (i in 1:length(x)){
    # Neuron to be saved
    n = x[[i]]

    # IDs must be characters
    id = as.character(names(x)[[i]])

    # Create group if it does not exist
    if (!file.h5$exists(id)){
      grp = file.h5$create_group(id)
    } else {
      grp = file.h5[[id]]
    }

    # Write basic info about this neuron
    # NOTE: I'm not sure whether neuron names can show up in any other flavors
    if (!is.null(n$NeuronName)){
      # Strangely storing this one attribute takes ~15% of the total time
      hdf5r::h5attr(grp, "neuron_name") <- n$NeuronName
    }

    if (inherits(n, 'neuron')){
      write.neuron.hdf5.v1.skeleton(n, grp, raw=raw, serialized=serialized,
                                    overwrite.neurons=overwrite.neurons)
    } else if (inherits(n, 'dotprops')){
      write.neuron.hdf5.v1.dotprops(n, grp, raw=raw, serialized=serialized,
                                    overwrite.neurons=overwrite.neurons)
    } else if (inherits(n, 'mesh3d')){
      write.neuron.hdf5.v1.mesh(n, grp, raw=raw, serialized=serialized,
                                overwrite.neurons=overwrite.neurons)
    } else {
      stop("Don't know how to write object of class ", class(n))
    }
    pb$tick()
  }
  file.h5$close_all()
}


# hidden
# Writes a single skeleton to given neuron group
write.neuron.hdf5.v1.skeleton <- function(n, grp,
                                          serialized=T,
                                          raw=F,
                                          overwrite.neurons=F){
  # Check if there already is a skeleton
  if (grp$exists("skeleton")){
    if (!overwrite.neurons){
      stop("Neuron ", grp$get_obj_name(), " already has a skeleton. Set ",
           "`overwrite.neurons=T` to overwrite existing data.")
    }
    # Delete skeleton group
    grp$link_delete('skeleton')
  }
  skgrp = grp$create_group("skeleton")

  if (raw){
    # Write basic info to skeleton group
    if (!is.null(n$units_nm)){
      hdf5r::h5attr(skgrp, "units_nm") <- n$units_nm
    }
    if (nat:::has_soma(n)){
      hdf5r::h5attr(skgrp, "soma") <- nat::rootpoints(n)
    }

    # Write data frame columns
    skgrp[["node_id"]] <- n$d$PointNo
    skgrp[["parent_id"]] <- n$d$Parent
    skgrp[["x"]] <- n$d$X
    skgrp[["y"]] <- n$d$Y
    skgrp[["z"]] <- n$d$Z
    skgrp[["radius"]] <- n$d$W / 2  # from diameter back to radius
  }

  if (serialized){
    # Serialize into ASCII encoded characters
    chars = rawToChar(serialize(n, connection = NULL, ascii=T))

    # Write string as attribute to grp
    h5attr(skgrp, '.serialized_nat') <- chars
  }

}


# hidden
# Writes a single dotprops to given neuron group
write.neuron.hdf5.v1.dotprops <- function(n, grp, serialized=T, raw=F,
                                          overwrite.neurons=F){
  # Check if there already is a dotprops
  if (grp$exists("dotprops")){
    if (!overwrite.neurons){
      stop("Neuron ", grp$get_obj_name(), " already has a dotprops. Set ",
           "`overwrite.neurons=T` to overwrite existing data.")
    }
    # Delete dotprops group
    grp$link_delete('dotprops')
  }

  # Create dotprops grp
  dpgrp = grp$create_group("dotprops")


  if (raw){
    # Write basic info to dotprops group
    if (!is.null(n$units_nm)){
      hdf5r::h5attr(dpgrp, "units_nm") <- n$units_nm
    }
    if (!is.null(n$soma)){
      hdf5r::h5attr(dpgrp, "soma") <- n$soma
    }
    k = attr(n, "k")
    if (!is.null(k)){
      hdf5r::h5attr(dpgrp, "k") <- k
    } else {
      # I don't see a reason why we would ever not have a `k` but since this
      # is critical for the schema, better to have a check
      stop("Dotprops for neuron ", grp$get_obj_name(), " do not appear to have ",
           "a `k` attribute.")
    }

    # Write data frames
    # I'm not entirely sure why but to align with the Python implementation
    # we have to transpose the two matrices
    dpgrp[["points"]] <- t(n$points)
    dpgrp[["vect"]] <- t(n$vect)
    dpgrp[["alpha"]] <- n$alpha
  }

  if (serialized){
    # Serialize into ASCII encoded characters
    chars = rawToChar(serialize(n, connection = NULL, ascii=T))

    # Write string as attribute to grp
    h5attr(dpgrp, '.serialized_nat') <- chars
  }

}


# hidden
# Writes a single mesh to given neuron group
write.neuron.hdf5.v1.mesh <- function(n, grp, serialized=T, raw=F,
                                      overwrite.neurons=F){
  # Check if there already is a mesh
  if (grp$exists("mesh")){
    if (!overwrite.neurons){
      stop("Neuron ", grp$get_obj_name(), " already has a mesh. Set ",
           "`overwrite.neurons=T` to overwrite existing data.")
    }
    # Delete mesh group
    grp$link_delete('mesh')
  }
  # Create mesh grp
  megrp = grp$create_group("mesh")

  if (raw){
    # Write basic info to mesh group
    if (!is.null(n$units_nm)){
      hdf5r::h5attr(megrp, "units_nm") <- n$units_nm
    }
    if (!is.null(n$soma)){
      hdf5r::h5attr(megrp, "soma") <- n$soma
    }

    # Write data frames
    # Vertices are stored as (4, N) matrix in homogeneous coordinates
    # -> we drop that 4th dimension here
    megrp[["vertices"]] <- n$vb[1:3,]
    # Note that we need to re-index faces to start with vertex ID 0
    megrp[["faces"]] <- n$it - 1

    if (!is.null(n$skeleton_map)){
      megrp[["skeleton_map"]] <- n$skeleton_map
    }
  }

  if (serialized){
    # Serialize into ASCII encoded characters
    chars = rawToChar(serialize(n, connection = NULL, ascii=T))

    # Write string as attribute to grp
    h5attr(megrp, '.serialized_nat') <- chars
  }

}

