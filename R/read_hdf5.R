#' Read neurons from a Hdf5 file
#'
#' @details This function will load \code{neurons}, \code{dotprops} and
#'   \code{meshes} objects saved in an Hdf5 file.
#'
#' @export
#' @param f Path to Hdf5 file.
#' @param read A string defining which representation(s) (mesh, skeletons and/
#'   or dotprops) to load from the file. Some illustrative examples:
#'   - 'mesh', 'skeleton' or 'dotprops' will return only the given representation
#'   - 'mesh->skeleton->dotprops' will return a mesh if the neuron has one,
#'     a skeleton if it does not and dotprops if it has neither mesh nor skeleton
#'   - 'mesh,skeleton,dotprops' will return all available representations
#'   - 'mesh,dotprops' will only return meshes and dotprops
#'   - 'mesh,skeleton->dotprops' will return the mesh and a skeleton or
#'     alternatively the dotprops\
#'   Note that neurons which have none of the requested representations are
#'   silently skipped!
#' @param subset A list If provided, will read only a subset of neurons from the
#'   file. IDs that don't exist are silently ignored. Also note that due to Hdf5
#'   restrictions numeric IDs will be automatically converted to strings.
#' @param annotations Whether to load annotations associated with the neuron(s):
#'   - ``TRUE`` reads all available annotations
#'   - ``FALSE`` ignores any annotations
#'   - e.g. ``"connenctors"`` reads only "connectors"
#'     Non-existing annotations are silently ignored!
#' @param strict If TRUE, will read only the attributes/columns which are
#'   absolutely required to construct the respective neuron representation. This
#'   is useful if you either want to keep memory usage low or if any additional
#'   attributes are causing troubles. If FALSE (default), will read every
#'   attribute and dataframe column and attach it to the neuron.
#' @param reader Which reader to use to parse the given format. By default
#'   ("auto") will try to pick the correct parser for you depending on the
#'   ``format_spec`` attribute in the Hdf5 file. You can also directly provide a
#'   function.
#' @param .parallel Defaults to ``auto`` which means only use parallel
#'  processing if more than 200 neurons are imported. Spawning and joining
#'  processes causes overhead and is considerably slower for imports of small
#'  numbers of neurons. Integer will be interpreted as the number of cores
#'  (otherwise defaults to ``parallel::detectCores()/2``).
#' @param ... additional arguments passed to version-specific readers
#' @seealso \code{\link{write.neuron.hdf5}}, \code{\link{inspect.hdf5}}
#'
#' @examples
#' \dontrun{
#' # note that we override the default NeuronName field
#' n=read.neuron(system.file("tests/testthat/testdata","neuron","EBT7R.CNG.swc",package='nat'),
#'   NeuronName="EBT7R")
#' }
read.neuron.hdf5 <- function(f,
                             read='mesh->skeleton->dotprops',
                             subset=NULL,
                             annotations=TRUE,
                             strict=FALSE,
                             reader='auto',
                             parallel='auto', ...) {

  # TODOs:
  # - parallel reading
  # - annotations

  # First make sure that the `read` string is correct
  # Drop accidental whitespaces
  read = gsub(" ", "", read)
  # Go over the requested representations
  # (e.g "dotprops,skeleton" = dotprops AND skeletons)
  for (r in strsplit(read, ',')[[1]]) {
    # Cycle through preferences
    # (e.g. "dotprops->skeleton" = dotprops or skeleton)
    for (pr in strsplit(r, '->')[[1]]) {
      if (!pr %in% c('dotprops', 'skeleton', 'mesh')) {
        stop('`read` contains irregular expression: ', pr)
      }
    }
  }

  # Get info for file
  info = inspect.hdf5(f, inspect.neurons=F, inspect.annotations=F)

  # The reader to be used depends on the format specifier in the file
  if (reader=='auto') {
    reader = switch(info$format_spec,
                    navis_hdf5_v1=read.neuron.hdf5.v1)
  } else if (!is.function(reader)) {
    stop('`reader` must be "auto" or a function')
  }

  reader(f, read=read, subset=subset, annotations=annotations, strict=strict)
}


# hidden
#' @rdname read.neuron.hdf5
read.neuron.hdf5.v1 <- function(f,
                                read='mesh->skeleton->dotprops',
                                subset=NULL,
                                annotations=TRUE,
                                strict=FALSE,
                                reader='auto',
                                parallel='auto', ...) {

  # Get info for file
  info = inspect.hdf5(f, inspect.neurons=F, inspect.annotations=F)

  # If no subset specified, load all neurons
  if (is.null(subset)) {
    subset = info$neurons
  } else {
    # Force to str
    subset = as.character(subset)
    # Intersect with the neurons that are actually available
    subset = intersect(subset, info$neurons)
    # Complain if none left
    if (length(subset) == 0) {
      stop("None of the requested neurons appear to be in the Hdf5 file")
    }
  }

  # Open the file
  file.h5 <- hdf5r::H5File$new(f, mode = "r")

  # Load neurons
  nl = list()
  pb <- progress::progress_bar$new(total = length(subset))
  for (n in subset) {
    # Open this neuron's group
    grp = hdf5r::openGroup(file.h5, n)

    # The neuron's ID is the name of its base group
    # Note that we have to drop the leading "/"
    nid = substr(grp$get_obj_name(), 2, nchar(grp$get_obj_name()))

    # Get neuron-level attributes
    nattrs = hdf5r::h5attributes(grp)
    # Drop un-expected attributes if strict
    if (strict) {
      nattrs = nattrs[names(nattrs) %in% c("neuron_name", 'units_nm')]
    }

    # Load annotations (if present and requested)
    # if not requests or not present, `this_an` will be an empty list
    this_an = read.neuron.hdf5.v1.annotations(grp,
                                              annotations=annotations)

    # Go over the requested representations
    # (e.g "dotprops,skeleton" = dotprops AND skeletons)
    for (r in strsplit(read, ',')[[1]]) {
      # Cycle through preferences
      # (e.g. "mesh->dotprops" = mesh or if not available the dotprops)
      for (pr in strsplit(r, '->')[[1]]) {
        if (pr %in% names(grp)) {
          # Get the correct function to read this representation
          f = switch(pr,
                     skeleton=read.neuron.hdf5.v1.skeleton,
                     mesh=read.neuron.hdf5.v1.mesh,
                     dotprops=read.neuron.hdf5.v1.dotprops)
          # Read the neuron
          neuron = f(grp, strict=strict)
          # Add neuron-level attributes unless they have been set at
          # representation (i.e. skeleton, mesh or dotprop) level
          toset = nattrs[!names(nattrs) %in% names(neuron)]
          neuron[names(toset)] = toset
          # ID always comes from the group
          neuron$id = nid
          # Add annotations
          if (length(this_an)){
            neuron[names(this_an)] = this_an
          }
          # Attach it to list of neurons
          nl = c(nl, list(neuron))
          # We are in the priority loop - if we found what we wanted break out
          break
        }
      }
    }
  pb$tick()
  }
  # Close file
  file.h5$close_all()

  # Return the neuronlist
  nat::as.neuronlist(nl)
}


# hidden
read.neuron.hdf5.v1.skeleton <- function(grp, strict=F){
  # Get skeleton group from the base neuron grp
  skgrp = hdf5r::openGroup(grp, "skeleton")

  # Get skeleton-level attributes
  skattrs = hdf5r::h5attributes(skgrp)
  # Drop un-expected attributes if strict
  if (strict) {
    skattrs = skattrs[names(skattrs) %in% c("neuron_name", 'units_nm')]
  }

  # Make sure all expected columns are present
  expected = c("node_id", "parent_id", "x", "y", "z")
  miss = setdiff(expected, names(skgrp))
  if (length(miss)) {
    stop("Skeleton for neuron ",
         grp$get_obj_name(),
         " is missing required column(s):",
         miss)
  }

  # Fingers crossed that all vectors have the same length
  # -> could add a check but have to see how much overhead that causes
  # Generate the SWC dataframe
  swc = data.frame(PointNo=skgrp[['node_id']][],
                   Label=0,
                   X=skgrp[['x']][],
                   Y=skgrp[['y']][],
                   Z=skgrp[['z']][],
                   W=-1,  # placeholder
                   Parent=skgrp[['parent_id']][])

  # Add optional "radius" column
  if ('radius' %in% names(skgrp)) {
    swc$W = skgrp[['radius']][] * 2
  }

  # Add soma if provided
  if ('soma' %in% names(skattrs)) {
    sp = skattrs$soma
    # 1 is the code for soma
    swc$Label[match(sp, swc$PointNo)]=1L
  } else {
    sp = NULL
  }

  # Add other columns unless strict=T
  if (!strict) {
    add_cols = setdiff(names(skgrp), c(expected, "radius"))
    if (length(add_cols)){
      for (col in add_cols){
        data = data.frame(skgrp[[col]][])
        colnames(data) <- c(col)
        swc = cbind(swc, data)
      }
    }
  }

  # Create the actual neuron
  n=nat::as.neuron(swc,
                   origin=sp,
                   InputFileName=grp$get_filename())

  # Add other attributes
  n[names(skattrs)] = skattrs

  # Connectors are expected to be 0 if not present
  # -> this might be replaced later
  n$connectors = NULL

  n
}


# hidden
read.neuron.hdf5.v1.mesh <- function(grp, strict=F){
  # Get mesh group from the base neuron grp
  megrp = hdf5r::openGroup(grp, "mesh")

  # Get mesh-level attributes
  meattrs = hdf5r::h5attributes(megrp)
  # Drop un-expected attributes if strict
  if (strict) {
    meattrs = meattrs[names(meattrs) %in% c("neuron_name", 'units_nm', 'soma')]
  }

  # Make sure all expected data are present
  expected = c("vertices", "faces")
  miss = setdiff(expected, names(megrp))
  if (length(miss)) {
    stop("Mesh for neuron ",
         grp$get_obj_name(),
         " is missing required column(s): ",
         miss)
  }

  # Generate the mesh3d
  # Note that vertex indices in the faces start at 0
  # -> we have to increment by +1
  n = rgl::tmesh3d(vertices=megrp[['vertices']][,],
                   indices=megrp[['faces']][,] + 1,
                   homogeneous=F)

  # Add other data sets unless strict=T
  if (!strict) {
    add_ds = setdiff(names(megrp), expected)
    if (length(add_ds)){
      for (ds in add_ds){
        # Note that whether we need to use [] or [,] depends on the dimensions
        # of the data set
        if (length(megrp[[ds]]$dims) == 1){
          n[ds] = megrp[[ds]][]
        } else if (length(megrp[[ds]]$dims) == 2){
          n[ds] = megrp[[ds]][,]
        } else {
          stop("We currently don't cater for datasets with ",
               length(megrp[[ds]]$dims), " dimensions")
        }
      }
    }
  }

  # Add other attributes
  n[names(meattrs)]=meattrs

  # Connectors are expected to be 0 if not present
  # -> this might be replaced later
  n$connectors=NULL

  n
}


# hidden
read.neuron.hdf5.v1.dotprops <- function(grp, strict=F){
  # Get dotprops group from the base neuron grp
  dpgrp = hdf5r::openGroup(grp, "dotprops")

  # Get dotprops-level attributes
  dpattrs = hdf5r::h5attributes(dpgrp)
  # Drop un-expected attributes if strict
  if (strict) {
    dpattrs = dpattrs[names(dpattrs) %in% c("neuron_name", 'units_nm', 'soma', 'k')]
  }

  # Make sure all expected data are present
  expected = c("points", "vect", "alpha")
  miss = setdiff(expected, names(dpgrp))
  if (length(miss)) {
    stop("Dotprops for neuron ",
         grp$get_obj_name(),
         " is missing required column(s): ",
         miss)
  }

  rlist = list(points=nat::xyzmatrix(t(dpgrp[['points']][,])),
               alpha=dpgrp[['alpha']][],
               vect=t(dpgrp[['vect']][,]))
  rlist$labels = NULL

  # Add other attributes -> this includes `k`
  rlist[names(dpattrs)] = dpattrs

  # Add other data sets unless strict=T
  if (!strict) {
    add_ds = setdiff(names(dpgrp), expected)
    if (length(add_ds)){
      for (ds in add_ds){
        # Note that whether we need to use [] or [,] depends on the dimensions
        # of the data set
        if (length(dpgrp[[ds]]$dims) == 1){
          n[ds] = dpgrp[[ds]][]
        } else if (length(dpgrp[[ds]]$dims) == 2){
          n[ds] = dpgrp[[ds]][,]
        } else {
          stop("We currently don't cater for datasets with ",
               length(dpgrp[[ds]]$dims), " dimensions")
        }
      }
    }
  }

  # Turn into dotprops
  as.dotprops(rlist)
}


# hidden
read.neuron.hdf5.v1.annotations <- function(grp, annotations=T){
  # If no annotations requested or no annotations present return empty list
  if (is.null(annotations) | annotations == F | !"annotations" %in% names(grp)) {
    return(list())
  }

  an = list()
  # Open annotations group
  angrp = hdf5r::openGroup(grp, "annotations")

  if (annotations == T) {
    # If annotation=T load all available annotations...
    annotations = names(angrp)
  } else {
    # ... else load only requested annotations
    annotations = intersect(annotations, names(angrp))
  }
  for (a in annotations) {
    an[[a]] <- group.to.dataframe(angrp[[a]])
  }
  an
}


# hidden
group.to.dataframe <- function(grp,
                               ss=NULL,
                               excl=NULL,
                               incl_attrs=F){
  # Available data sets in group
  # -> we expect each column to be a single column, i.e. a  1-d vector
  datasets = names(grp)

  # If subset of columns is provided
  if (!is.null(ss)){
    ss = intersect(ss, datasets)
  } else {
    ss = datasets
  }

  # If any columns excluded
  if (!is.null(excl)){
    ss = setdiff(ss, excl)
  }

  # Add other columns unless strict=T
  df = list()
  if (length(ss)){
    for (col in ss){
      data = data.frame(grp[[col]][])
      colnames(data) <- c(col)
      df = c(df, data)
    }
  df = data.frame(df)
  df
  }
}
