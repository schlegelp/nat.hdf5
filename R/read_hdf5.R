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
#'   Use ``inspect.hdf5`` to get an idea of the contents of a given file.
#' @param annotations Whether to load annotations associated with the neuron(s):
#'   - ``TRUE`` reads all available annotations
#'   - ``FALSE`` ignores any annotations
#'   - e.g. ``"connectors"`` reads only "connectors"
#'   Non-existing annotations are silently ignored!
#' @param strict If `TRUE`, will read only the attributes/columns which are
#'   absolutely required to construct the respective neuron representation. This
#'   is useful if you either want to keep memory usage low or if any additional
#'   attributes are causing troubles. If FALSE (default), will read every
#'   attribute and dataframe column and attach it to the neuron.
#' @param prefer.raw If `TRUE`, we will create the neurons from scratch if the
#'  raw data is available. If `FALSE` or only the serialized byte stream is
#'  available, neurons are simply unserialized (similar to `load`).
#' @param reader Which reader to use to parse the given format. By default
#'   ("auto") will try to pick the correct parser for you depending on the
#'   ``format_spec`` attribute in the Hdf5 file. You can also directly provide a
#'   function.
#' @param on.error Determines what to do if a neuron can not be parsed. By
#'   default we "stop" immediately but you can also choose to just "warn" or
#'   or "ignore" errors entirely. In the latter two cases, neurons that could
#'   not be read will simply be omitted.
#' @param ret.errors If `TRUE`, will return a list containing the `neurons` and
#'  any `errors` encountered while reading the data.
#' @param .parallel Defaults to ``auto`` which means only use parallel
#'  processing if more than 10 neurons are imported. Spawning and joining
#'  processes causes overhead and is considerably slower for imports of small
#'  numbers of neurons. Integer will be interpreted as the number of cores
#'  (otherwise defaults to ``parallel::detectCores()/2``).
#' @param ... additional arguments passed to version-specific readers
#' @seealso \code{\link{write.neurons.hdf5}}, \code{\link{inspect.hdf5}}
#'
#' @examples
#' \dontrun{
#' n=read.neurons.hdf5('/path/to/hdf5_file.h5')
#' }
read.neurons.hdf5 <- function(f,
                              read='mesh->skeleton->dotprops',
                              subset=NULL,
                              annotations=TRUE,
                              strict=FALSE,
                              prefer.raw=FALSE,
                              reader='auto',
                              on.error=c('stop', 'warn', 'ignore'),
                              ret.errors=FALSE,
                              .parallel='auto', ...) {
  on.error = match.arg(on.error)

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
                    hnf_v1=read.neurons.hdf5.v1)
  } else if (!is.function(reader)) {
    stop('`reader` must be "auto" or a function')
  }

  if (is.null(reader)){
    stop("No reader found for format ", info$format_spec)
  }

  reader(f,
         read=read,
         subset=subset,
         annotations=annotations,
         strict=strict,
         prefer.raw=prefer.raw,
         on.error=on.error,
         ret.errors=ret.errors,
         .parallel=.parallel,
         ...)
}


# hidden
# Reads version 1 of the schema
#' @rdname read.neurons.hdf5
read.neurons.hdf5.v1 <- function(f,
                                 read='mesh->skeleton->dotprops',
                                 subset=NULL,
                                 annotations=TRUE,
                                 strict=FALSE,
                                 prefer.raw=FALSE,
                                 reader='auto',
                                 on.error=c('stop', 'warn', 'ignore'),
                                 ret.errors=FALSE,
                                 .parallel='auto', ...) {
  on.error = match.arg(on.error)

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

  # Only use parallel processing if we have more than 10 neurons
  if (.parallel == 'auto'){
    if (length(subset) <= 10){
      .parallel = F
    } else {
      .parallel = T
    }
  }

  # If .parallel is not a number
  if (.parallel == TRUE){
    # Use half of the available cores if not specified
    ncores = as.integer(parallel::detectCores() / 2)
  } else if (.parallel == FALSE){
    # No parallel processing = 1 core
    ncores = 1L
  } else {
    # Make sure .parallel is a number
    ncores = as.integer(.parallel)
  }

  # Load neurons
  # pbmclapply wraps mclapply with a progress bar
  # mclapply in turn wraps lapply for multi-processing
  #nl = pbmcapply::pbmclapply(subset,
  #                           FUN=read.neuron.hdf5.v1,
  #                           f=f,
  #                           annotations=annotations,
  #                           read=read,
  #                           strict=strict,
  #                           mc.cores=ncores,
  #                           mc.silent=F,
  #                           mc.preschedule=T
  #                           )
  # pbmclapply wraps mclapply with a progress bar
  # mclapply in turn wraps lapply for multi-processing
  #

  # Max sets the max number of neurons that are processed per chunk
  mx = ceiling(length(subset)/ncores)
  # For small requests make sure that we don't have ridiculously small chunks
  mx = max(mx, 50)
  # For really big requests make sure that we can update the progress bar in
  # reasonable intervals
  mx = min(mx, 300)

  # Do the actual splitting
  y <- seq_along(subset)
  chunks <- split(subset, ceiling(y/mx))

  # Read in parallel chunks
  # `res` that contain for each chunk a list of `neurons` and `errors`
  res = pbmcapply::pbmclapply(chunks,
                              FUN=read.neurons.hdf5.v1.seq,
                              f=f,
                              annotations=annotations,
                              read=read,
                              strict=strict,
                              prefer.raw=prefer.raw,
                              on.error=on.error,
                              mc.cores=ncores,
                              mc.silent=F,
                              mc.preschedule=T
                              )

  # Parse errors and neurons from the list
  nl = NULL
  errors = NULL
  for (i in 1:length(res)){
    nl = c(nl, res[[i]]$neurons)
    errors = c(errors, res[[i]]$errors)
  }

  if (on.error == 'warn'){
    if (length(errors) != 0){
      warning(length(errors),
              " of ",
              length(nl) + length(errors),
              " representation could not be parsed.")
    }
  }

  # Return the neuronlist
  # Note that `nl` is a list of lists of neurons (e.g. a mesh AND a skeleton
  # for a given neuron) -> we need to unlist
  nl = nat::as.neuronlist(nl, recursive=F)

  names(nl) <- subset

  if (!ret.errors){
    return(nl)
  } else {
    return(list(neurons=nl, errors=errors))
  }
}


# hidden
# Read a single neuron from given file
# typically called from read.neurons.hdf5.v1 using parallel processes
read.neuron.hdf5.v1 <- function(n, f, annotations, read, strict, ...){
  # Open the file
  file.h5 <- hdf5r::H5File$new(f, mode = "r")

  # Open this neuron's group (hdf5 groups are like folders)
  grp = hdf5r::openGroup(file.h5, n)

  # Get neuron-level attributes
  nattrs = hdf5r::h5attributes(grp)
  # Drop un-expected attributes if strict
  if (strict) {
    nattrs = nattrs[names(nattrs) %in% c("neuron_name", 'units_nm')]
  }

  # Load annotations (if present and requested)
  # if not requested (i.e. `annotations=F`)
  # or no annotations present, `this_an` will be an empty list
  this_an = read.neuron.hdf5.v1.annotations(grp,
                                            annotations=annotations)

  # Go over the requested representations
  # (e.g "dotprops,skeleton" = dotprops AND skeletons)
  nl = list()
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
        # representation (i.e. skeleton, mesh or dotprops) level
        toset = nattrs[!names(nattrs) %in% names(neuron)]
        neuron[names(toset)] = toset

        # ID always comes from the group
        neuron$id = n

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
  # Close file again
  file.h5$close_all()
  # Return nl
  nl
}


# hidden
# This is the version that loads neurons sequentially
# Typically called by mclapply to read chunks of neurons in parallel processes
#' @rdname read.neuron.hdf5
read.neurons.hdf5.v1.seq <- function(f,
                                     read='mesh->skeleton->dotprops',
                                     subset=NULL,
                                     annotations=TRUE,
                                     strict=FALSE,
                                     prefer.raw=FALSE,
                                     reader='auto',
                                     on.error=c('raise', 'warn', 'ignore'),
                                    .parallel='auto', ...) {

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
  errs = list()
  pb <- progress::progress_bar$new(total = length(subset))
  for (n in subset) {
    # Open this neuron's group
    grp = hdf5r::openGroup(file.h5, n)

    # Get neuron-level attributes
    nattrs = hdf5r::h5attributes(grp)
    # Drop unexpected attributes if strict
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
          if (on.error != 'stop'){
            # Try parsing this neuron and keep track of errors if fails
            neuron = NULL
            e = tryCatch({neuron = f(grp,
                                     strict=strict,
                                     prefer.raw=prefer.raw)
                          NULL},
                         error = function(cond) conditionMessage(cond))
            if (!is.null(e)){
              errs[[n]][[pr]] = e
            }
          } else {
            neuron = f(grp, strict=strict)
          }

          if (!is.null(neuron)){
            # Add neuron-level attributes unless they have been set at
            # representation (i.e. skeleton, mesh or dotprop) level
            toset = nattrs[!names(nattrs) %in% names(neuron)]
            neuron[names(toset)] = toset
            # ID always comes from the group
            neuron$id = n
            # Add annotations
            if (length(this_an)){
              neuron[names(this_an)] = this_an
            }
            # Attach it to list of neurons
            nl = c(nl, list(neuron))
            # We are in the priority loop - break out if we found what we wanted
          }
          break
        }
      }
    }
  pb$tick()
  }
  # Close file
  file.h5$close_all()

  # Return neuronlist and errors
  list(neurons=nat::as.neuronlist(nl),
       errors=errs)
}


# hidden
# Read skeleton from given Hdf5 group into a nat neuron
read.neuron.hdf5.v1.skeleton <- function(grp, strict=F, prefer.raw=F){
  # Get skeleton group from the base neuron group
  skgrp = hdf5r::openGroup(grp, "skeleton")

  # Get skeleton-level attributes
  skattrs = hdf5r::h5attributes(skgrp)

  # Read the serialized representation if it exists
  if (!is.null(skattrs$.serialized_nat)){
    if (!prefer.raw | !"node_id" %in% names(skgrp)){
      return(unserialize(charToRaw(skattrs[['.serialized_nat']])))
    }
  }

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

  # Note to self: not sure this is the correct way to annotate the soma
  # but nat:::has_soma requires this tag
  if (!is.null(sp)){
    n$tags$soma = sp
  }

  # Add other attributes
  n[names(skattrs)] = skattrs

  # Connectors are expected to be 0 if not present
  # -> this might be replaced later
  n$connectors = NULL

  n
}


# hidden
# Read mesh from given Hdf5 group into a nat mesh3d
read.neuron.hdf5.v1.mesh <- function(grp, strict=F, prefer.raw=F){
  # Get mesh group from the base neuron grp
  megrp = hdf5r::openGroup(grp, "mesh")

  # Get mesh-level attributes
  meattrs = hdf5r::h5attributes(megrp)

  # Read the serialized representation if it exists
  if (!is.null(meattrs$.serialized_nat)){
    if (!prefer.raw | !"vertices" %in% names(megrp)){
      return(unserialize(charToRaw(meattrs[['.serialized_nat']])))
    }
  }

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
# Read dotprops from given Hdf5 group into a nat dotprops
read.neuron.hdf5.v1.dotprops <- function(grp, strict=F, prefer.raw=F){
  # Get dotprops group from the base neuron grp
  dpgrp = hdf5r::openGroup(grp, "dotprops")

  # Get dotprops-level attributes
  dpattrs = hdf5r::h5attributes(dpgrp)

  # Read the serialized representation if it exists
  if (!is.null(dpattrs$.serialized_nat)){
    if (!prefer.raw | !"points" %in% names(dpgrp)){
      return(unserialize(charToRaw(dpattrs[['.serialized_nat']])))
    }
  }

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

  # Add other attributes -> this includes `k` and pot. soma position
  rlist[names(dpattrs)] = dpattrs

  # Note that `k` needs to be an attribute and not directly attached
  attr(rlist, 'k') <- dpattrs$k

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
  nat::as.dotprops(rlist)
}


# hidden
# Read annotations from given Hdf5 group into a single dataframe
read.neuron.hdf5.v1.annotations <- function(grp, annotations=T){
  # If no annotations requested or no annotations present return empty list
  if (is.null(annotations)){
    return(list())
  }
  if (annotations == F){
    return(list())
  }
  if (!grp$exists("annotations")){
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
# Helper function to parse all data sets in a given Hdf5 group into a single
# dataframe. This expects each dataset to be a 1d vector
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
