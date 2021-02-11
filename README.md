# nat.hdf5
A library to reading/writing `nat` neuron data from/to Hdf5 files. The schema
(version 1) is currently being finalized and is documented
[here](https://github.com/schlegelp/navis/blob/master/docs/source/hdf5_format.md).

Ups:
- language- and library-agnostic, i.e. you can use that data from outside 
  R and the natverse 
- you can add/edit/remove individual neurons without having to rewrite the whole
  file

Downs:
- slower to read/write than R's `save`/`load` because we have to parse data and
  generate the neuron object(s) from scratch
- deleting data from an Hdf5 file does not reduce file size but frees space for 
  data added in the future which leads to pot. larger files

# Done
- [x] function to inspect Hdf5 file: `inspect.hdf5`
- [x] function to read neurons from Hdf5 file: `read.neurons.hdf5`
- [x] parallel reading
- [x] function to write neurons from Hdf5 file

# TODOs
- [ ] read arbitrary attributes + `strict` parameter for `read.neurons.hdf5`
- [ ] function to remove select neurons from Hdf5 file

# Thoughts on future directions
- optionally write neurons as serialized byte streams for faster reading
- rewrite to work with either Hdf5 or a simple zip file

# Install

```R
> # Install from Github
> if (!require("remotes")) install.packages("remotes")
> remotes::install_github("schlegelp/nat.hdf5")
```

## Requirements

For OS X and Linux the HDF5 library needs to be installed via one of the (shell) commands specified below:

| System                                    | Command
|:------------------------------------------|:---------------------------------|
|**OS X (using Homebrew)**                  | `brew install hdf5`
|**Debian-based systems (including Ubuntu)**| `sudo apt-get install libhdf5-dev` 
|**Systems supporting yum and RPMs**        | `sudo yum install hdf5-devel`

HDF5 1.8.14 has been pre-compiled for Windows and is available at
https://github.com/mannau/h5-libwin - thus no manual installation is required.

# Use
```R
> # Load library
> library(nat.hdf5)

> # Inspect contents of a file
> inspect.hdf5('neurons.h5')
$format_spec
[1] "navis_hdf5_v1"

$format_url
[1] "https://github.com/schlegelp/navis"

$neurons
[1] "1734350788" "1734350908" "722817260"  "754534424"  "754538881" 

> # Read all (available) representations
> sk = read.neurons.hdf5('neurons.h5', read='skeleton,mesh,dotprops')

> # Read all skeletons
> sk = read.neurons.hdf5('neurons.h5', read='skeleton')

> # Read skeletons for a subset of neurons
> sk = read.neurons.hdf5('neurons.h5', read='skeleton',
                         subset=c('1734350788', '1734350908'))
```
