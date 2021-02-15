<!-- badges: start -->
[![R-CMD-check](https://github.com/schlegelp/nat.hdf5/workflows/R-CMD-check/badge.svg)](https://github.com/schlegelp/nat.hdf5/actions)
<!-- badges: end -->

# nat.hdf5
A library to reading/writing `nat` neuron data from/to Hdf5 files. The schema
for the "Hierarchical Neuron Format" (HNF) is under active development and can  
be found here [here](https://github.com/flyconnectome/hnf).

Ups:
- language- and library-agnostic, i.e. you can use that data from outside 
  R and the natverse 
- each neuron can have multiple representations (skeletons, dotprops, meshes)
- you can add/edit/remove individual neurons without having to rewrite the whole
  file

Downs:
- slower to read/write than R's `save`/`load` - in particular if you read from  
  raw data (see benchmark below)
- larger files compared to `save`/`load`

# Done
- [x] function to inspect Hdf5 file: `inspect.hdf5`
- [x] function to read neurons from Hdf5 file: `read.neurons.hdf5`
- [x] parallel reading
- [x] function to write neurons from Hdf5 file
- [x] optionally write neurons as serialized byte streams for faster reading

# TODOs
- [ ] read arbitrary attributes + `strict` parameter for `read.neurons.hdf5`
- [ ] function to remove select neurons from Hdf5 file
- [ ] more efficient handling of raw data with the same datatype (pool x/y/z coordinates)
- [ ] rewrite to work with either Hdf5 or a simple zip file

# Benchmark
A quick test with ~1k skeletons from the Janelia hemibrain dataset (using 6
cores for reading):
```
Reading from SWC (12Mb on disk):       ~10s

Writing to RDS (4Mb on disk):            1s
Reading from RDS:                       <1s

Writing raw to Hdf5 (19Mb on disk):     45s
Reading raw from Hdf5:                  20s

Serializing to Hdf5 (17Mb on disk):     20s
De-serializing from Hdf5:                8s
```

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
> library(nat)
> library(nat.hdf5)

> # Save 10 of the 20 kcs20 dotprops to a new Hdf5 file
> write.neurons.hdf5(kcs20[1:10], '~/Downloads/test.h5', append=F)

> # Inspect contents
> inspect.hdf5('~/Downloads/test.h5')
$format_spec
[1] "hnf_v1"

$format_url
[1] "https://github.com/schlegelp/nat.hdf5"

$neurons
 [1] "ChaMARCM-F000586_seg002" "FruMARCM-F000270_seg001" "FruMARCM-F001115_seg002" "FruMARCM-M001051_seg002" "FruMARCM-M001205_seg002"
 [6] "FruMARCM-M001339_seg001" "GadMARCM-F000050_seg001" "GadMARCM-F000122_seg001" "GadMARCM-F000142_seg002" "GadMARCM-F000423_seg001"
 
> # Read neurons back
> kcs10 = read.neurons.hdf5('~/Downloads/test.h5')
> kcs10
'neuronlist' containing 10 'dotprops' objects and 'data.frame' with 0 vars [233.7 kB]

> # Add the remaining KCs to the file 
> write.neurons.hdf5(kcs20[11:20], '~/Downloads/test.h5', append=T)

> # Check contents again 
> length(inspect.hdf5('~/Downloads/test.h5')$neurons)
[1] 20

> # Read a subset of the dotprops 
> ss = read.neurons.hdf5('~/Downloads/test.h5',
                         subset=c("ChaMARCM-F000586_seg002", "FruMARCM-F000270_seg001"))
> names(ss)
[1] "ChaMARCM-F000586_seg002" "FruMARCM-F000270_seg001"

```

For details please see the help for `read.neurons.hdf5` and `write.neurons.hdf5`.
