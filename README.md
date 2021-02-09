# nat.hdf5
A library to reading/writing `nat` neuron data from/to Hdf5 files. The schema
(version 1) is currently being finalized and is documented
[here](https://github.com/schlegelp/navis/blob/master/docs/source/hdf5_format.md).

# Done
- [x] function to inspect Hdf5 file: `inspect.hdf5`
- [x] function to read neurons from Hdf5 file: `read.neurons.hdf5`
- [x] parallel reading

# TODOs
- [ ] function to write neurons from Hdf5 file

# Install

```R
> # Install from Github
> if (!require("remotes")) install.packages("remotes")
> remotes::install_github("schlegelp/nat.hdf5")
```

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

> # Read all representations
> sk = read.neurons.hdf5('neurons.h5', read='skeleton,mesh,dotprops')

> # Read all skeletons
> sk = read.neurons.hdf5('neurons.h5', read='skeleton')

> # Read skeletons for a subset of neurons
> sk = read.neurons.hdf5('neurons.h5', read='skeleton',
                         subset=c('1734350788', '1734350908'))
```
