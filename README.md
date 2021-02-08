# nat.hdf5
A library to reading/writing `nat` neuron data from/to Hdf5 files. The schema
(version 1) is currently being finalized and is documented
[here](https://github.com/schlegelp/navis/blob/master/docs/source/hdf5_format.md).

# Done
- [x] function to inspect Hdf5 file: `inspect.hdf5`
- [x] function to read neurons from Hdf5 file

# TODOs
- [ ] parallel reads
- [ ] function to write neurons from Hdf5 file

# Install

```R
# Install from Github
if (!require("remotes")) install.packages("remotes")
remotes::install_github("schlegelp/nat.hdf5")
```

# Use
```R
# Load library
library(nat.hdf5)

# TODO
...
```
