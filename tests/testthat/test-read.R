test_that("nat.hdf5::read.neurons.hdf5 works", {
  fp = '../../data-raw/test_file.h5'

  # Read dotprops only
  dp = read.neuron.hdf5(fp, read='dotprops', annotations=F)

  # Expect neuronlist
  expect_true(is.neuronlist(dp))

  # Expect 5 neurons
  expect_equal(length(dp), 5)

  # Expect all dotprops
  expect_true(all(as.logical(lapply(dp, is.dotprops))))

  # Read meshes with annotations
  me = read.neuron.hdf5(fp, read='mesh', annotations=T)

  # Read meshes plus skeletons
  nl = read.neuron.hdf5(fp, read='mesh,skeleton', annotations=F)

  # Expect 10 neurons nows
  expect_equal(length(nl), 5)

})
