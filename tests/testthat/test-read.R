test_that("test nat.hdf5::read.neurons.hdf5", {
  # Test file contains 5 neurons that each have dotprops, mesh and a skeleton.
  # Test file is encoded was written in Python.
  fp = 'test_file.h5'

  # Read dotprops only
  dp = read.neurons.hdf5(fp, read='dotprops', annotations=F)

  # Expect neuronlist
  expect_true(nat::is.neuronlist(dp))

  # Expect 5 neurons
  expect_equal(length(dp), 5)

  # Expect all dotprops
  expect_true(all(as.logical(lapply(dp, nat::is.dotprops))))

  # Read meshes with annotations
  me = read.neurons.hdf5(fp, read='mesh', annotations=T)

  # Read meshes plus skeletons
  nl = read.neurons.hdf5(fp, read='mesh,skeleton', annotations=F)

  # Expect 10 neurons now
  expect_equal(length(nl), 10)

})
