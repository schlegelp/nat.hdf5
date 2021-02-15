test_that("test save raw neuron data", {
  # Test writing kcs20 to temporary file
  test_filename <- tempfile(fileext = ".h5")
  # Note that we have to force IDs here
  write.neurons.hdf5(nat::kcs20, test_filename, overwrite.neurons=T,
                     serialized=F, raw=T, force.id=T)

  # Try to read back in
  nl = read.neurons.hdf5(test_filename, read='dotprops,mesh,skeleton')

  # Expect as many neurons as we have in kcs20
  expect_equal(length(nl), (length(nat::kcs20)))

  # Expect all dotprops
  expect_true(all(as.logical(lapply(nl, nat::is.dotprops))))
})


test_that("test save serialized neurons", {
  # Test writing kcs20 to temporary file
  test_filename <- tempfile(fileext = ".h5")
  # Note that we have to force IDs here
  write.neurons.hdf5(nat::kcs20, test_filename, overwrite.neurons=T,
                     serialized=T, raw=F, force.id=T)

  # Try to read back in
  nl = read.neurons.hdf5(test_filename, read='dotprops,mesh,skeleton')

  # Expect as many neurons as we have in kcs20
  expect_equal(length(nl), (length(nat::kcs20)))

  # Expect all dotprops
  expect_true(all(as.logical(lapply(nl, nat::is.dotprops))))
})
