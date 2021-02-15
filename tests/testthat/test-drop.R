test_that("test dropping neurons from file", {
  # Write kcs20 to temporary file
  test_filename <- tempfile(fileext = ".h5")
  write.neurons.hdf5(nat::kcs20, test_filename, overwrite.neurons=T,
                     serialized=F, raw=T, force.id=T)

  # Get contents
  info = inspect.hdf5(test_filename)
  expect_equal(length(info$neurons), length(nat::kcs20))

  # Drop the first ten neurons
  drop.neurons.hdf5(test_filename, which=info$neurons[1:10])

  # Expect 10 remaining dotprops
  new_info = inspect.hdf5(test_filename)
  expect_equal(length(new_info$neurons), length(nat::kcs20) - 10)
})
