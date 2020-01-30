resampling_sp$param_set$values$folds
resampling_sp$param_set$values$repeats

resampling_sp$instantiate(task = ecuador)
resampling_sp$instance
# is a result of
# instance = private$.sample(task$row_ids, task$coordinates())
resampling_sp$test_set(1)
resampling_sp$train_set(1)

instance = private$.sample(task$row_ids, task$coordinates())

reprex::reprex(x = {
  library("data.table")
  set.seed(123)
  # simulate 50 plots that have been re-visited four times in different years
  ids = rep(1:50, 4)
  # coordinates
  coords = seq(from = -20, to = 20, by = 0.01)
  # repeat x- and y-coordinates four times
  coords = data.table(x = rep(sample(coords, 50), 4),
                      y = rep(sample(coords, 50), 4))

  # adjust ResamplingSpCVCoords$.sample

  .sample_by_group = function(ids, coords, group) {
    # remember how many groups there are
    n_gr = length(group) / uniqueN(group)
    coords = cbind(coords, "group" = group)
    # just use unique ids for the spatial resampling
    un = coords[!duplicated(group)]
    # delete the grouping column again
    un[, group := NULL]
    # inds = kmeans(coords, centers = self$param_set$values$folds)
    # kmeans clustering
    inds = kmeans(un, centers = 5)
    # repeat the cluster classes as many times as there are groups
    cluster = rep(inds$cluster, n_gr)

    data.table(
      row_id = ids,
      fold = cluster,
      key = "fold"
    )
  }

  dt = .sample_by_group(ids = ids, coords = coords, group = ids)
  # visualize the output
  plot(coords[, .(x, y)])
  points(coords[dt[fold == 1, row_id]], col = "blue", pch = 16)
  points(coords[dt[fold == 2, row_id]], col = "red", pch = 16)
  points(coords[dt[fold == 3, row_id]], col = "green", pch = 16)
  points(coords[dt[fold == 4, row_id]], col = "yellow", pch = 16)
  points(coords[dt[fold == 5, row_id]], col = "pink", pch = 16)

  # and just to verify that each coordinate has been sampled several times
  dt[, .N, by = "fold"]
}
)

