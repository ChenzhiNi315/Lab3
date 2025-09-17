test_that("Dijkstra function tests normal functionality", {
  # Standard diagram testing
  wiki_graph <- data.frame(
    v1 = c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
    v2 = c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
    w = c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)
  )

  expect_equal(dijkstra(wiki_graph, 1), c(0, 7, 9, 20, 20, 11))
  expect_equal(dijkstra(wiki_graph, 3), c(9, 10, 0, 11, 11, 2))

  # Single node graph testing
  single_node_graph <- data.frame(v1 = 1, v2 = 1, w = 0)
  expect_equal(dijkstra(single_node_graph, 1), 0)

  # Simple connected graph testing
  simple_graph <- data.frame(v1 = c(1,2), v2 = c(2,1), w = c(1,1))
  expect_equal(dijkstra(simple_graph, 1), c(0, 1))
})

test_that("Dijkstra function tests invalid input", {
  wiki_graph <- data.frame(
    v1 = c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
    v2 = c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
    w = c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)
  )

  # Negative weight test
  negative_graph <- data.frame(v1 = c(1,2), v2 = c(2,1), w = c(1,-1))
  expect_error(dijkstra(negative_graph, 1))

  # Invalid initial node test
  expect_error(dijkstra(wiki_graph, 7))

  # Invalid graph structure testing
  invalid_graph <- data.frame(from = c(1,2), to = c(2,1), weight = c(1,1))
  expect_error(dijkstra(invalid_graph, 1))

  # Non data box input test
  expect_error(dijkstra("not_a_dataframe", 1))

  # Non scalar initial node testing
  expect_error(dijkstra(wiki_graph, c(1,2)))
})

