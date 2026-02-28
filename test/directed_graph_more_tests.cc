/*
 * Repository:  https://github.com/kingkybel/DirectedGraph
 * File Name:   test/directed_graph_more_tests.cc
 * Description: Additional unit tests for DirectedGraph
 *
 * Copyright (C) 2024 Dieter J Kybelksties <github@kybelksties.com>
 */
#include "directed_graph.h"
#include "test_property_classes.h"
#include <gtest/gtest.h>
#include <vector>
#include <string>

using namespace util::graph;

class DirectedGraphMoreTest : public ::testing::Test
{
};

TEST_F(DirectedGraphMoreTest, vertex_edge_path_test)
{
    VertexEdgePath<std::string, std::string> path;
    path.add("v1", "v2", "e1");
    path.add("v2", "v3", "e2");

    auto steps = path.getPath();
    ASSERT_EQ(steps.size(), 2UL);
    
    ASSERT_EQ(std::get<0>(steps[0]), "v1");
    ASSERT_EQ(std::get<1>(steps[0]), "e1");
    ASSERT_EQ(std::get<2>(steps[0]), "v2");

    ASSERT_EQ(std::get<0>(steps[1]), "v2");
    ASSERT_EQ(std::get<1>(steps[1]), "e2");
    ASSERT_EQ(std::get<2>(steps[1]), "v3");
}

TEST_F(DirectedGraphMoreTest, apply_visitors_test)
{
    directed_graph_base<Comparable, Comparable> graph;
    Comparable A{"A"}, B{"B"}, C{"C"};
    Comparable E1{"E1"}, E2{"E2"};

    graph.addEdge(A, B, E1);
    graph.addEdge(B, C, E2);

    std::vector<std::string> visitedVertices;
    auto vertexVisitor = [&visitedVertices](Comparable const& v) {
        visitedVertices.push_back(v.id_);
    };
    graph.applyVertices(vertexVisitor);
    ASSERT_EQ(visitedVertices.size(), 3UL);
    EXPECT_NE(std::find(visitedVertices.begin(), visitedVertices.end(), "A"), visitedVertices.end());
    EXPECT_NE(std::find(visitedVertices.begin(), visitedVertices.end(), "B"), visitedVertices.end());
    EXPECT_NE(std::find(visitedVertices.begin(), visitedVertices.end(), "C"), visitedVertices.end());

    std::vector<std::string> visitedEdges;
    auto edgeVisitor = [&visitedEdges](Comparable const& e) {
        visitedEdges.push_back(e.id_);
    };
    graph.applyEdges(edgeVisitor);
    ASSERT_EQ(visitedEdges.size(), 2UL);
    EXPECT_NE(std::find(visitedEdges.begin(), visitedEdges.end(), "E1"), visitedEdges.end());
    EXPECT_NE(std::find(visitedEdges.begin(), visitedEdges.end(), "E2"), visitedEdges.end());
}

TEST_F(DirectedGraphMoreTest, apply_visitors_with_filter_test)
{
    directed_graph_base<Comparable, Comparable> graph;
    Comparable A{"A"}, B{"B"}, C{"C"};
    Comparable E1{"E1"}, E2{"E2"};

    graph.addEdge(A, B, E1);
    graph.addEdge(B, C, E2);

    std::vector<std::string> visitedVertices;
    auto vertexVisitor = [&visitedVertices](Comparable const& v) {
        visitedVertices.push_back(v.id_);
    };
    auto vertexFilter = [](Comparable const& v) {
        return v.id_ == "A" || v.id_ == "C";
    };
    graph.applyVertices(vertexVisitor, vertexFilter);
    ASSERT_EQ(visitedVertices.size(), 2UL);
    EXPECT_NE(std::find(visitedVertices.begin(), visitedVertices.end(), "A"), visitedVertices.end());
    EXPECT_NE(std::find(visitedVertices.begin(), visitedVertices.end(), "C"), visitedVertices.end());
    EXPECT_EQ(std::find(visitedVertices.begin(), visitedVertices.end(), "B"), visitedVertices.end());

    std::vector<std::string> visitedEdges;
    auto edgeVisitor = [&visitedEdges](Comparable const& e) {
        visitedEdges.push_back(e.id_);
    };
    auto edgeFilter = [](Comparable const& e) {
        return e.id_ == "E2";
    };
    graph.applyEdges(edgeVisitor, edgeFilter);
    ASSERT_EQ(visitedEdges.size(), 1UL);
    EXPECT_EQ(visitedEdges[0], "E2");
}

TEST_F(DirectedGraphMoreTest, throw_on_error_test)
{
    using DG = directed_graph_base<Comparable, Comparable, throw_on_error, disallow_cycles, disallow_parallel_edges, adding_edge_requires_vertices, no_overwrite_edge_property>;
    DG graph;
    Comparable A{"A"}, B{"B"};
    Comparable E1{"E1"};
    Comparable E2{"E2"};

    graph.addVertex(A);
    graph.addVertex(B);

    // Test parallel_edge_error
    graph.addEdge(A, B, E1);
    EXPECT_THROW(graph.addEdge(A, B, E2), parallel_edge_error);

    // Test cycle_error
    EXPECT_THROW(graph.addEdge(B, A, E1), cycle_error);

    // Test vertex_existence_error
    Comparable C{"C"};
    EXPECT_THROW(graph.addEdge(A, C, E1), vertex_existence_error);
}

TEST_F(DirectedGraphMoreTest, single_parent_throw_test)
{
    using DG = directed_graph_base<Comparable, Comparable, throw_on_error, single_parent>;
    DG graph;
    Comparable A{"A"}, B{"B"}, C{"C"};
    Comparable E1{"E1"}, E2{"E2"};

    graph.addEdge(A, C, E1);
    EXPECT_THROW(graph.addEdge(B, C, E2), single_parent_error);
}

TEST_F(DirectedGraphMoreTest, get_parallel_edges_test)
{
    directed_graph_base<Comparable, Comparable, allow_parallel_edges> graph;
    Comparable A{"A"}, B{"B"};
    Comparable E1{"E1"}, E2{"E2"}, E3{"E3"};

    graph.addEdge(A, B, E1);
    graph.addEdge(A, B, E2);
    graph.addEdge(B, A, E3);

    auto ab_edges = graph.getParallelEdges(A, B);
    ASSERT_EQ(ab_edges.size(), 2UL);
    EXPECT_NE(std::find(ab_edges.begin(), ab_edges.end(), E1.id_), ab_edges.end());
    EXPECT_NE(std::find(ab_edges.begin(), ab_edges.end(), E2.id_), ab_edges.end());

    auto ba_edges = graph.getParallelEdges(B, A);
    ASSERT_EQ(ba_edges.size(), 1UL);
    EXPECT_EQ(ba_edges[0], E3.id_);

    auto nonexistent_edges = graph.getParallelEdges(A, Comparable{"C"});
    EXPECT_TRUE(nonexistent_edges.empty());
}

TEST_F(DirectedGraphMoreTest, has_edge_with_property_test)
{
    directed_graph_base<Comparable, Comparable, allow_parallel_edges> graph;
    Comparable A{"A"}, B{"B"};
    Comparable E1{"E1"}, E2{"E2"};

    graph.addEdge(A, B, E1);
    
    EXPECT_TRUE(graph.hasEdge(A, B));
    EXPECT_TRUE(graph.hasEdge(A, B, E1));
    EXPECT_FALSE(graph.hasEdge(A, B, E2));
    EXPECT_FALSE(graph.hasEdge(B, A));

    graph.addEdge(A, B, E2);
    EXPECT_TRUE(graph.hasEdge(A, B, E1));
    EXPECT_TRUE(graph.hasEdge(A, B, E2));
}
