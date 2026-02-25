/*
 * Repository:  https://github.com/kingkybel/CPP-utilities
 * File Name:   test/directed_graph_tests.cc
 * Description: Unit tests for string utilities
 *
 * Copyright (C) 2024 Dieter J Kybelksties <github@kybelksties.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * @date: 2024-10-12
 * @author: Dieter J Kybelksties
 */
#include <dkyb/to_string.h>
// #define DO_TRACE_
#include <dkyb/traceutil.h>
// #define DO_GRAPH_DEBUG_TRACE_
#include "directed_graph.h"
#include "directed_graph_tests_debug_functions.h"
#include "directed_graph_to_string.h"
#include "test_property_classes.h"

#include <cmath>
#include <gtest/gtest.h>
#ifdef DO_TRACE_
    #include <string>
#endif

using namespace std;
using namespace util;
using namespace util::graph;

class DirectedGraphTest : public ::testing::Test
{
  protected:
    void SetUp() override
    {
    }

    void TearDown() override
    {
    }
};

TEST_F(DirectedGraphTest, sub_graphs_test)
{
    NotHashableOrComparable A{"A"};
    NotHashableOrComparable B{"B"};
    NotHashableOrComparable C{"C"};
    NotHashableOrComparable D{"D"};
    NotHashableOrComparable E{"E"};
    NotHashableOrComparable F{"F"};
    NotHashableOrComparable G{"G"};
    NotHashableOrComparable H{"H"};
    NotHashableOrComparable I{"I"};
    NotHashableOrComparable J{"J"};
    NotHashableOrComparable K{"K"};

    NotHashableOrComparable E01{"E01"};
    NotHashableOrComparable E02{"E02"};
    NotHashableOrComparable E03{"E03"};
    NotHashableOrComparable E04{"E04"};
    NotHashableOrComparable E05{"E05"};
    NotHashableOrComparable E06{"E06"};
    NotHashableOrComparable E07{"E07"};
    NotHashableOrComparable E08{"E08"};
    NotHashableOrComparable E09{"E09"};
    NotHashableOrComparable E10{"E10"};
    NotHashableOrComparable E11{"E11"};

    directed_graph_base<NotHashableOrComparable, NotHashableOrComparable> basicDirectedGraph{};
    //    PRINT_TYPES_AND_BOOLS(basicDirectedGraph);
    basicDirectedGraph.addVertex(A);
    ASSERT_EQ(basicDirectedGraph.getDisconnectedSubGraphs().size(), 1UL)
        << "should have 1 component-graph" << basicDirectedGraph;
    basicDirectedGraph.addVertices(B, C, D, E, F, G, H, I, J, K);
    ASSERT_EQ(basicDirectedGraph.getDisconnectedSubGraphs().size(), 11UL)
        << "should have 11 component-graphs" << basicDirectedGraph;
    basicDirectedGraph.addEdge(A, B, E01);
    ASSERT_EQ(basicDirectedGraph.getDisconnectedSubGraphs().size(), 10UL)
        << "should have 10 component-graphs" << basicDirectedGraph;
    basicDirectedGraph.addPath(
        B,
        {
            {E02, C},
            {E03, D},
            {E04, B}
    }
    );
    ASSERT_EQ(basicDirectedGraph.getDisconnectedSubGraphs().size(), 8UL)
        << "should have 8 component-graphs" << basicDirectedGraph;
    basicDirectedGraph.addPath(
        E,
        {
            {E05, F},
            {E06, G},
            {E07, H},
            {E08, H},
            {E09, I},
            {E10, J},
            {E11, K}
    }
    );
    ASSERT_EQ(basicDirectedGraph.getDisconnectedSubGraphs().size(), 2UL)
        << "should have 2 component-graphs" << basicDirectedGraph;
    basicDirectedGraph.addEdge(H, C, E03);
    ASSERT_EQ(basicDirectedGraph.getDisconnectedSubGraphs().size(), 1UL)
        << "should have 1 component-graphs" << basicDirectedGraph;
    basicDirectedGraph.removeVertex(H);
    ASSERT_EQ(basicDirectedGraph.getDisconnectedSubGraphs().size(), 3UL)
        << "should have 3 component-graphs" << basicDirectedGraph;
    basicDirectedGraph.removeEdge(E, F, E05);
    ASSERT_EQ(basicDirectedGraph.getDisconnectedSubGraphs().size(), 4UL)
        << "should have 3 component-graphs" << basicDirectedGraph;

    TRACE1(basicDirectedGraph)
    SHOW_SUB_GRAPHS(basicDirectedGraph)
}

TEST_F(DirectedGraphTest, directed_graph_base_test)
{
    NotHashableOrComparable A{"A"};
    NotHashableOrComparable B{"B"};
    NotHashableOrComparable C{"C"};
    NotHashableOrComparable D{"D"};
    NotHashableOrComparable E{"E"};
    NotHashableOrComparable F{"F"};
    NotHashableOrComparable G{"G"};
    NotHashableOrComparable H{"H"};
    NotHashableOrComparable I{"I"};
    NotHashableOrComparable J{"J"};
    NotHashableOrComparable K{"K"};

    NotHashableOrComparable E01{"E01"};
    NotHashableOrComparable E02{"E02"};
    NotHashableOrComparable E03{"E03"};
    NotHashableOrComparable E04{"E04"};
    NotHashableOrComparable E05{"E05"};
    NotHashableOrComparable E06{"E06"};
    NotHashableOrComparable E07{"E07"};
    NotHashableOrComparable E08{"E08"};
    NotHashableOrComparable E09{"E09"};
    NotHashableOrComparable E10{"E10"};
    NotHashableOrComparable E11{"E11"};

    directed_graph_base<NotHashableOrComparable, NotHashableOrComparable> basicDirectedGraph{};
    //    PRINT_TYPES_AND_BOOLS(basicDirectedGraph);

    ASSERT_TRUE(basicDirectedGraph.addEdge(A, B, E01)) << "when adding an edge with none of the vertices, add vertices";
    ASSERT_TRUE(basicDirectedGraph.hasVertices(A, B));
    ASSERT_TRUE(basicDirectedGraph.hasEdge(A, B, E01));

    ASSERT_TRUE(basicDirectedGraph.addVertex(A)) << "adding vertex 'A' to empty graph should work";
    ASSERT_TRUE(basicDirectedGraph.addVertex(A)) << "adding vertex 'A' to empty graph should work";
    ASSERT_TRUE(basicDirectedGraph.addEdge(A, A, E01));
    ASSERT_TRUE(basicDirectedGraph.addEdge(A, A, E01));
    ASSERT_EQ(basicDirectedGraph.getParallelEdges(A, A).size(), 2UL) << basicDirectedGraph;
    ASSERT_FALSE(basicDirectedGraph.hasVertices(D, E, F, G, H));
    ASSERT_TRUE(basicDirectedGraph.addVertices(D, E, F, G, H)) << "adding multiple non-existent vertices should work";
    ASSERT_TRUE(basicDirectedGraph.hasVertices(D, E, F, G, H));
    ASSERT_TRUE(basicDirectedGraph.addEdge(D, E, E02));
    ASSERT_TRUE(basicDirectedGraph.addEdge(D, E, E02));
    ASSERT_TRUE(basicDirectedGraph.addEdge(F, E, E03));
    ASSERT_TRUE(basicDirectedGraph.addEdge(H, G, E02));
    ASSERT_TRUE(basicDirectedGraph.addEdge(H, H, E03));
    ASSERT_TRUE(basicDirectedGraph.addEdge(E, E, E05));
    ASSERT_TRUE(basicDirectedGraph.addEdge(E, D, E10));

    ASSERT_EQ(basicDirectedGraph.getDisconnectedSubGraphs().size(), 5UL)
        << "should now have 5 disconnected sub-graphs" << basicDirectedGraph << std::endl;
    ASSERT_TRUE(basicDirectedGraph.removeVertex(A));
    ASSERT_EQ(basicDirectedGraph.getDisconnectedSubGraphs().size(), 5UL)
        << "should still have 5 disconnected sub-graphs" << basicDirectedGraph << std::endl;
    ASSERT_TRUE(basicDirectedGraph.removeVertex(A));
    ASSERT_EQ(basicDirectedGraph.getDisconnectedSubGraphs().size(), 4UL)
        << "should now have 4 disconnected sub-graphs" << basicDirectedGraph << std::endl;
}

TEST_F(DirectedGraphTest, directed_graph_base_subgraph_test)
{
    NotHashableOrComparable A{"A"};
    NotHashableOrComparable B{"B"};
    NotHashableOrComparable C{"C"};
    NotHashableOrComparable D{"D"};
    NotHashableOrComparable E{"E"};
    NotHashableOrComparable F{"F"};
    NotHashableOrComparable G{"G"};
    NotHashableOrComparable H{"H"};
    NotHashableOrComparable I{"I"};
    NotHashableOrComparable J{"J"};
    NotHashableOrComparable K{"K"};

    NotHashableOrComparable E01{"E01"};
    NotHashableOrComparable E02{"E02"};
    NotHashableOrComparable E03{"E03"};
    NotHashableOrComparable E04{"E04"};
    NotHashableOrComparable E05{"E05"};
    NotHashableOrComparable E06{"E06"};
    NotHashableOrComparable E07{"E07"};
    NotHashableOrComparable E08{"E08"};
    NotHashableOrComparable E09{"E09"};
    NotHashableOrComparable E10{"E10"};
    NotHashableOrComparable E11{"E11"};

    directed_graph_base<NotHashableOrComparable, NotHashableOrComparable> basicDirectedGraph{};
    basicDirectedGraph.addVertices(A, B, C, D);
    ASSERT_EQ(basicDirectedGraph.getDisconnectedSubGraphs().size(), 4UL);
    basicDirectedGraph.addEdge(A, B, E01);
    ASSERT_EQ(basicDirectedGraph.getDisconnectedSubGraphs().size(), 3UL);
    basicDirectedGraph.addEdge(C, D, E01);
    ASSERT_EQ(basicDirectedGraph.getDisconnectedSubGraphs().size(), 2UL);
    basicDirectedGraph.addEdge(A, D, E01);
    ASSERT_EQ(basicDirectedGraph.getDisconnectedSubGraphs().size(), 1UL);
    basicDirectedGraph.removeVertex(A);
    ASSERT_EQ(basicDirectedGraph.getDisconnectedSubGraphs().size(), 2UL);
    basicDirectedGraph.removeEdge(C, D);
    ASSERT_EQ(basicDirectedGraph.getDisconnectedSubGraphs().size(), 3UL);
}

TEST_F(DirectedGraphTest, connected_graph_subgraph_test)
{
    NotHashableOrComparable A{"A"};
    NotHashableOrComparable B{"B"};
    NotHashableOrComparable C{"C"};
    NotHashableOrComparable D{"D"};
    NotHashableOrComparable E{"E"};
    NotHashableOrComparable F{"F"};
    NotHashableOrComparable G{"G"};
    NotHashableOrComparable H{"H"};
    NotHashableOrComparable I{"I"};
    NotHashableOrComparable J{"J"};
    NotHashableOrComparable K{"K"};

    NotHashableOrComparable E01{"E01"};
    NotHashableOrComparable E02{"E02"};
    NotHashableOrComparable E03{"E03"};
    NotHashableOrComparable E04{"E04"};
    NotHashableOrComparable E05{"E05"};
    NotHashableOrComparable E06{"E06"};
    NotHashableOrComparable E07{"E07"};
    NotHashableOrComparable E08{"E08"};
    NotHashableOrComparable E09{"E09"};
    NotHashableOrComparable E10{"E10"};
    NotHashableOrComparable E11{"E11"};

    connected_graph<NotHashableOrComparable, NotHashableOrComparable> connectedGraph{};
    connectedGraph.addVertex(A);
    ASSERT_EQ(connectedGraph.getDisconnectedSubGraphs().size(), 1UL);
    SHOW_SUB_GRAPHS(connectedGraph)

    connectedGraph.addEdge(A, B, E01);
    ASSERT_EQ(connectedGraph.getDisconnectedSubGraphs().size(), 1UL);
    SHOW_SUB_GRAPHS(connectedGraph)
    connectedGraph.addEdge(A, D, E01);
    ASSERT_EQ(connectedGraph.getDisconnectedSubGraphs().size(), 1UL);
    SHOW_SUB_GRAPHS(connectedGraph)
    connectedGraph.addEdge(A, D, E01);
    ASSERT_EQ(connectedGraph.getDisconnectedSubGraphs().size(), 1UL);
    SHOW_SUB_GRAPHS(connectedGraph)
    connectedGraph.removeVertex(A);
    ASSERT_EQ(connectedGraph.getDisconnectedSubGraphs().size(), 1UL);
    SHOW_SUB_GRAPHS(connectedGraph)

    connectedGraph.addPath(
        C,
        {
            {E10, A},
            {E01, B},
            {E02, C},
            {E03, D}
    }
    );
    ASSERT_EQ(connectedGraph.getDisconnectedSubGraphs().size(), 1UL);
    SHOW_SUB_GRAPHS(connectedGraph)

    connectedGraph.addPath(
        D,
        {
            {E04, E},
            {E05, F},
            {E06, G},
            {E07, H}
    }
    );
    ASSERT_EQ(connectedGraph.getDisconnectedSubGraphs().size(), 1UL);
    SHOW_SUB_GRAPHS(connectedGraph)
}

TEST_F(DirectedGraphTest, directed_acyclic_graph_test)
{
    Hashable A{"A"};
    Hashable B{"B"};
    Hashable C{"C"};
    Hashable D{"D"};
    Hashable E{"E"};
    Hashable F{"F"};
    Hashable G{"G"};
    Hashable H{"H"};
    Hashable I{"I"};
    Hashable J{"J"};
    Hashable K{"K"};

    Comparable E01{"E01"};
    Comparable E02{"E02"};
    Comparable E03{"E03"};
    Comparable E04{"E04"};
    Comparable E05{"E05"};
    Comparable E06{"E06"};
    Comparable E07{"E07"};
    Comparable E08{"E08"};
    Comparable E09{"E09"};
    Comparable E10{"E10"};
    Comparable E11{"E11"};

    directed_acyclic_graph<Hashable, Comparable> directedAcyclicGraph{};
    //    PRINT_TYPES_AND_BOOLS(directedAcyclicGraph);

    ASSERT_TRUE(directedAcyclicGraph.addEdge(A, B, E01))
        << "when adding an edge with none of the vertices, add vertices";
    ASSERT_TRUE(directedAcyclicGraph.hasVertices(A, B));
    ASSERT_TRUE(directedAcyclicGraph.hasEdge(A, B, E01));

    ASSERT_TRUE(directedAcyclicGraph.addVertex(A)) << "re-adding existing vertex 'A' is successful";
    ASSERT_EQ(directedAcyclicGraph.getParallelEdges(A, A).size(), 0UL);
    ASSERT_FALSE(directedAcyclicGraph.hasVertices(D, E, F, G, H));
    ASSERT_TRUE(directedAcyclicGraph.addVertices(D, E, F, G, H)) << "adding multiple non-existent vertices should work";
    ASSERT_TRUE(directedAcyclicGraph.hasVertices(D, E, F, G, H));
    ASSERT_TRUE(directedAcyclicGraph.addEdge(D, E, E02));
    ASSERT_TRUE(directedAcyclicGraph.addEdge(D, E, E02)) << "adding parallel edges is allowed";
    ASSERT_FALSE(directedAcyclicGraph.addEdge(E, D, E10)) << "adding this edge would create a cycle";
    ASSERT_TRUE(directedAcyclicGraph.addEdge(F, E, E03));
    ASSERT_TRUE(directedAcyclicGraph.addEdge(H, G, E02));

    ASSERT_EQ(directedAcyclicGraph.getDisconnectedSubGraphs().size(), 4UL)
        << "should now have 3 disconnected sub-graphs" << directedAcyclicGraph << std::endl;
    ASSERT_TRUE(directedAcyclicGraph.removeVertex(A));
    ASSERT_TRUE(directedAcyclicGraph.removeVertex(A)) << "second vertex A should work";
    ASSERT_EQ(directedAcyclicGraph.getDisconnectedSubGraphs().size(), 3UL)
        << "should now have 4 disconnected sub-graphs" << directedAcyclicGraph << std::endl;
}

TEST_F(DirectedGraphTest, forest_graph_tests)
{
    HashableAndComparable A{"A"};
    HashableAndComparable B{"B"};
    HashableAndComparable C{"C"};
    HashableAndComparable D{"D"};
    HashableAndComparable E{"E"};
    HashableAndComparable F{"F"};
    HashableAndComparable G{"G"};
    HashableAndComparable H{"H"};
    HashableAndComparable I{"I"};
    HashableAndComparable J{"J"};
    HashableAndComparable K{"K"};

    NotHashableOrComparable E01{"E01"};
    NotHashableOrComparable E02{"E02"};
    NotHashableOrComparable E03{"E03"};
    NotHashableOrComparable E04{"E04"};
    NotHashableOrComparable E05{"E05"};
    NotHashableOrComparable E06{"E06"};
    NotHashableOrComparable E07{"E07"};
    NotHashableOrComparable E08{"E08"};
    NotHashableOrComparable E09{"E09"};
    NotHashableOrComparable E10{"E10"};
    NotHashableOrComparable E11{"E11"};

    forest_graph<HashableAndComparable, NotHashableOrComparable> forestGraph{};
    //    PRINT_TYPES_AND_BOOLS(forestGraph);

    ASSERT_TRUE(forestGraph.addEdge(A, B, E01))
        << "adding an edge with none of the vertices should create the vertices";
    ASSERT_TRUE(forestGraph.addVertex(A)) << "re-adding vertex 'A' to graph should work";
    ASSERT_FALSE(forestGraph.addEdge(A, B, E01)) << "adding an edge with only one existing vertex should not work";
    ASSERT_TRUE(forestGraph.addVertex(B));
    ASSERT_TRUE(forestGraph.addVertex(C));
    ASSERT_EQ(forestGraph.getDisconnectedSubGraphs().size(), 2UL) << forestGraph;
    ASSERT_FALSE(forestGraph.hasVertices(D, E, F, G, H));
    forestGraph.addVertices(D, E, F, G, H);
    ASSERT_TRUE(forestGraph.hasVertices(D, E, F, G, H));
    ASSERT_EQ(forestGraph.getDisconnectedSubGraphs().size(), 7UL) << forestGraph;

    ASSERT_FALSE(forestGraph.addEdge(A, B, E01)) << "parallel edges are disallowed in forest";
    ASSERT_FALSE(forestGraph.addEdge(A, A, E02))
        << "Adding an edge from a vertex to itself creates a cycle and should not work\n"
        << forestGraph;
    ASSERT_EQ(forestGraph.getDisconnectedSubGraphs().size(), 7UL);
    ASSERT_FALSE(forestGraph.addEdge(B, A, E03))
        << "Adding an opposite edge to an existing edge creates a cycle and should not work";
    ASSERT_EQ(forestGraph.getDisconnectedSubGraphs().size(), 7UL);
    ASSERT_TRUE(forestGraph.addEdge(B, C, E02));
    ASSERT_TRUE(forestGraph.addEdge(C, D, E03));
    ASSERT_TRUE(forestGraph.addEdge(C, E, E04));
    ASSERT_FALSE(forestGraph.addEdge(B, E, E05)) << "adding the edge would violate single-parent-property";
    ASSERT_TRUE(forestGraph.addEdge(D, F, E06));
    ASSERT_TRUE(forestGraph.addEdge(G, H, E08));
    ASSERT_EQ(forestGraph.getDisconnectedSubGraphs().size(), 2UL) << forestGraph;
    ASSERT_FALSE(forestGraph.addEdge(E, A, E11)) << "Adding a cycle should not work";

    ASSERT_TRUE(forestGraph.hasEdge(G, H, E08));
    ASSERT_TRUE(forestGraph.removeEdge(G, H, E08)) << "removing an existing edge should work";
    ASSERT_FALSE(forestGraph.hasEdge(G, H, E08)) << "after removing an edge it should no longer be present";
    ASSERT_FALSE(forestGraph.hasEdge(G, H)) << "after removing an edge it should no longer be present";
    ASSERT_EQ(forestGraph.getDisconnectedSubGraphs().size(), 3UL) << forestGraph;
    ASSERT_TRUE(forestGraph.removeEdge(G, H, E08)) << "removing non-existent edge is OK";
    ASSERT_TRUE(forestGraph.removeEdge(G, H)) << "removing non-existent edge is OK";

    ASSERT_TRUE(forestGraph.hasEdge(C, E, E04));
    ASSERT_TRUE(forestGraph.removeEdge(C, E));
    ASSERT_FALSE(forestGraph.hasEdge(C, E));

    ASSERT_TRUE(forestGraph.hasEdge(D, F, E06));
    ASSERT_TRUE(forestGraph.hasEdge(D, F));
    ASSERT_TRUE(forestGraph.removeEdge(B, C, E06)) << "removing an edge between existing nodes but not-matching "
                                                      "edge-property is removing a non-existing edge and OK";
    ASSERT_TRUE(forestGraph.hasEdge(B, C, E02)) << forestGraph;

    ASSERT_TRUE(forestGraph.addEdge(B, E, E06))
        << "parallel edges are not allowed for this type of graph - but overwrite edge";
    ASSERT_TRUE(forestGraph.hasEdge(B, E, E06));
    ASSERT_EQ(forestGraph.getParallelEdges(B, E).size(), 1UL);
    ASSERT_TRUE(forestGraph.hasEdge(B, E, E06)) << forestGraph;

    ASSERT_TRUE(forestGraph.hasVertex(C));
    ASSERT_TRUE(forestGraph.hasEdge(B, C, E02));
    ASSERT_TRUE(forestGraph.hasEdge(C, D, E03));
    ASSERT_TRUE(forestGraph.removeVertex(C));
    ASSERT_FALSE(forestGraph.hasVertex(C));
    ASSERT_FALSE(forestGraph.hasEdge(B, C, E02)) << "removing the source-vertex should remove the edge";
    ASSERT_FALSE(forestGraph.hasEdge(C, D, E03)) << "removing the target-vertex should remove the edge" << forestGraph;
}

TEST_F(DirectedGraphTest, forest_graph_2_tests)
{
    HashableAndComparable A{"A"};
    HashableAndComparable B{"B"};
    HashableAndComparable C{"C"};
    HashableAndComparable D{"D"};
    HashableAndComparable E{"E"};
    HashableAndComparable F{"F"};
    HashableAndComparable G{"G"};
    HashableAndComparable H{"H"};
    HashableAndComparable I{"I"};
    HashableAndComparable J{"J"};
    HashableAndComparable K{"K"};

    NotHashableOrComparable E01{"E01"};
    NotHashableOrComparable E02{"E02"};
    NotHashableOrComparable E03{"E03"};
    NotHashableOrComparable E04{"E04"};
    NotHashableOrComparable E05{"E05"};
    NotHashableOrComparable E06{"E06"};
    NotHashableOrComparable E07{"E07"};
    NotHashableOrComparable E08{"E08"};
    NotHashableOrComparable E09{"E09"};
    NotHashableOrComparable E10{"E10"};
    NotHashableOrComparable E11{"E11"};

    forest_graph<HashableAndComparable, NotHashableOrComparable> forestGraph{};
    ASSERT_TRUE(forestGraph.addEdge(A, B, E01));
    ASSERT_TRUE(forestGraph.addEdge(B, C, E02));
    ASSERT_TRUE(forestGraph.removeVertex(B));
}

TEST_F(DirectedGraphTest, connected_graph_stay_connected_tests)
{
    NotHashableOrComparable A{"A"};
    NotHashableOrComparable B{"B"};
    NotHashableOrComparable C{"C"};
    NotHashableOrComparable D{"D"};
    NotHashableOrComparable E{"E"};
    NotHashableOrComparable F{"F"};
    NotHashableOrComparable G{"G"};
    NotHashableOrComparable H{"H"};
    NotHashableOrComparable I{"I"};
    NotHashableOrComparable J{"J"};
    NotHashableOrComparable K{"K"};

    HashableAndComparable E01{"E01"};
    HashableAndComparable E02{"E02"};
    HashableAndComparable E03{"E03"};
    HashableAndComparable E04{"E04"};
    HashableAndComparable E05{"E05"};
    HashableAndComparable E06{"E06"};
    HashableAndComparable E07{"E07"};
    HashableAndComparable E08{"E08"};
    HashableAndComparable E09{"E09"};
    HashableAndComparable E10{"E10"};
    HashableAndComparable E11{"E11"};

    connected_graph<NotHashableOrComparable, HashableAndComparable, stay_connected> connectedGraph{};
    ASSERT_TRUE(connectedGraph.addVertex(A));
    ASSERT_TRUE(connectedGraph.addVertex(A));
    ASSERT_FALSE(connectedGraph.addVertex(B));
    ASSERT_FALSE(connectedGraph.addVertices(B, C, D));

    ASSERT_TRUE(connectedGraph.addEdge(A, B, E01));
    ASSERT_FALSE(connectedGraph.addEdge(C, D, E01));
    ASSERT_TRUE(connectedGraph.addEdge(A, C, E01));
    ASSERT_TRUE(connectedGraph.addEdge(C, C, E01));
    ASSERT_TRUE(connectedGraph.addEdge(C, D, E01));
    ASSERT_TRUE(connectedGraph.addEdge(A, D, E01));
    ASSERT_TRUE(connectedGraph.addEdge(D, A, E01));
    ASSERT_TRUE(connectedGraph.removeVertex(A));
    ASSERT_TRUE(connectedGraph.removeEdge(C, D, E02));
    ASSERT_TRUE(connectedGraph.removeEdge(C, D, E01));

    TRACE1(connectedGraph)
    SHOW_SUB_GRAPHS(connectedGraph)
}

TEST_F(DirectedGraphTest, connected_graph_disallow_split_tests)
{
    Hashable A{"A"};
    Hashable B{"B"};
    Hashable C{"C"};
    Hashable D{"D"};
    Hashable E{"E"};
    Hashable F{"F"};
    Hashable G{"G"};
    Hashable H{"H"};
    Hashable I{"I"};
    Hashable J{"J"};
    Hashable K{"K"};

    NotHashableOrComparable E01{"E01"};
    NotHashableOrComparable E02{"E02"};
    NotHashableOrComparable E03{"E03"};
    NotHashableOrComparable E04{"E04"};
    NotHashableOrComparable E05{"E05"};
    NotHashableOrComparable E06{"E06"};
    NotHashableOrComparable E07{"E07"};
    NotHashableOrComparable E08{"E08"};
    NotHashableOrComparable E09{"E09"};
    NotHashableOrComparable E10{"E10"};
    NotHashableOrComparable E11{"E11"};

    connected_graph<Hashable, NotHashableOrComparable, disallow_split> connectedGraph{};
    ASSERT_TRUE(connectedGraph.addVertex(A));
    ASSERT_TRUE(connectedGraph.addVertex(A));
    ASSERT_FALSE(connectedGraph.addVertex(B));
    ASSERT_FALSE(connectedGraph.addVertices(B, C, D));

    ASSERT_TRUE(connectedGraph.addEdge(A, B, E01));
    ASSERT_FALSE(connectedGraph.addEdge(C, D, E01));
    ASSERT_TRUE(connectedGraph.addEdge(A, C, E01));
    ASSERT_TRUE(connectedGraph.addEdge(C, C, E01));
    ASSERT_TRUE(connectedGraph.addEdge(C, D, E01));
    ASSERT_TRUE(connectedGraph.addEdge(A, D, E01));
    ASSERT_TRUE(connectedGraph.addEdge(D, A, E01));
    ASSERT_FALSE(connectedGraph.removeVertex(A)) << "removing A would create a disconnected graph, which is disallowed";
    ASSERT_TRUE(connectedGraph.removeVertex(B))
        << "removing B does not create a disconnected graph and should be allowed";
    ASSERT_TRUE(connectedGraph.addEdge(D, E, E03));
    ASSERT_FALSE(connectedGraph.removeEdge(D, E, E03)) << "removing edge D->E would leave E isolated" << connectedGraph;

    TRACE1(connectedGraph)
    SHOW_SUB_GRAPHS(connectedGraph)
}

TEST_F(DirectedGraphTest, complexity_metrics_test)
{
    NotHashableOrComparable A{"A"};
    NotHashableOrComparable B{"B"};
    NotHashableOrComparable C{"C"};
    NotHashableOrComparable E01{"E01"};
    NotHashableOrComparable E02{"E02"};

    directed_graph_base<NotHashableOrComparable, NotHashableOrComparable> graph{};
    ASSERT_TRUE(graph.addEdge(A, B, E01));
    ASSERT_TRUE(graph.addEdge(B, C, E02));

    auto complexity = graph.complexity();
    ASSERT_EQ(complexity.numVertices, 3UL);
    ASSERT_EQ(complexity.numEdges, 2UL);
    ASSERT_NEAR(complexity.density, 2.0 / 6.0, 1e-9);
    ASSERT_NEAR(complexity.average_degree, 4.0 / 3.0, 1e-9);
    ASSERT_NEAR(complexity.cyclomatic_complexity, 0.0, 1e-9);
    ASSERT_EQ(complexity.diameter, 2);

    auto explicitComponentsComplexity = graph.complexity(2);
    ASSERT_NEAR(explicitComponentsComplexity.cyclomatic_complexity, 1.0, 1e-9);
}

TEST_F(DirectedGraphTest, disconnected_subgraphs_are_sorted_by_complexity_test)
{
    NotHashableOrComparable A{"A"};
    NotHashableOrComparable B{"B"};
    NotHashableOrComparable C{"C"};
    NotHashableOrComparable D{"D"};
    NotHashableOrComparable E{"E"};
    NotHashableOrComparable E01{"E01"};
    NotHashableOrComparable E02{"E02"};
    NotHashableOrComparable E03{"E03"};

    directed_graph_base<NotHashableOrComparable, NotHashableOrComparable> graph{};
    ASSERT_TRUE(graph.addEdge(A, B, E01));
    ASSERT_TRUE(graph.addEdge(B, C, E02));
    ASSERT_TRUE(graph.addVertices(D, E));
    ASSERT_TRUE(graph.addEdge(D, E, E03));

    auto subGraphs = graph.getDisconnectedSubGraphs();
    ASSERT_EQ(subGraphs.size(), 2UL);

    auto it = subGraphs.begin();
    ASSERT_EQ(it->first.numVertices, 3UL);
    ASSERT_EQ(it->first.numEdges, 2UL);
    ++it;
    ASSERT_EQ(it->first.numVertices, 2UL);
    ASSERT_EQ(it->first.numEdges, 1UL);
}
