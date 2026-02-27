/*
 * Repository:  https://github.com/kingkybel/DirectedGraph
 * File Name:   include/directed_graph.h
 * Description: directed graph utility functions based on boost::graph
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
 * @date: 2024-10-08
 * @author: Dieter J Kybelksties
 */

#ifndef NS_UTIL_GRAPH_DIRECTED_GRAPH_H_INCLUDED
#define NS_UTIL_GRAPH_DIRECTED_GRAPH_H_INCLUDED

#include <dkyb/traits.h>
// #define DO_TRACE_
#include <dkyb/traceutil.h>
#ifdef DO_TRACE_
    #include <dkyb/to_string.h>
#endif
// #define DO_GRAPH_DEBUG_TRACE_
#include "directed_graph_tests_debug_functions.h"
#include "directed_graph_traits.h"
#include "edge_properties.h"
#include "vertex_properties.h"

#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/connected_components.hpp>
#include <boost/graph/depth_first_search.hpp>
#include <boost/graph/dijkstra_shortest_paths.hpp>
#include <boost/graph/strong_components.hpp>
#include <boost/graph/visitors.hpp>
#include <deque>
#include <functional>
#include <gtest/gtest_prod.h>
#include <optional>
#include <stdexcept>
#include <string>
#include <unordered_set>
#include <vector>

template <
    typename PropTypeT,
    typename AllowParallelEdgesT,
    typename AllowMultipleVerticesT,
    typename ExpectedSelectorEdgeT,
    typename ExpectedSelectorVertexT,
    typename PreferComparableContainer>
void testCorrectTypes();

namespace util::graph
{

/**
 * @brief Exception to be thrown when a new edge would create a cycle.
 * This is only thrown when exceptions are enabled by trait DirectedGraphTraits::throw_on_error.
 */
struct cycle_error : public std::invalid_argument
{
    explicit cycle_error(std::string const &msg)
        : std::invalid_argument(msg)
    {
    }
};

/**
 * @brief Exception to be thrown when a new edge would create a parallel.
 * This is only thrown when exceptions are enabled by trait DirectedGraphTraits::throw_on_error.
 */
struct parallel_edge_error : public std::invalid_argument
{
    explicit parallel_edge_error(std::string const &msg)
        : std::invalid_argument(msg)
    {
    }
};

/**
 * @brief Exception to be thrown when a vertex is required to exist and does not or when it must not exist and does.
 * This is only thrown when exceptions are enabled by trait DirectedGraphTraits::throw_on_error.
 */
struct vertex_existence_error : public std::invalid_argument
{
    explicit vertex_existence_error(std::string const &msg)
        : std::invalid_argument(msg)
    {
    }
};

/**
 * @brief Exception to be thrown when adding an edge would add a parent to a vertex that already got one.
 * This is only thrown when exceptions are enabled by trait DirectedGraphTraits::throw_on_error.
 */
struct single_parent_error : public std::invalid_argument
{
    explicit single_parent_error(std::string const &msg)
        : std::invalid_argument(msg)
    {
    }
};

/**
 * @brief Exception to be thrown when an action would cause a connected graph to become dis-connected.
 * This is only thrown when exceptions are enabled by trait DirectedGraphTraits::throw_on_error.
 */
struct disconnection_error : public std::invalid_argument
{
    explicit disconnection_error(std::string const &msg)
        : std::invalid_argument(msg)
    {
    }
};

template <typename VertexProperty, typename EdgeProperty>
class VertexEdgePath
{
  public:
    void add(VertexProperty const &source, VertexProperty const &target, EdgeProperty const &edge)
    {
        if (vertices_.empty())
        {
            vertices_.push_back(source);
        }
        vertices_.push_back(target);
        edges_.push_back(edge);
    }

    [[nodiscard]] std::deque<std::tuple<VertexProperty, EdgeProperty, VertexProperty>> getPath() const
    {
        std::deque<std::tuple<VertexProperty, EdgeProperty, VertexProperty>> reval{};
        for (size_t i = 0UL; i < edges_.size(); ++i)
        {
            // there always is 1 more vertices in the path than edges
            reval.push_back(std::make_tuple(vertices_[i], edges_[i], vertices_[i + 1]));
        }
        return reval;
    }

  private:
    std::deque<VertexProperty> vertices_{};
    std::deque<VertexProperty> edges_{};
};

/**
 * @brief A class for directed graphs.
 * @tparam VertexDataType vertex property type, no restrictions, but may influence the storage
 * @tparam EdgeDataType edge property type, no restrictions, but may influence the storage
 * @tparam Options variadic list of options
 *  <ul>
 *  <li>dis/allow_multiple_vertices: (dis-)allow multiple vertices with the same property</li>
 *  <li>dis/allow_parallel_edges: (dis-)allow parallel edges between two vertices</li>
 *  <li>dis/allow_cycles: (dis-)allow cycles in the graph</li>
 *  <li>dis/connected: each vertex connected to each other by an undirected path, or not </li>
 *  <li>stay_connected/disallow_split: either enforce a connected graph to stay connected by removing all but one (the
 *      most complex) sub-graph, or by disallowing actions that would split the graph</li>
 *  <li>dis/connected: each vertex connected to each other by an undirected path, or not </li>
 *  <li>single_parent/multiple_parent: vertices can only have a maximum of 1 parent or multiple parents</li>
 *  <li>no_/throw_on_error: throw exceptions on logic error or return false instead</li>
 *  <li>no_/overwrite_edge_property: if no parallel edges allowed then edge property can be overwritten or not</li>
 *  <li>adding_edge_adds_vertices/adding_edge_requires_vertices: adding an edge can add missing vertices or not</li>
 *  <li>prefer_comparable_container/prefer_hash_container: where a vertex-/edge- property allows for hash-container-
 *      storage or ordered-container-storage, this option allows to set the preference</li>
 *  <li>fallback_selector<boost-selector-type>: Depending on vertex- and edge- properties an appropriate
 * storage-selector will be chosen automatically. If a property is neither comparable or hashable a fallback will be
 * selected. this is by default a boost::listS, but can be overridden this this</li>
 *  </ul>
 */
template <typename VertexDataType, typename EdgeDataType, typename... Options>
class directed_graph_base
{
    // friend-declarations for unit-tests
    FRIEND_TEST(DirectedGraphTest, directed_graph_base_test);

    FRIEND_TEST(DirectedGraphTest, directed_acyclic_graph_tests);

    template <
        typename PropTypeT,
        typename AllowParallelEdgesT,
        typename AllowMultipleVerticesT,
        typename ExpectedSelectorEdgeT,
        typename ExpectedSelectorVertexT,
        typename PreferComparableContainer>
    friend void ::testCorrectTypes();
    //

  private:
    static_assert(util::is_equality_comparable_v<VertexDataType>, "VertexDataType must be equality comparable");
    static_assert(util::is_equality_comparable_v<EdgeDataType>, "EdgeDataType must be equality comparable");
    // Detect which options are enabled
    // clang-format off
        static constexpr bool AllowMultipleVertices =
            set_or_default_bool_option<allow_multiple_vertices, disallow_multiple_vertices, true, Options...>::value;
        static constexpr bool AllowParallelEdges =
            set_or_default_bool_option<allow_parallel_edges, disallow_parallel_edges, true, Options...>::value;
        static constexpr bool PreferComparableContainer =
            set_or_default_bool_option<prefer_comparable_container, prefer_hash_container, true, Options...>::value;
        static constexpr bool AllowCycles =
            set_or_default_bool_option<allow_cycles, disallow_cycles, true, Options...>::value;
        static constexpr bool Connected =
            set_or_default_bool_option<connected, disconnected, false, Options...>::value;
        static constexpr bool StayConnectedStrategy =
            set_or_default_bool_option<stay_connected, disallow_split, false, Options...>::value;
        static constexpr bool SingleParent =
            set_or_default_bool_option<single_parent, multiple_parent, false, Options...>::value;
        static constexpr bool ThrowOnError =
            set_or_default_bool_option<throw_on_error, no_throw_on_error, false, Options...>::value;
        static constexpr bool OverWriteEdgeProperty =
            set_or_default_bool_option<overwrite_edge_property, no_overwrite_edge_property, true, Options...>::value;
        static constexpr bool AddVerticesViaEdge =
            set_or_default_bool_option<adding_edge_adds_vertices, adding_edge_requires_vertices, true, Options...>::value;
    public:
      static constexpr bool doesAllowMultipleVertices()       { return AllowMultipleVertices; }
      static constexpr bool doesAllowParallelEdges()          { return AllowParallelEdges; }
      static constexpr bool doesPreferComparableContainer()   { return PreferComparableContainer; }
      static constexpr bool doesAllowCycles()                 { return AllowCycles; }
      static constexpr bool isConnected()                     { return Connected; }
      static constexpr bool isStayConnectedStrategy()         { return StayConnectedStrategy; }
      static constexpr bool isSingleParent()                  { return SingleParent; }
      static constexpr bool doesThrowOnError()                { return ThrowOnError; }
      static constexpr bool doesOverWriteEdgeProperty()       { return OverWriteEdgeProperty; }
      static constexpr bool canAddVerticesViaEdge()           { return AddVerticesViaEdge; }

    // Determine vertex and edge storage types
    using FallbackSelector = typename find_fallback_selector<boost::listS, Options...>::type;
    using OutEdgeListS = typename storage_selector<
        appropriate_storage_selector_t<EdgeDataType, FallbackSelector, AllowParallelEdges, PreferComparableContainer>,
        Options...>::type;
    using VertexStorage = typename storage_selector<
        appropriate_storage_selector_t<VertexDataType, FallbackSelector, AllowMultipleVertices, PreferComparableContainer>,
        Options...>::type;
    using EdgeStorage = typename storage_selector<
        appropriate_storage_selector_t<EdgeDataType, FallbackSelector, AllowParallelEdges, PreferComparableContainer>,
        Options...>::type;
    // clang-format on

  public:
    DECLARE_TEST_DEBUG_HELPERS

    using EdgePropertiesType   = EdgeProperties<EdgeDataType, Options...>;
    using VertexPropertiesType = VertexProperties<VertexDataType, Options...>;

    using Graph = boost::adjacency_list<
        OutEdgeListS,
        VertexStorage,
        boost::directedS,
        VertexPropertiesType,
        EdgePropertiesType,
        boost::no_property,
        EdgeStorage>;

    using VertexDescriptor = typename boost::graph_traits<Graph>::vertex_descriptor;
    using EdgeDescriptor   = typename boost::graph_traits<Graph>::edge_descriptor;

    // undirected copy of the graph used for some boost-graph-algorithms
    using UndirectedGraph = boost::adjacency_list<
        boost::vecS,
        boost::vecS,
        boost::undirectedS,
        VertexPropertiesType,
        EdgePropertiesType,
        boost::no_property,
        boost::listS>;

    using UndirectedVertexDescriptor = typename boost::graph_traits<UndirectedGraph>::vertex_descriptor;
    using UndirectedEdgeDescriptor   = typename boost::graph_traits<UndirectedGraph>::edge_descriptor;

  private:
    Graph graph_;

  public:
    /**
     * @brief Check if a vertex exists.
     * @param vertex vertex to look for
     * @return true if the vertex exists in the graph false otherwise
     */
    bool hasVertex(VertexPropertiesType const &vertex) const noexcept
    {
        for (auto vertexDesc: boost::make_iterator_range(boost::vertices(graph_)))
        {
            if (graph_[vertexDesc] == vertex)
            {
                return true;
            }
        }
        return false;
    }

    /**
     * @brief Variadic function to check if all provided vertices exist.
     * @tparam Vertices variadic list of VertexDataType objects
     * @param vertices the vertices to check
     * @return true, is all vertices exist, false if at least one does not exist
     */
    template <typename... Vertices>
    bool hasVertices(Vertices const &...vertices) const noexcept
    {
        return (hasVertex(vertices) && ...);
    }

    /**
     * @brief Check whether an edge exist between source and target. If edge is given, then it has to match also.
     * @param vertexSource source-vertex of the edge
     * @param vertexTarget target-vertex of the edge
     * @param edge optional edge-property
     * @return true if the edge exists, false otherwise
     */
    bool hasEdge(
        VertexPropertiesType const       &vertexSource,
        VertexPropertiesType const       &vertexTarget,
        std::optional<EdgePropertiesType> edge = std::nullopt
    ) const noexcept
    {
        auto [haveSrc, src] = getVertexDescriptor_(vertexSource);
        auto [haveTrg, trg] = getVertexDescriptor_(vertexTarget);
        if (!haveSrc || !haveTrg)
        {
            return false;
        }
        auto [edgeIter, found] = boost::edge(src, trg, graph_);
        if (!found)
        {
            return false;
        }
        // If an edge property is specified, check if it matches the edge property in the graph
        if (edge)
        {
            if constexpr (!AllowParallelEdges)
            {
                return graph_[edgeIter] == *edge;
            }
            else
            {
                auto edges                = getEdges();
                auto [out_begin, out_end] = boost::out_edges(src, graph_);
                for (auto it = out_begin; it != out_end; ++it)
                {
                    if (graph_[*it] == edge)
                    {
                        return true;
                    }
                }
            }
        }
        else
        {
            return true;
        }
        return false;
    }

    /**
     * @brief Add a vertex, if possible.
     * @param vertex the vertex to add
     * @return true if vertex successfully added, or exists, false otherwise
     * @throw vertex_existence_error if graph is configured to throw and vertex cannot be added
     */
    bool addVertex(VertexPropertiesType const &vertex)
    {
        // if we don't allow multiple vertices, but the vertex already exists, then all is OK -> return true
        if constexpr (!AllowMultipleVertices)
        {
            if (hasVertex(vertex))
            {
                return true;
            }
        }

        // in connected graphs we cannot add a vertex that does not already exist, without creating a dis-connected
        // graph so this is an error, throw or return false
        if constexpr (Connected)
        {
            auto [v_begin, v_end] = boost::vertices(graph_);
            if (v_begin != v_end) // the graph is not empty
            {
                // vertex exists, so OK
                if (hasVertex(vertex))
                {
                    return true;
                }
                if constexpr (ThrowOnError)
                {
                    throw vertex_existence_error("In a connected graph adding a vertex when vertices already exist, "
                                                 "would create a disconnected graph. Use addEdge() instead.");
                }
                else
                {
                    return false;
                }
            }
        }
        // in all other cases add the vertex
        boost::add_vertex(vertex, graph_);
        return true;
    }

    /**
     * @brief Add multiple vertices to the graph.
     * @tparam Vertices vertices type
     * @param vertices the vertices to add
     * @throw vertex_existence_error on the first failure to add a vertex, if graph is configured to throw
     */
    template <typename... Vertices>
    bool addVertices(Vertices &&...vertices)
    {
        return (addVertex(std::forward<Vertices>(vertices)) && ...);
    }

    /**
     * Return a collection of all VertexProperties paired with the descriptors.
     * @return the collection of vertices
     */
    std::deque<std::pair<VertexPropertiesType, VertexDescriptor>> getVertexProperties() const
    {
        std::deque<std::pair<VertexPropertiesType, VertexDescriptor>> vertices;

        for (auto vertexDesc: boost::make_iterator_range(boost::vertices(graph_)))
        {
            // Push the vertex property into the deque
            vertices.push_back(std::make_pair(graph_[vertexDesc], vertexDesc));
        }

        return vertices;
    }

    /**
     * @brief Add an edge, if possible.
     * @param vertexSource source-vertex of the edge
     * @param vertexTarget target-vertex of the edge
     * @param edge edge-property
     * @return true if the edge has been added or has already existed before
     * @throw vertex_existence_error if source or target vertices do not exist, or in case of connected graphs,
     *                               if the addition of the edge would create a disconnected graph
     * @throw edge_existence_error if the edge between source and target already exists and parallel edges are
     *                             not allowed
     * @throw cycle_error if adding the edge would create a cycle, and cycles are not allowed
     * @throw single_parent_error if adding an edge would create an additional parent when there already is one
     */
    bool addEdge(
        VertexPropertiesType const &vertexSource,
        VertexPropertiesType const &vertexTarget,
        EdgePropertiesType const   &edge
    )
    {
        if constexpr (!AllowParallelEdges)
        {
            if (hasEdge(vertexSource, vertexTarget))
            {
                if constexpr (!OverWriteEdgeProperty)
                {
                    // exactly the same edge already exist, so this is OK
                    if (hasEdge(vertexSource, vertexTarget, edge))
                    {
                        return true;
                    }
                    if constexpr (ThrowOnError)
                    {
                        throw parallel_edge_error("Parallel edges are not allowed");
                    }
                    else
                    {
                        return false;
                    }
                }
            }
        }

        if constexpr (!AllowCycles)
        {
            if (vertexSource == vertexTarget || createsCycle_(vertexSource, vertexTarget, edge))
            {
                if constexpr (ThrowOnError)
                {
                    throw cycle_error("Adding this edge would create a cycle");
                }
                else
                {
                    return false;
                }
            }
        }

        if constexpr (SingleParent)
        {
            if (!getEdges<EdgeDirection::Parent>(vertexTarget).empty())
            {
                if constexpr (ThrowOnError)
                {
                    throw single_parent_error("Adding this edge would violate single-parent-property");
                }
                else
                {
                    return false;
                }
            }
        }

        if constexpr (!Connected && !AddVerticesViaEdge)
        {
            // if we cannot add vertices via the edge and the graph is disconnected, then we cannot add the edge when
            // any of the vertices is missing
            if (!hasVertices(vertexSource, vertexTarget))
            {
                if constexpr (ThrowOnError)
                {
                    throw vertex_existence_error("Both vertices must exist in the graph when adding an edge");
                }
                else
                {
                    return false;
                }
            }
        }
        else
        {
            // in connected graphs or if property AddVerticesViaEdge is set, we can add edges if
            // - graph is empty - both vertices will be created
            // - only one vertex exists - the other will be created
            // - both source and target exist - none needs to be created
            // if graph is not empty and both vertices do not exist, then adding the vertices would create a
            // disconnected sub-graph, so this is not allowed -> error/return false
            if (!hasVertex(vertexSource) && !hasVertex(vertexTarget))
            {
                auto [v_begin, v_end] = boost::vertices(graph_);
                if (v_begin == v_end) // no vertices
                {
                    // need to use boost function here because addVertex prevents "free" vertex
                    boost::add_vertex(vertexSource, graph_);
                    boost::add_vertex(vertexTarget, graph_);
                }
                else
                {
                    if constexpr (ThrowOnError)
                    {
                        throw vertex_existence_error(
                            "Creating an edge with two new vertices would create a disconnected graph"
                        );
                    }
                    else
                    {
                        return false;
                    }
                }
            }
            else if (!hasVertex(vertexSource))
            {
                // need to use boost function here because addVertex prevents "free" vertex
                boost::add_vertex(vertexSource, graph_);
            }
            else if (!hasVertex(vertexTarget))
            {
                // need to use boost function here because addVertex prevents "free" vertex
                boost::add_vertex(vertexTarget, graph_);
            }
        }

        auto [haveSrc, src] = getVertexDescriptor_(vertexSource);
        auto [haveTrg, trg] = getVertexDescriptor_(vertexTarget);

        if (haveSrc && haveTrg)
        {
            auto [edgeDesc, success] = boost::add_edge(src, trg, edge, graph_);
            return success;
        }
        return false;
    }

    /**
     * @brief Retrieve a collection of all parallel edges between sourceVertex and targetVertex.
     * @param sourceVertex property of the source-vertex
     * @param targetVertex property of the target-vertex
     * @return a collection of all parallel edges between sourceVertex and targetVertex
     */
    std::deque<EdgeDataType>
        getParallelEdges(VertexDataType const &sourceVertex, VertexDataType const &targetVertex) const noexcept
    {
        if (!hasVertex(sourceVertex) || !hasVertex(targetVertex))
        {
            return {}; // Return empty if any of the vertices is not in the graph
        }

        std::deque<EdgeDataType> parallelEdges;
        auto [edgeIter_begin, edgeIter_end] = boost::edges(graph_);

        for (auto it = edgeIter_begin; it != edgeIter_end; ++it)
        {
            VertexDescriptor src = boost::source(*it, graph_);
            VertexDescriptor tgt = boost::target(*it, graph_);

            // Check if both sourceVertex and targetVertex match
            if (graph_[src] == sourceVertex && graph_[tgt] == targetVertex)
            {
                parallelEdges.push_back(graph_[*it].data);
            }
        }

        return parallelEdges;
    }

    /**
     * Method to add a path using an initializer list of {Edge, Vertex} pairs
     * @param startVertex first vertex in the path
     * @param path edge-vertex pairs
     * @return true, if the whole path was added, false otherwise
     */
    bool addPath(VertexDataType const &startVertex, std::initializer_list<std::pair<EdgeDataType, VertexDataType>> path)
    {
        bool reval{};
        // Add the start vertex to the graph
        if (!hasVertex(startVertex))
        {
            reval = addVertex(startVertex);
        }
        else
        {
            reval = true;
        }

        VertexDataType currentVertex = startVertex;

        // Iterate over the initializer list
        for (auto const &pair: path)
        {
            if (!reval)
            {
                continue;
            }

            EdgeDataType const   &edge       = pair.first;  // Extract the edge
            VertexDataType const &nextVertex = pair.second; // Extract the next vertex

            if (!Connected && !AddVerticesViaEdge)
            {
                // Add the next vertex
                reval &= addVertex(nextVertex);
            }

            // Add the edge between the current and next vertex
            reval &= addEdge(currentVertex, nextVertex, edge);

            // Update currentVertex to the next vertex for the next iteration
            currentVertex = nextVertex;
        }

        return reval;
    }

    /**
     * @brief Remove a vertex if it exists, don't fail if the vertex does not exist.
     * @param vertex property of the vertex to remove
     * @return true, if the vertex existed and was removed, false if it did not exist
     */
    bool removeVertex(VertexPropertiesType const &vertex)
    {
        auto [haveVertex, vertexDesc] = getVertexDescriptor_(vertex);
        if (!haveVertex)
        {
            // non-critical so no throw
            return false;
        }

        if constexpr (Connected)
        {
            return removeConnectedVertex_(vertex, vertexDesc);
        }

        for (auto const &[srcProp, trgProp, edgeProp, srcDesc, trgDesc, edgeDesc]:
             getEdges<EdgeDirection::ParentAndChild>(vertex))
        {
            removeEdge(srcProp, trgProp);
        }
        boost::remove_vertex(vertexDesc, graph_);

        return true;
    }

    /**
     * @brief Remove an edgeProp if it exists.
     * @param vertexSource source vertex
     * @param vertexTarget target vertex
     * @param optional edgeProp
     * @return true if an has been removed or did not exist, false otherwise
     */
    bool removeEdge(
        VertexPropertiesType const       &vertexSource,
        VertexPropertiesType const       &vertexTarget,
        std::optional<EdgePropertiesType> edgeProp = std::nullopt
    )
    {
        auto [haveSrc, src] = getVertexDescriptor_(vertexSource);
        auto [haveTrg, trg] = getVertexDescriptor_(vertexTarget);
        if (!haveSrc || !haveTrg)
        {
            return true;
        }

        auto [edgeIter, found] = boost::edge(src, trg, graph_);
        if (!found)
        {
            // edgeProp doesn't exist, so OK
            return true;
        }

        if (edgeProp)
        {
            // edgeProp was given, so we need to check whether the edge between src and trg has prop edgeProp
            if (hasEdge(vertexSource, vertexTarget, edgeProp))
            {
                if constexpr (Connected)
                {
                    return removeConnectedEdge_(src, trg, edgeProp);
                }
                boost::remove_edge(edgeIter, graph_);
            }
            return true;
        }
        else
        {
            // we don't care about edgeProp and remove
            boost::remove_edge(edgeIter, graph_);
        }

        return true;
    }

    /**
     * @brief Apply a visitor to all vertices, filtered by a given filter function.
     * @tparam Visitor visitor function type
     * @tparam FilterFunc filter function type
     * @param vis visitor object
     * @param filter vertex filter
     */
    template <typename Visitor, typename FilterFunc = std::function<bool(VertexDataType const &)>>
    void applyVertices(Visitor &vis, FilterFunc filter = allVerticesFilter)
    {
        auto vertices = boost::vertices(graph_);
        for (auto it = vertices.first; it != vertices.second; ++it)
        {
            if (filter(graph_[*it].data)) // Apply the filter
            {
                vis(graph_[*it].data); // Apply the visitor
            }
        }
    }

    /**
     * @brief Apply a visitor to all edges, filtered by a given filter function.
     * @tparam Visitor visitor function type
     * @tparam FilterFunc filter function type
     * @param vis visitor object
     * @param filter edge filter
     */
    template <typename Visitor, typename FilterFunc = std::function<bool(EdgeDataType const &)>>
    void applyEdges(Visitor &vis, FilterFunc filter = allEdgesFilter)
    {
        auto edges = boost::edges(graph_);
        for (auto it = edges.first; it != edges.second; ++it)
        {
            if (filter(graph_[*it].data)) // Apply the filter
            {
                vis(graph_[*it].data); // Apply the visitor
            }
        }
    }

    /**
     * @brief Default vertex filter function.
     * @return true for all vertices
     */
    static constexpr bool allVerticesFilter(VertexDataType const &) noexcept
    {
        return true;
    }

    /**
     * @brief Default edge filter function.
     * @return true for all edges
     */
    static constexpr bool allEdgesFilter(EdgeDataType const &) noexcept
    {
        return true;
    }

    enum EdgeDirection
    {
        All,
        Parent,
        Child,
        ParentAndChild
    };

    /**
     * @brief Get a collection of edges and their properties.
     * @tparam direction get children of vertex, parents or both
     * @param vertexDesc optional vertex-descriptor serving as filter (source. parent or both)
     * @return a possibly filtered collection of edges, described by properties and descriptors
     */
    template <EdgeDirection direction = EdgeDirection::All>
    std::deque<std::tuple<
        VertexPropertiesType,
        VertexPropertiesType,
        EdgePropertiesType,
        VertexDescriptor,
        VertexDescriptor,
        EdgeDescriptor>>
        getEdges(std::optional<VertexPropertiesType> vertexProp = std::optional<VertexPropertiesType>{}) const
    {
        std::optional<VertexDescriptor> vertexDesc{};
        if (vertexProp)
        {
            if (!hasVertex(vertexProp.value()))
            {
                return {};
            }
            auto [found, desc] = getVertexDescriptor_(vertexProp.value());
            vertexDesc         = desc;
        }
        return getEdges_<direction>(vertexDesc);
    }

    /**
     * @brief Get a collection of edges and their properties.
     * @tparam direction get children of vertex, parents or both
     * @param vertexDesc optional vertex-descriptor serving as filter (source. parent or both)
     * @return a possibly filtered collection of edges, described by properties and descriptors
     */
    template <EdgeDirection direction = EdgeDirection::All>
    std::deque<std::tuple<
        VertexPropertiesType,
        VertexPropertiesType,
        EdgePropertiesType,
        VertexDescriptor,
        VertexDescriptor,
        EdgeDescriptor>>
        getEdges_(std::optional<VertexDescriptor> vertexDesc = std::optional<VertexDescriptor>{}) const
    {
        std::deque<std::tuple<
            VertexPropertiesType,
            VertexPropertiesType,
            EdgePropertiesType,
            VertexDescriptor,
            VertexDescriptor,
            EdgeDescriptor>>
                                            revalEdges;
        std::optional<VertexPropertiesType> vertexProp{};

        if constexpr (direction != EdgeDirection::All)
        {
            if (vertexDesc)
            {
                vertexProp = graph_[vertexDesc.value()];
            }
        }

        // Iterate over all edges in the graph
        for (auto edgeDesc: boost::make_iterator_range(boost::edges(graph_)))
        {
            // Get the source and target vertices for the current edge
            auto src = boost::source(edgeDesc, graph_);
            auto tgt = boost::target(edgeDesc, graph_);

            // Get the edge property for the current edge
            auto edgeProp = graph_[edgeDesc];

            // Get the vertex properties for source and target
            auto srcProp = graph_[src];
            auto tgtProp = graph_[tgt];

            // get all ignores the vertexDesc parameter
            if constexpr (direction == EdgeDirection::All)
            {
                revalEdges.emplace_back(srcProp, tgtProp, edgeProp, src, tgt, edgeDesc);
            }
            else if constexpr (direction == EdgeDirection::ParentAndChild || direction == EdgeDirection::Parent)
            {
                // get edges where the found vertex is parent of the given vertex
                if (vertexDesc && tgtProp == vertexProp)
                {
                    revalEdges.emplace_back(srcProp, tgtProp, edgeProp, src, tgt, edgeDesc);
                }
            }
            // get edges where the found vertex is child of the given vertex
            else if constexpr (direction == EdgeDirection::Child)
            {
                if (vertexDesc && srcProp == vertexProp)
                {
                    revalEdges.emplace_back(srcProp, tgtProp, edgeProp, src, tgt, edgeDesc);
                }
            }
        }

        return revalEdges;
    }

    /**
     * @brief A complexity structure for graphs.
     */
    struct GraphComplexity
    {
        size_t numVertices{};
        size_t numEdges{};
        double density{};
        double average_degree{};
        double cyclomatic_complexity{};
        int    diameter{};
        auto operator<=>(GraphComplexity const &rhs) const = default;
    };


    GraphComplexity complexity(long long numConnectedComponents = -1)
    {
        GraphComplexity complexity;

        // Number of vertices and edges
        complexity.numVertices = boost::num_vertices(graph_);
        complexity.numEdges    = boost::num_edges(graph_);

        // Graph density
        complexity.density = (complexity.numVertices > 1) ? static_cast<double>(complexity.numEdges) /
                                                                (complexity.numVertices * (complexity.numVertices - 1))
                                                          : 0.0;

        // Average degree
        complexity.average_degree =
            (complexity.numVertices > 0) ? static_cast<double>(2 * complexity.numEdges) / complexity.numVertices : 0.0;

        // Cyclomatic complexity (number of independent cycles)
        if (numConnectedComponents < 0)
        {
            numConnectedComponents = getDisconnectedSubGraphs().size();
        }
        complexity.cyclomatic_complexity = static_cast<double>(complexity.numEdges) -
                                           static_cast<double>(complexity.numVertices) +
                                           static_cast<double>(numConnectedComponents);

        complexity.diameter                               = 0;
        auto [undirGraph, dir2undirVertex, dir2undirEdge] = getUndirectedGraph();
        using UndirVertexDescriptor                       = boost::graph_traits<UndirectedGraph>::vertex_descriptor;

        std::unordered_map<UndirVertexDescriptor, int> vertex_indices;
        int                                            i = 0;
        for (auto vertexDesc: boost::make_iterator_range(boost::vertices(undirGraph)))
        {
            vertex_indices[vertexDesc] = i++;
        }
        auto index_map = boost::make_assoc_property_map(vertex_indices);

        using UndirEdgeDescriptor = boost::graph_traits<UndirectedGraph>::edge_descriptor;
        std::map<UndirEdgeDescriptor, int> default_weights;
        for (auto edgeDesc: boost::make_iterator_range(boost::edges(undirGraph)))
        {
            default_weights[edgeDesc] = 1; // Assign default weight
        }
        auto weight_map = boost::make_assoc_property_map(default_weights);

        // Run Dijkstra from each vertex to compute diameter
        for (auto vertexDesc: boost::make_iterator_range(boost::vertices(undirGraph)))
        {
            std::vector<int> distances(boost::num_vertices(undirGraph), std::numeric_limits<int>::max());
            boost::dijkstra_shortest_paths(
                undirGraph,
                vertexDesc,
                boost::distance_map(&distances[0]).weight_map(weight_map).vertex_index_map(index_map)
            );

            for (int dist: distances)
            {
                if (dist < std::numeric_limits<int>::max())
                {
                    complexity.diameter = std::max(complexity.diameter, dist);
                }
            }
        }

        return complexity;
    }

    /**
     * @brief Get a collection of all disconnected sub-graphs of the graph, most complex first.
     * @return a collection of all disconnected sub-graphs
     */
    std::multimap<GraphComplexity, directed_graph_base, std::greater<GraphComplexity>>
        getDisconnectedSubGraphs() // const
    {
        auto [undirGraph, dir2undirVertex, dir2undirEdge] = getUndirectedGraph();

        auto numDGEs     = boost::num_edges(undirGraph);
        auto numVertices = boost::num_vertices(undirGraph);
        // Map to store the component for each vertex
        std::deque<int> component_map;
        component_map.resize(numVertices);
        // Get the number of weakly connected_managed components
        int num_components = boost::connected_components(undirGraph, &component_map[0]);

        // Create a collection of disconnected subGraphs
        std::deque<std::deque<VertexDataType>> subGraphs(num_components);

        // Map each vertex to its corresponding component
        for (auto vertexDesc: boost::make_iterator_range(boost::vertices(graph_)))
        {
            auto undirVertexDesc = dir2undirVertex[vertexDesc];
            int  component_index = component_map[undirVertexDesc]; // Use the mapped vertex in undirGraph
            subGraphs[component_index].push_back(graph_[vertexDesc].data);
        }

        auto                                                                               allEdges = getEdges();
        std::multimap<GraphComplexity, directed_graph_base, std::greater<GraphComplexity>> revalGraphs{};
        for (auto const &vertexList: subGraphs)
        {
            directed_graph_base sub{};
            for (auto vertex: vertexList)
            {
                if (!sub.hasVertex(vertex))
                {
                    sub.addVertex(vertex);
                }
                for (auto [srcProp, trgProp, edgeProp, srcDesc, trgDesc, edgeDesc]: allEdges)
                {
                    if (vertex == srcProp)
                    {
                        if (!sub.hasVertex(trgProp))
                        {
                            sub.addVertex(trgProp);
                        }
                        sub.addEdge(srcProp, trgProp, edgeProp);
                    }
                }
            }
            // every subgraph now only has one subgraph, so don't need to calculate again
            revalGraphs.emplace(sub.complexity(1UL), sub);
        }
        return revalGraphs;
    }

    /**
     * @brief Function to return all paths from one vertex to another.
     * Each path includes both vertices and edge properties.
     *
     * @param vertexSource Source vertex
     * @param vertexTarget Target vertex
     * @return std::deque<std::deque<std::pair<VertexDataType, EdgeDataType>>> Collection of paths, where each path is
     * a sequence of {VertexDataType, EdgeDataType} pairs
     */
    std::deque<VertexEdgePath<VertexPropertiesType, EdgePropertiesType>>
        getAllPaths(VertexPropertiesType const &vertexSource, VertexPropertiesType const &vertexTarget) const
    {
        // Check if both source and target vertices exist
        VertexDescriptor src, tgt;
        if (!getVertexDescriptor_(vertexSource, src) || !getVertexDescriptor_(vertexTarget, tgt))
        {
            return {}; // Return an empty deque if either vertex does not exist
        }

        // Store all paths
        std::deque<VertexEdgePath<VertexDataType, EdgeDataType>> paths;
        VertexEdgePath<VertexDataType, EdgeDataType>             current_path;
        std::unordered_set<VertexDescriptor>                     visited;

        // DFS helper to explore all paths
        std::function<void(VertexDescriptor)> dfs = [&](VertexDescriptor v) {
            visited.insert(v);

            // Explore outgoing edges
            auto outEdges = boost::out_edges(v, graph_);
            for (auto edgeIt = outEdges.first; edgeIt != outEdges.second; ++edgeIt)
            {
                VertexDescriptor source   = boost::source(*edgeIt, graph_);
                VertexDescriptor target   = boost::target(*edgeIt, graph_);
                EdgeDataType     edgeProp = graph_[*edgeIt]; // Get the edge property

                // Add the current vertex and edge to the path
                current_path.add(graph_[source], edgeProp, graph_[target]);

                // If the target vertex is the destination, save the full path
                if (target == tgt)
                {
                    paths.push_back(current_path);
                }
                else if (visited.find(target) == visited.end())
                {
                    // Continue the DFS if the target has not been visited yet
                    dfs(target);
                }
            }

            visited.erase(v);
        };

        // Start DFS from the source vertex
        dfs(src);

        return paths;
    }

    /**
     * @note HERE BE DRAGONS!
     * @brief This is a getter for the underlying boost graph (adjacency_list) by reference. Use this in your own
     *        responsibility. The basic_directed_graph - template makes guarantees about shape, cyclicity, etc which are
     *        enforced by the implementation, but not by boost-methods. If you use use features that break these
     *        promises you are likely to get undefined or at least unexpected behaviour.
     * @return the underlying boost::adjacency_list<>
     */
    Graph &graph()
    {
        return graph_;
    }

    /**
     * @brief get an undirected copy of this graph with vectors as storage.
     * @return an undirected vector-only copy
     */
    std::tuple<
        UndirectedGraph,
        std::map<VertexDescriptor, UndirectedVertexDescriptor>,
        std::map<EdgeDescriptor, UndirectedEdgeDescriptor>>
        getUndirectedGraph() const
    {
        UndirectedGraph                                        undirectedGraph{};
        std::map<VertexDescriptor, UndirectedVertexDescriptor> dir2undirVertex{};
        std::map<EdgeDescriptor, UndirectedEdgeDescriptor>     dir2undirEdge{};

        // Track added edges to avoid duplicates
        std::unordered_set<
            std::pair<VertexDescriptor, VertexDescriptor>,
            boost::hash<std::pair<VertexDescriptor, VertexDescriptor>>>
            added_edges;

        // Copy vertex properties
        for (auto vertexDesc: boost::make_iterator_range(boost::vertices(graph_)))
        {
            // Assume the add_vertex function copies vertex properties
            auto new_vertex = boost::add_vertex(graph_[vertexDesc], undirectedGraph);
            // Optionally map new_vertex to v if needed
            dir2undirVertex[vertexDesc] = new_vertex;
        }

        // Iterate over the edges in the directed graph
        for (auto edge: boost::make_iterator_range(edges(graph_)))
        {
            auto source_vertex = source(edge, graph_);
            auto target_vertex = target(edge, graph_);

            // Ensure undirected edge (min, max) is only added once
            auto edge_pair = std::minmax(source_vertex, target_vertex);
            if (added_edges.insert(edge_pair).second)
            {
                // Copy edge properties when adding the edge
                auto [undirEdge, inserted] = boost::add_edge(
                    dir2undirVertex[edge_pair.first],
                    dir2undirVertex[edge_pair.second],
                    graph_[edge],
                    undirectedGraph
                );
                if (inserted)
                {
                    dir2undirEdge[edge] = undirEdge;
                }
            }
        }

        return std::make_tuple(undirectedGraph, dir2undirVertex, dir2undirEdge);
    }

  private:
    /**
     * @brief Safe retrieval of the vertex-descriptor (pointer/iterator/index/...) of a given vertex property.
     * This iterates through all vertices until it finds a matching vertex.
     * @param vertex vertex property
     * @param descriptor [out] the descriptor, if it can be found
     * @return true, if the descriptor was found, false otherwise
     */
    std::tuple<bool, VertexDescriptor> getVertexDescriptor_(VertexPropertiesType const &vertex) const noexcept
    {
        auto [vertIter_begin, vertIter_end] = boost::vertices(graph_);
        for (auto it = vertIter_begin; it != vertIter_end; ++it)
        {
            if (graph_[*it] == vertex)
            {
                return std::make_tuple(true, *it);
            }
        }
        return std::make_tuple(false, VertexDescriptor{});
    }

    /**
     * @brief Cycle detection implementation.
     * @param vertexSource property of edge-source
     * @param vertexTarget property of edge-target
     * @param edge edge-property
     * @return true, if adding an edge from source to target would create a cycle in the graph, false otherwise
     */
    bool createsCycle_(
        VertexPropertiesType const &vertexSource,
        VertexPropertiesType const &vertexTarget,
        EdgePropertiesType const   &edge
    )
    {
        // Get vertex descriptors for source and target vertices
        auto [haveSrc, src] = getVertexDescriptor_(vertexSource);
        auto [haveTrg, trg] = getVertexDescriptor_(vertexTarget);

        if (!haveSrc || !haveTrg)
        {
            return false;
        }

        // Check if an edge between the source and target already exists
        auto [existingEdge, exists] = boost::edge(src, trg, graph_);

        // If the edge already exists, no need to check for cycles
        if (exists)
        {
            return false; // Adding another parallel edge doesn't introduce a cycle
        }

        // Add the edge temporarily
        boost::add_edge(src, trg, edge, graph_);

        // Check for cycles using DFS or BFS (Vertex*Descriptor* is always hashable!)
        std::unordered_map<VertexDescriptor, bool> visited(boost::num_vertices(graph_));
        std::deque<VertexDescriptor>               recursion_stack;

        bool hasCycle = dfsCycleCheck_(src, visited, recursion_stack);

        // Remove the temporarily added edge
        boost::remove_edge(src, trg, graph_);

        return hasCycle;
    }

    /**
     * @brief Depth-first-search-based cycle check.
     * @param vertexDescriptor the storage-location of the vertex (pointer/iterator/...)
     * @param visited map to indicate whether a vertex has been visited (true) or not (false)
     * @param recursion_stack the work do do
     * @return true if a cycle has been detected
     */
    bool dfsCycleCheck_(
        VertexDescriptor                            vertexDescriptor,
        std::unordered_map<VertexDescriptor, bool> &visited,
        std::deque<VertexDescriptor>               &recursion_stack
    )
    {
        if (!visited[vertexDescriptor])
        {
            visited[vertexDescriptor] = true;
            recursion_stack.push_back(vertexDescriptor);

            auto neighbors = boost::adjacent_vertices(vertexDescriptor, graph_);
            for (auto neighbor = neighbors.first; neighbor != neighbors.second; ++neighbor)
            {
                if ((!visited[*neighbor] && dfsCycleCheck_(*neighbor, visited, recursion_stack)) ||
                    std::find(recursion_stack.begin(), recursion_stack.end(), *neighbor) != recursion_stack.end())
                {
                    return true;
                }
            }
        }

        recursion_stack.pop_back();
        return false;
    }

    /**
     * @brief Check whether removing a vertex would cause a disconnected graph.
     * @param vertexDesc vertex-descriptor
     * @return true, if removing a vertex would cause a split, false otherwise
     */
    bool causesDisconnection_(VertexPropertiesType const &vertexProp)
    {
        auto [undirGraph, dir2undirVertex, dir2undirEdge] = getUndirectedGraph();
        bool                       found                  = false;
        UndirectedVertexDescriptor vertexDesc{};

        for (auto vertexDescIter: boost::make_iterator_range(vertices(undirGraph)))
        {
            if (undirGraph[vertexDescIter].data == vertexProp)
            {
                vertexDesc = vertexDescIter;
                found      = true;
                break;
            }
        }
        if (!found)
        {
            return false;
        }

        // Remove the vertex
        boost::clear_vertex(vertexDesc, undirGraph);
        boost::remove_vertex(vertexDesc, undirGraph);

        // Create an index map for vertices
        std::unordered_map<UndirectedVertexDescriptor, int> vertex_index_map;
        int                                                 index = 0;
        for (auto vertexDesc: boost::make_iterator_range(boost::vertices(undirGraph)))
        {
            vertex_index_map[vertexDesc] = index++;
        }
        auto index_map = boost::make_assoc_property_map(vertex_index_map);

        // Prepare component storage
        std::unordered_map<UndirectedVertexDescriptor, int> component(boost::num_vertices(undirGraph));
        // Check the number of connected components
        auto num_conn_comp = boost::connected_components(undirGraph, index_map);
        return num_conn_comp > 1;
    }

    /**
     * @brief Remove a vertex from a connected graph.
     * @param vertex vertex properties
     * @param vertexDesc vertex descriptor
     * @return true if vertex successfully remove or non-existent, false otherwise
     * @throws disconnection_error if graph configured to throw and splits are disallowed
     */
    bool removeConnectedVertex_(VertexPropertiesType const &vertex, VertexDescriptor vertexDesc)
    {
        if constexpr (Connected)
        {
            if constexpr (StayConnectedStrategy)
            {
                boost::clear_vertex(vertexDesc, graph_);
                boost::remove_vertex(vertexDesc, graph_);
                auto subGraphs = getDisconnectedSubGraphs();
                if (!subGraphs.empty())
                {
                    graph_ = subGraphs.begin()->second.graph_;
                }
                else
                {
                    graph_.clear();
                }
                return true;
            }
            else // disallow split
            {
                if (!hasVertex(vertex))
                {
                    return true;
                }
                if (causesDisconnection_(vertex))
                {
                    if constexpr (ThrowOnError)
                    {
                        throw disconnection_error("removing the vertex would create a disconnected graph");
                    }
                    else
                    {
                        return false;
                    }
                }
                boost::clear_vertex(vertexDesc, graph_);
                boost::remove_vertex(vertexDesc, graph_);
                return true;
            }
        }

        return false;
    }

    /**
     * @brief Check whether removing an edgeDesc would cause a disconnected graph.
     * @param edgeDesc edge-descriptor
     * @return true, if removing the edge would cause a split, false otherwise
     */
    bool causesDisconnection_(VertexDescriptor src, VertexDescriptor trg, std::optional<EdgePropertiesType> edge)
    {
        auto [undirGraph, dir2undirVertex, dir2undirEdge] = getUndirectedGraph();
        bool                       found                  = false;
        UndirectedVertexDescriptor undirSrc               = dir2undirVertex[src];
        UndirectedVertexDescriptor undirTrg               = dir2undirVertex[trg];

        // Remove the edge from the undirected graph
        auto [foundUndirEdgeDesc, foundEdge] = boost::edge(undirSrc, undirTrg, undirGraph);
        if (foundEdge)
        {
            boost::remove_edge(foundUndirEdgeDesc, undirGraph);
        }
        else
        {
            return false;
        }

        // Create an index map for vertices
        std::unordered_map<UndirectedVertexDescriptor, int> vertex_index_map;
        int                                                 index = 0;
        for (auto vertexDesc: boost::make_iterator_range(boost::vertices(undirGraph)))
        {
            vertex_index_map[vertexDesc] = index++;
        }
        auto index_map = boost::make_assoc_property_map(vertex_index_map);

        // Prepare component storage
        std::unordered_map<UndirectedVertexDescriptor, int> component(boost::num_vertices(undirGraph));
        // Check the number of connected components
        auto num_conn_comp = boost::connected_components(undirGraph, index_map);
        return num_conn_comp > 1;
    }

    bool removeConnectedEdge_(
        VertexDescriptor const           &vertexDescSource,
        VertexDescriptor const           &vertexDescTarget,
        std::optional<EdgePropertiesType> edge = std::nullopt
    )
    {
        if constexpr (Connected)
        {
            if constexpr (StayConnectedStrategy)
            {
                boost::remove_edge(vertexDescSource, vertexDescTarget, graph_);
                auto subGraphs = getDisconnectedSubGraphs();
                if (!subGraphs.empty())
                {
                    graph_ = subGraphs.begin()->second.graph_;
                }
                else
                {
                    graph_.clear();
                }
                return true;
            }
            else // disallow split
            {
                if (causesDisconnection_(vertexDescSource, vertexDescTarget, edge))
                {
                    if constexpr (ThrowOnError)
                    {
                        throw disconnection_error("removing the edge would create a disconnected graph");
                    }
                    else
                    {
                        return false;
                    }
                }
                auto [edgeDesc, haveEdge] = boost::edge(vertexDescSource, vertexDescTarget, graph_);
                boost::remove_edge(edgeDesc, graph_);

                return true;
            }
        }

        return true;
    }
};

/**
 * @brief A typedef for simple graphs.
 * <ul>
 * <li>no vertices with same property allowed</li>
 * <li>parallel edges are not allowed</li>
 * </ul>
 * @tparam VertexDataType vertex data type, no restrictions, but may influence the storage
 * @tparam EdgeDataType edge data type, no restrictions, but may influence the storage
 */
template <typename VertexDataType, typename EdgeDataType, typename... Options>
struct simple_graph
    : public directed_graph_base<
          VertexDataType,
          EdgeDataType,
          disallow_multiple_vertices,
          disallow_parallel_edges,
          Options...>
{
    static_assert(
        !has_disallowed_option_v<allow_multiple_vertices>,
        "simple_graph cannot have multiple vertices with same property"
    );
    static_assert(!has_disallowed_option_v<allow_parallel_edges>, "simple_graph cannot have parallel edges");
};

/**
 * @brief A typedef for directed acyclic graphs.
 * <ul>
 * <li>no cycles allowed</li>
 * </ul>
 * @tparam VertexDataType vertex data type, no restrictions, but may influence the storage
 * @tparam EdgeDataType edge data type, no restrictions, but may influence the storage
 */
template <typename VertexDataType, typename EdgeDataType, typename... Options>
struct directed_acyclic_graph : public directed_graph_base<VertexDataType, EdgeDataType, disallow_cycles, Options...>
{
    static_assert(!has_disallowed_option_v<allow_cycles>, "directed_acyclic_graph cannot have cycles");
};

/**
 * @brief A typedef for forest graphs - possibly disconnected tree-like sub-graphs.
 * <ul>
 * <li>vertices with same property are not allowed</li>
 * <li>no parallel edges are allowed</li>
 * <li>no cycles allowed</li>
 * <li>no vertex can have more than one parent</li>
 * </ul>
 * @tparam VertexDataType vertex data type, no restrictions, but may influence the storage
 * @tparam EdgeDataType edge data type, no restrictions, but may influence the storage
 */
template <typename VertexDataType, typename EdgeDataType, typename... Options>
struct forest_graph
    : public directed_graph_base<
          VertexDataType,
          EdgeDataType,
          disallow_multiple_vertices,
          disallow_parallel_edges,
          disallow_cycles,
          single_parent,
          Options...>
{
    static_assert(
        !has_disallowed_option_v<allow_multiple_vertices>,
        "forest_graph cannot have multiple vertices with same property"
    );
    static_assert(!has_disallowed_option_v<allow_parallel_edges>, "forest_graph cannot have parallel edges");
    static_assert(!has_disallowed_option_v<allow_cycles>, "forest_graph cannot have cycles");
    static_assert(!has_disallowed_option_v<multiple_parent>, "forest_graph cannot vertices with multiple parents");
};

/**
 * @brief A struct connected graphs.
 * <ul>
 * <li>the graph is always connected, i.e. there is always ever only 1 (max) sub-graph</li>
 * <li>connection strategies
 * <ul>
 * <li>stay_connected: any action that splits the graph is allowed, only the most complex sub-graph is retained</li>
 * <li>disallow_split: any action that would create disconnected sub-graphs results in error (return false or
 * throw)</li>
 * </ul></li>
 * </ul>
 * @tparam VertexDataType vertex data type, no restrictions, but may influence the storage
 * @tparam EdgeDataType edge data type, no restrictions, but may influence the storage
 */
template <typename VertexDataType, typename EdgeDataType, typename... Options>
struct connected_graph : public directed_graph_base<VertexDataType, EdgeDataType, connected, Options...>
{
    static_assert(!has_disallowed_option_v<disconnected>, "connected graph cannot be disconnected");
};

}; // namespace util::graph

#endif // NS_UTIL_GRAPH_DIRECTED_GRAPH_H_INCLUDED
