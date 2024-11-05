/*
 * Repository:  https://github.com/kingkybel/CPP-utilities
 * File Name:   include/graph_to_string.h
 * Description: graph stream operators
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
 * @date: 2024-10-05
 * @author: Dieter J Kybelksties
 */

#ifndef NS_UTIL_GRAPH_TO_STRING_H_INCLUDED
#define NS_UTIL_GRAPH_TO_STRING_H_INCLUDED

#include "directed_graph.h"

#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphviz.hpp>
#include <dkyb/to_string.h>
#include <fstream>
#include <ostream>
#include <sstream>
#include <type_traits>

/**
 * @brief Create a map between vertex-descriptors and ordinal-numbers to serve as nodes in graphwiz.
 * @tparam VertexDataType vertex property type
 * @tparam EdgeProperty edge property type
 * @tparam Options option of the graph
 * @param dirGraph directed graph object
 * @return map between the vertex-descriptors of the graph and successive numbers starting from 0
 */
template <typename VertexDataType, typename EdgeProperty, typename... Options>
std::unordered_map<
    typename util::graph::directed_graph_base<VertexDataType, EdgeProperty, Options...>::VertexDescriptor,
    size_t>
    makeVertexMap(util::graph::directed_graph_base<VertexDataType, EdgeProperty, Options...> const& dirGraph)
{
    std::unordered_map<
        typename util::graph::directed_graph_base<VertexDataType, EdgeProperty, Options...>::VertexDescriptor,
        size_t>
        reval;
    using GraphType = std::remove_cv<util::graph::directed_graph_base<VertexDataType, EdgeProperty, Options...>>::type;
    auto const& [v_begin, v_end] = boost::vertices(static_cast<GraphType>(dirGraph).graph());

    auto verts = dirGraph.getVertexProperties();
    if constexpr(!GraphType::VertexPropertiesType::HasIndex)
    {
        size_t index = 0UL;
        for (auto [prop, desc]: verts)
        {
            reval[desc] = index;
            index++;
        }
    }
    else
    {
        for (auto [prop, desc]: verts)
        {
            reval[desc] = prop.index;
        }
    }
    return reval;
}

/**
 * @brief std::ostream operator for streaming directed graphs
 * @tparam CharT_ char-type
 * @tparam VertexProperty streamable vertex property
 * @tparam EdgeProperty streamable edge property
 * @tparam Options variadic graph-options
 * @param os the out-stream to stream to
 * @param dirGraph the graph to stream
 * @return the modified out-stream
 */
template <typename CharT_, typename VertexProperty, typename EdgeProperty, typename... Options>
std::basic_ostream<CharT_>& operator<<(
    std::basic_ostream<CharT_>&                                                       os,
    util::graph::directed_graph_base<VertexProperty, EdgeProperty, Options...> const& dirGraph
)
{
    auto vertexMap = makeVertexMap(dirGraph);
    os << std::endl << "digraph G {";
    auto vertices = dirGraph.getVertexProperties();
    os << std::endl;
    for (const auto&[prop, desc]: vertices)
    {
        os << "\t" << vertexMap[desc] << "\t[label=" << prop.data << "];" << std::endl;
    }
    auto edges = dirGraph.getEdges();
    for (const auto [srcProp, trgProp, edgeProp, srcDesc, trgDesc, edgeDesc]: edges)
    {
        os << "\t" << vertexMap[srcDesc] << " -> " << vertexMap[trgDesc]
           << "\t[label=" << edgeProp.data << "];" << std::endl;
    }
    os << "}" << std::endl;
    return os;
}

#endif // NS_UTIL_GRAPH_TO_STRING_H_INCLUDED
