/*
 * Repository:  https://github.com/kingkybel/CPP-utilities
 * File Name:   test/directed_graph_tests_debug_functions.cc
 * Description: Functions to show graph-settings
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

#ifndef NS_UTIL_DIRECTED_GRAPH_TESTS_DEBUG_FUNCTIONS_H
#define NS_UTIL_DIRECTED_GRAPH_TESTS_DEBUG_FUNCTIONS_H

#undef TRACING_ADDED_
#ifndef DO_TRACE_
    #define DO_TRACE_
    #define TRACING_ADDED
#endif
#include <dkyb/traceutil.h>
#include <typeinfo>

namespace util
{
namespace graph
{
#ifdef DO_GRAPH_DEBUG_TRACE_

    #define DECLARE_TEST_DEBUG_HELPERS                                                                                 \
        template <typename Type>                                                                                       \
        std::string scanForBoostSelector()                                                                             \
        {                                                                                                              \
            std::string              typeStr{typeid(Type).name()};                                                     \
            std::vector<std::string> selectors = {                                                                     \
                /*ORDER OF STRINGS MATTERS!*/                                                                          \
                "hash_setS",                                                                                           \
                "hash_mapS",                                                                                           \
                "hash_multisetS",                                                                                      \
                "hash_multimapS",                                                                                      \
                "multisetS",                                                                                           \
                "multimapS",                                                                                           \
                "vecS",                                                                                                \
                "listS",                                                                                               \
                "setS",                                                                                                \
                "mapS",                                                                                                \
            };                                                                                                         \
                                                                                                                       \
            for (auto const &selector: selectors)                                                                      \
            {                                                                                                          \
                if (typeStr.find(selector) != std::string::npos)                                                       \
                {                                                                                                      \
                    return std::string("boost::") + selector;                                                          \
                }                                                                                                      \
            }                                                                                                          \
                                                                                                                       \
            return ""; /* No match found */                                                                            \
        }                                                                                                              \
                                                                                                                       \
        void printTypesAndBools(const std::string &graphVar)                                                           \
        {                                                                                                              \
            TRACE_STR1(== == == == == == == == == == == == == == == == == == == ==);                                   \
            TRACE1(graphVar);                                                                                          \
            TRACE0                                                                                                     \
            TRACE1(AllowMultipleVertices);                                                                             \
            TRACE1(AllowParallelEdges);                                                                                \
            TRACE1(AllowCycles);                                                                                       \
            TRACE1(ThrowOnError);                                                                                      \
            TRACE1(SingleParent);                                                                                      \
            TRACE1(Connected);                                                                                         \
            TRACE1(OverWriteEdgeProperty);                                                                             \
            auto outEdgeStorage = scanForBoostSelector<OutEdgeListS>();                                                \
            TRACE1(outEdgeStorage);                                                                                    \
            auto vertexStorage = scanForBoostSelector<VertexStorage>();                                                \
            TRACE1(vertexStorage);                                                                                     \
            auto edgeStorage = scanForBoostSelector<EdgeStorage>();                                                    \
            TRACE1(edgeStorage);                                                                                       \
            auto fallbackStorage = scanForBoostSelector<FallbackSelector>();                                           \
            TRACE1(fallbackStorage);                                                                                   \
            TRACE0                                                                                                     \
        }

    #define PRINT_TYPES_AND_BOOLS(graphP)                                                                              \
        {                                                                                                              \
            graphP.printTypesAndBools(#graphP);                                                                        \
        }
    #define SHOW_SUB_GRAPHS(graph)                                                                                     \
        {                                                                                                              \
            size_t i{};                                                                                                \
            std::cout << "------ " << #graph << " ------" << std::endl;                                                \
            for (auto sub: graph.getDisconnectedSubGraphs())                                                           \
            {                                                                                                          \
                std::cout << "// subgraph(" << i << ")";                                                               \
                std::cout << "\n// vertices=" << sub.first.numVertices << " edges=" << sub.first.numEdges              \
                          << "\n// cyclomatic_complexity=" << sub.first.cyclomatic_complexity                          \
                          << "\n// diameter=" << sub.first.diameter << "\n// density=" << sub.first.density            \
                          << "\n// average_degree=" << sub.first.average_degree << sub.second << std::endl;            \
                i++;                                                                                                   \
            }                                                                                                          \
        }
#else
    #define DECLARE_TEST_DEBUG_HELPERS
    #define PRINT_TYPES_AND_BOOLS(graphP)
    #define SHOW_SUB_GRAPHS(graph)
#endif
} // namespace graph
} // namespace util

#ifdef TRACING_ADDED_
    #undef DO_TRACE_
    #undef TRACING_ADDED
#endif

#endif // NS_UTIL_DIRECTED_GRAPH_TESTS_DEBUG_FUNCTIONS_H
