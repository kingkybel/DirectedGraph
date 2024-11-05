/*
 * Repository:  https://github.com/kingkybel/CPP-utilities
 * File Name:   test/directed_graph_traits_tests.cc
 * Description: Unit tests storage selector and option-selection traits
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
 * @date: 2024-10-11
 * @author: Dieter J Kybelksties
 */

#include "directed_graph.h"
#include "test_property_classes.h"

#include <dkyb/to_string.h>
// #define DO_TRACE_
#include <dkyb/traceutil.h>
#include <gtest/gtest.h>
#include <string>
#include <typeinfo>

using namespace std;
using namespace util;
using namespace util::graph;

class GraphTraitsTest : public ::testing::Test
{
  protected:
    void SetUp() override
    {
    }

    void TearDown() override
    {
    }
};

template <typename Type>
std::string getBoostSelector()
{
    std::string              typeStr{typeid(Type).name()};
    std::vector<std::string> selectors = {
        // ORDER OF STRINGS MATTERS!
        "hash_setS",
        "hash_mapS",
        "hash_multisetS",
        "hash_multimapS",
        "multisetS",
        "multimapS",
        "vecS",
        "listS",
        "setS",
        "mapS",
    };

    for (auto const &selector: selectors)
    {
        if (typeStr.find(selector) != std::string::npos)
        {
            return selector;
        }
    }

    return ""; // No match found
}

template <
    typename PropType,
    typename AllowParallelEdges,
    typename AllowMultipleVertices,
    typename ExpectedSelectorEdge,
    typename ExpectedSelectorVertex,
    typename PreferComparableContainer,
    typename FallbackSelector>
void testCorrectTypes()
{
    using edge_prop_t   = PropType;
    using vertex_prop_t = PropType;
    // clang-format off
    using edge_storage_t = directed_graph_base<edge_prop_t,
                                              vertex_prop_t,
                                              fallback_selector<FallbackSelector>,
                                              AllowParallelEdges,
                                              PreferComparableContainer>::OutEdgeListS;

    using vertex_storage_t = directed_graph_base<edge_prop_t,
                                                 vertex_prop_t,
                                                 fallback_selector<FallbackSelector>,
                                                 AllowMultipleVertices,
                                                 PreferComparableContainer>::VertexStorage;

    using fallback_storage_t = directed_graph_base<edge_prop_t,
                                                 vertex_prop_t,
                                                 fallback_selector<FallbackSelector>,
                                                 AllowMultipleVertices,
                                                 PreferComparableContainer>::FallbackSelector;
    // clang-format on
    ASSERT_EQ(typeid(fallback_storage_t), typeid(FallbackSelector))
        << "FallbackSelector Expected:" << getBoostSelector<fallback_storage_t>() << " actual:" << getBoostSelector<FallbackSelector>()
        << std::endl;

    ASSERT_EQ(typeid(edge_storage_t), typeid(ExpectedSelectorEdge))
        << "edge(" << PropType::name() << ","
        << (AllowParallelEdges::value ? "allow_parallel_edges" : "disallow_parallel_edges")
        << ") Expected:" << getBoostSelector<ExpectedSelectorEdge>() << " actual:" << getBoostSelector<edge_storage_t>()
        << std::endl;

    ASSERT_EQ(typeid(vertex_storage_t), typeid(ExpectedSelectorVertex))
        << "vertex(" << PropType::name() << "_"
        << (AllowMultipleVertices::value ? "allow_multiple_vertices" : "disallow_multiple_vertices")
        << ") Expected:" << getBoostSelector<ExpectedSelectorVertex>()
        << " actual:" << getBoostSelector<vertex_storage_t>() << std::endl;
}

TEST_F(GraphTraitsTest, comparable_and_hash_test)
{
    // just make sure the traits of the classes above fit the criteria
    ASSERT_TRUE(util::has_std_hash_v<Hashable>);
    ASSERT_TRUE(util::is_equality_comparable_v<Hashable>);
    ASSERT_FALSE(util::is_less_comparable_v<Hashable>);

    ASSERT_TRUE(util::has_std_hash_v<HashableAndComparable>);
    ASSERT_TRUE(util::is_equality_comparable_v<HashableAndComparable>);
    ASSERT_TRUE(util::is_less_comparable_v<HashableAndComparable>);

    ASSERT_FALSE(util::has_std_hash_v<Comparable>);
    ASSERT_TRUE(util::is_equality_comparable_v<Comparable>);
    ASSERT_TRUE(util::is_less_comparable_v<Comparable>);

    ASSERT_FALSE(util::has_std_hash_v<NotHashableOrComparable>);
    ASSERT_TRUE(util::is_equality_comparable_v<NotHashableOrComparable>);
    ASSERT_FALSE(util::is_less_comparable_v<NotHashableOrComparable>);
}

TEST_F(GraphTraitsTest, storage_selector_test)
{
    // tests the storage selector returns the correct boost-selector type depending on whether the given type is
    // hashable, is comparable and intended multiplicity in the graph

    // clang-format off
// test that comparable container is preferred if possible
testCorrectTypes<Comparable,              allow_parallel_edges,    allow_multiple_vertices,    boost::multisetS,      boost::multisetS,      prefer_comparable_container, boost::vecS>();
testCorrectTypes<Comparable,              disallow_parallel_edges, allow_multiple_vertices,    boost::setS,           boost::multisetS,      prefer_comparable_container, boost::vecS>();
testCorrectTypes<Comparable,              disallow_parallel_edges, allow_multiple_vertices,    boost::setS,           boost::multisetS,      prefer_comparable_container, boost::vecS>();
testCorrectTypes<Comparable,              disallow_parallel_edges, disallow_multiple_vertices, boost::setS,           boost::setS,           prefer_comparable_container, boost::vecS>();

testCorrectTypes<Hashable,                allow_parallel_edges,    allow_multiple_vertices,    boost::hash_multisetS, boost::hash_multisetS, prefer_comparable_container, boost::vecS>();
testCorrectTypes<Hashable,                disallow_parallel_edges, allow_multiple_vertices,    boost::hash_setS,      boost::hash_multisetS, prefer_comparable_container, boost::vecS>();
testCorrectTypes<Hashable,                allow_parallel_edges,    disallow_multiple_vertices, boost::hash_multisetS, boost::hash_setS,      prefer_comparable_container, boost::vecS>();
testCorrectTypes<Hashable,                disallow_parallel_edges, disallow_multiple_vertices, boost::hash_setS,      boost::hash_setS,      prefer_comparable_container, boost::vecS>();

testCorrectTypes<HashableAndComparable,   allow_parallel_edges,    allow_multiple_vertices,    boost::multisetS,      boost::multisetS,      prefer_comparable_container, boost::vecS>();
testCorrectTypes<HashableAndComparable,   disallow_parallel_edges, allow_multiple_vertices,    boost::setS,           boost::multisetS,      prefer_comparable_container, boost::vecS>();
testCorrectTypes<HashableAndComparable,   allow_parallel_edges,    disallow_multiple_vertices, boost::multisetS,      boost::setS,           prefer_comparable_container, boost::vecS>();
testCorrectTypes<HashableAndComparable,   disallow_parallel_edges, disallow_multiple_vertices, boost::setS,           boost::setS,           prefer_comparable_container, boost::vecS>();

testCorrectTypes<NotHashableOrComparable, allow_parallel_edges,    allow_multiple_vertices,    boost::vecS,           boost::vecS,           prefer_comparable_container, boost::vecS>();
testCorrectTypes<NotHashableOrComparable, disallow_parallel_edges, allow_multiple_vertices,    boost::vecS,           boost::vecS,           prefer_comparable_container, boost::vecS>();
testCorrectTypes<NotHashableOrComparable, disallow_parallel_edges, allow_multiple_vertices,    boost::vecS,           boost::vecS,           prefer_comparable_container, boost::vecS>();
testCorrectTypes<NotHashableOrComparable, disallow_parallel_edges, disallow_multiple_vertices, boost::vecS,           boost::vecS,           prefer_comparable_container, boost::vecS>();

// test that hash-container is preferred if possible
testCorrectTypes<Comparable,              allow_parallel_edges,    allow_multiple_vertices,    boost::multisetS,      boost::multisetS,      prefer_hash_container, boost::vecS>();
testCorrectTypes<Comparable,              disallow_parallel_edges, allow_multiple_vertices,    boost::setS,           boost::multisetS,      prefer_hash_container, boost::vecS>();
testCorrectTypes<Comparable,              disallow_parallel_edges, allow_multiple_vertices,    boost::setS,           boost::multisetS,      prefer_hash_container, boost::vecS>();
testCorrectTypes<Comparable,              disallow_parallel_edges, disallow_multiple_vertices, boost::setS,           boost::setS,           prefer_hash_container, boost::vecS>();

testCorrectTypes<Hashable,                allow_parallel_edges,    allow_multiple_vertices,    boost::hash_multisetS, boost::hash_multisetS, prefer_hash_container, boost::vecS>();
testCorrectTypes<Hashable,                disallow_parallel_edges, allow_multiple_vertices,    boost::hash_setS,      boost::hash_multisetS, prefer_hash_container, boost::vecS>();
testCorrectTypes<Hashable,                allow_parallel_edges,    disallow_multiple_vertices, boost::hash_multisetS, boost::hash_setS,      prefer_hash_container, boost::vecS>();
testCorrectTypes<Hashable,                disallow_parallel_edges, disallow_multiple_vertices, boost::hash_setS,      boost::hash_setS,      prefer_hash_container, boost::vecS>();

testCorrectTypes<HashableAndComparable,   allow_parallel_edges,    allow_multiple_vertices,    boost::hash_multisetS, boost::hash_multisetS, prefer_hash_container, boost::vecS>();
testCorrectTypes<HashableAndComparable,   disallow_parallel_edges, allow_multiple_vertices,    boost::hash_setS,      boost::hash_multisetS, prefer_hash_container, boost::vecS>();
testCorrectTypes<HashableAndComparable,   allow_parallel_edges,    disallow_multiple_vertices, boost::hash_multisetS, boost::hash_setS,      prefer_hash_container, boost::vecS>();
testCorrectTypes<HashableAndComparable,   disallow_parallel_edges, disallow_multiple_vertices, boost::hash_setS,      boost::hash_setS,      prefer_hash_container, boost::vecS>();

testCorrectTypes<NotHashableOrComparable, allow_parallel_edges,    allow_multiple_vertices,    boost::vecS,           boost::vecS,           prefer_hash_container, boost::vecS>();
testCorrectTypes<NotHashableOrComparable, disallow_parallel_edges, allow_multiple_vertices,    boost::vecS,           boost::vecS,           prefer_hash_container, boost::vecS>();
testCorrectTypes<NotHashableOrComparable, disallow_parallel_edges, allow_multiple_vertices,    boost::vecS,           boost::vecS,           prefer_hash_container, boost::vecS>();
testCorrectTypes<NotHashableOrComparable, disallow_parallel_edges, disallow_multiple_vertices, boost::vecS,           boost::vecS,           prefer_hash_container, boost::vecS>();

// test that fallback can be changed
testCorrectTypes<Comparable,              allow_parallel_edges,    allow_multiple_vertices,    boost::multisetS,      boost::multisetS,      prefer_hash_container, boost::listS>();
testCorrectTypes<Comparable,              disallow_parallel_edges, allow_multiple_vertices,    boost::setS,           boost::multisetS,      prefer_hash_container, boost::listS>();
testCorrectTypes<Comparable,              disallow_parallel_edges, allow_multiple_vertices,    boost::setS,           boost::multisetS,      prefer_hash_container, boost::listS>();
testCorrectTypes<Comparable,              disallow_parallel_edges, disallow_multiple_vertices, boost::setS,           boost::setS,           prefer_hash_container, boost::listS>();

testCorrectTypes<Hashable,                allow_parallel_edges,    allow_multiple_vertices,    boost::hash_multisetS, boost::hash_multisetS, prefer_hash_container, boost::listS>();
testCorrectTypes<Hashable,                disallow_parallel_edges, allow_multiple_vertices,    boost::hash_setS,      boost::hash_multisetS, prefer_hash_container, boost::listS>();
testCorrectTypes<Hashable,                allow_parallel_edges,    disallow_multiple_vertices, boost::hash_multisetS, boost::hash_setS,      prefer_hash_container, boost::listS>();
testCorrectTypes<Hashable,                disallow_parallel_edges, disallow_multiple_vertices, boost::hash_setS,      boost::hash_setS,      prefer_hash_container, boost::listS>();

testCorrectTypes<HashableAndComparable,   allow_parallel_edges,    allow_multiple_vertices,    boost::hash_multisetS, boost::hash_multisetS, prefer_hash_container, boost::listS>();
testCorrectTypes<HashableAndComparable,   disallow_parallel_edges, allow_multiple_vertices,    boost::hash_setS,      boost::hash_multisetS, prefer_hash_container, boost::listS>();
testCorrectTypes<HashableAndComparable,   allow_parallel_edges,    disallow_multiple_vertices, boost::hash_multisetS, boost::hash_setS,      prefer_hash_container, boost::listS>();
testCorrectTypes<HashableAndComparable,   disallow_parallel_edges, disallow_multiple_vertices, boost::hash_setS,      boost::hash_setS,      prefer_hash_container, boost::listS>();

testCorrectTypes<NotHashableOrComparable, allow_parallel_edges,    allow_multiple_vertices,    boost::listS,          boost::listS,          prefer_hash_container, boost::listS>();
testCorrectTypes<NotHashableOrComparable, disallow_parallel_edges, allow_multiple_vertices,    boost::listS,          boost::listS,          prefer_hash_container, boost::listS>();
testCorrectTypes<NotHashableOrComparable, disallow_parallel_edges, allow_multiple_vertices,    boost::listS,          boost::listS,          prefer_hash_container, boost::listS>();
testCorrectTypes<NotHashableOrComparable, disallow_parallel_edges, disallow_multiple_vertices, boost::listS,          boost::listS,          prefer_hash_container, boost::listS>();
    // clang-format on
}

TEST_F(GraphTraitsTest, has_option_test)
{
    // clang-format off
    // formatter needs to remain switched off, because otherwise "(...)" around the template is removed and the
    // ASSERT_TRUE/FALSE - macros interpret comma separated arguments
    ASSERT_TRUE((has_option<allow_multiple_vertices, allow_multiple_vertices, allow_parallel_edges>::value));
    ASSERT_FALSE((has_option<disallow_multiple_vertices, allow_multiple_vertices, allow_parallel_edges>::value));
    ASSERT_TRUE((has_option<allow_parallel_edges, allow_multiple_vertices, allow_parallel_edges>::value));
    ASSERT_FALSE((has_option<disallow_parallel_edges, allow_multiple_vertices, allow_parallel_edges>::value));
    ASSERT_FALSE((has_option<disallow_cycles>::value));
    // clang-format on
}

template <typename... Options>
struct OptionStruct
{
    OptionStruct()
    {
        //        TRACE2("Contr", AllowMultipleVertices);
        //        TRACE2("Contr", AllowParallelEdges);
        //        TRACE2("Contr", AllowCycles);
        //        TRACE2("Contr", ThrowOnError);
        //        TRACE2("Contr", OverWriteEdgeProperty);
    }

    // clang-format off
    static constexpr bool AllowMultipleVertices =
           set_or_default_bool_option<allow_multiple_vertices, disallow_multiple_vertices,  true, Options...>::value;
    static constexpr bool AllowParallelEdges =
           set_or_default_bool_option<allow_parallel_edges,    disallow_parallel_edges,     true, Options...>::value;
    static constexpr bool PreferComparableContainer =
            set_or_default_bool_option<prefer_comparable_container, prefer_hash_container,  true, Options...>::value;
    static constexpr bool AllowCycles =
           set_or_default_bool_option<allow_cycles,            disallow_cycles,             true, Options...>::value;
    static constexpr bool Connected =
           set_or_default_bool_option<connected,               disconnected,                false, Options...>::value;
    static constexpr bool SingleParent =
           set_or_default_bool_option<single_parent,           multiple_parent,             false, Options...>::value;
    static constexpr bool ThrowOnError =
           set_or_default_bool_option<throw_on_error,          no_throw_on_error,           false, Options...>::value;
    static constexpr bool OverWriteEdgeProperty =
           set_or_default_bool_option<overwrite_edge_property,  no_overwrite_edge_property, true, Options...>::value;
    // clang-format on
};

TEST_F(GraphTraitsTest, option_default_test)
{
    {
        OptionStruct<> options;
        ASSERT_TRUE(options.AllowMultipleVertices);
        ASSERT_TRUE(options.AllowParallelEdges);
        ASSERT_TRUE(options.PreferComparableContainer);
        ASSERT_TRUE(options.AllowCycles);
        ASSERT_FALSE(options.Connected);
        ASSERT_FALSE(options.SingleParent);
        ASSERT_FALSE(options.ThrowOnError);
        ASSERT_TRUE(options.OverWriteEdgeProperty);
    }
    {
        OptionStruct<disallow_multiple_vertices, single_parent> options;
        ASSERT_FALSE(options.AllowMultipleVertices);
        ASSERT_TRUE(options.AllowParallelEdges);
        ASSERT_TRUE(options.PreferComparableContainer);
        ASSERT_TRUE(options.AllowCycles);
        ASSERT_FALSE(options.Connected);
        ASSERT_TRUE(options.SingleParent);
        ASSERT_FALSE(options.ThrowOnError);
        ASSERT_TRUE(options.OverWriteEdgeProperty);
    }
    {
        OptionStruct<std::string> options;
        ASSERT_TRUE(options.AllowMultipleVertices);
        ASSERT_TRUE(options.AllowParallelEdges);
        ASSERT_TRUE(options.PreferComparableContainer);
        ASSERT_TRUE(options.AllowCycles);
        ASSERT_FALSE(options.Connected);
        ASSERT_FALSE(options.SingleParent);
        ASSERT_FALSE(options.ThrowOnError);
        ASSERT_TRUE(options.OverWriteEdgeProperty);
    }
    {
        OptionStruct<connected, disconnected> options;
        ASSERT_TRUE(options.AllowMultipleVertices);
        ASSERT_TRUE(options.AllowParallelEdges);
        ASSERT_TRUE(options.PreferComparableContainer);
        ASSERT_TRUE(options.AllowCycles);
        ASSERT_TRUE(options.Connected);
        ASSERT_FALSE(options.SingleParent);
        ASSERT_FALSE(options.ThrowOnError);
        ASSERT_TRUE(options.OverWriteEdgeProperty);
    }
}
