/*
 * Repository:  https://github.com/kingkybel/CPP-utilities
 * File Name:   test/test_property_classes.h
 * Description: test-classes to use as properties
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

#ifndef NS_UTIL_GRAPH_DIRECTED_GRAPH_TEST_PROPERTY_CLASSES_H_INCLUDED
#define NS_UTIL_GRAPH_DIRECTED_GRAPH_TEST_PROPERTY_CLASSES_H_INCLUDED
#include "directed_graph_to_string.h"

#include <dkyb/to_string.h>
#include <sstream>
#include <string>
#include <utility>

struct Comparable
{
    Comparable(std::string id = "")
        : id_(std::move(id))
    {
    }

    static std::string name()
    {
        return "comparable";
    }

    friend bool operator==(Comparable const& lhs, Comparable const& rhs)
    {
        return lhs.id_ == rhs.id_;
    }

    bool operator<(Comparable const&) const
    {
        return true;
    }

    friend std::ostream& operator<<(std::ostream& os, Comparable const& v)
    {
        os << v.id_;
        return os;
    }

    std::string id_;
};

struct Hashable
{
    Hashable(std::string id = "")
        : id_(std::move(id))
    {
    }

    static std::string name()
    {
        return "hashable";
    }

    friend bool operator==(Hashable const& lhs, Hashable const& rhs)
    {
        return lhs.id_ == rhs.id_;
    }

    friend std::ostream& operator<<(std::ostream& os, Hashable const& v)
    {
        os << v.id_;
        return os;
    }

    std::string id_;
};

namespace std
{
template <>
struct hash<Hashable>
{
    std::size_t operator()(Hashable const& s) const
    {
        return 4'711;
    }
};
} // namespace std

struct HashableAndComparable
{
    HashableAndComparable(std::string id = "")
        : id_(std::move(id))
    {
    }

    static std::string name()
    {
        return "hashable+comparable";
    }

    friend bool operator==(HashableAndComparable const& lhs, HashableAndComparable const& rhs)
    {
        return lhs.id_ == rhs.id_;
    }

    bool operator<(HashableAndComparable const&) const
    {
        return true;
    }

    friend std::ostream& operator<<(std::ostream& os, HashableAndComparable const& v)
    {
        os << v.id_;
        return os;
    }

    std::string id_;
};

namespace std
{
template <>
struct hash<HashableAndComparable>
{
    std::size_t operator()(HashableAndComparable const& s) const
    {
        return 666;
    }
};
} // namespace std

struct NotHashableOrComparable
{
    NotHashableOrComparable(std::string id = "")
        : id_(std::move(id))
    {
    }

    static std::string name()
    {
        return "not hashable, not comparable";
    }

    friend bool operator==(NotHashableOrComparable const& lhs, NotHashableOrComparable const& rhs)
    {
        return lhs.id_ == rhs.id_;
    }

    std::string id_;

    friend std::ostream& operator<<(std::ostream& os, NotHashableOrComparable const& v)
    {
        os << v.id_;
        return os;
    }
};

template <typename GraphType>
inline std::string graphConfig(GraphType const& graph)
{
    std::stringstream ss;
    ss << std::boolalpha << std::endl
       << "GraphConfig:" << std::endl
       << "AllowMultipleVertices     = " << graph.doesAllowMultipleVertices() << std::endl
       << "AllowParallelEdges        = " << graph.doesAllowParallelEdges() << std::endl
       << "AllowCycles               = " << graph.doesAllowCycles() << std::endl
       << "PreferComparableContainer = " << graph.doesPreferComparableContainer() << std::endl
       << "ThrowOnError              = " << graph.doesThrowOnError() << std::endl
       << "SingleParent              = " << graph.isSingleParent() << std::endl
       << "Connected                 = " << graph.isConnected() << std::endl
       << "StayConnectedStrategy     = "
       << (graph.isStayConnectedStrategy() ? "stay connected" : "fail on disconnection attempt") << std::endl
       << "OverWriteEdgeProperty     = " << graph.doesOverWriteEdgeProperty() << std::endl
       << "AddVerticesViaEdge        = " << graph.canAddVerticesViaEdge() << std::endl
       << std::endl
       << "Graph:" << std::endl
       << graph << std::endl;
    return ss.str();
}

#endif // NS_UTIL_GRAPH_DIRECTED_GRAPH_TEST_PROPERTY_CLASSES_H_INCLUDED
