/*
 * Repository:  https://github.com/kingkybel/CPP-utilities
 * File Name:   include/vertex_properties.h
 * Description: vertex-properties template that can be configured with boost-traits
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
 * @date: 2024-10-26
 * @author: Dieter J Kybelksties
 */

#ifndef NS_UTIL_GRAPH_VERTEX_PROPERTIES_H
#define NS_UTIL_GRAPH_VERTEX_PROPERTIES_H

#include "directed_graph_traits.h"

namespace util::graph
{
/**
 * Vertex properties struct with conditional members
 * @tparam VertexDataType payload data type
 * @tparam Properties variadic list of properties, recognises boost-property types
 */
template <typename VertexDataType, typename... Properties>
struct VertexProperties
{
    VertexDataType        data;
    static constexpr bool HasIndex  = has_option_v<boost::vertex_index_t, Properties...>;
    static constexpr bool HasIndex1 = has_option_v<boost::vertex_index1_t, Properties...>;
    static constexpr bool HasIndex2 = has_option_v<boost::vertex_index2_t, Properties...>;
    static constexpr bool HasName   = has_option_v<boost::vertex_name_t, Properties...>;
    static constexpr bool HasRoot   = has_option_v<boost::vertex_root_t, Properties...>;

    typename std::conditional<HasIndex, size_t const&, std::monostate>::type index{}; // read-only
    typename std::conditional<HasIndex1, size_t, std::monostate>::type       index1{};
    typename std::conditional<HasIndex2, size_t, std::monostate>::type       index2{};
    typename std::conditional<HasName, std::string, std::monostate>::type    name{};
    typename std::conditional<HasRoot, size_t, std::monostate>::type         root{};

    VertexProperties(VertexDataType const& vertexData = VertexDataType{})
        : data(vertexData)
        , index(index_)
    {
        if constexpr (HasIndex)
        {
            index_ = nextID<VertexProperties>();
        }
    }

    friend bool operator==(VertexProperties const& lhs, VertexProperties const& rhs)
    {
        return lhs.data == rhs.data;
    }

    // Only enable operator< if EdgeDataType is less-comparable
    template <typename T = VertexDataType>
    std::enable_if_t<util::is_less_comparable_v<T>, bool> friend
        operator<(VertexProperties const& lhs, VertexProperties const& rhs)
    {
        return lhs.data < rhs.data;
    }

  private:
    typename std::conditional<HasIndex, size_t, std::monostate>::type index_{};
};

} // namespace util::graph

namespace std
{
/**
 * Specialization of std::hash for VertexProperties if VertexDataType is hashable
 * @tparam VertexDataType payload data type
 * @tparam Properties variadic list of properties, recognises boost-property types
 */
template <typename VertexDataType, typename... Properties>
struct hash<util::graph::VertexProperties<VertexDataType, Properties...>>
{
    std::size_t operator()(util::graph::VertexProperties<VertexDataType, Properties...> const& vp) const
    {
        if constexpr (util::has_std_hash_v<VertexDataType>)
        {
            return std::hash<VertexDataType>{}(vp.data);
        }
        else
        {
            static_assert(util::has_std_hash_v<VertexDataType>, "VertexDataType is not hashable.");
            return 0;
        }
    }
};
} // namespace std
#endif // NS_UTIL_GRAPH_VERTEX_PROPERTIES_H
