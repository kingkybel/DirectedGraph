/*
 * Repository:  https://github.com/kingkybel/CPP-utilities
 * File Name:   include/edge_properties.h
 * Description: edge-properties template that can be configured with boost-traits
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

#ifndef NS_UTIL_GRAPH_EDGE_PROPERTIES_H
#define NS_UTIL_GRAPH_EDGE_PROPERTIES_H

#include "directed_graph_traits.h"

namespace util::graph
{
/**
 * Edge properties struct with conditional members
 * @tparam EdgeDataType payload data
 * @tparam Properties variadic list of properties, recognises boost-property types
 */
template <typename EdgeDataType, typename... Properties>
struct EdgeProperties
{
    EdgeDataType          data;
    static constexpr bool HasIndex  = has_option_v<boost::edge_index_t, Properties...>;
    static constexpr bool HasName   = has_option_v<boost::edge_name_t, Properties...>;
    static constexpr bool HasWeight = has_option_v<boost::edge_weight_t, Properties...>;
    static constexpr bool HasWeigh2 = has_option_v<boost::edge_weight2_t, Properties...>;
    static constexpr bool HasColor  = has_option_v<boost::edge_color_t, Properties...>;

    typename std::conditional<HasName, std::string, std::monostate>::type  name{};
    typename std::conditional<HasWeight, double, std::monostate>::type     weight{};
    typename std::conditional<HasWeigh2, double, std::monostate>::type     weight2{};
    typename std::conditional<HasColor, std::string, std::monostate>::type color{};

    EdgeProperties(EdgeDataType const& edgeData = EdgeDataType{})
    : data(edgeData)
    {
        if constexpr (HasIndex)
        {
            index = nextID<EdgeProperties>();
        }
        if constexpr (HasWeight)
        {
            weight = 1.0;
        }
        if constexpr (HasWeigh2)
        {
            weight = 1.0;
        }
    }

    friend bool operator==(EdgeProperties const& lhs, EdgeProperties const& rhs)
    {
        return lhs.data == rhs.data;
    }
    // Only enable operator< if EdgeDataType is less-comparable
    template <typename T = EdgeDataType>
    std::enable_if_t<util::is_less_comparable_v<T>, bool>
        friend operator<(EdgeProperties const& lhs, EdgeProperties const& rhs) {
        return lhs.data < rhs.data;
    }

  private:
    typename std::conditional<HasIndex, size_t, std::monostate>::type index{};
};

} // namespace util::graph


namespace std
{
/**
 * @brief Specialization of std::hash for VertexProperties if VertexDataType is hashable
 * @tparam EdgeDataType payload data type
 * @tparam Properties variadic list of properties, recognises boost-property types
 */
template <typename EdgeDataType, typename... Properties>
struct hash<util::graph::EdgeProperties<EdgeDataType, Properties...>>
{
    std::size_t operator()(util::graph::EdgeProperties<EdgeDataType, Properties...> const& vp) const
    {
        if constexpr (util::has_std_hash_v<EdgeDataType>)
        {
            return std::hash<EdgeDataType>{}(vp.data);
        }
        else
        {
            static_assert(util::has_std_hash_v<EdgeDataType>, "EdgeDataType is not hashable.");
            return 0;
        }
    }
};
} // namespace std

#endif // NS_UTIL_GRAPH_EDGE_PROPERTIES_H
