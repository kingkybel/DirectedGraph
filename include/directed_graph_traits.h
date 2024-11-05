/*
 * Repository:  https://github.com/kingkybel/DirectedGraph
 * File Name:   include/directed_graph_traits.h
 * Description: graph traits to use with directed graphs in directed_graph.h
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
 * @date: 2024-11-05
 * @author: Dieter J Kybelksties
 */

#ifndef UTIL_GRAPH_DIRECTED_GRAPH_TRAITS_H
#define UTIL_GRAPH_DIRECTED_GRAPH_TRAITS_H

#include <boost/graph/adjacency_list.hpp>
#include <dkyb/traits.h>

namespace util::graph
{

/**
 * @brief Function to return consecutive IDs starting from 0(zero).
 * @note Put in a function in order to avoid static variables in the library.
 *       Template parameter ensures, that each class can have its own sequence.
 *       nextID<Class1>() has a different sequence from nextID<Class2>().
 * @return the next ID
 */
template <typename T_>
inline size_t nextID()
{
    static size_t id_counter = 0UL;
    id_counter++;
    return id_counter - 1;
}

// Trait to check if a type is a valid Boost storage selector
template <typename T>
struct is_valid_boost_storage_selector : std::false_type
{
};

// Specialize for valid storage selectors
template <>
struct is_valid_boost_storage_selector<boost::vecS> : std::true_type
{
};

template <>
struct is_valid_boost_storage_selector<boost::listS> : std::true_type
{
};

template <>
struct is_valid_boost_storage_selector<boost::setS> : std::true_type
{
};

template <>
struct is_valid_boost_storage_selector<boost::multisetS> : std::true_type
{
};

template <>
struct is_valid_boost_storage_selector<boost::hash_setS> : std::true_type
{
};

/**
 * Storage selection
 * Key Points:
 * <ul>
 * <li>appropriate_storage_selector is used to determine the default storage type based on the options for parallel
 * edges and multiple vertices.</li>
 * <li>storage_selector allows custom storage to override the default, only if the custom storage type is explicitly
 * provided.</li>
 * <li>Fallback Logic ensures that if no custom storage is given, the appropriate_storage_selector result is used by
 * default.</li>
 */
template <typename Default, typename... Options>
struct storage_selector;

/**
 * @brief Tag types for custom vertex storage.
 */
template <typename T>
struct custom_vertex_storage_t
{
    using type = T;
};

/**
 * @brief Tag types for custom edge storage.
 */
template <typename T>
struct custom_edge_storage_t
{
    using type = T;
};

/**
 * @brief Helper to extract custom storage types.
 * @tparam Default default type to select
 * @tparam Options the alternative types
 */
template <typename Default, typename... Options>
struct storage_selector
{
    using type = Default;
};

/**
 * @brief Selector template for storage types.
 * @tparam Default the default
 * @tparam First first in the variadic lis
 * @tparam Rest the rest of the variadic list
 */
template <typename Default, typename First, typename... Rest>
struct storage_selector<Default, First, Rest...>
{
    using type = std::conditional_t<
        std::is_same_v<custom_vertex_storage_t<typename First::type>, First> ||
            std::is_same_v<custom_edge_storage_t<typename First::type>, First>,
        typename First::type,
        typename storage_selector<Default, Rest...>::type>; // static recursion
};

// Template to define a fallback selector
template <typename FallbackSelector>
struct fallback_selector
{
    static_assert(
        is_valid_boost_storage_selector<FallbackSelector>::value,
        "FallbackSelector must be a valid Boost storage selector (e.g., vecS, setS, hash_setS, etc.)"
    );
    using type = FallbackSelector;
};

// Primary template: No fallback_selector found, use DefaultType
template <typename DefaultType, typename... Options>
struct find_fallback_selector
{
    using type = DefaultType;
};

// Specialization: Match found, use the type from fallback_selector
template <typename DefaultType, typename FallbackType, typename... Rest>
struct find_fallback_selector<DefaultType, fallback_selector<FallbackType>, Rest...>
{
    using type = FallbackType;
};

// Recursively search the Options list
template <typename DefaultType, typename First, typename... Rest>
struct find_fallback_selector<DefaultType, First, Rest...> : find_fallback_selector<DefaultType, Rest...>
{
};

/**
 * @brief Storage-selector-template to find an appropriate storage selector for vertices and edges.
 * Checks whether a given @c ElementType can be used as key in hash-containers by verifying the existence of
 *
 * @code size_t std::hash(const &ElementType) @endcode
 *
 * It also checks whether @c ElementType can be used as key in ordered containers (set/map) by checking @c ElementType
 * is equality-/less- comparable.
 * Further the @c AllowMultiple parameter is evaluated to check whether the *multi* or *single* version of the
 * selector needs to be used.
 * Hash-containers are preferred, then comparable associative containers and the fallback is vector-selector.
 *
 * @tparam ElementType vertex or edge property type
 * @tparam AllowMultiple allow multiple (true) or only single (false) vertices/edges
 * @tparam PreferComparable prefer containers with comparable requirement (true) over hash-containers (false)
 * @return @c typedef one of {boost::hash_multisetS, boost::hash_setS, boost::multisetS, boost::setS, boost::vecS} to @c
 * type
 */
template <
    typename ElementType,
    typename FallbackSelector,
    bool AllowMultiple,
    bool PreferComparable,
    typename... Options>
struct appropriate_storage_selector
{
  private:
    // If hash exists, use unordered_set or unordered_multiset
    using hash_based_selector = std::conditional_t<AllowMultiple, boost::hash_multisetS, boost::hash_setS>;

    // If comparable but no hash, use set or multiset
    using comparable_based_selector = std::conditional_t<AllowMultiple, boost::multisetS, boost::setS>;

    // Fallback to vector if neither hash nor comparability is available
    using fallback_selector = FallbackSelector;

    // Default storage type, based on traits of ElementType
    using default_storage = std::conditional_t<
        PreferComparable,
        std::conditional_t<
            util::is_equality_comparable_v<ElementType> && util::is_less_comparable_v<ElementType>, // if comparable
            comparable_based_selector, // Use comparable-based selector
            std::conditional_t<
                util::has_std_hash_v<ElementType>, // Else if hash exists
                hash_based_selector,               // Use hash-based selector
                fallback_selector                  // Else fallback to vector
                >>,
        // Default storage type, based on traits of ElementType
        std::conditional_t<
            util::has_std_hash_v<ElementType>, // If hash exists
            hash_based_selector,               // Use hash-based selector
            std::conditional_t<                // Else if comparable
                util::is_equality_comparable_v<ElementType> && util::is_less_comparable_v<ElementType>,
                comparable_based_selector, // Use comparable-based selector
                fallback_selector          // Else fallback to vector
                >>>;

  public:
    // Use custom storage if provided, otherwise fallback to default
    using type = typename storage_selector<default_storage, Options...>::type;
};

/**
 * @brief Alias the appropriate_storage_selector::type.
 */
template <typename ElementType, typename FallbackSelector, bool AllowMultiple, bool PreferComparable>
using appropriate_storage_selector_t =
    appropriate_storage_selector<ElementType, FallbackSelector, AllowMultiple, PreferComparable>::type;

// clang-format off

        struct allow_multiple_vertices : public std::true_type {
        };
        using allow_multiple_vertices_t = allow_multiple_vertices::type;
        bool constexpr allow_multiple_vertices_v = allow_multiple_vertices::value;

        struct disallow_multiple_vertices : public std::false_type {
        };
        using disallow_multiple_vertices_t = disallow_multiple_vertices::type;
        bool constexpr disallow_multiple_vertices_v = disallow_multiple_vertices::value;

        struct allow_parallel_edges : public std::true_type {
        };
        using allow_parallel_edges_t = allow_parallel_edges::type;
        bool constexpr allow_parallel_edges_v = allow_parallel_edges::value;

        struct disallow_parallel_edges : public std::false_type {
        };
        using disallow_parallel_edges_t = disallow_parallel_edges::type;
        bool constexpr disallow_parallel_edges_v = disallow_parallel_edges::value;

        struct prefer_comparable_container : public std::true_type {
        };
        using prefer_comparable_container_t = prefer_comparable_container::type;
        bool constexpr prefer_comparable_container_v = prefer_comparable_container::value;

        struct prefer_hash_container : public std::false_type {};
        using prefer_hash_container_t = prefer_hash_container::type;
        bool constexpr prefer_hash_container_v = prefer_hash_container::value;

        struct allow_cycles : public std::true_type {};
        using allow_cycles_t = allow_cycles::type;
        bool constexpr allow_cycles_v = allow_cycles::value;

        struct disallow_cycles : public std::false_type {};
        using disallow_cycles_t = disallow_cycles::type;
        bool constexpr disallow_cycles_v = disallow_cycles::value;

        struct throw_on_error : public std::true_type {};
        using throw_on_error_t = throw_on_error::type;
        bool constexpr throw_on_error_v = throw_on_error::value;

        struct no_throw_on_error : public std::false_type {};
        using no_throw_on_error_t = no_throw_on_error::type;
        bool constexpr no_throw_on_error_v = no_throw_on_error::value;

        struct overwrite_edge_property : public std::true_type {};
        using overwrite_edge_property_t = overwrite_edge_property::type;
        bool constexpr overwrite_edge_property_v = overwrite_edge_property::value;

        struct no_overwrite_edge_property : public std::false_type {};
        using no_overwrite_edge_property_t = no_overwrite_edge_property::type;
        bool constexpr no_overwrite_edge_property_v = no_overwrite_edge_property::value;

        struct single_parent : public std::false_type {};
        using single_parent_t = single_parent::type;
        bool constexpr single_parent_v = single_parent::value;

        struct multiple_parent : public std::true_type {};
        using multiple_parent_t = multiple_parent::type;
        bool constexpr multiple_parent_v = multiple_parent::value;

        struct connected : public std::false_type {};
        using connected_t = connected::type;
        bool constexpr connected_v = connected::value;

        struct stay_connected : public std::false_type {};
        using stay_connected_t = stay_connected::type;
        bool constexpr stay_connected_v = stay_connected::value;

        struct disallow_split : public std::true_type {};
        using disallow_split_t = disallow_split::type;
        bool constexpr disallow_split_v = disallow_split::value;

        struct disconnected : public std::true_type {};
        using disconnected_t = disconnected::type;
        bool constexpr disconnected_v = disconnected::value;

        struct adding_edge_adds_vertices : public std::true_type {};
        using adding_edge_adds_vertices_t = adding_edge_adds_vertices::type;
        bool constexpr adding_edge_adds_vertices_v = adding_edge_adds_vertices::value;

        struct adding_edge_requires_vertices : public std::false_type {};
        using adding_edge_requires_vertices_t = adding_edge_requires_vertices::type;
        bool constexpr adding_edge_requires_vertices_v = adding_edge_requires_vertices::value;

/**
 * @brief Helper to detect tag presence
 */
template <typename T, typename... Options>
struct has_option : std::false_type{};

template <typename T, typename First, typename... Rest>
struct has_option<T, First, Rest...>
    : std::conditional_t<std::is_same_v<T, First>, std::true_type, has_option<T, Rest...>>{};

template <typename T, typename... Rest>
using has_option_t = has_option<T, Rest...>::type;

template <typename T, typename... Rest>
bool constexpr has_option_v = has_option<T, Rest...>::value;

/**
 * @brief Helper to detect tag presence
 */
template <typename T, typename... Options>
struct has_disallowed_option : std::false_type{};

template <typename T, typename First, typename... Rest>
struct has_disallowed_option<T, First, Rest...>
    : std::conditional_t<std::is_same_v<T, First>, std::true_type, has_option<T, Rest...>>{};

template <typename T, typename... Rest>
using has_disallowed_option_t = has_disallowed_option<T, Rest...>::type;

template <typename T, typename... Rest>
bool constexpr has_disallowed_option_v = has_disallowed_option<T, Rest...>::value;

/**
 * @brief The set_or_default_bool_option template
 * Core template to select between Positive, Negative, or Default
 * @tparam Positive Positive tag (enable-tag)
 * @tparam Negative Negative tag (disable tag)
 * @tparam Default Default value, when neither of the above tags is found
 * @tparam Options variadic list of options
 */
template <typename Positive, typename Negative, bool Default, typename... Options>
struct set_or_default_bool_option
{
    // This logic first checks for Positive, then Negative, and falls back to Default
    static constexpr bool value =
        has_option<Positive, Options...>::value ? true : (has_option<Negative, Options...>::value ? false : Default);
};

// clang-format on

} // namespace util::graph

#endif // UTIL_GRAPH_DIRECTED_GRAPH_TRAITS_H
