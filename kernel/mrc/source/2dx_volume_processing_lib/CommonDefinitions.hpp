/* 
 * File:   TypeDefs.hpp
 * Author: biyanin
 *
 * Created on January 20, 2015, 2:17 PM
 */

#ifndef COMMONDEFINITIONS_HPP
#define	COMMONDEFINITIONS_HPP

#include <list>
#include <map>

#include "MillerIndex.hpp"
#include "Reflection.hpp"

/*
 * A map to store unique miller indices as keys and reflections as values
 */
typedef std::map<MillerIndex, Reflection> ReflectionMap;

/*
 * A multimap to store miller indices as keys and reflections as values.
 * This map could have multiple miller indices with various values.
 */
typedef std::multimap<MillerIndex, Reflection> ReflectionMultiMap;

/*
 * A list of reflections.
 */
typedef std::list<Reflection> ReflectionList;

/*
 * Complex numbers used in the software
 */
typedef std::complex<double> Complex2dx;

/*
 * A pair class which could be used to insert values to map and multimap class
 */
typedef std::pair<MillerIndex, Reflection> MI_Reflection_Pair;

#endif	/* TYPEDEFS_HPP */

