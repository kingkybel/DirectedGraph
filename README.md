# DirectedGraph

A header-only template library to create different types of directed graphs.

This is a light template wrapper around the boost graph library. The intention is to allow developers to create
different morphologies of directed graphs and enforce them via templates and code.

Instantiations of directed graphs can be customised by optional type-tags that govern shape and behaviour of the
resulting graph type.

# Purpose

This repository provides strongly-typed directed-graph abstractions on top of Boost Graph Library (BGL).
It is intended to:

- make graph-shape constraints explicit at compile time (for example connected or acyclic variants),
- keep graph usage ergonomic for application code while still using BGL internally,
- provide tests that validate both behavior and template-trait selection.

# Recent Build Changes

The project was updated to keep builds stable with newer toolchains (Boost 1.90 and `g++-14`):

- `run_tests` now links against the `DirectedGraphs` interface target so C++23 requirements are propagated to tests.
  This ensures modern language features used in headers (such as `<=>`) are compiled with the correct standard.
- `-Wno-error=maybe-uninitialized` was added to extra compile flags to avoid failing on a GCC 14 false-positive warning
  emitted from Boost Graph internals under optimization (`-O2`) while retaining `-Werror` for other warnings.

# Installation

The installation and build is tested on *ubuntu24.04 LTS*

## dependencies

BOOST:

```
# create a directory where you like to clone googletest, eg: ~/Repos and change to it
mkdir ~/Repos ; cd ~/Repos
```

Download, build and install boost, minimum version 1.90.0

```bash
# amend next three lines if you want a newer version of boost
BOOST_MAJOR=1
BOOST_MID=90
BOOST_MINOR=0
BOOST_VER=${BOOST_MAJOR}_${BOOST_MID}_${BOOST_MINOR}
BOOST_VER_DOT=${BOOST_MAJOR}.${BOOST_MID}.${BOOST_MINOR}
wget -O boost_${BOOST_VER}.tar.gz https://sourceforge.net/projects/boost/files/boost/${BOOST_VER_DOT}/boost_${BOOST_VER}.tar.gz/download
tar -xvf boost_${BOOST_VER}.tar.gz
cd boost_${BOOST_VER}
./bootstrap.sh --prefix=/usr/
./b2
sudo ./b2 install
```

googletest:

```bash
# create a directory where you like to clone googletest, eg: ~/Repos and change to it
mkdir ~/Repos ; cd ~/Repos
git clone https://github.com/google/googletest.git
cd googletest
mkdir build
cd build
mkdir build
cmake ..
make -j $(nproc)
sudo make install
```

## use cmake to install the header-only library

```bash
# git clone --recurse-submodules -j $(nproc) https://github.com/kingkybel/DirectedGraph.git
# or:
git clone --recurse-submodules -j $(nproc) git@github.com:kingkybel/DirectedGraph.git
cd DirectedGraph
# change the next line to change the install prefix to your liking
INSTALL_PREFIX=/usr
mkdir ./build
cd build
cmake -Wno-dev -DCMAKE_INSTALL_PREFIX=${INSTALL_PREFIX} ..
cmake --build . --parallel $(nproc)
sudo cmake --install .
```

This will install the headers from the include-folder to `${INSTALL_PREFIX}/dkyb`

To use the headers in your code, make sure that ${INSTALL_PREFIX} is in the include directories of your project.
Include the file in your code e.g:

```c++
#include <dkyb/directed_graph.h>
```
