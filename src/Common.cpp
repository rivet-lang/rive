#include "Common.hpp"

#include <iostream>

namespace rivet {

void panic(std::string msg) {
    std::cerr << "rivet panic:" << msg << std::endl;
    std::exit(1);
}

}  // namespace rivet
