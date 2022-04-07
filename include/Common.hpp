#ifndef RIVET_COMMON_H
#define RIVET_COMMON_H

#include <string>

namespace rivet {

[[noreturn]] void panic(std::string msg);

}  // namespace rivet

#endif  // RIVET_COMMON_H
