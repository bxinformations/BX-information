// MIT License

// Copyright (c) 2019 seladb

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#pragma once

#include <string>
#include <new>
#include <stdint.h>
#include "memplumber-internals.hpp"

// Prototypes
void* operator new(std::size_t size, const char* file, int line);
void* operator new[](std::size_t size, const char* file, int line);
void operator delete(void* pointer, const char* file, int line);
void operator delete[](void* pointer, const char* file, int line);

// required for Windows compilers only
#if defined(_WIN32) || defined(WIN32) || defined(_WIN64) || defined(WIN64)
void operator delete[](void* pointer);
void operator delete(void* pointer, std::size_t size);
void operator delete[](void* pointer, std::size_t size);
#endif


/**
 * @class MemPlumber
 * The main interface for MemPlumber. Contains the different methods for start & stop debugging, dump memory leaks and so on
 */
class MemPlumber {
    private:
        MemPlumber();

        // disable copy c'tor
        MemPlumber(const MemPlumber& other);
        
    public:

        /**
         * Start collecting information about memory allocations. Note that before calling this method no information is collected.
         * @param[in] verbose A flag indicating whether to dump information on each memory allocation and deallocation. The default value is false
         * @param[in] fileDumperName If the "verbose" flag is set to true, it is possible to dump the verbose information to a file. 
         * If this parameter is set to an empty string (which is also the default value), the verbose information will be dumped to stdout
         * @param[in] append If the "verbose" flag is set to true and "fileDumperName" is a non-empty string and if this file already exists on disk,
         * this parameter indicates whether to append the verbose information to the existing file or start writing from scratch
         */
        static void start(bool verbose = false, const char* fileDumperName = "", bool append = false) {
            __start(verbose, fileDumperName, append);
        }

        /**
         * Stop collecting information about memory allocations.
         */
        static void stop() {
            __stop();
        }

        /**
         * Stop collecting information about memory allocations and also free all the memory that was already allocated and collected.
         */
        static void stopAndFreeAllMemory() {
            __stop_and_free_all_mem();
        }

        /**
         * Present information about memory allocations that were not yet freed.
         * @param[out] memLeakCount The number of memory allocations that were not yet freed
         * @param[out] memLeakSize The total amount of memory that was allocated but not yet freed
         * @param[in] verbose A flag indicating whether to dump information on all memory allocations that were not yet freed. The default value is false
         * @param[in] fileDumperName If the "verbose" flag is set to true, it is possible to dump the verbose information to a file. 
         * If this parameter is set to an empty string (which is also the default value), the verbose information will be dumped to stdout
         * @param[in] append If the "verbose" flag is set to true and "fileDumperName" is a non-empty string and if this file already exists on disk,
         * this parameter indicates whether to append the verbose information to the existing file or start writing from scratch
         */
        static void memLeakCheck(size_t& memLeakCount, uint64_t& memLeakSize, bool verbose = false, const char* fileDumperName = "", bool append = false) {
            __mem_leak_check(memLeakCount, memLeakSize, verbose, fileDumperName, append);
        }

        /**
         * Present information about memory allocations of static variables. This information is available only if the library was compiled with the 
         * -DCOLLECT_STATIC_VAR_DATA flag and if the main method of the application was renamed and replaced by calling MEMPLUMBER_MAIN(<renamed_original_main_method>);
         * @param[out] memCount The number of static memory allocations
         * @param[out] memSize The total amount of memory that was allocated in static variables
         * @param[in] verbose A flag indicating whether to dump information on all static memory allocations. The default value is false
         * @param[in] fileDumperName If the "verbose" flag is set to true, it is possible to dump the verbose information to a file. 
         * If this parameter is set to an empty string (which is also the default value), the verbose information will be dumped to stdout
         * @param[in] append If the "verbose" flag is set to true and "fileDumperName" is a non-empty string and if this file already exists on disk,
         * this parameter indicates whether to append the verbose information to the existing file or start writing from scratch
         */
        static void staticMemCheck(size_t& memCount, uint64_t& memSize, bool verbose = false, const char* fileDumperName = "", bool append = false) {
            __static_mem_check(memCount, memSize, verbose, fileDumperName, append);
        }
};

#ifdef COLLECT_STATIC_VAR_DATA

#define MEMPLUMBER_MAIN(programMain) \
    int main(int argc, char* argv[]) { \
        __program_started(); \
        return programMain(argc, argv); \
    }

#endif //COLLECT_STATIC_VAR_DATA
