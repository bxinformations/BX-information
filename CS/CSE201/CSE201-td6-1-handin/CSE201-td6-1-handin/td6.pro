TEMPLATE = app
CONFIG += console c++11
CONFIG -= app_bundle
CONFIG -= qt
CONFIG+=debug

QMAKE_CXXFLAGS_DEBUG -= -O2
QMAKE_CXXFLAGS_DEBUG += -O0

GRADINGLIB_HEADERS += gradinglib/gradinglib.hpp
GRADINGLIB_SOURCES += gradinglib/gradinglib.cpp

GRADING_HEADERS += grading/grading.hpp
GRADING_SOURCES += grading/grading.cpp


HEADERS += \
    $$GRADINGLIB_HEADERS \
    $$GRADING_HEADERS \
    common.hpp \
    td6.hpp

SOURCES += \
        $$GRADINGLIB_SOURCES \
        $$GRADING_SOURCES \
        common.cpp \
        main.cpp \
        td6.cpp

TARGET = grading

