TEMPLATE = app

CONFIG += console c++11
CONFIG -= app_bundle
CONFIG -= qt
CONFIG+=debug

GRADINGLIB_HEADERS += gradinglib/gradinglib.hpp
GRADINGLIB_SOURCES += gradinglib/gradinglib.cpp

GRADING_HEADERS += grading/grading.hpp
GRADING_SOURCES += grading/grading.cpp

MEMPLUMBER_HEADERS += grading/memplumber.hpp \
                      grading/memplumber-internals.hpp

MEMPLUMBER_SOURCES += grading/memplumber.cpp

QMAKE_CXXFLAGS_RELEASE -= -O2
QMAKE_CXXFLAGS_RELEASE += -O0


HEADERS += \
    $$GRADINGLIB_HEADERS \
    $$GRADING_HEADERS \
    $$MEMPLUMBER_HEADERS \
    support.hpp \
    td3.hpp

SOURCES += \
        $$GRADINGLIB_SOURCES \
        $$GRADING_SOURCES \
        $$MEMPLUMBER_SOURCES \
        main.cpp \
        support.cpp \
        td3.cpp

TARGET = grading

