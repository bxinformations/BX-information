TEMPLATE = app
CONFIG += console c++11
CONFIG -= app_bundle
CONFIG -= qt
CONFIG+=debug

GRADINGLIB_HEADERS += gradinglib/gradinglib.hpp
GRADINGLIB_SOURCES += gradinglib/gradinglib.cpp

GRADING_HEADERS += grading/grading.hpp
GRADING_SOURCES += grading/grading.cpp


HEADERS += \
    $$GRADINGLIB_HEADERS \
    $$GRADING_HEADERS \
    td5.hpp

SOURCES += \
        $$GRADINGLIB_SOURCES \
        $$GRADING_SOURCES \
        main.cpp \
        td5.cpp

TARGET = grading

