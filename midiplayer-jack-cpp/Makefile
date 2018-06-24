NAME ?= midiplayer-jack-cpp
LIBS ?= `pkg-config --cflags --libs jack`
CXX ?= g++
BUILD_DIR ?= build

$(NAME):
	mkdir -p -- $(BUILD_DIR)
	$(CXX) -std=c++17 -O2 -Wall $(LIBS) src/main.c++ -o $(BUILD_DIR)/$(NAME)

clean:
	rm -f -- $(BUILD_DIR)/$(NAME)
