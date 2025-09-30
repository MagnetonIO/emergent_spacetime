# Makefile for E = Ic² Framework

GHC = ghc
GHCFLAGS = -Wall -O2 -isrc
SRC_DIR = src
BUILD_DIR = build
SOURCES = $(wildcard $(SRC_DIR)/*.hs)
MAIN = $(SRC_DIR)/Main.hs
EXECUTABLE = info-energy

.PHONY: all clean run

all: $(EXECUTABLE)

$(EXECUTABLE): $(SOURCES)
	@echo "Building E = Ic² framework..."
	@mkdir -p $(BUILD_DIR)
	$(GHC) $(GHCFLAGS) -o $(EXECUTABLE) $(MAIN) -odir $(BUILD_DIR) -hidir $(BUILD_DIR)
	@echo "Build complete! Run with: ./$(EXECUTABLE)"

run: $(EXECUTABLE)
	./$(EXECUTABLE)

clean:
	rm -rf $(BUILD_DIR) $(EXECUTABLE)
	find $(SRC_DIR) -name "*.hi" -delete
	find $(SRC_DIR) -name "*.o" -delete

help:
	@echo "E = Ic² Framework Build System"
	@echo "=============================="
	@echo "make        - Build the framework"
	@echo "make run    - Build and run the demonstration"
	@echo "make clean  - Clean build artifacts"
	@echo "make help   - Show this help message"