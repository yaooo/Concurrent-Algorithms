COMPONENTS = ConcurrentSudoku.class Partial.class SimplePartial.class AdvancedPartial.class	\
Sudoku.class

all: $(COMPONENTS)

%.class: %.scala
	 fsc $<

clean:
	rm -rf *.class
