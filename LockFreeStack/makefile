COMPONENTS = LockFreeStack.class Test.class

all: $(COMPONENTS)

%.class: %.scala
	 fsc $<

clean:
	rm -rf *.class
