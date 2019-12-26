COMPONENTS = Deque.class ListDeque.class LockFreeDeque.class Test.class 

all: $(COMPONENTS)

%.class: %.scala
	 fsc $<

clean:
	rm -rf *.class
