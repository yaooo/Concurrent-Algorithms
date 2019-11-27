COMPONENTS = LinkedArrayList.class LockFreeLinkedArrayList.class SimpleQueueTest.class

all: $(COMPONENTS)

%.class: %.scala
	 fsc $<

clean:
	rm -rf *.class
