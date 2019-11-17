COMPONENTS = LinkedArrayList.class SimpleQueueTest.class LockFreeLinkedArrayList.class

all: $(COMPONENTS)

%.class: %.scala
	 fsc $<

clean:
	rm -rf *.class
