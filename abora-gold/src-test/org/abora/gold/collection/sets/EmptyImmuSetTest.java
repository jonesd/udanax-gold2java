package org.abora.gold.collection.sets;

import org.abora.gold.collection.steppers.Stepper;
import org.abora.gold.java.exception.AboraRuntimeException;
import org.abora.gold.spaces.integers.IntegerPos;

import junit.framework.TestCase;

public class EmptyImmuSetTest extends TestCase {

	public void testMakeEmpty() {
		EmptyImmuSet set = makeEmptyImmuSet();
		assertNotNull(set);
		assertTrue(set.isEmpty());
	}

	public void testMakeEmptyAliased() {
		EmptyImmuSet set1 = makeEmptyImmuSet();
		EmptyImmuSet set2 = makeEmptyImmuSet();
		assertSame(set1, set2);
	}

	private EmptyImmuSet makeEmptyImmuSet() {
		return (EmptyImmuSet)ImmuSet.make();
	}
	
	public void testCount() {
		assertEquals(0, makeEmptyImmuSet().count());
	}
	
	public void testStepper() {
		Stepper stepper = makeEmptyImmuSet().stepper();
		assertTrue(stepper.end());
	}

	public void testTheOne() {
		try {
			makeEmptyImmuSet().theOne();
			fail("expected exception");
		} catch (AboraRuntimeException e) {
			assertEquals(AboraRuntimeException.NOT_ONE_ELEMENT, e.getMessage());
		}
	}
	
	public void testWith() {
		EmptyImmuSet set = makeEmptyImmuSet();
		IntegerPos pos = IntegerPos.make(23);
		ImmuSet set1 = set.with(pos);
		assertFalse(set.hasMember(pos));
		assertTrue(set1.hasMember(pos));
	}

	public void testWithout() {
		EmptyImmuSet set = makeEmptyImmuSet();
		IntegerPos pos = IntegerPos.make(23);
		ImmuSet set1 = set.without(pos);
		assertFalse(set.hasMember(pos));
		assertFalse(set1.hasMember(pos));
	}

	public void testHasMember() {
		EmptyImmuSet set = makeEmptyImmuSet();
		IntegerPos pos = IntegerPos.make(23);
		assertFalse(set.hasMember(pos));
	}
}
