package org.abora.gold.collection.tables.test;

import org.abora.gold.collection.tables.Pair;
import org.abora.gold.java.exception.AboraRuntimeException;
import org.abora.gold.spaces.integers.IntegerPos;
import org.abora.gold.xpp.basic.Heaper;

import junit.framework.TestCase;



public class PairTest extends TestCase {

	public void testMake() {
		Heaper heaperLeft = new Heaper();
		Heaper heaperRight = new Heaper();
		Pair pair = Pair.make(heaperLeft, heaperRight);
		assertSame("left", heaperLeft, pair.fetchLeft());
		assertSame("right", heaperRight, pair.fetchRight());
	}

	public void testMakeWithNulls() {
		try {
			Pair.make(null, new Heaper());
			fail("null left");
		} catch (AboraRuntimeException e) {
			assertEquals(AboraRuntimeException.OBSOLETE_USAGE_MUST_USE_PAIR_WITH_NULLS, e.getMessage());
		}
		try {
			Pair.make(new Heaper(), null);
			fail("null right");
		} catch (AboraRuntimeException e) {
			assertEquals(AboraRuntimeException.OBSOLETE_USAGE_MUST_USE_PAIR_WITH_NULLS, e.getMessage());
		}

		try {
			Pair.make(null, null);
			fail("null left and right");
		} catch (AboraRuntimeException e) {
			assertEquals(AboraRuntimeException.OBSOLETE_USAGE_MUST_USE_PAIR_WITH_NULLS, e.getMessage());
		}
	}

	/**
	 * @deprecated
	 */
	public void testPairWithNulls() {
		Heaper heaperLeft = new Heaper();
		Heaper heaperRight = new Heaper();

		Pair pair = Pair.pairWithNulls(null, heaperRight);
		assertSame("left", null, pair.fetchLeft());
		assertSame("right", heaperRight, pair.fetchRight());

		pair = Pair.pairWithNulls(heaperLeft, null);
		assertSame("left", heaperLeft, pair.fetchLeft());
		assertSame("right", null, pair.fetchRight());

		pair = Pair.pairWithNulls(null, null);
		assertSame("left", null, pair.fetchLeft());
		assertSame("right", null, pair.fetchRight());
	}
	
	public void testIsEqual() {
		Heaper heaper1 = IntegerPos.make(1);
		Heaper heaper2 = IntegerPos.make(2);
		
		Pair pair12 = Pair.make(heaper1, heaper2);
		Pair pair21 = Pair.make(heaper2, heaper1);
		Pair pair1n = Pair.pairWithNulls(heaper1, null);
		Pair pairn2 = Pair.pairWithNulls(null, heaper2);
		Pair pairnn = Pair.pairWithNulls(null, null);

		assertTrue("pair12 = pair12", pair12.isEqual(pair12));
		assertTrue("pair1n = pair1n", pair1n.isEqual(pair1n));
		assertTrue("pairn2 = pairn2", pairn2.isEqual(pairn2));
		assertTrue("pairnn = pairnn", pairnn.isEqual(pairnn));
		
		assertFalse("pair12 != pair21", pair12.isEqual(pair21));
		assertFalse("pair12 != pair21", pair12.isEqual(pair21));
		
		assertFalse("pair12 != x", pair12.isEqual(heaper1));
	}

	public void XtestReversed() {
		Heaper heaper1 = new Heaper();
		Heaper heaper2 = new Heaper();
		
		Pair pair12 = Pair.make(heaper1, heaper2);
		
		Pair pairR = pair12.reversed();
		assertFalse(pair12.isEqual(pairR));
		assertSame("left", heaper2, pairR.fetchLeft());
		assertSame("right", heaper1, pairR.fetchRight());
		
		Pair pairRR = pairR.reversed();
		assertTrue(pair12.isEqual(pairRR));
	}
}
