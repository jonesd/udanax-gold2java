/**
 * The MIT License
 * Copyright (c) 2003 David G Jones
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package info.dgjones.abora.gold.collection.sets;

import info.dgjones.abora.gold.AboraGoldTestCase;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;

public class EmptyImmuSetTest extends AboraGoldTestCase {

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
