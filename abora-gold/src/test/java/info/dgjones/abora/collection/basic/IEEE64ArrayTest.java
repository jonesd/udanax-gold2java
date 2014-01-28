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
package info.dgjones.abora.collection.basic;

import java.util.Arrays;

import info.dgjones.abora.gold.AboraGoldTestCase;
import info.dgjones.abora.gold.collection.basic.IEEE32Array;
import info.dgjones.abora.gold.collection.basic.IEEE64Array;
import info.dgjones.abora.gold.collection.basic.Int32Array;
import info.dgjones.abora.gold.x.PrimFloatSpec;
import info.dgjones.abora.gold.x.PrimFloatValue;
import info.dgjones.abora.gold.x.PrimIEEE64;
import info.dgjones.abora.gold.x.PrimIntValue;

public class IEEE64ArrayTest extends AboraGoldTestCase {
	private static final double DIFF = 0.000001;

	public IEEE64ArrayTest(String arg0) {
		super(arg0);
	}

	public void testMakeCount() {
		IEEE64Array array = IEEE64Array.make(0);
		assertEquals(0, array.count());
		
		array = IEEE64Array.make(1);
		assertEquals(1, array.count());
		assertEquals(0.0, array.iEEE64At(0), DIFF);
	}

	public void testMake() {
		IEEE64Array array = IEEE64Array.make(AssertArrays.makeIEEE64ArrayEmpty());
		assertEquals(0, array.count());

		array = IEEE64Array.make(AssertArrays.makeIEEE64Array12345());
		assertEquals(5, array.count());
		AssertArrays.assertEquals(AssertArrays.makeIEEE64Array12345(), array);

		array = IEEE64Array.make(7, AssertArrays.makeIEEE64Array12345());
		assertEquals(7, array.count());
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{1.1, 2.2, 3.3, 4.4, 5.5, 0, 0}), array);

		array = IEEE64Array.make(7, AssertArrays.makeIEEE64Array12345(), 1, 2, 5);
		assertEquals(7, array.count());
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{0, 0, 0, 0, 0, 2.2, 3.3}), array);		

		try {
			IEEE64Array.make(4, AssertArrays.makeIEEE64Array12345());
			fail("4");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}
	
	public void testMakeBuffer() {
		IEEE64Array array = IEEE64Array.make(new double[] {});
		assertEquals(0, array.count());

		array = IEEE64Array.make(new double[] {1.1, 2.2});
		assertEquals(2, array.count());
		assertEquals(1.1f, array.iEEE64At(0), DIFF);
		assertEquals(2.2f, array.iEEE64At(1), DIFF);		
	}

	public void testIEEE64At() {
		IEEE64Array a = IEEE64Array.make(new double[] { 0.0, 1.1, -2.2, 3.3 });

		assertEquals(0.0, a.iEEE64At(0), DIFF);
		assertEquals(1.1, a.iEEE64At(1), DIFF);
		assertEquals(-2.2, a.iEEE64At(2), DIFF);
		assertEquals(3.3, a.iEEE64At(3), DIFF);

		try {
			a.iEEE64At(-1);
			fail("expected -1 failure");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			a.iEEE64At(4);
			fail("expected -1 failure");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testIEEE64AtEmpty() {
		IEEE64Array a = IEEE64Array.make(0);
		try {
			a.iEEE64At(0);
			fail("expected 0 failure");
		} catch (IndexOutOfBoundsException e) {
			// OutOfBounds
		}
	}
	
	public void testFloatAt() {
		IEEE64Array a = IEEE64Array.make(new double[] { 0.0, 1.1, -2.2, 3.3 });

		assertEquals(0.0, a.floatAt(0), DIFF);
		assertEquals(1.1, a.floatAt(1), DIFF);
		assertEquals(-2.2, a.floatAt(2), DIFF);
		assertEquals(3.3, a.floatAt(3), DIFF);

		try {
			a.floatAt(-1);
			fail("expected -1 failure");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			a.floatAt(4);
			fail("expected -1 failure");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testFetchValue() {
		IEEE64Array a = IEEE64Array.make(new double[] { 0.0, 1.1, -2.2, 3.3 });

		assertEquals(0.0, ((PrimFloatValue)a.fetchValue(0)).asIEEE64(), DIFF);
		assertEquals(1.1, ((PrimFloatValue)a.fetchValue(1)).asIEEE64(), DIFF);
		assertEquals(-2.2, ((PrimFloatValue)a.fetchValue(2)).asIEEE64(), DIFF);
		assertEquals(3.3, ((PrimFloatValue)a.fetchValue(3)).asIEEE64(), DIFF);

		try {
			a.fetchValue(-1);
			fail("expected -1 failure");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			a.fetchValue(4);
			fail("expected -1 failure");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testCount() {
		assertEquals(IEEE64Array.make(new double[] {}).count(), 0); 
		assertEquals(IEEE64Array.make(new double[] {0.0}).count(), 1);
		assertEquals(IEEE64Array.make(new double[] {0.0, 1.1}).count(), 2);  
	}
	
	public void testStoreIEEE64() {
		IEEE64Array empty = IEEE64Array.make(0);
		IEEE64Array tri = IEEE64Array.make(3);

		tri.storeIEEE64(0, Float.MIN_VALUE);
		assertEquals(tri.iEEE64At(0), Float.MIN_VALUE, DIFF);
		tri.storeIEEE64(1, 1.1f);
		assertEquals(tri.iEEE64At(1), 1.1f, DIFF);
		tri.storeIEEE64(2, Float.MAX_VALUE);
		assertEquals(tri.iEEE64At(2), Float.MAX_VALUE, DIFF);
		
		try {
			tri.storeIEEE64(-1, 1.1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			tri.storeIEEE64(3, 1.1);
			fail("3");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		
		try {
			empty.storeIEEE64(0, 1.1);
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testStoreFloat() {
		IEEE64Array empty = IEEE64Array.make(0);
		IEEE64Array tri = IEEE64Array.make(3);

		tri.storeFloat(0, Float.MIN_VALUE);
		assertTrue(tri.iEEE64At(0) == Float.MIN_VALUE);
		tri.storeFloat(1, 1.1f);
		assertEquals(tri.iEEE64At(1), 1.1, DIFF);
		tri.storeFloat(2, Float.MAX_VALUE);
		assertTrue(tri.iEEE64At(2) == Float.MAX_VALUE);

		tri.storeFloat(0, Double.MIN_VALUE);
		assertTrue(tri.iEEE64At(0) == Double.MIN_VALUE);
		tri.storeFloat(1, 1.1d);
		assertTrue(tri.iEEE64At(1) == 1.1);
		assertTrue(tri.iEEE64At(1) == 1.1);
		tri.storeFloat(2, Double.MAX_VALUE);
		assertTrue(tri.iEEE64At(2) == Double.MAX_VALUE);
		
		try {
			tri.storeFloat(-1, 1.1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			tri.storeFloat(3, 1.1);
			fail("3");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		
		try {
			empty.storeFloat(0, 1.1);
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testStoreValue() {
		IEEE64Array empty = IEEE64Array.make(0);
		IEEE64Array tri = IEEE64Array.make(3);

		tri.storeValue(0, PrimIEEE64.make(Float.MIN_VALUE));
		assertTrue(tri.iEEE64At(0) == Float.MIN_VALUE);
		tri.storeValue(1, PrimIEEE64.make(1.1));
		assertEquals(tri.iEEE64At(1), 1.1, DIFF);
		tri.storeValue(2, PrimIEEE64.make(Float.MAX_VALUE));
		assertTrue(tri.iEEE64At(2) == Float.MAX_VALUE);

		tri.storeValue(0, PrimIEEE64.make(Double.MIN_VALUE));
		assertTrue(tri.iEEE64At(0) == Double.MIN_VALUE);
		tri.storeValue(0, PrimIEEE64.make(1.1));
		assertTrue(tri.iEEE64At(0) == 1.1);
		assertTrue(tri.iEEE64At(0) == 1.1);
		tri.storeValue(2, PrimIEEE64.make(Double.MAX_VALUE));
		assertTrue(tri.iEEE64At(2) == Double.MAX_VALUE);
		
		try {
			tri.storeValue(-1, PrimIEEE64.make(1.1f));
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			tri.storeValue(3, PrimIEEE64.make(1.1f));
			fail("3");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		
		try {
			empty.storeValue(0, PrimIEEE64.make(1.1f));
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		
		// Null value
		try {tri.storeValue(0, null);
			fail("null");
		} catch (NullPointerException e) {
			// expected
		}
	}

	public void testStoreAll() {
		IEEE64Array array = IEEE64Array.make(0);
		array.storeAll(PrimIEEE64.make(1.1));
		AssertArrays.assertEquals(IEEE64Array.make(0), array, DIFF);
		
		array = IEEE64Array.make(1);
		array.storeAll(PrimIEEE64.make(1.1));
		AssertArrays.assertEquals(IEEE64Array.make(new double[] {1.1}), array, DIFF);
		
		array = IEEE64Array.make(3);
		array.storeAll(PrimIEEE64.make(2.2));
		AssertArrays.assertEquals(IEEE64Array.make(new double[] {2.2, 2.2, 2.2}), array, DIFF);
		
		array = AssertArrays.makeIEEE64Array12345();
		array.storeAll(PrimIEEE64.make(9.9), 2, 1);
		AssertArrays.assertEquals(IEEE64Array.make(new double[] {1.1, 9.9, 9.9, 4.4, 5.5}), array, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		array.storeAll(null, 2, 1);
		AssertArrays.assertEquals(IEEE64Array.make(new double[] {1.1, 0.0, 0.0, 4.4, 5.5}), array, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		array.storeAll(PrimIEEE64.make(9.9), -1, 1);
		AssertArrays.assertEquals(IEEE64Array.make(new double[] {1.1, 9.9, 9.9, 9.9, 9.9}), array, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		array.storeAll(PrimIEEE64.make(9.9), 2);
		AssertArrays.assertEquals(IEEE64Array.make(new double[] {9.9, 9.9, 3.3, 4.4, 5.5}), array, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		array.storeAll(PrimIEEE64.make(9.9), 0, 1);
		AssertArrays.assertEquals(IEEE64Array.make(new double[] {1.1, 2.2, 3.3, 4.4, 5.5}), array, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		array.storeAll(PrimIEEE64.make(9.9), 2, 1);
		AssertArrays.assertEquals(IEEE64Array.make(new double[] {1.1, 9.9, 9.9, 4.4, 5.5}), array, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		try {
			array.storeAll(PrimIEEE64.make(9.9), 6);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = AssertArrays.makeIEEE64Array12345();
		try {
			array.storeAll(PrimIEEE64.make(9.9), 4, 2);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// Store incompatible type
		array = AssertArrays.makeIEEE64Array12345();
		try {
			array.storeAll(PrimIntValue.make(9), 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}
	
	public void testStoreMany() {
		// empty
		IEEE64Array array = AssertArrays.makeIEEE64ArrayEmpty();
		array.storeMany(0, AssertArrays.makeIEEE64ArrayEmpty());
		AssertArrays.assertEquals(AssertArrays.makeIEEE64ArrayEmpty(), array, DIFF);

		// simple
		array = AssertArrays.makeIEEE64Array12345();
		array.storeMany(0, AssertArrays.makeIEEE64Array12321());
		AssertArrays.assertEquals(AssertArrays.makeIEEE64Array12321(), array, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		array.storeMany(0, AssertArrays.makeIEEE64Array12321());
		AssertArrays.assertEquals(AssertArrays.makeIEEE64Array12321(), array, DIFF);

		array = AssertArrays.makeIEEE64Array12321();
		array.storeMany(1, IEEE64Array.make(new double[]{8.8, 7.7, 6.6, 5.5, 4.4, 3,3}), 2, 3);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{1.1, 5.5, 4.4, 2.2, 1.1}), array, DIFF);

		// copy different types of array
		array = AssertArrays.makeIEEE64Array12321();
		array.storeMany(1, IEEE64Array.make(new double[]{8.8, 7.7}));
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{1.1, 8.8, 7.7, 2.2, 1.1}), array, DIFF);

		array = AssertArrays.makeIEEE64Array12321();
		try {
			array.storeMany(1, Int32Array.make(new int[]{8, 7}));
			fail();
		} catch (ClassCastException e) {
			// expected
		}

		// attempt to copy beyond this extent
		array = AssertArrays.makeIEEE64Array12345();
		try {
			array.storeMany(1, AssertArrays.makeIEEE64Array12321());
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}

		// insufficient source elements
		array = AssertArrays.makeIEEE64Array12345();
		try {
			array.storeMany(0, AssertArrays.makeIEEE64Array12321(), 2, 4);
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}
	}
	
	public void testCopyToBuffer() {
		IEEE64Array array = AssertArrays.makeIEEE64Array12345();
		double[] out = new double[3];
		array.copyToBuffer(out, 3, 1);
		assertTrue(Arrays.equals(out, new double[] {2.2, 3.3, 4.4}));		

		array = AssertArrays.makeIEEE64Array12345();
		out = new double[1];
		array.copyToBuffer(out, -1, 0);
		assertTrue(Arrays.equals(out, new double[] {1.1}));		

		array = AssertArrays.makeIEEE64Array12345();
		out = new double[1];
		array.copyToBuffer(out, -1, 4);
		assertTrue(Arrays.equals(out, new double[] {5.5}));		

		array = AssertArrays.makeIEEE64Array12345();
		out = new double[3];
		array.copyToBuffer(out, 3, 0);
		assertTrue(Arrays.equals(out, new double[] {1.1, 2.2, 3.3}));		

		array = AssertArrays.makeIEEE64Array12345();
		out = new double[3];
		array.copyToBuffer(out, -1, 2);
		assertTrue(Arrays.equals(out, new double[] {3.3, 4.4, 5.5}));		

		array = AssertArrays.makeIEEE64Array12345();
		out = new double[3];
		array.copyToBuffer(out, -1, 3);
		assertTrue(Arrays.equals(out, new double[] {4.4, 5.5, 0.0}));		

		array = AssertArrays.makeIEEE64Array12345();
		out = new double[0];
		array.copyToBuffer(out, -1, 3);
		assertTrue(Arrays.equals(out, new double[] {}));		

		array = AssertArrays.makeIEEE64Array12345();
		out = new double[3];
		try {
			array.copyToBuffer(out, -1, -1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = AssertArrays.makeIEEE64Array12345();
		out = new double[3];
		try {
			array.copyToBuffer(out, 1, 5);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		
	}
	
	public void testIsEqual() {
		double[] floatE = new double[] {};
		double[] float1 = new double[] {1.1};
		double[] float12 = new double[] {1.1, 2.2};
		double[] float11 = new double[] {1.1, 1.1};
		
		assertTrue(IEEE64Array.make(floatE).isEqual(IEEE64Array.make(floatE)));
		assertTrue(IEEE64Array.make(float1).isEqual(IEEE64Array.make(float1)));
		assertTrue(IEEE64Array.make(float12).isEqual(IEEE64Array.make(float12)));
		assertFalse(IEEE64Array.make(float11).isEqual(IEEE64Array.make(float12)));
		assertFalse(IEEE64Array.make(floatE).isEqual(IEEE64Array.make(float12)));
		assertFalse(IEEE64Array.make(float1).isEqual(PrimIEEE64.make(1.1f)));
		assertFalse(IEEE64Array.make(float12).isEqual(IEEE64Array.make(floatE)));
	}
	
	public void testIndexOf() {		
		int index = AssertArrays.makeIEEE64ArrayEmpty().indexOf(PrimIEEE64.make(1.1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeIEEE64Array1().indexOf(PrimIEEE64.make(1.1), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeIEEE64Array1().indexOf(PrimIEEE64.make(1.1), 0, 0);
		assertEquals(-1, index);
		
		index = AssertArrays.makeIEEE64Array12345().indexOf(PrimIEEE64.make(1.1), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeIEEE64Array12345().indexOf(PrimIEEE64.make(1.1), 0, 2);
		assertEquals(-1, index);

		index = AssertArrays.makeIEEE64Array12345().indexOf(PrimIEEE64.make(1.1), 1, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeIEEE64Array12345().indexOf(PrimIEEE64.make(5.5), 0, 1);
		assertEquals(4, index);
		
		index = AssertArrays.makeIEEE64Array12321().indexOf(PrimIEEE64.make(2.2), 0, 1);
		assertEquals(1, index);

		index = AssertArrays.makeIEEE64Array12321().indexOf(PrimIEEE64.make(2.2), 0, 2);
		assertEquals(3, index);

		index = AssertArrays.makeIEEE64Array12321().indexOf(PrimIEEE64.make(1.1), -1, -1);
		assertEquals(4, index);

		index = AssertArrays.makeIEEE64Array12321().indexOf(PrimIEEE64.make(2.2), -1, -1);
		assertEquals(3, index);

		index = AssertArrays.makeIEEE64Array12321().indexOf(PrimIEEE64.make(2.2), -1, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeIEEE64Array12321().indexOf(PrimIEEE64.make(2.2), -1, -2);
		assertEquals(1, index);

		index = AssertArrays.makeIEEE64Array12321().indexOf(PrimIEEE64.make(2.2), -1, -3);
		assertEquals(-1, index);

		index = AssertArrays.makeIEEE64Array12321().indexOf(PrimIEEE64.make(2.2), -3, -1);
		assertEquals(1, index);

		index = AssertArrays.makeIEEE64Array12321().indexOf(PrimIEEE64.make(2.2), -1, 1);
		assertEquals(-1, index);

		try {
			AssertArrays.makeIEEE64Array12321().indexOf(PrimIEEE64.make(2.2), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			AssertArrays.makeIEEE64Array12321().indexOf(PrimIntValue.make(2), -3, 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}
	
	public void testIndexPast() {		
		int index = AssertArrays.makeIEEE64ArrayEmpty().indexPast(PrimIEEE64.make(1.1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeIEEE64Array1().indexPast(PrimIEEE64.make(1.1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeIEEE64Array1().indexPast(PrimIEEE64.make(1.1), 0, 0);
		assertEquals(-1, index);
		
		index = AssertArrays.makeIEEE64Array12345().indexPast(PrimIEEE64.make(1.1), 0, 1);
		assertEquals(1, index);

		index = AssertArrays.makeIEEE64Array12345().indexPast(PrimIEEE64.make(1.1), 0, 2);
		assertEquals(2, index);

		index = AssertArrays.makeIEEE64Array12345().indexPast(PrimIEEE64.make(1.1), 1, 1);
		assertEquals(1, index);

		index = AssertArrays.makeIEEE64Array12345().indexPast(PrimIEEE64.make(5.5), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeIEEE64Array12345().indexPast(PrimIEEE64.make(5.5), 3, 1);
		assertEquals(3, index);

		index = AssertArrays.makeIEEE64Array12345().indexPast(PrimIEEE64.make(5.5), 4, 1);
		assertEquals(-1, index);
		
		index = AssertArrays.makeIEEE64Array12321().indexPast(PrimIEEE64.make(2.2), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeIEEE64Array12321().indexPast(PrimIEEE64.make(2.2), 0, 2);
		assertEquals(2, index);

		index = AssertArrays.makeIEEE64Array12321().indexPast(PrimIEEE64.make(1.1), -1, -1);
		assertEquals(3, index);

		index = AssertArrays.makeIEEE64Array12321().indexPast(PrimIEEE64.make(2.2), -1, -1);
		assertEquals(4, index);

		index = AssertArrays.makeIEEE64Array12321().indexPast(PrimIEEE64.make(2.2), -1, 1);
		assertEquals(4, index);

		index = AssertArrays.makeIEEE64Array12321().indexPast(PrimIEEE64.make(2.2), -1, 2);
		assertEquals(-1, index);

		index = AssertArrays.makeIEEE64Array12321().indexPast(PrimIEEE64.make(2.2), -1, -2);
		assertEquals(2, index);

		index = AssertArrays.makeIEEE64Array12321().indexPast(PrimIEEE64.make(2.2), -1, -3);
		assertEquals(0, index);

		index = AssertArrays.makeIEEE64Array12321().indexPast(PrimIEEE64.make(2.2), -3, -1);
		assertEquals(2, index);

		index = AssertArrays.makeIEEE64Array12321().indexPast(PrimIEEE64.make(1.1), -1, 1);
		assertEquals(-1, index);

		try {
			AssertArrays.makeIEEE64Array12321().indexPast(PrimIEEE64.make(2.2), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}
	
	public void testIndexOfElements() {
		// empty
		IEEE64Array array = AssertArrays.makeIEEE64ArrayEmpty();
		IEEE64Array search = AssertArrays.makeIEEE64Array1();
		assertEquals(-1, array.indexOfElements(search));

		array = AssertArrays.makeIEEE64ArrayEmpty();
		search = AssertArrays.makeIEEE64ArrayEmpty();
		assertEquals(-1, array.indexOfElements(search));

// TODO skip zero length other?
//		array = AssertArrays.makeIEEE64Array12345();
//		search = AssertArrays.makeIEEE64ArrayEmpty();
//		assertEquals(-1, array.indexOfElements(search));

		// forward search
		array = IEEE64Array.make(new double[]{1.1, 2.2, 3.3, 1.1, 2.2});
		search = IEEE64Array.make(new double[]{1.1, 2.2});
		assertEquals(0, array.indexOfElements(search));
		assertEquals(3, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		array = AssertArrays.makeIEEE64Array12321();
		search = IEEE64Array.make(new double[]{4.4, 9.9, 2.2, 8.8});
		assertEquals(1, array.indexOfElements(search, 1, 2, 0, 1));
		assertEquals(3, array.indexOfElements(search, 1, 2, 0, 2));
		assertEquals(-1, array.indexOfElements(search, 1, 2, 0, 3));
		
		// reverse search		
		array = IEEE64Array.make(new double[]{1.1, 2.2, 3.3f, 1.1, 2.2});
		search = IEEE64Array.make(new double[]{1.1, 2.2});
		assertEquals(3, array.indexOfElements(search, -1, 0, -2, -1));
		assertEquals(0, array.indexOfElements(search, -1, 0, -2, -2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, -2, -3));

		// overlapping search
		// TODO should this succeed?
		array = IEEE64Array.make(new double[]{1.1, 1.1, 1.1});
		search = IEEE64Array.make(new double[]{1.1, 1.1});
		assertEquals(0, array.indexOfElements(search));
		assertEquals(1, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		// nth == 0 immediate fail
		array = AssertArrays.makeIEEE64Array12321();
		search = IEEE64Array.make(new double[]{1.1});
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 0));

		// overflowing otherCount
		array = AssertArrays.makeIEEE64Array12321();
		search = IEEE64Array.make(new double[]{1.1, 2.2});
		try {
			array.indexOfElements(search, 3);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			array.indexOfElements(search, 2, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// invalid start		
		array = AssertArrays.makeIEEE64Array12321();
		search = IEEE64Array.make(new double[]{1.1, 2.2});
// TODO different invalid start values if + or - 
//		try {
//			array.indexOfElements(search, -1, 0, 6, 0);
//			fail();
//		} catch (IndexOutOfBoundsException e) {
//			// expected
//		}
		try {
			array.indexOfElements(search, -1, 0, -1, -1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}		
	}
	
	public void testAddElements() {
		IEEE64Array array = AssertArrays.makeIEEE64ArrayEmpty();
		array.addElements(0, AssertArrays.makeIEEE64Array12321(), -1, 0);
		AssertArrays.assertEquals(AssertArrays.makeIEEE64ArrayEmpty(), array, DIFF);

		array = AssertArrays.makeIEEE64Array1();
		array.addElements(0, IEEE64Array.make(new double[]{9}), -1, 0);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{10.1}), array, DIFF);

		array = IEEE64Array.make(5);
		array.addElements(0, AssertArrays.makeIEEE64Array12321(), -1, 0);
		AssertArrays.assertEquals(AssertArrays.makeIEEE64Array12321(), array, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		array.addElements(0, AssertArrays.makeIEEE64Array12321(), -1, 0);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{2.2, 4.4, 6.6, 6.6, 6.6}), array, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		array.addElements(2, AssertArrays.makeIEEE64Array12321(), -1, 0);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{1.1, 2.2, 4.4, 6.6, 8.8}), array, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		array.addElements(2, AssertArrays.makeIEEE64Array12321(), -1);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{1.1, 2.2, 4.4, 6.6, 8.8}), array, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		array.addElements(2, AssertArrays.makeIEEE64Array12321());
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{1.1, 2.2, 4.4, 6.6, 8.8}), array, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		array.addElements(2, AssertArrays.makeIEEE64Array12321(), 2, 1);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{1.1, 2.2, 5.5, 7.7, 5.5}), array, DIFF);

		// compatible array types
		array = AssertArrays.makeIEEE64Array12345();
		array.addElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{2.2, 4.4, 6.6, 6.6, 6.6}), array, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		try {
			array.addElements(2, AssertArrays.makeIEEE64Array12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// incompatible array types
		array = AssertArrays.makeIEEE64Array12345();
		try {
			array.addElements(0, AssertArrays.makeInt32Array12321(), -1, 0);
			fail("incompatible");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testSubtractElements() {
		IEEE64Array array = AssertArrays.makeIEEE64ArrayEmpty();
		array.subtractElements(0, AssertArrays.makeIEEE64Array12321(), -1, 0);
		AssertArrays.assertEquals(AssertArrays.makeIEEE64ArrayEmpty(), array, DIFF);

		array = AssertArrays.makeIEEE64Array1();
		array.subtractElements(0, IEEE64Array.make(new double[]{9}), -1, 0);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{-7.9}), array, DIFF);

		array = IEEE64Array.make(5);
		array.subtractElements(0, AssertArrays.makeIEEE64Array12321(), -1, 0);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{-1.1, -2.2, -3.3, -2.2, -1.1}), array, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		array.subtractElements(0, AssertArrays.makeIEEE64Array12321(), -1, 0);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{0.0, 0.0, 0.0, 2.2, 4.4}), array, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		array.subtractElements(2, AssertArrays.makeIEEE64Array12321(), -1, 0);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{1.1, 2.2, 2.2, 2.2, 2.2}), array, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		array.subtractElements(2, AssertArrays.makeIEEE64Array12321(), -1);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{1.1, 2.2, 2.2, 2.2, 2.2}), array, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		array.subtractElements(2, AssertArrays.makeIEEE64Array12321());
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{1.1, 2.2, 2.2, 2.2, 2.2}), array, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		array.subtractElements(2, AssertArrays.makeIEEE64Array12321(), 2, 1);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{1.1, 2.2, 1.1, 1.1, 5.5}), array, DIFF);

		// compatible array types
		array = AssertArrays.makeIEEE64Array12345();
		array.subtractElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{0.0, 0.0, 0.0, 2.2, 4.4}), array, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		try {
			array.subtractElements(2, AssertArrays.makeIEEE64Array12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// incompatible array types
		array = AssertArrays.makeIEEE64Array12345();
		try {
			array.subtractElements(0, AssertArrays.makeInt32Array12321(), -1, 0);
			fail("incompatible");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testCompare() {
		// same
		IEEE64Array array1 = AssertArrays.makeIEEE64ArrayEmpty();
		IEEE64Array array2 = AssertArrays.makeIEEE64ArrayEmpty();
		assertEquals(0, array1.compare(array2));

		array1 = AssertArrays.makeIEEE64Array12345();
		array2 = AssertArrays.makeIEEE64Array12345();
		assertEquals(0, array1.compare(array2));
		
		array1 = AssertArrays.makeIEEE64Array12321();
		array2 = AssertArrays.makeIEEE64Array12321();
		assertEquals(0, array1.compare(array2));

		// different
		array1 = IEEE64Array.make(new double[] {0.1});
		array2 = IEEE64Array.make(new double[] {0.2});
		assertEquals(-1, array1.compare(array2));
		
		array1 = IEEE64Array.make(new double[] {0.2});
		array2 = IEEE64Array.make(new double[] {0.1});
		assertEquals(1, array1.compare(array2));

		array1 = AssertArrays.makeIEEE64Array12321();
		array2 = AssertArrays.makeIEEE64Array12345();
		assertEquals(-1, array1.compare(array2));

		array1 = AssertArrays.makeIEEE64Array12345();
		array2 = AssertArrays.makeIEEE64Array12321();
		assertEquals(1, array1.compare(array2));
		
		// auto-filling with 0
		array1 = AssertArrays.makeIEEE64ArrayEmpty();
		array2 = AssertArrays.makeIEEE64Array1();
		assertEquals(-1, array1.compare(array2));

		array1 = AssertArrays.makeIEEE64Array1();
		array2 = AssertArrays.makeIEEE64ArrayEmpty();
		assertEquals(1, array1.compare(array2));

		array1 = IEEE64Array.make(new double[]{0.0, 0.0});
		array2 = IEEE64Array.make(new double[]{0.0});
		assertEquals(0, array1.compare(array2));

		array1 = IEEE64Array.make(new double[]{1.0, -1.0});
		array2 = IEEE64Array.make(new double[]{1.0});
		assertEquals(-1, array1.compare(array2));

		// compare sub-regions		
		array1 = AssertArrays.makeIEEE64Array12321();
		array2 = AssertArrays.makeIEEE64Array12345();
		assertEquals(1, array1.compare(array2, 2, 2, 1));

		array1 = AssertArrays.makeIEEE64Array12321();
		array2 = AssertArrays.makeIEEE64Array12345();
		assertEquals(1, array1.compare(array2, 2, 2));

		array1 = AssertArrays.makeIEEE64Array12321();
		array2 = AssertArrays.makeIEEE64Array12345();
		assertEquals(0, array1.compare(array2, 1, 4));

		// trim down count
		array1 = AssertArrays.makeIEEE64Array12321();
		array2 = AssertArrays.makeIEEE64Array12345();
		assertEquals(-1, array1.compare(array2, 10));
		
		// compare near minimum held value
		array1 = IEEE64Array.make(new double[] { Double.MIN_VALUE});
		array2 = IEEE64Array.make(new double[] { Double.MAX_VALUE});
		assertEquals(-1, array1.compare(array2));
		
		// compare different type
		array1 = IEEE64Array.make(new double[] {0.1});
		IEEE32Array arrayIEEE32 = IEEE32Array.make(new float[] {1.0f});
		assertEquals(-1, array1.compare(arrayIEEE32));

		array1 = IEEE64Array.make(new double[] {1.0});
		arrayIEEE32 = IEEE32Array.make(new float[] {1.0f});
		assertEquals(0, array1.compare(arrayIEEE32));
		
		// compare incompatible type
		array1 = AssertArrays.makeIEEE64Array12345();
		Int32Array arrayInt32 = AssertArrays.makeInt32Array12321();
		try {
			array1.compare(arrayInt32);
			fail("incompatible");				
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}
	
	public void testCopyGrow() {
		IEEE64Array array = AssertArrays.makeIEEE64ArrayEmpty();
		IEEE64Array copy = (IEEE64Array) array.copyGrow(0);
		AssertArrays.assertEquals(AssertArrays.makeIEEE64ArrayEmpty(), copy, DIFF);
		assertNotSame(array, copy);
		
		array = AssertArrays.makeIEEE64Array12345();
		copy = (IEEE64Array) array.copyGrow(0);
		AssertArrays.assertEquals(AssertArrays.makeIEEE64Array12345(), copy, DIFF);
		assertNotSame(array, copy);

		array = AssertArrays.makeIEEE64Array12345();
		copy = (IEEE64Array) array.copyGrow(3);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{1.1, 2.2, 3.3, 4.4, 5.5, 0.0, 0.0, 0.0}), copy, DIFF);
		assertNotSame(array, copy);
	}

	public void testCopy() {
		// full copy
		IEEE64Array array = AssertArrays.makeIEEE64ArrayEmpty();
		IEEE64Array copy = (IEEE64Array) array.copy();
		AssertArrays.assertEquals(AssertArrays.makeIEEE64ArrayEmpty(), copy, DIFF);
		assertNotSame(array, copy);
		
		array = AssertArrays.makeIEEE64Array12345();
		copy = (IEEE64Array) array.copy();
		AssertArrays.assertEquals(AssertArrays.makeIEEE64Array12345(), copy, DIFF);
		AssertArrays.assertEquals(AssertArrays.makeIEEE64Array12345(), array, DIFF);
		assertNotSame(array, copy);
		
		// partial copy
		array = AssertArrays.makeIEEE64Array12345();
		copy = (IEEE64Array) array.copy(2, 1);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{2.2, 3.3}), copy, DIFF);
		assertNotSame(array, copy);

		array = AssertArrays.makeIEEE64Array12345();
		copy = (IEEE64Array) array.copy(2, 0);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{1.1, 2.2}), copy, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		copy = (IEEE64Array) array.copy(2, 3);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{4.4, 5.5}), copy, DIFF);

		// partial copy with too large count
		array = AssertArrays.makeIEEE64Array12345();
		try {
			array.copy(5, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// partial copy with trailing space
		array = AssertArrays.makeIEEE64Array12345();
		copy = (IEEE64Array) array.copy(2, 1);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{2.2, 3.3}), copy, DIFF);

		// partial copy with leading space
		array = AssertArrays.makeIEEE64Array12345();
		copy = (IEEE64Array) array.copy(2, 1, 1);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{0.0, 2.2, 3.3}), copy, DIFF);

		// partial copy with leading space
		array = AssertArrays.makeIEEE64Array12345();
		copy = (IEEE64Array) array.copy(2, 1, 0, 1);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{2.2, 3.3, 0.0}), copy, DIFF);

		// partial copy with leading and trailing space
		array = AssertArrays.makeIEEE64Array12345();
		copy = (IEEE64Array) array.copy(2, 1, 2, 1);
		AssertArrays.assertEquals(IEEE64Array.make(new double[]{0.0, 0.0, 2.2, 3.3, 0.0}), copy, DIFF);
	}
	
	public void testToString() {
		assertEquals("[empty]", AssertArrays.makeIEEE64ArrayEmpty().toString());
		assertEquals("[1.1 2.2 3.3 4.4 5.5]", AssertArrays.makeIEEE64Array12345().toString());
	}

	public void testSpec() {
		PrimFloatSpec spec = (PrimFloatSpec)AssertArrays.makeIEEE64ArrayEmpty().spec();
		assertEquals(spec.bitCount(), AssertArrays.makeIEEE64ArrayEmpty().bitCount());		
	}

	public void testZeroElements() {
		IEEE64Array array = IEEE64Array.make(0);
		array.zeroElements(0, -1);
		AssertArrays.assertEquals(IEEE64Array.make(0), array, DIFF);
		
		// zero all elements
		array = IEEE64Array.make(1);
		array.storeFloat(0, 1.1);
		array.zeroElements(0, -1);
		AssertArrays.assertEquals(IEEE64Array.make(new double[] {0}), array, DIFF);
		
		array = AssertArrays.makeIEEE64Array12345();
		array.zeroElements(0, -1);
		AssertArrays.assertEquals(IEEE64Array.make(new double[] {0, 0, 0, 0, 0}), array, DIFF);

		// zero subset of elements
		array = AssertArrays.makeIEEE64Array12345();
		array.zeroElements(1, -1);
		AssertArrays.assertEquals(IEEE64Array.make(new double[] {1.1, 0, 0, 0, 0}), array, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		array.zeroElements(1, 2);
		AssertArrays.assertEquals(IEEE64Array.make(new double[] {1.1, 0, 0, 4.4, 5.5}), array, DIFF);

		array = AssertArrays.makeIEEE64Array12345();
		array.zeroElements(4, 1);
		AssertArrays.assertEquals(IEEE64Array.make(new double[] {1.1, 2.2, 3.3, 4.4, 0.0}), array, DIFF);

		// silently truncate from
		array = AssertArrays.makeIEEE64Array12345();
		//TODO should this actually throw an exception?
		array.zeroElements(5, -1);
		AssertArrays.assertEquals(AssertArrays.makeIEEE64Array12345(), array, DIFF);

		// extend count outside range
		array = AssertArrays.makeIEEE64Array12345();
		try {
			array.zeroElements(4, 2);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testElementsHash() {
		// complete hash
		int hash = AssertArrays.makeIEEE64ArrayEmpty().elementsHash();
		assertFalse(0 == hash);
		
		assertFalse(AssertArrays.makeIEEE64Array12345().elementsHash() == AssertArrays.makeIEEE64Array12321().elementsHash());

		// partial hash
		IEEE64Array array = AssertArrays.makeIEEE64Array12345();
		assertFalse(0 == array.elementsHash());
		assertFalse(array.elementsHash(0) == array.elementsHash(1));
		assertFalse(array.elementsHash(1, 1) == array.elementsHash(1));
		
		// out of range
		try {
			AssertArrays.makeIEEE64Array12345().elementsHash(5, 1);
			fail("5,1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testContentsHash() {
		int hash = AssertArrays.makeIEEE64ArrayEmpty().contentsHash();
		assertFalse(0 == hash);
		
		IEEE64Array array = AssertArrays.makeIEEE64Array12345();
		assertFalse(0 == array.contentsHash());
		
		assertFalse(AssertArrays.makeIEEE64Array12345().contentsHash() == AssertArrays.makeIEEE64Array12321().contentsHash());
	}
}

