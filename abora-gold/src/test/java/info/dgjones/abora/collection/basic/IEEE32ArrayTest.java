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
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.x.PrimFloatSpec;
import info.dgjones.abora.gold.x.PrimFloatValue;
import info.dgjones.abora.gold.x.PrimIEEE32;
import info.dgjones.abora.gold.x.PrimIEEE64;

public class IEEE32ArrayTest extends AboraGoldTestCase {
	private static final float DIFF = 0.000001f;

	public IEEE32ArrayTest(String arg0) {
		super(arg0);
	}

	public void testMakeCount() {
		IEEE32Array array = IEEE32Array.make(0);
		assertEquals(0, array.count());
		
		array = IEEE32Array.make(1);
		assertEquals(1, array.count());
		assertEquals(0.0f, array.iEEE32At(0), DIFF);
	}

	public void testMake() {
		IEEE32Array array = IEEE32Array.make(AssertArrays.makeIEEE32ArrayEmpty());
		assertEquals(0, array.count());

		array = IEEE32Array.make(AssertArrays.makeIEEE32Array12345());
		assertEquals(5, array.count());
		AssertArrays.assertEquals(AssertArrays.makeIEEE32Array12345(), array);

		array = IEEE32Array.make(7, AssertArrays.makeIEEE32Array12345());
		assertEquals(7, array.count());
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{1.1f,2.2f,3.3f,4.4f,5.5f,0f,0f}), array);

		array = IEEE32Array.make(7, AssertArrays.makeIEEE32Array12345(), 1, 2, 5);
		assertEquals(7, array.count());
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{0f,0f,0f,0f,0f,2.2f,3.3f}), array);		

		try {
			IEEE32Array.make(4, AssertArrays.makeIEEE32Array12345());
			fail("4");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}
	
	public void testMakeBuffer() {
		IEEE32Array array = IEEE32Array.make(new float[] {});
		assertEquals(0, array.count());

		array = IEEE32Array.make(new float[] {1.1f, 2.2f});
		assertEquals(2, array.count());
		assertEquals(1.1f, array.iEEE32At(0), DIFF);
		assertEquals(2.2f, array.iEEE32At(1), DIFF);		
	}


	public void testIEEE32At() {
		IEEE32Array a = IEEE32Array.make(new float[] { 0.0f, 1.1f, -2.2f, 3.3f });

		assertEquals(0.0f, a.iEEE32At(0), DIFF);
		assertEquals(1.1f, a.iEEE32At(1), DIFF);
		assertEquals(-2.2f, a.iEEE32At(2), DIFF);
		assertEquals(3.3f, a.iEEE32At(3), DIFF);

		try {
			a.iEEE32At(-1);
			fail("expected -1 failure");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			a.iEEE32At(4);
			fail("expected -1 failure");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testIEEE32AtEmpty() {
		IEEE32Array a = IEEE32Array.make(0);
		try {
			a.iEEE32At(0);
			fail("expected 0 failure");
		} catch (IndexOutOfBoundsException e) {
			// OutOfBounds
		}
	}
	
	public void testFloatAt() {
		IEEE32Array a = IEEE32Array.make(new float[] { 0.0f, 1.1f, -2.2f, 3.3f });

		assertEquals(0.0f, a.floatAt(0), DIFF);
		assertEquals(1.1f, a.floatAt(1), DIFF);
		assertEquals(-2.2f, a.floatAt(2), DIFF);
		assertEquals(3.3f, a.floatAt(3), DIFF);

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
		IEEE32Array a = IEEE32Array.make(new float[] { 0.0f, 1.1f, -2.2f, 3.3f });

		assertEquals(0.0f, ((PrimFloatValue)a.fetchValue(0)).asIEEE32(), DIFF);
		assertEquals(1.1f, ((PrimFloatValue)a.fetchValue(1)).asIEEE32(), DIFF);
		assertEquals(-2.2f, ((PrimFloatValue)a.fetchValue(2)).asIEEE32(), DIFF);
		assertEquals(3.3f, ((PrimFloatValue)a.fetchValue(3)).asIEEE32(), DIFF);

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
		assertEquals(IEEE32Array.make(new float[] {}).count(), 0); 
		assertEquals(IEEE32Array.make(new float[] {0.0f}).count(), 1);
		assertEquals(IEEE32Array.make(new float[] {0.0f, 1.1f}).count(), 2);  
	}
	
	public void testStoreIEEE32() {
		IEEE32Array empty = IEEE32Array.make(0);
		IEEE32Array tri = IEEE32Array.make(3);

		tri.storeIEEE32(0, Float.MIN_VALUE);
		assertEquals(tri.iEEE32At(0), Float.MIN_VALUE, DIFF);
		tri.storeIEEE32(1, 1.1f);
		assertEquals(tri.iEEE32At(1), 1.1f, DIFF);
		tri.storeIEEE32(2, Float.MAX_VALUE);
		assertEquals(tri.iEEE32At(2), Float.MAX_VALUE, DIFF);
		
		try {
			tri.storeIEEE32(-1, 1.1f);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			tri.storeIEEE32(3, 1.1f);
			fail("3");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		
		try {
			empty.storeIEEE32(0, 1.1f);
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testStoreFloat() {
		IEEE32Array empty = IEEE32Array.make(0);
		IEEE32Array tri = IEEE32Array.make(3);

		tri.storeFloat(0, Float.MIN_VALUE);
		assertTrue(tri.iEEE32At(0) == Float.MIN_VALUE);
		tri.storeFloat(1, 1.1f);
		assertEquals(tri.iEEE32At(1), 1.1f, DIFF);
		tri.storeFloat(2, Float.MAX_VALUE);
		assertTrue(tri.iEEE32At(2) == Float.MAX_VALUE);

		tri.storeFloat(0, Double.MIN_VALUE);
		assertFalse(tri.iEEE32At(0) == Double.MIN_VALUE);
		tri.storeFloat(1, 1.1d);
		assertTrue(tri.iEEE32At(1) == 1.1f);
		assertFalse(tri.iEEE32At(1) == 1.1);
		tri.storeFloat(2, Double.MAX_VALUE);
		assertFalse(tri.iEEE32At(2) == Double.MAX_VALUE);
		
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
		IEEE32Array empty = IEEE32Array.make(0);
		IEEE32Array tri = IEEE32Array.make(3);

		tri.storeValue(0, PrimIEEE32.make(Float.MIN_VALUE));
		assertTrue(tri.iEEE32At(0) == Float.MIN_VALUE);
		tri.storeValue(1, PrimIEEE32.make(1.1f));
		assertEquals(tri.iEEE32At(1), 1.1f, DIFF);
		tri.storeValue(2, PrimIEEE32.make(Float.MAX_VALUE));
		assertTrue(tri.iEEE32At(2) == Float.MAX_VALUE);

		tri.storeValue(0, PrimIEEE64.make(Double.MIN_VALUE));
		assertFalse(tri.iEEE32At(0) == Double.MIN_VALUE);
		tri.storeValue(0, PrimIEEE64.make(1.1));
		assertTrue(tri.iEEE32At(0) == 1.1f);
		assertFalse(tri.iEEE32At(0) == 1.1);
		tri.storeValue(2, PrimIEEE64.make(Double.MAX_VALUE));
		assertFalse(tri.iEEE32At(2) == Double.MAX_VALUE);
		
		try {
			tri.storeValue(-1, PrimIEEE32.make(1.1f));
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			tri.storeValue(3, PrimIEEE32.make(1.1f));
			fail("3");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		
		try {
			empty.storeValue(0, PrimIEEE32.make(1.1f));
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
		IEEE32Array array = IEEE32Array.make(0);
		array.storeAll(PrimIEEE32.make(1.1f));
		AssertArrays.assertEquals(IEEE32Array.make(0), array, DIFF);
		
		array = IEEE32Array.make(1);
		array.storeAll(PrimIEEE32.make(1.1f));
		AssertArrays.assertEquals(IEEE32Array.make(new float[] {1.1f}), array, DIFF);
		
		array = IEEE32Array.make(3);
		array.storeAll(PrimIEEE32.make(2.2f));
		AssertArrays.assertEquals(IEEE32Array.make(new float[] {2.2f, 2.2f, 2.2f}), array, DIFF);
		
		array = AssertArrays.makeIEEE32Array12345();
		array.storeAll(PrimIEEE32.make(9.9f), 2, 1);
		AssertArrays.assertEquals(IEEE32Array.make(new float[] {1.1f, 9.9f, 9.9f, 4.4f, 5.5f}), array, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		array.storeAll(null, 2, 1);
		AssertArrays.assertEquals(IEEE32Array.make(new float[] {1.1f, 0.0f, 0.0f, 4.4f, 5.5f}), array, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		array.storeAll(PrimIEEE32.make(9.9f), -1, 1);
		AssertArrays.assertEquals(IEEE32Array.make(new float[] {1.1f, 9.9f, 9.9f, 9.9f, 9.9f}), array, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		array.storeAll(PrimIEEE32.make(9.9f), 2);
		AssertArrays.assertEquals(IEEE32Array.make(new float[] {9.9f, 9.9f, 3.3f, 4.4f, 5.5f}), array, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		array.storeAll(PrimIEEE32.make(9.9f), 0, 1);
		AssertArrays.assertEquals(IEEE32Array.make(new float[] {1.1f, 2.2f, 3.3f, 4.4f, 5.5f}), array, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		array.storeAll(PrimIEEE64.make(9.9), 2, 1);
		AssertArrays.assertEquals(IEEE32Array.make(new float[] {1.1f, 9.9f, 9.9f, 4.4f, 5.5f}), array, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		try {
			array.storeAll(PrimIEEE32.make(9.9f), 6);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = AssertArrays.makeIEEE32Array12345();
		try {
			array.storeAll(PrimIEEE32.make(9.9f), 4, 2);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// Store incompatible type
		array = AssertArrays.makeIEEE32Array12345();
		try {
			array.storeAll(IntegerPos.make(9), 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}
	
	public void testStoreMany() {
		// empty
		IEEE32Array array = AssertArrays.makeIEEE32ArrayEmpty();
		array.storeMany(0, AssertArrays.makeIEEE32ArrayEmpty());
		AssertArrays.assertEquals(AssertArrays.makeIEEE32ArrayEmpty(), array, DIFF);

		// simple
		array = AssertArrays.makeIEEE32Array12345();
		array.storeMany(0, AssertArrays.makeIEEE32Array12321());
		AssertArrays.assertEquals(AssertArrays.makeIEEE32Array12321(), array, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		array.storeMany(0, AssertArrays.makeIEEE32Array12321());
		AssertArrays.assertEquals(AssertArrays.makeIEEE32Array12321(), array, DIFF);

		array = AssertArrays.makeIEEE32Array12321();
		array.storeMany(1, IEEE32Array.make(new float[]{8.8f, 7.7f, 6.6f, 5.5f, 4.4f, 3,3f}), 2, 3);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{1.1f, 5.5f, 4.4f, 2.2f, 1.1f}), array, DIFF);

		// copy different types of array
		array = AssertArrays.makeIEEE32Array12321();
		array.storeMany(1, IEEE64Array.make(new double[]{8.8, 7.7}));
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{1.1f, 8.8f, 7.7f, 2.2f, 1.1f}), array, DIFF);

		array = AssertArrays.makeIEEE32Array12321();
		try {
			array.storeMany(1, Int32Array.make(new int[]{8, 7}));
			fail();
		} catch (ClassCastException e) {
			// expected
		}

		// attempt to copy beyond this extent
		array = AssertArrays.makeIEEE32Array12345();
		try {
			array.storeMany(1, AssertArrays.makeIEEE32Array12321());
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}

		// insufficient source elements
		array = AssertArrays.makeIEEE32Array12345();
		try {
			array.storeMany(0, AssertArrays.makeIEEE32Array12321(), 2, 4);
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}
	}
	
	public void testCopyToBuffer() {
		IEEE32Array array = AssertArrays.makeIEEE32Array12345();
		float[] out = new float[3];
		array.copyToBuffer(out, 3, 1);
		assertTrue(Arrays.equals(out, new float[] {2.2f, 3.3f, 4.4f}));		

		array = AssertArrays.makeIEEE32Array12345();
		out = new float[1];
		array.copyToBuffer(out, -1, 0);
		assertTrue(Arrays.equals(out, new float[] {1.1f}));		

		array = AssertArrays.makeIEEE32Array12345();
		out = new float[1];
		array.copyToBuffer(out, -1, 4);
		assertTrue(Arrays.equals(out, new float[] {5.5f}));		

		array = AssertArrays.makeIEEE32Array12345();
		out = new float[3];
		array.copyToBuffer(out, 3, 0);
		assertTrue(Arrays.equals(out, new float[] {1.1f, 2.2f, 3.3f}));		

		array = AssertArrays.makeIEEE32Array12345();
		out = new float[3];
		array.copyToBuffer(out, -1, 2);
		assertTrue(Arrays.equals(out, new float[] {3.3f, 4.4f, 5.5f}));		

		array = AssertArrays.makeIEEE32Array12345();
		out = new float[3];
		array.copyToBuffer(out, -1, 3);
		assertTrue(Arrays.equals(out, new float[] {4.4f, 5.5f, 0.0f}));		

		array = AssertArrays.makeIEEE32Array12345();
		out = new float[0];
		array.copyToBuffer(out, -1, 3);
		assertTrue(Arrays.equals(out, new float[] {}));		

		array = AssertArrays.makeIEEE32Array12345();
		out = new float[3];
		try {
			array.copyToBuffer(out, -1, -1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = AssertArrays.makeIEEE32Array12345();
		out = new float[3];
		try {
			array.copyToBuffer(out, 1, 5);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		
	}
	
	public void testIsEqual() {
		float[] floatE = new float[] {};
		float[] float1 = new float[] {1.1f};
		float[] float12 = new float[] {1.1f, 2.2f};
		float[] float11 = new float[] {1.1f, 1.1f};
		
		assertTrue(IEEE32Array.make(floatE).isEqual(IEEE32Array.make(floatE)));
		assertTrue(IEEE32Array.make(float1).isEqual(IEEE32Array.make(float1)));
		assertTrue(IEEE32Array.make(float12).isEqual(IEEE32Array.make(float12)));
		assertFalse(IEEE32Array.make(float11).isEqual(IEEE32Array.make(float12)));
		assertFalse(IEEE32Array.make(floatE).isEqual(IEEE32Array.make(float12)));
		assertFalse(IEEE32Array.make(float1).isEqual(PrimIEEE32.make(1.1f)));
		assertFalse(IEEE32Array.make(float12).isEqual(IEEE32Array.make(floatE)));
	}
	
	public void testIndexOf() {		
		int index = AssertArrays.makeIEEE32ArrayEmpty().indexOf(PrimIEEE32.make(1.1f), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeIEEE32Array1().indexOf(PrimIEEE32.make(1.1f), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeIEEE32Array1().indexOf(PrimIEEE32.make(1.1f), 0, 0);
		assertEquals(-1, index);
		
		index = AssertArrays.makeIEEE32Array12345().indexOf(PrimIEEE32.make(1.1f), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeIEEE32Array12345().indexOf(PrimIEEE32.make(1.1f), 0, 2);
		assertEquals(-1, index);

		index = AssertArrays.makeIEEE32Array12345().indexOf(PrimIEEE32.make(1.1f), 1, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeIEEE32Array12345().indexOf(PrimIEEE32.make(5.5f), 0, 1);
		assertEquals(4, index);
		
		index = AssertArrays.makeIEEE32Array12321().indexOf(PrimIEEE32.make(2.2f), 0, 1);
		assertEquals(1, index);

		index = AssertArrays.makeIEEE32Array12321().indexOf(PrimIEEE32.make(2.2f), 0, 2);
		assertEquals(3, index);

		index = AssertArrays.makeIEEE32Array12321().indexOf(PrimIEEE32.make(1.1f), -1, -1);
		assertEquals(4, index);

		index = AssertArrays.makeIEEE32Array12321().indexOf(PrimIEEE32.make(2.2f), -1, -1);
		assertEquals(3, index);

		index = AssertArrays.makeIEEE32Array12321().indexOf(PrimIEEE32.make(2.2f), -1, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeIEEE32Array12321().indexOf(PrimIEEE32.make(2.2f), -1, -2);
		assertEquals(1, index);

		index = AssertArrays.makeIEEE32Array12321().indexOf(PrimIEEE32.make(2.2f), -1, -3);
		assertEquals(-1, index);

		index = AssertArrays.makeIEEE32Array12321().indexOf(PrimIEEE32.make(2.2f), -3, -1);
		assertEquals(1, index);

		index = AssertArrays.makeIEEE32Array12321().indexOf(PrimIEEE32.make(2.2f), -1, 1);
		assertEquals(-1, index);

		try {
			AssertArrays.makeIEEE32Array12321().indexOf(PrimIEEE32.make(2.2f), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			AssertArrays.makeIEEE32Array12321().indexOf(IntegerPos.make(2), -3, 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}
	
	public void testIndexPast() {		
		int index = AssertArrays.makeIEEE32ArrayEmpty().indexPast(PrimIEEE32.make(1.1f), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeIEEE32Array1().indexPast(PrimIEEE32.make(1.1f), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeIEEE32Array1().indexPast(PrimIEEE32.make(1.1f), 0, 0);
		assertEquals(-1, index);
		
		index = AssertArrays.makeIEEE32Array12345().indexPast(PrimIEEE32.make(1.1f), 0, 1);
		assertEquals(1, index);

		index = AssertArrays.makeIEEE32Array12345().indexPast(PrimIEEE32.make(1.1f), 0, 2);
		assertEquals(2, index);

		index = AssertArrays.makeIEEE32Array12345().indexPast(PrimIEEE32.make(1.1f), 1, 1);
		assertEquals(1, index);

		index = AssertArrays.makeIEEE32Array12345().indexPast(PrimIEEE32.make(5.5f), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeIEEE32Array12345().indexPast(PrimIEEE32.make(5.5f), 3, 1);
		assertEquals(3, index);

		index = AssertArrays.makeIEEE32Array12345().indexPast(PrimIEEE32.make(5.5f), 4, 1);
		assertEquals(-1, index);
		
		index = AssertArrays.makeIEEE32Array12321().indexPast(PrimIEEE32.make(2.2f), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeIEEE32Array12321().indexPast(PrimIEEE32.make(2.2f), 0, 2);
		assertEquals(2, index);

		index = AssertArrays.makeIEEE32Array12321().indexPast(PrimIEEE32.make(1.1f), -1, -1);
		assertEquals(3, index);

		index = AssertArrays.makeIEEE32Array12321().indexPast(PrimIEEE32.make(2.2f), -1, -1);
		assertEquals(4, index);

		index = AssertArrays.makeIEEE32Array12321().indexPast(PrimIEEE32.make(2.2f), -1, 1);
		assertEquals(4, index);

		index = AssertArrays.makeIEEE32Array12321().indexPast(PrimIEEE32.make(2.2f), -1, 2);
		assertEquals(-1, index);

		index = AssertArrays.makeIEEE32Array12321().indexPast(PrimIEEE32.make(2.2f), -1, -2);
		assertEquals(2, index);

		index = AssertArrays.makeIEEE32Array12321().indexPast(PrimIEEE32.make(2.2f), -1, -3);
		assertEquals(0, index);

		index = AssertArrays.makeIEEE32Array12321().indexPast(PrimIEEE32.make(2.2f), -3, -1);
		assertEquals(2, index);

		index = AssertArrays.makeIEEE32Array12321().indexPast(PrimIEEE32.make(1.1f), -1, 1);
		assertEquals(-1, index);

		try {
			AssertArrays.makeIEEE32Array12321().indexPast(PrimIEEE32.make(2.2f), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}
	
	public void testIndexOfElements() {
		// empty
		IEEE32Array array = AssertArrays.makeIEEE32ArrayEmpty();
		IEEE32Array search = AssertArrays.makeIEEE32Array1();
		assertEquals(-1, array.indexOfElements(search));

		array = AssertArrays.makeIEEE32ArrayEmpty();
		search = AssertArrays.makeIEEE32ArrayEmpty();
		assertEquals(-1, array.indexOfElements(search));

// TODO skip zero length other?
//		array = makeIEEE32Array12345();
//		search = makeIEEE32ArrayEmpty();
//		assertEquals(-1, array.indexOfElements(search));

		// forward search
		array = IEEE32Array.make(new float[]{1.1f, 2.2f, 3.3f, 1.1f, 2.2f});
		search = IEEE32Array.make(new float[]{1.1f, 2.2f});
		assertEquals(0, array.indexOfElements(search));
		assertEquals(3, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		array = AssertArrays.makeIEEE32Array12321();
		search = IEEE32Array.make(new float[]{4.4f, 9.9f, 2.2f, 8.8f});
		assertEquals(1, array.indexOfElements(search, 1, 2, 0, 1));
		assertEquals(3, array.indexOfElements(search, 1, 2, 0, 2));
		assertEquals(-1, array.indexOfElements(search, 1, 2, 0, 3));
		
		// reverse search		
		array = IEEE32Array.make(new float[]{1.1f, 2.2f, 3.3f, 1.1f, 2.2f});
		search = IEEE32Array.make(new float[]{1.1f, 2.2f});
		assertEquals(3, array.indexOfElements(search, -1, 0, -2, -1));
		assertEquals(0, array.indexOfElements(search, -1, 0, -2, -2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, -2, -3));

		// overlapping search
		// TODO should this succeed?
		array = IEEE32Array.make(new float[]{1.1f, 1.1f, 1.1f});
		search = IEEE32Array.make(new float[]{1.1f, 1.1f});
		assertEquals(0, array.indexOfElements(search));
		assertEquals(1, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		// nth == 0 immediate fail
		array = AssertArrays.makeIEEE32Array12321();
		search = IEEE32Array.make(new float[]{1.1f});
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 0));

		// overflowing otherCount
		array = AssertArrays.makeIEEE32Array12321();
		search = IEEE32Array.make(new float[]{1.1f, 2.2f});
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
		array = AssertArrays.makeIEEE32Array12321();
		search = IEEE32Array.make(new float[]{1.1f, 2.2f});
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
		IEEE32Array array = AssertArrays.makeIEEE32ArrayEmpty();
		array.addElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
		AssertArrays.assertEquals(AssertArrays.makeIEEE32ArrayEmpty(), array, DIFF);

		array = AssertArrays.makeIEEE32Array1();
		array.addElements(0, IEEE32Array.make(new float[]{9f}), -1, 0);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{10.1f}), array, DIFF);

		array = IEEE32Array.make(5);
		array.addElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
		AssertArrays.assertEquals(AssertArrays.makeIEEE32Array12321(), array, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		array.addElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{2.2f, 4.4f, 6.6f, 6.6f, 6.6f}), array, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		array.addElements(2, AssertArrays.makeIEEE32Array12321(), -1, 0);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{1.1f, 2.2f, 4.4f, 6.6f, 8.8f}), array, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		array.addElements(2, AssertArrays.makeIEEE32Array12321(), -1);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{1.1f, 2.2f, 4.4f, 6.6f, 8.8f}), array, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		array.addElements(2, AssertArrays.makeIEEE32Array12321());
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{1.1f, 2.2f, 4.4f, 6.6f, 8.8f}), array, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		array.addElements(2, AssertArrays.makeIEEE32Array12321(), 2, 1);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{1.1f, 2.2f, 5.5f, 7.7f, 5.5f}), array, DIFF);

		// compatible array types
		array = AssertArrays.makeIEEE32Array12345();
		array.addElements(0, AssertArrays.makeIEEE64Array12321(), -1, 0);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{2.2f, 4.4f, 6.6f, 6.6f, 6.6f}), array, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		try {
			array.addElements(2, AssertArrays.makeIEEE32Array12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// incompatible array types
		array = AssertArrays.makeIEEE32Array12345();
		try {
			array.addElements(0, AssertArrays.makeInt32Array12321(), -1, 0);
			fail("incompatible");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testSubtractElements() {
		IEEE32Array array = AssertArrays.makeIEEE32ArrayEmpty();
		array.subtractElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
		AssertArrays.assertEquals(AssertArrays.makeIEEE32ArrayEmpty(), array, DIFF);

		array = AssertArrays.makeIEEE32Array1();
		array.subtractElements(0, IEEE32Array.make(new float[]{9f}), -1, 0);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{-7.9f}), array, DIFF);

		array = IEEE32Array.make(5);
		array.subtractElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{-1.1f, -2.2f, -3.3f, -2.2f, -1.1f}), array, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		array.subtractElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{0.0f, 0.0f, 0.0f, 2.2f, 4.4f}), array, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		array.subtractElements(2, AssertArrays.makeIEEE32Array12321(), -1, 0);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{1.1f, 2.2f, 2.2f, 2.2f, 2.2f}), array, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		array.subtractElements(2, AssertArrays.makeIEEE32Array12321(), -1);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{1.1f, 2.2f, 2.2f, 2.2f, 2.2f}), array, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		array.subtractElements(2, AssertArrays.makeIEEE32Array12321());
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{1.1f, 2.2f, 2.2f, 2.2f, 2.2f}), array, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		array.subtractElements(2, AssertArrays.makeIEEE32Array12321(), 2, 1);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{1.1f, 2.2f, 1.1f, 1.1f, 5.5f}), array, DIFF);

		// compatible array types
		array = AssertArrays.makeIEEE32Array12345();
		array.subtractElements(0, AssertArrays.makeIEEE64Array12321(), -1, 0);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{0.0f, 0.0f, 0.0f, 2.2f, 4.4f}), array, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		try {
			array.subtractElements(2, AssertArrays.makeIEEE32Array12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// incompatible array types
		array = AssertArrays.makeIEEE32Array12345();
		try {
			array.subtractElements(0, AssertArrays.makeInt32Array12321(), -1, 0);
			fail("incompatible");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testCompare() {
		// same
		IEEE32Array array1 = AssertArrays.makeIEEE32ArrayEmpty();
		IEEE32Array array2 = AssertArrays.makeIEEE32ArrayEmpty();
		assertEquals(0, array1.compare(array2));

		array1 = AssertArrays.makeIEEE32Array12345();
		array2 = AssertArrays.makeIEEE32Array12345();
		assertEquals(0, array1.compare(array2));
		
		array1 = AssertArrays.makeIEEE32Array12321();
		array2 = AssertArrays.makeIEEE32Array12321();
		assertEquals(0, array1.compare(array2));

		// different
		array1 = IEEE32Array.make(new float[] {0.1f});
		array2 = IEEE32Array.make(new float[] {0.2f});
		assertEquals(-1, array1.compare(array2));
		
		array1 = IEEE32Array.make(new float[] {0.2f});
		array2 = IEEE32Array.make(new float[] {0.1f});
		assertEquals(1, array1.compare(array2));
		
		array1 = AssertArrays.makeIEEE32Array12321();
		array2 = AssertArrays.makeIEEE32Array12345();
		assertEquals(-1, array1.compare(array2));

		array1 = AssertArrays.makeIEEE32Array12345();
		array2 = AssertArrays.makeIEEE32Array12321();
		assertEquals(1, array1.compare(array2));
		
		// auto-filling with 0
		array1 = AssertArrays.makeIEEE32ArrayEmpty();
		array2 = AssertArrays.makeIEEE32Array1();
		assertEquals(-1, array1.compare(array2));

		array1 = AssertArrays.makeIEEE32Array1();
		array2 = AssertArrays.makeIEEE32ArrayEmpty();
		assertEquals(1, array1.compare(array2));

		array1 = IEEE32Array.make(new float[]{0.0f, 0.0f});
		array2 = IEEE32Array.make(new float[]{0.0f});
		assertEquals(0, array1.compare(array2));

		array1 = IEEE32Array.make(new float[]{1.0f, -1.0f});
		array2 = IEEE32Array.make(new float[]{1.0f});
		assertEquals(-1, array1.compare(array2));

		// compare sub-regions		
		array1 = AssertArrays.makeIEEE32Array12321();
		array2 = AssertArrays.makeIEEE32Array12345();
		assertEquals(1, array1.compare(array2, 2, 2, 1));

		array1 = AssertArrays.makeIEEE32Array12321();
		array2 = AssertArrays.makeIEEE32Array12345();
		assertEquals(1, array1.compare(array2, 2, 2));

		array1 = AssertArrays.makeIEEE32Array12321();
		array2 = AssertArrays.makeIEEE32Array12345();
		assertEquals(0, array1.compare(array2, 1, 4));

		// trim down count
		array1 = AssertArrays.makeIEEE32Array12321();
		array2 = AssertArrays.makeIEEE32Array12345();
		assertEquals(-1, array1.compare(array2, 10));
		
		// compare near minimum held value
		array1 = IEEE32Array.make(new float[] { Float.MIN_VALUE});
		array2 = IEEE32Array.make(new float[] { Float.MAX_VALUE});
		assertEquals(-1, array1.compare(array2));

		// compare different type (watch out for minor changes when converting from float->double)
		array1 = IEEE32Array.make(new float[] {1.0f});
		IEEE64Array arrayIEEE64 = IEEE64Array.make(new double[] {0.1});
		assertEquals(1, array1.compare(arrayIEEE64));

		array1 = IEEE32Array.make(new float[] {1.0f});
		arrayIEEE64 = IEEE64Array.make(new double[] {1.0});
		assertEquals(0, array1.compare(arrayIEEE64));

		// compare incompatible type
		array1 = AssertArrays.makeIEEE32Array12345();
		Int32Array arrayInt32 = AssertArrays.makeInt32Array12321();
		try {
			array1.compare(arrayInt32);
			fail("incompatible");				
		} catch (UnsupportedOperationException e) {
			// expected
		}		
	}
	
	public void testCopyGrow() {
		IEEE32Array array = AssertArrays.makeIEEE32ArrayEmpty();
		IEEE32Array copy = (IEEE32Array) array.copyGrow(0);
		AssertArrays.assertEquals(AssertArrays.makeIEEE32ArrayEmpty(), copy, DIFF);
		assertNotSame(array, copy);
		
		array = AssertArrays.makeIEEE32Array12345();
		copy = (IEEE32Array) array.copyGrow(0);
		AssertArrays.assertEquals(AssertArrays.makeIEEE32Array12345(), copy, DIFF);
		assertNotSame(array, copy);

		array = AssertArrays.makeIEEE32Array12345();
		copy = (IEEE32Array) array.copyGrow(3);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 0.0f, 0.0f, 0.0f}), copy, DIFF);
		assertNotSame(array, copy);
	}

	public void testCopy() {
		// full copy
		IEEE32Array array = AssertArrays.makeIEEE32ArrayEmpty();
		IEEE32Array copy = (IEEE32Array) array.copy();
		AssertArrays.assertEquals(AssertArrays.makeIEEE32ArrayEmpty(), copy, DIFF);
		assertNotSame(array, copy);
		
		array = AssertArrays.makeIEEE32Array12345();
		copy = (IEEE32Array) array.copy();
		AssertArrays.assertEquals(AssertArrays.makeIEEE32Array12345(), copy, DIFF);
		AssertArrays.assertEquals(AssertArrays.makeIEEE32Array12345(), array, DIFF);
		assertNotSame(array, copy);
		
		// partial copy
		array = AssertArrays.makeIEEE32Array12345();
		copy = (IEEE32Array) array.copy(2, 1);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{2.2f, 3.3f}), copy, DIFF);
		assertNotSame(array, copy);

		array = AssertArrays.makeIEEE32Array12345();
		copy = (IEEE32Array) array.copy(2, 0);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{1.1f, 2.2f}), copy, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		copy = (IEEE32Array) array.copy(2, 3);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{4.4f, 5.5f}), copy, DIFF);

		// partial copy with too large count
		array = AssertArrays.makeIEEE32Array12345();
		try {
			array.copy(5, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// partial copy with trailing space
		array = AssertArrays.makeIEEE32Array12345();
		copy = (IEEE32Array) array.copy(2, 1);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{2.2f, 3.3f}), copy, DIFF);

		// partial copy with leading space
		array = AssertArrays.makeIEEE32Array12345();
		copy = (IEEE32Array) array.copy(2, 1, 1);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{0.0f, 2.2f, 3.3f}), copy, DIFF);

		// partial copy with leading space
		array = AssertArrays.makeIEEE32Array12345();
		copy = (IEEE32Array) array.copy(2, 1, 0, 1);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{2.2f, 3.3f, 0.0f}), copy, DIFF);

		// partial copy with leading and trailing space
		array = AssertArrays.makeIEEE32Array12345();
		copy = (IEEE32Array) array.copy(2, 1, 2, 1);
		AssertArrays.assertEquals(IEEE32Array.make(new float[]{0.0f, 0.0f, 2.2f, 3.3f, 0.0f}), copy, DIFF);
	}
	
	public void testToString() {
		assertEquals("[empty]", AssertArrays.makeIEEE32ArrayEmpty().toString());
		assertEquals("[1.1 2.2 3.3 4.4 5.5]", AssertArrays.makeIEEE32Array12345().toString());
	}
	
	public void testSpec() {
		PrimFloatSpec spec = (PrimFloatSpec)AssertArrays.makeIEEE32ArrayEmpty().spec();
		assertEquals(spec.bitCount(), AssertArrays.makeIEEE32ArrayEmpty().bitCount());		
	}

	public void testZeroElements() {
		IEEE32Array array = IEEE32Array.make(0);
		array.zeroElements(0, -1);
		AssertArrays.assertEquals(IEEE32Array.make(0), array, DIFF);
		
		// zero all elements
		array = IEEE32Array.make(1);
		array.storeFloat(0, 1.1);
		array.zeroElements(0, -1);
		AssertArrays.assertEquals(IEEE32Array.make(new float[] {0f}), array, DIFF);
		
		array = AssertArrays.makeIEEE32Array12345();
		array.zeroElements(0, -1);
		AssertArrays.assertEquals(IEEE32Array.make(new float[] {0f, 0f, 0f, 0f, 0f}), array, DIFF);

		// zero subset of elements
		array = AssertArrays.makeIEEE32Array12345();
		array.zeroElements(1, -1);
		AssertArrays.assertEquals(IEEE32Array.make(new float[] {1.1f, 0f, 0f, 0f, 0f}), array, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		array.zeroElements(1, 2);
		AssertArrays.assertEquals(IEEE32Array.make(new float[] {1.1f, 0f, 0f, 4.4f, 5.5f}), array, DIFF);

		array = AssertArrays.makeIEEE32Array12345();
		array.zeroElements(4, 1);
		AssertArrays.assertEquals(IEEE32Array.make(new float[] {1.1f, 2.2f, 3.3f, 4.4f, 0.0f}), array, DIFF);

		// silently truncate from
		array = AssertArrays.makeIEEE32Array12345();
		//TODO should this actually throw an exception?
		array.zeroElements(5, -1);
		AssertArrays.assertEquals(AssertArrays.makeIEEE32Array12345(), array, DIFF);

		// extend count outside range
		array = AssertArrays.makeIEEE32Array12345();
		try {
			array.zeroElements(4, 2);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testElementsHash() {
		// complete hash
		int hash = AssertArrays.makeIEEE32ArrayEmpty().elementsHash();
		assertFalse(0 == hash);
		
		assertFalse(AssertArrays.makeIEEE32Array12345().elementsHash() == AssertArrays.makeIEEE32Array12321().elementsHash());

		// partial hash
		IEEE32Array array = AssertArrays.makeIEEE32Array12345();
		assertFalse(0 == array.elementsHash());
		assertFalse(array.elementsHash(0) == array.elementsHash(1));
		assertFalse(array.elementsHash(1, 1) == array.elementsHash(1));
		
		// out of range
		try {
			AssertArrays.makeIEEE32Array12345().elementsHash(5, 1);
			fail("5,1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testContentsHash() {
		int hash = AssertArrays.makeIEEE32ArrayEmpty().contentsHash();
		assertFalse(0 == hash);
		
		IEEE32Array array = AssertArrays.makeIEEE32Array12345();
		assertFalse(0 == array.contentsHash());
		
		assertFalse(AssertArrays.makeIEEE32Array12345().contentsHash() == AssertArrays.makeIEEE32Array12321().contentsHash());
	}
}

