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
import info.dgjones.abora.gold.collection.basic.PrimIntegerArray;
import info.dgjones.abora.gold.x.PrimIEEE32;
import info.dgjones.abora.gold.x.PrimIEEE64;
import info.dgjones.abora.gold.x.PrimIntValue;
import info.dgjones.abora.gold.x.PrimIntegerSpec;

public class Int32ArrayTest extends AboraGoldTestCase {

	public Int32ArrayTest(String arg0) {
		super(arg0);
	}

	public void testMakeCount() {
		Int32Array array = Int32Array.make(0);
		assertEquals(0, array.count());

		array = Int32Array.make(1);
		assertEquals(1, array.count());
		assertEquals(0, array.int32At(0));
		
		try {
			Int32Array.make(-1);
			fail("-1");
		} catch (NegativeArraySizeException e) {
			//expected
		}
	}

	public void testMake() {
		Int32Array array = Int32Array.make(AssertArrays.makeInt32ArrayEmpty());
		assertEquals(0, array.count());

		array = Int32Array.make(AssertArrays.makeInt32Array12345());
		assertEquals(5, array.count());
		AssertArrays.assertEquals(AssertArrays.makeInt32Array12345(), array);

		array = Int32Array.make(7, AssertArrays.makeInt32Array12345());
		assertEquals(7, array.count());
		AssertArrays.assertEquals(Int32Array.make(new int[]{1,2,3,4,5,0,0}), array);

		array = Int32Array.make(7, AssertArrays.makeInt32Array12345(), 1, 2, 5);
		assertEquals(7, array.count());
		AssertArrays.assertEquals(Int32Array.make(new int[]{0,0,0,0,0,2,3}), array);		

		try {
			Int32Array.make(4, AssertArrays.makeInt32Array12345());
			fail("4");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}
	
	public void testMakeBuffer() {
		Int32Array array = Int32Array.make(new int[] {});
		assertEquals(0, array.count());

		array = Int32Array.make(new int[] {1, 2});
		assertEquals(2, array.count());
		assertEquals(1, array.int32At(0));
		assertEquals(2, array.int32At(1));		
	}

	public void testInt32At() {
		Int32Array a = Int32Array.make(new int[] { 0, 1, -2, 3 });

		assertEquals(0, a.int32At(0));
		assertEquals(1, a.int32At(1));
		assertEquals(-2, a.int32At(2));
		assertEquals(3, a.int32At(3));

		try {
			a.int32At(-1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			a.int32At(4);
			fail("4");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testInt32AtEmpty() {
		Int32Array a = Int32Array.make(0);
		try {
			a.int32At(0);
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// OutOfBounds
		}
	}

	public void testIntegerAt() {
		Int32Array a = Int32Array.make(new int[] { 0, 1, -2, 3 });

		assertEquals(0, a.integerAt(0));
		assertEquals(1, a.integerAt(1));
		assertEquals(-2, a.integerAt(2));
		assertEquals(3, a.integerAt(3));

		try {
			a.integerAt(-1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			a.integerAt(4);
			fail("4");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testFetchValue() {
		Int32Array a = Int32Array.make(new int[] { 0, 1, -2, 3 });

		assertEquals(0, ((PrimIntValue) a.fetchValue(0)).asInt32());
		assertEquals(1, ((PrimIntValue) a.fetchValue(1)).asInt32());
		assertEquals(-2, ((PrimIntValue) a.fetchValue(2)).asInt32());
		assertEquals(3, ((PrimIntValue) a.fetchValue(3)).asInt32());

		try {
			a.fetchValue(-1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			a.fetchValue(4);
			fail("4");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testCount() {
		assertEquals(0, AssertArrays.makeInt32ArrayEmpty().count());
		assertEquals(1, Int32Array.make(new int[] { 0 }).count());
		assertEquals(2, Int32Array.make(new int[] { 0, 1 }).count());
	}

	public void testStoreInt32() {
		Int32Array empty = Int32Array.make(0);
		Int32Array tri = Int32Array.make(3);

		tri.storeInt32(0, Integer.MIN_VALUE);
		assertEquals(tri.int32At(0), Integer.MIN_VALUE);
		tri.storeInt32(1, (int) 1);
		assertEquals(tri.int32At(1), 1);
		tri.storeInt32(2, Integer.MAX_VALUE);
		assertEquals(tri.int32At(2), Integer.MAX_VALUE);

		try {
			tri.storeInt32(-1, (int) 1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			tri.storeInt32(3, (int) 1);
			fail("3");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			empty.storeInt32(0, (int) 1);
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testStoreInteger() {
		Int32Array empty = Int32Array.make(0);
		Int32Array tri = Int32Array.make(3);

		// Store integer values within spec
		tri.storeInteger(0, Integer.MIN_VALUE);
		assertTrue(tri.int32At(0) == Integer.MIN_VALUE);
		tri.storeInteger(1, 1);
		assertEquals(tri.int32At(1), 1);
		tri.storeInteger(2, Integer.MAX_VALUE);
		assertTrue(tri.int32At(2) == Integer.MAX_VALUE);

//		// Store integer values outside of spec
//		try {
//			tri.storeInteger(0, (long) Integer.MIN_VALUE - 1);
//			fail("MIN_VALUE - 1");
//		} catch (IllegalArgumentException e) {
//			// expected
//		}
//		try {
//			tri.storeInteger(2, (long) Integer.MAX_VALUE + 1);
//			fail("MAX_VALUE + 1");
//		} catch (IllegalArgumentException e) {
//			//expected
//		}

		// Store outside array boundary
		try {
			tri.storeInteger(-1, 1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			tri.storeInteger(3, 1);
			fail("3");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			empty.storeInteger(0, 1);
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testStoreValue() {
		Int32Array empty = Int32Array.make(0);
		Int32Array tri = Int32Array.make(3);

		// Store integer values within spec
		tri.storeValue(0, PrimIntValue.make(Integer.MIN_VALUE));
		assertTrue(tri.int32At(0) == Integer.MIN_VALUE);
		tri.storeValue(1, PrimIntValue.make(1));
		assertEquals(tri.int32At(1), 1);
		tri.storeValue(2, PrimIntValue.make(Integer.MAX_VALUE));
		assertTrue(tri.int32At(2) == Integer.MAX_VALUE);

//		// Store integer values outside of spec
//		try {
//			tri.storeValue(0, PrimIntValue.make((long) Integer.MIN_VALUE - 1));
//			fail("MIN_VALUE - 1");
//		} catch (IllegalArgumentException e) {
//			// expected
//		}
//		try {
//			tri.storeValue(2, PrimIntValue.make((long) Integer.MAX_VALUE + 1));
//			fail("MAX_VALUE + 1");
//		} catch (IllegalArgumentException e) {
//			// expected
//		}

		// Store outside array boundary
		try {
			tri.storeValue(-1, PrimIntValue.make(1));
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			tri.storeValue(3, PrimIntValue.make(1));
			fail("3");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			empty.storeValue(0, PrimIntValue.make(1));
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		
		// non-compatible store
		try {
			tri.storeValue(0, PrimIEEE64.make(1.1));
			fail("classCast");
		} catch (ClassCastException e) {
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
		Int32Array array = Int32Array.make(0);
		array.storeAll(PrimIntValue.make(1));
		AssertArrays.assertEquals(Int32Array.make(0), array);

		array = Int32Array.make(1);
		array.storeAll(PrimIntValue.make(1));
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1 }), array);

		array = Int32Array.make(3);
		array.storeAll(PrimIntValue.make(2));
		AssertArrays.assertEquals(Int32Array.make(new int[] { 2, 2, 2 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.storeAll(PrimIntValue.make(9), 2, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 9, 9, 4, 5 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.storeAll(null, 2, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 0, 0, 4, 5 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.storeAll(PrimIntValue.make(9), -1, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 9, 9, 9, 9 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.storeAll(PrimIntValue.make(9), 2);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 9, 9, 3, 4, 5 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.storeAll(PrimIntValue.make(9), 0, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 2, 3, 4, 5 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.storeAll(PrimIntValue.make(9), 2, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 9, 9, 4, 5 }), array);

		// Store outside of array bounds
		array = AssertArrays.makeInt32Array12345();
		try {
			array.storeAll(PrimIntValue.make(9), 6);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = AssertArrays.makeInt32Array12345();
		try {
			array.storeAll(PrimIntValue.make(9), 4, 2);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// Store incompatible type
		array = AssertArrays.makeInt32Array12345();
		try {
			array.storeAll(PrimIEEE32.make(9.9f), 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}

		// Store integer values outside of spec
//		array = AssertArrays.makeInt32Array12345();
//		try {
//			array.storeAll(PrimIntValue.make((long) Integer.MAX_VALUE + 1), 1);
//			fail();
//		} catch (IllegalArgumentException e) {
//			// expected
//		}
	}

	public void testStoreMany() {
		// empty
		Int32Array array = AssertArrays.makeInt32ArrayEmpty();
		array.storeMany(0, AssertArrays.makeInt32ArrayEmpty());
		AssertArrays.assertEquals(AssertArrays.makeInt32ArrayEmpty(), array);

		// simple
		array = AssertArrays.makeInt32Array12345();
		array.storeMany(0, AssertArrays.makeInt32Array12321());
		AssertArrays.assertEquals(AssertArrays.makeInt32Array12321(), array);

		array = AssertArrays.makeInt32Array12345();
		array.storeMany(0, AssertArrays.makeInt32Array12321());
		AssertArrays.assertEquals(AssertArrays.makeInt32Array12321(), array);

		array = AssertArrays.makeInt32Array12321();
		array.storeMany(1, Int32Array.make(new int[] { 8, 7, 6, 5, 4, 3, 3 }), 2, 3);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 5, 4, 2, 1 }), array);

		array = AssertArrays.makeInt32Array12321();
		array.storeMany(1, Int32Array.make(new int[] { 8, 7 }));
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 8, 7, 2, 1 }), array);

		// Store incompatible type
		array = AssertArrays.makeInt32Array12321();
		try {
			array.storeMany(1, IEEE64Array.make(new double[] { 8.8, 7.7 }));
			fail();
		} catch (ClassCastException e) {
			// expected
		}

		// attempt to copy beyond this extent
		array = AssertArrays.makeInt32Array12345();
		try {
			array.storeMany(1, AssertArrays.makeInt32Array12321());
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}

		// insufficient source elements
		array = AssertArrays.makeInt32Array12345();
		try {
			array.storeMany(0, AssertArrays.makeInt32Array12321(), 2, 4);
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}

		// Store integer values outside of spec
//		array = AssertArrays.makeInt32Array12321();
//		try {
//			array.storeMany(1, Int64Array.make(new long[] { (long)Integer.MAX_VALUE + 1 }));
//			fail("MAX_VALUE + 1");
//		} catch (IllegalArgumentException e) {
//			//expected
//		}
	}

	public void testCopyToBuffer() {
		Int32Array array = AssertArrays.makeInt32Array12345();
		int[] out = new int[3];
		array.copyToBuffer(out, 3, 1);
		assertTrue(Arrays.equals(out, new int[] { 2, 3, 4 }));

		array = AssertArrays.makeInt32Array12345();
		out = new int[1];
		array.copyToBuffer(out, -1, 0);
		assertTrue(Arrays.equals(out, new int[] { 1 }));

		array = AssertArrays.makeInt32Array12345();
		out = new int[1];
		array.copyToBuffer(out, -1, 4);
		assertTrue(Arrays.equals(out, new int[] { 5 }));

		array = AssertArrays.makeInt32Array12345();
		out = new int[3];
		array.copyToBuffer(out, 3, 0);
		assertTrue(Arrays.equals(out, new int[] { 1, 2, 3 }));

		array = AssertArrays.makeInt32Array12345();
		out = new int[3];
		array.copyToBuffer(out, -1, 2);
		assertTrue(Arrays.equals(out, new int[] { 3, 4, 5 }));

		array = AssertArrays.makeInt32Array12345();
		out = new int[3];
		array.copyToBuffer(out, -1, 3);
		assertTrue(Arrays.equals(out, new int[] { 4, 5, 0 }));

		array = AssertArrays.makeInt32Array12345();
		out = new int[0];
		array.copyToBuffer(out, -1, 3);
		assertTrue(Arrays.equals(out, new int[] {
		}));

		array = AssertArrays.makeInt32Array12345();
		out = new int[3];
		try {
			array.copyToBuffer(out, -1, -1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = AssertArrays.makeInt32Array12345();
		out = new int[3];
		try {
			array.copyToBuffer(out, 1, 5);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

	}

	public void testIsEqual() {
		int[] intsE = new int[] {};
		int[] ints1 = new int[] { 1 };
		int[] ints12 = new int[] { 1, 2 };
		int[] ints11 = new int[] { 1, 1 };

		// Equal matches
		assertTrue(Int32Array.make(intsE).isEqual(Int32Array.make(intsE)));
		assertTrue(Int32Array.make(ints1).isEqual(Int32Array.make(ints1)));
		assertTrue(Int32Array.make(ints12).isEqual(Int32Array.make(ints12)));
		assertTrue(Int32Array.make(ints1).isEqual(AssertArrays.makeInt32Array1()));
		
		// Unequal compatible matches
		assertFalse(Int32Array.make(ints11).isEqual(Int32Array.make(ints12)));
		assertFalse(Int32Array.make(intsE).isEqual(Int32Array.make(ints12)));
		assertFalse(Int32Array.make(ints12).isEqual(Int32Array.make(intsE)));

		// single value
		assertFalse(Int32Array.make(ints1).isEqual(PrimIntValue.make(1)));
		
		// incompatible array
		try {
			assertFalse(Int32Array.make(ints1).isEqual(AssertArrays.makeIEEE32Array1()));
			fail("ieee32");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testIndexOf() {
		int index = AssertArrays.makeInt32ArrayEmpty().indexOf(PrimIntValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array1().indexOf(PrimIntValue.make(1), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeInt32Array1().indexOf(PrimIntValue.make(1), 0, 0);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array12345().indexOf(PrimIntValue.make(1));
		assertEquals(0, index);

		index = AssertArrays.makeInt32Array12345().indexOf(PrimIntValue.make(1), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeInt32Array12345().indexOf(PrimIntValue.make(1), 0, 2);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array12345().indexOf(PrimIntValue.make(1), 1, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array12345().indexOf(PrimIntValue.make(5), 0, 1);
		assertEquals(4, index);

		index = AssertArrays.makeInt32Array12321().indexOf(PrimIntValue.make(2), 0, 1);
		assertEquals(1, index);

		index = AssertArrays.makeInt32Array12321().indexOf(PrimIntValue.make(2), 0, 2);
		assertEquals(3, index);

		index = AssertArrays.makeInt32Array12321().indexOf(PrimIntValue.make(1), -1, -1);
		assertEquals(4, index);

		index = AssertArrays.makeInt32Array12321().indexOf(PrimIntValue.make(2), -1, -1);
		assertEquals(3, index);

		index = AssertArrays.makeInt32Array12321().indexOf(PrimIntValue.make(2), -1, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array12321().indexOf(PrimIntValue.make(2), -1, -2);
		assertEquals(1, index);

		index = AssertArrays.makeInt32Array12321().indexOf(PrimIntValue.make(2), -1, -3);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array12321().indexOf(PrimIntValue.make(2), -3, -1);
		assertEquals(1, index);

		index = AssertArrays.makeInt32Array12321().indexOf(PrimIntValue.make(2), -1, 1);
		assertEquals(-1, index);

		try {
			AssertArrays.makeInt32Array12321().indexOf(PrimIntValue.make(2), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			AssertArrays.makeInt32Array12321().indexOf(PrimIEEE64.make(2.0f), -3, 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testIndexOfInteger() {
		int index = AssertArrays.makeInt32Array12321().indexOfInteger(1);
		assertEquals(0, index);
		index = AssertArrays.makeInt32Array12321().indexOfInteger(1, 1);
		assertEquals(4, index);
		index = AssertArrays.makeInt32Array12321().indexOfInteger(4, 1);
		assertEquals(-1, index);
	}

	public void testIndexPast() {
		int index = AssertArrays.makeInt32ArrayEmpty().indexPast(PrimIntValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array1().indexPast(PrimIntValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array1().indexPast(PrimIntValue.make(1), 0, 0);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array12345().indexPast(PrimIntValue.make(1));
		assertEquals(1, index);

		index = AssertArrays.makeInt32Array12345().indexPast(PrimIntValue.make(1), 0, 1);
		assertEquals(1, index);

		index = AssertArrays.makeInt32Array12345().indexPast(PrimIntValue.make(1), 0, 2);
		assertEquals(2, index);

		index = AssertArrays.makeInt32Array12345().indexPast(PrimIntValue.make(1), 1, 1);
		assertEquals(1, index);

		index = AssertArrays.makeInt32Array12345().indexPast(PrimIntValue.make(5), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeInt32Array12345().indexPast(PrimIntValue.make(5), 3, 1);
		assertEquals(3, index);

		index = AssertArrays.makeInt32Array12345().indexPast(PrimIntValue.make(5), 4, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array12321().indexPast(PrimIntValue.make(2), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeInt32Array12321().indexPast(PrimIntValue.make(2), 0, 2);
		assertEquals(2, index);

		index = AssertArrays.makeInt32Array12321().indexPast(PrimIntValue.make(1), -1, -1);
		assertEquals(3, index);

		index = AssertArrays.makeInt32Array12321().indexPast(PrimIntValue.make(2), -1, -1);
		assertEquals(4, index);

		index = AssertArrays.makeInt32Array12321().indexPast(PrimIntValue.make(2), -1, 1);
		assertEquals(4, index);

		index = AssertArrays.makeInt32Array12321().indexPast(PrimIntValue.make(2), -1, 2);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array12321().indexPast(PrimIntValue.make(2), -1, -2);
		assertEquals(2, index);

		index = AssertArrays.makeInt32Array12321().indexPast(PrimIntValue.make(2), -1, -3);
		assertEquals(0, index);

		index = AssertArrays.makeInt32Array12321().indexPast(PrimIntValue.make(2), -1, -4);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array12321().indexPast(PrimIntValue.make(2), -3, -1);
		assertEquals(2, index);

		index = AssertArrays.makeInt32Array12321().indexPast(PrimIntValue.make(1), -1, 1);
		assertEquals(-1, index);

		try {
			AssertArrays.makeInt32Array12321().indexPast(PrimIntValue.make(2), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			AssertArrays.makeInt32Array12321().indexPast(PrimIEEE64.make(2.0), -3, 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testIndexPastInteger() {
		int index = AssertArrays.makeInt32Array12321().indexPastInteger(1);
		assertEquals(1, index);
		index = AssertArrays.makeInt32Array12321().indexPastInteger(1, 1);
		assertEquals(1, index);
	}

	public void testIndexOfElements() {
		// empty
		Int32Array array = AssertArrays.makeInt32ArrayEmpty();
		Int32Array search = AssertArrays.makeInt32Array1();
		assertEquals(-1, array.indexOfElements(search));

		array = AssertArrays.makeInt32ArrayEmpty();
		search = AssertArrays.makeInt32ArrayEmpty();
		assertEquals(-1, array.indexOfElements(search));

		// TODO skip zero length other?
		//		array = makeInt32Array12345();
		//		search = makeInt32ArrayEmpty();
		//		assertEquals(-1, array.indexOfElements(search));

		// forward search
		array = Int32Array.make(new int[] { 1, 2, 3, 1, 2 });
		search = Int32Array.make(new int[] { 1, 2 });
		assertEquals(0, array.indexOfElements(search));
		assertEquals(3, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		array = AssertArrays.makeInt32Array12321();
		search = Int32Array.make(new int[] { 4, 9, 2, 8 });
		assertEquals(1, array.indexOfElements(search, 1, 2, 0, 1));
		assertEquals(3, array.indexOfElements(search, 1, 2, 0, 2));
		assertEquals(-1, array.indexOfElements(search, 1, 2, 0, 3));

		// forward search with compatible int array
		array = Int32Array.make(new int[] { 1, 2, 3, 1, 2 });
		Int32Array searchInt32 = Int32Array.make(new int[] { 1, 2 });
		assertEquals(0, array.indexOfElements(searchInt32));
		assertEquals(3, array.indexOfElements(searchInt32, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(searchInt32, -1, 0, 0, 3));

		// reverse search		
		array = Int32Array.make(new int[] { 1, 2, 3, 1, 2 });
		search = Int32Array.make(new int[] { 1, 2 });
		assertEquals(3, array.indexOfElements(search, -1, 0, -2, -1));
		assertEquals(0, array.indexOfElements(search, -1, 0, -2, -2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, -2, -3));

		// overlapping search
		// TODO should this succeed?
		array = Int32Array.make(new int[] { 1, 1, 1 });
		search = Int32Array.make(new int[] { 1, 1 });
		assertEquals(0, array.indexOfElements(search));
		assertEquals(1, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		// nth == 0 immediate fail
		array = AssertArrays.makeInt32Array12321();
		search = Int32Array.make(new int[] { 1 });
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 0));

		// overflowing otherCount
		array = AssertArrays.makeInt32Array12321();
		search = Int32Array.make(new int[] { 1, 2 });
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
		array = AssertArrays.makeInt32Array12321();
		search = Int32Array.make(new int[] { 1, 2 });
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

		// incompatible arrays
		array = Int32Array.make(new int[] { 1, 2, 3, 1, 2 });
		IEEE32Array searchIEEE = IEEE32Array.make(new float[] { 1.0f, 2.0f });
		try {
			assertEquals(0, array.indexOfElements(searchIEEE));
			fail("ieee");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testAddElements() {
		Int32Array array = AssertArrays.makeInt32ArrayEmpty();
		array.addElements(0, AssertArrays.makeInt32Array12321(), -1, 0);
		AssertArrays.assertEquals(AssertArrays.makeInt32ArrayEmpty(), array);

		array = AssertArrays.makeInt32Array1();
		array.addElements(0, Int32Array.make(new int[] { 9 }), -1, 0);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 10 }), array);

		array = Int32Array.make(5);
		array.addElements(0, AssertArrays.makeInt32Array12321(), -1, 0);
		AssertArrays.assertEquals(AssertArrays.makeInt32Array12321(), array);

		array = AssertArrays.makeInt32Array12345();
		array.addElements(0, AssertArrays.makeInt32Array12321(), -1, 0);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 2, 4, 6, 6, 6 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.addElements(2, AssertArrays.makeInt32Array12321(), -1, 0);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 2, 4, 6, 8 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.addElements(2, AssertArrays.makeInt32Array12321(), -1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 2, 4, 6, 8 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.addElements(2, AssertArrays.makeInt32Array12321());
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 2, 4, 6, 8 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.addElements(2, AssertArrays.makeInt32Array12321(), 2, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 2, 5, 7, 5 }), array);

		// element arithmetic overflows
		final int large = 2000000000;
		array = Int32Array.make(new int[] {large, -large, -large});
		array.addElements(0, Int32Array.make(new int[] {large, large, -large}));
		AssertArrays.assertEquals(Int32Array.make(new int[]{-294967296, 0, 294967296}), array);

		// compatible array types
		array = AssertArrays.makeInt32Array12345();
		array.addElements(0, AssertArrays.makeInt8Array12321(), -1, 0);
		AssertArrays.assertEquals(Int32Array.make(new int[]{2, 4, 6, 6, 6}), array);

		array = AssertArrays.makeInt32Array12345();
		try {
			array.addElements(2, AssertArrays.makeInt32Array12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// incompatible arrays
		array = AssertArrays.makeInt32ArrayEmpty();
		try {
			array.addElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
			fail("ieee");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testSubtractElements() {
		Int32Array array = AssertArrays.makeInt32ArrayEmpty();
		array.subtractElements(0, AssertArrays.makeInt32Array12321(), -1, 0);
		AssertArrays.assertEquals(AssertArrays.makeInt32ArrayEmpty(), array);

		array = AssertArrays.makeInt32Array1();
		array.subtractElements(0, Int32Array.make(new int[] { 9 }), -1, 0);
		AssertArrays.assertEquals(Int32Array.make(new int[] { -8 }), array);

		array = Int32Array.make(5);
		array.subtractElements(0, AssertArrays.makeInt32Array12321(), -1, 0);
		AssertArrays.assertEquals(Int32Array.make(new int[] { -1, -2, -3, -2, -1 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.subtractElements(0, AssertArrays.makeInt32Array12321(), -1, 0);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 0, 0, 0, 2, 4 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.subtractElements(2, AssertArrays.makeInt32Array12321(), -1, 0);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 2, 2, 2, 2 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.subtractElements(2, AssertArrays.makeInt32Array12321(), -1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 2, 2, 2, 2 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.subtractElements(2, AssertArrays.makeInt32Array12321());
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 2, 2, 2, 2 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.subtractElements(2, AssertArrays.makeInt32Array12321(), 2, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 2, 1, 1, 5 }), array);

		// element arithmetic overflows
		final int large = 2000000000;
		array = Int32Array.make(new int[] {large, large, -large});
		array.subtractElements(0, Int32Array.make(new int[] {-large, large, large}));
		AssertArrays.assertEquals(Int32Array.make(new int[]{-294967296, 0, 294967296}), array);

		// compatible array types
		array = AssertArrays.makeInt32Array12345();
		array.subtractElements(0, AssertArrays.makeInt8Array12321(), -1, 0);
		AssertArrays.assertEquals(Int32Array.make(new int[]{0, 0, 0, 2, 4}), array);

		// extend count beyond end of array
		array = AssertArrays.makeInt32Array12345();
		try {
			array.subtractElements(2, AssertArrays.makeInt32Array12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// incompatible arrays
		array = AssertArrays.makeInt32ArrayEmpty();
		try {
			array.subtractElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
			fail("ieee");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testCompare() {
		// same
		Int32Array array1 = AssertArrays.makeInt32ArrayEmpty();
		Int32Array array2 = AssertArrays.makeInt32ArrayEmpty();
		assertEquals(0, array1.compare(array2));

		array1 = AssertArrays.makeInt32Array12345();
		array2 = AssertArrays.makeInt32Array12345();
		assertEquals(0, array1.compare(array2));

		array1 = AssertArrays.makeInt32Array12321();
		array2 = AssertArrays.makeInt32Array12321();
		assertEquals(0, array1.compare(array2));

		// different
		array1 = AssertArrays.makeInt32Array12321();
		array2 = AssertArrays.makeInt32Array12345();
		assertEquals(-1, array1.compare(array2));

		array1 = AssertArrays.makeInt32Array12345();
		array2 = AssertArrays.makeInt32Array12321();
		assertEquals(1, array1.compare(array2));

		// auto-filling with 0
		array1 = AssertArrays.makeInt32ArrayEmpty();
		array2 = AssertArrays.makeInt32Array1();
		assertEquals(-1, array1.compare(array2));

		array1 = AssertArrays.makeInt32Array1();
		array2 = AssertArrays.makeInt32ArrayEmpty();
		assertEquals(1, array1.compare(array2));

		array1 = Int32Array.make(new int[] { 0, 0 });
		array2 = Int32Array.make(new int[] { 0 });
		assertEquals(0, array1.compare(array2));

		array1 = Int32Array.make(new int[]{1, -1});
		array2 = Int32Array.make(new int[]{1});
		assertEquals(-1, array1.compare(array2));

		// compare sub-regions		
		array1 = AssertArrays.makeInt32Array12321();
		array2 = AssertArrays.makeInt32Array12345();
		assertEquals(1, array1.compare(array2, 2, 2, 1));

		array1 = AssertArrays.makeInt32Array12321();
		array2 = AssertArrays.makeInt32Array12345();
		assertEquals(1, array1.compare(array2, 2, 2));

		array1 = AssertArrays.makeInt32Array12321();
		array2 = AssertArrays.makeInt32Array12345();
		assertEquals(0, array1.compare(array2, 1, 4));

		// trim down count
		array1 = AssertArrays.makeInt32Array12321();
		array2 = AssertArrays.makeInt32Array12345();
		assertEquals(-1, array1.compare(array2, 10));

		// compare near minimum held value
		array1 = Int32Array.make(new int[] { Integer.MIN_VALUE});
		array2 = Int32Array.make(new int[] { Integer.MAX_VALUE});
		assertEquals(-1, array1.compare(array2));

		// different array types
		array1 = AssertArrays.makeInt32Array12345();
		Int32Array array2Int32 = AssertArrays.makeInt32Array12345();
		assertEquals(0, array1.compare(array2Int32));

		// incompatible array types
		array1 = AssertArrays.makeInt32Array12345();
		IEEE32Array array2IEEE32 = AssertArrays.makeIEEE32Array12345();
		try {
			array1.compare(array2IEEE32);
			fail("ieee32");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testCopyGrow() {
		Int32Array array = AssertArrays.makeInt32ArrayEmpty();
		Int32Array copy = (Int32Array) array.copyGrow(0);
		AssertArrays.assertEquals(AssertArrays.makeInt32ArrayEmpty(), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeInt32Array12345();
		copy = (Int32Array) array.copyGrow(0);
		AssertArrays.assertEquals(AssertArrays.makeInt32Array12345(), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeInt32Array12345();
		copy = (Int32Array) array.copyGrow(3);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 2, 3, 4, 5, 0, 0, 0 }), copy);
		assertNotSame(array, copy);
	}

	public void testCopy() {
		// full copy
		Int32Array array = AssertArrays.makeInt32ArrayEmpty();
		Int32Array copy = (Int32Array) array.copy();
		AssertArrays.assertEquals(AssertArrays.makeInt32ArrayEmpty(), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeInt32Array12345();
		copy = (Int32Array) array.copy();
		AssertArrays.assertEquals(AssertArrays.makeInt32Array12345(), copy);
		AssertArrays.assertEquals(AssertArrays.makeInt32Array12345(), array);
		assertNotSame(array, copy);

		// partial copy
		array = AssertArrays.makeInt32Array12345();
		copy = (Int32Array) array.copy(2, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 2, 3 }), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeInt32Array12345();
		copy = (Int32Array) array.copy(2, 0);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 2 }), copy);

		array = AssertArrays.makeInt32Array12345();
		copy = (Int32Array) array.copy(2, 3);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 4, 5 }), copy);

		// partial copy with too large count
		array = AssertArrays.makeInt32Array12345();
		try {
			array.copy(5, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// partial copy with trailing space
		array = AssertArrays.makeInt32Array12345();
		copy = (Int32Array) array.copy(2, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 2, 3 }), copy);

		// partial copy with leading space
		array = AssertArrays.makeInt32Array12345();
		copy = (Int32Array) array.copy(2, 1, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 0, 2, 3 }), copy);

		// partial copy with leading space
		array = AssertArrays.makeInt32Array12345();
		copy = (Int32Array) array.copy(2, 1, 0, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 2, 3, 0 }), copy);

		// partial copy with leading and trailing space
		array = AssertArrays.makeInt32Array12345();
		copy = (Int32Array) array.copy(2, 1, 2, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 0, 0, 2, 3, 0 }), copy);
	}

	public void testToString() {
		assertEquals("[empty]", AssertArrays.makeInt32ArrayEmpty().toString());
		assertEquals("[1 2 3 4 5]", AssertArrays.makeInt32Array12345().toString());
	}

	public void testZeroElements() {
		Int32Array array = Int32Array.make(0);
		array.zeroElements(0, -1);
		AssertArrays.assertEquals(Int32Array.make(0), array);
		
		// zero all elements
		array = Int32Array.make(1);
		array.storeInt32(0, 1);
		array.zeroElements(0, -1);
		AssertArrays.assertEquals(Int32Array.make(new int[] {0}), array);

		array = AssertArrays.makeInt32Array12345();
		array.zeroElements();
		AssertArrays.assertEquals(Int32Array.make(new int[] {0, 0, 0, 0, 0}), array);
		
		array = AssertArrays.makeInt32Array12345();
		array.zeroElements(0, -1);
		AssertArrays.assertEquals(Int32Array.make(new int[] {0, 0, 0, 0, 0}), array);

		// zero subset of elements
		array = AssertArrays.makeInt32Array12345();
		array.zeroElements(1, -1);
		AssertArrays.assertEquals(Int32Array.make(new int[] {1, 0, 0, 0, 0}), array);

		array = AssertArrays.makeInt32Array12345();
		array.zeroElements(1, 2);
		AssertArrays.assertEquals(Int32Array.make(new int[] {1, 0, 0, 4, 5}), array);

		array = AssertArrays.makeInt32Array12345();
		array.zeroElements(4, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] {1, 2, 3, 4, 0}), array);

		// silently truncate from
		array = AssertArrays.makeInt32Array12345();
		//TODO should this actually throw an exception?
		array.zeroElements(5, -1);
		AssertArrays.assertEquals(AssertArrays.makeInt32Array12345(), array);

		// extend count outside range
		array = AssertArrays.makeInt32Array12345();
		try {
			array.zeroElements(4, 2);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}
	
	public void testHold() {
		// simple store
		Int32Array array = AssertArrays.makeInt32Array12345();
		PrimIntegerArray held = array.hold(0, 99);
		assertNotSame(array, held);
		AssertArrays.assertEquals(Int32Array.make(new int[] {99, 2, 3, 4, 5}), held);
		AssertArrays.assertEquals(AssertArrays.makeInt32Array12345(), array);

		array = AssertArrays.makeInt32Array12345();
		held = array.hold(0, 99, true);
		assertSame(array, held);
		AssertArrays.assertEquals(Int32Array.make(new int[] {99, 2, 3, 4, 5}), array);

		// store of greater type
//		array = AssertArrays.makeInt32Array12345();
//		held = array.hold(0, PrimIntValue.make(Integer.MAX_VALUE + 1L));
//		assertNotSame(array, held);
//		assertEquals(Int64Array.make(new long[] {Integer.MAX_VALUE + 1L, 2, 3, 4, 5}), held);
//		assertEquals(AssertArrays.makeInt32Array12345(), array);

		// store of greater type - ignore the modify original request
//		array = AssertArrays.makeInt32Array12345();
//		held = array.hold(0, PrimIntValue.make(Integer.MAX_VALUE + 1L), true);
//		assertNotSame(array, held);
//		assertEquals(Int64Array.make(new long[] {Integer.MAX_VALUE + 1L, 2, 3, 4, 5}), held);
//		assertEquals(AssertArrays.makeInt32Array12345(), array);

		// extending same type
		array = AssertArrays.makeInt32ArrayEmpty();
		held = array.hold(0, 99);
		assertNotSame(array, held);
		AssertArrays.assertEquals(Int32Array.make(new int[] {99}), held);
		AssertArrays.assertEquals(AssertArrays.makeInt32ArrayEmpty(), array);

		array = AssertArrays.makeInt32Array12345();
		held = array.hold(5, 99);
		assertNotSame(array, held);
		AssertArrays.assertEquals(Int32Array.make(new int[] {1, 2, 3, 4, 5, 99}), held);
		AssertArrays.assertEquals(AssertArrays.makeInt32Array12345(), array);

		array = AssertArrays.makeInt32Array12345();
		held = array.hold(6, 99);
		assertNotSame(array, held);
		AssertArrays.assertEquals(Int32Array.make(new int[] {1, 2, 3, 4, 5, 0, 99}), held);
		AssertArrays.assertEquals(AssertArrays.makeInt32Array12345(), array);

		// extending greater type
//		array = AssertArrays.makeInt32Array12345();
//		held = array.hold(6, PrimIntValue.make(Integer.MAX_VALUE + 1L));
//		assertNotSame(array, held);
//		assertEquals(Int64Array.make(new long[] {1, 2, 3, 4, 5, 0, Integer.MAX_VALUE + 1L}), held);
//		assertEquals(AssertArrays.makeInt32Array12345(), array);

		// store before start
		try {
			AssertArrays.makeInt32ArrayEmpty().hold(-1, 1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			//expected
		}

		// store null
//		try {
//			AssertArrays.makeInt32ArrayEmpty().hold(1, null);
//			fail("null");
//		} catch (NullPointerException e) {
//			//expected
//		}
	}
	
	public void testElementsHash() {
		// complete hash
		int hash = AssertArrays.makeInt32ArrayEmpty().elementsHash();
		assertFalse(0 == hash);
		
		assertFalse(AssertArrays.makeInt32Array12345().elementsHash() == AssertArrays.makeInt32Array12321().elementsHash());

		// partial hash
		Int32Array array = AssertArrays.makeInt32Array12345();
		assertFalse(0 == array.elementsHash());
		assertFalse(array.elementsHash(0) == array.elementsHash(1));
		assertFalse(array.elementsHash(1, 1) == array.elementsHash(1));
		
		// out of range
		try {
			AssertArrays.makeInt32Array12345().elementsHash(5, 1);
			fail("5,1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testContentsHash() {
		int hash = AssertArrays.makeInt32ArrayEmpty().contentsHash();
		assertFalse(0 == hash);
		
		Int32Array array = AssertArrays.makeInt32Array12345();
		assertFalse(0 == array.contentsHash());
		
		assertFalse(AssertArrays.makeInt32Array12345().contentsHash() == AssertArrays.makeInt32Array12321().contentsHash());
	}

	public void testBitCount() {
		Int32Array array = AssertArrays.makeInt32Array1(); 
		assertEquals(((PrimIntegerSpec)array.spec()).bitCount(), Math.abs(array.bitCount()));
		assertTrue(array.bitCount() < 0);
	}
}
