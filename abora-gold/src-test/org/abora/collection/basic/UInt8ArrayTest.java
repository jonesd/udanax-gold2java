/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.collection.basic;

import java.util.Arrays;

import org.abora.gold.AboraGoldTestCase;
import org.abora.gold.collection.basic.IEEE32Array;
import org.abora.gold.collection.basic.IEEE64Array;
import org.abora.gold.collection.basic.Int32Array;
import org.abora.gold.collection.basic.UInt8Array;
import org.abora.gold.x.PrimIEEE32;
import org.abora.gold.x.PrimIEEE64;
import org.abora.gold.x.PrimIntValue;
import org.abora.gold.x.PrimIntegerSpec;

public class UInt8ArrayTest extends AboraGoldTestCase {

	public static final short UINT8_MIN_VALUE = 0;
	public static final short UINT8_MAX_VALUE = 255;

	public UInt8ArrayTest(String arg0) {
		super(arg0);
	}

	public static void main(String[] args) {
		junit.swingui.TestRunner.run(UInt8ArrayTest.class);
	}

	public void testMakeCount() {
		UInt8Array array = UInt8Array.make(0);
		assertEquals(0, array.count());

		array = UInt8Array.make(1);
		assertEquals(1, array.count());
		assertEquals(0, array.uInt8At(0));
		
		try {
			UInt8Array.make(-1);
			fail("-1");
		} catch (NegativeArraySizeException e) {
			//expected
		}
	}

	public void testMake() {
		UInt8Array array = UInt8Array.make(AssertArrays.makeUInt8ArrayEmpty());
		assertEquals(0, array.count());

		array = UInt8Array.make(AssertArrays.makeUInt8Array12345());
		assertEquals(5, array.count());
		AssertArrays.assertEquals(AssertArrays.makeUInt8Array12345(), array);

		array = UInt8Array.make(7, AssertArrays.makeUInt8Array12345());
		assertEquals(7, array.count());
		AssertArrays.assertEquals(UInt8Array.make(new short[]{1,2,3,4,5,0,0}), array);

		array = UInt8Array.make(7, AssertArrays.makeUInt8Array12345(), 1, 2, 5);
		assertEquals(7, array.count());
		AssertArrays.assertEquals(UInt8Array.make(new short[]{0,0,0,0,0,2,3}), array);		

		try {
			UInt8Array.make(4, AssertArrays.makeUInt8Array12345());
			fail("4");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}
	
	public void testMakeBuffer() {
		UInt8Array array = UInt8Array.make(new short[] {});
		assertEquals(0, array.count());

		array = UInt8Array.make(new short[] {1, 2});
		assertEquals(2, array.count());
		assertEquals(1, array.uInt8At(0));
		assertEquals(2, array.uInt8At(1));		
	}

	public void testUInt8At() {
		UInt8Array a = UInt8Array.make(new short[] { 0, 1, 127, 255 });

		assertEquals(0, a.uInt8At(0));
		assertEquals(1, a.uInt8At(1));
		assertEquals(127, a.uInt8At(2));
		assertEquals(255, a.uInt8At(3));

		try {
			a.uInt8At(-1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			a.uInt8At(4);
			fail("4");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testUInt8AtEmpty() {
		UInt8Array a = UInt8Array.make(0);
		try {
			a.uInt8At(0);
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// OutOfBounds
		}
	}

	public void testIntegerAt() {
		UInt8Array a = UInt8Array.make(new short[] { 0, 1, 2, 3 });

		assertEquals(0, a.integerAt(0));
		assertEquals(1, a.integerAt(1));
		assertEquals(2, a.integerAt(2));
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
		UInt8Array a = UInt8Array.make(new short[] { 0, 1, 2, 3 });

		assertEquals(0, ((PrimIntValue) a.fetchValue(0)).asUInt8());
		assertEquals(1, ((PrimIntValue) a.fetchValue(1)).asUInt8());
		assertEquals(2, ((PrimIntValue) a.fetchValue(2)).asUInt8());
		assertEquals(3, ((PrimIntValue) a.fetchValue(3)).asUInt8());

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
		assertEquals(0, AssertArrays.makeUInt8ArrayEmpty().count());
		assertEquals(1, UInt8Array.make(new short[] { 0 }).count());
		assertEquals(2, UInt8Array.make(new short[] { 0, 1 }).count());
	}

	public void testStoreUInt8() {
		UInt8Array empty = UInt8Array.make(0);
		UInt8Array tri = UInt8Array.make(3);

		tri.storeUInt8(0, UINT8_MIN_VALUE);
		assertEquals(tri.uInt8At(0), UINT8_MIN_VALUE);
		tri.storeUInt8(1, (short) 1);
		assertEquals(tri.uInt8At(1), 1);
		tri.storeUInt8(2, UINT8_MAX_VALUE);
		assertEquals(tri.uInt8At(2), UINT8_MAX_VALUE);

		// silent wrapping of values outside range
		tri.storeUInt8(0, (short)(UINT8_MIN_VALUE - 1));
		assertEquals(UINT8_MAX_VALUE, tri.uInt8At(0));
		
		tri.storeUInt8(1, (short)(UINT8_MAX_VALUE + 1));
		assertEquals(UINT8_MIN_VALUE, tri.uInt8At(1));

		try {
			tri.storeUInt8(-1, (short) 1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			tri.storeUInt8(3, (short) 1);
			fail("3");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			empty.storeUInt8(0, (short) 1);
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testStoreInteger() {
		UInt8Array empty = UInt8Array.make(0);
		UInt8Array tri = UInt8Array.make(3);

		// Store integer values within spec
		tri.storeInteger(0, UINT8_MIN_VALUE);
		assertTrue(tri.uInt8At(0) == UINT8_MIN_VALUE);
		tri.storeInteger(1, 1);
		assertEquals(tri.uInt8At(1), 1);
		tri.storeInteger(2, UINT8_MAX_VALUE);
		assertTrue(tri.uInt8At(2) == UINT8_MAX_VALUE);

		// Store integer values outside of spec
		try {
			tri.storeInteger(0, UINT8_MIN_VALUE - 1);
			fail("MIN_VALUE - 1");
		} catch (IllegalArgumentException e) {
			// expected
		}
		try {
			tri.storeInteger(2, UINT8_MAX_VALUE + 1);
			fail("MAX_VALUE + 1");
		} catch (IllegalArgumentException e) {
			//expected
		}

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
		UInt8Array empty = UInt8Array.make(0);
		UInt8Array tri = UInt8Array.make(3);

		// Store integer values within spec
		tri.storeValue(0, PrimIntValue.make(UINT8_MIN_VALUE));
		assertTrue(tri.uInt8At(0) == UINT8_MIN_VALUE);
		tri.storeValue(1, PrimIntValue.make(1));
		assertEquals(tri.uInt8At(1), 1);
		tri.storeValue(2, PrimIntValue.make(UINT8_MAX_VALUE));
		assertTrue(tri.uInt8At(2) == UINT8_MAX_VALUE);

		// Store integer values outside of spec
		try {
			tri.storeValue(0, PrimIntValue.make(UINT8_MIN_VALUE - 1));
			fail("MIN_VALUE - 1");
		} catch (IllegalArgumentException e) {
			// expected
		}
		try {
			tri.storeValue(2, PrimIntValue.make(UINT8_MAX_VALUE + 1));
			fail("MAX_VALUE + 1");
		} catch (IllegalArgumentException e) {
			// expected
		}

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
		UInt8Array array = UInt8Array.make(0);
		array.storeAll(PrimIntValue.make(1));
		AssertArrays.assertEquals(UInt8Array.make(0), array);

		array = UInt8Array.make(1);
		array.storeAll(PrimIntValue.make(1));
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 1 }), array);

		array = UInt8Array.make(3);
		array.storeAll(PrimIntValue.make(2));
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 2, 2, 2 }), array);

		array = AssertArrays.makeUInt8Array12345();
		array.storeAll(PrimIntValue.make(9), 2, 1);
		assertEquals(UInt8Array.make(new short[] { 1, 9, 9, 4, 5 }), array);

		array = AssertArrays.makeUInt8Array12345();
		array.storeAll(null, 2, 1);
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 1, 0, 0, 4, 5 }), array);

		array = AssertArrays.makeUInt8Array12345();
		array.storeAll(PrimIntValue.make(9), -1, 1);
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 1, 9, 9, 9, 9 }), array);

		array = AssertArrays.makeUInt8Array12345();
		array.storeAll(PrimIntValue.make(9), 2);
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 9, 9, 3, 4, 5 }), array);

		array = AssertArrays.makeUInt8Array12345();
		array.storeAll(PrimIntValue.make(9), 0, 1);
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 1, 2, 3, 4, 5 }), array);

		array = AssertArrays.makeUInt8Array12345();
		array.storeAll(PrimIntValue.make(9), 2, 1);
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 1, 9, 9, 4, 5 }), array);

		// Store outside of array bounds
		array = AssertArrays.makeUInt8Array12345();
		try {
			array.storeAll(PrimIntValue.make(9), 6);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = AssertArrays.makeUInt8Array12345();
		try {
			array.storeAll(PrimIntValue.make(9), 4, 2);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// Store incompatible type
		array = AssertArrays.makeUInt8Array12345();
		try {
			array.storeAll(PrimIEEE32.make(9.9f), 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}

		// Store integer values outside of spec
		array = AssertArrays.makeUInt8Array12345();
		try {
			array.storeAll(PrimIntValue.make(UINT8_MAX_VALUE + 1), 1);
			fail();
		} catch (IllegalArgumentException e) {
			// expected
		}
	}

	public void testStoreMany() {
		// empty
		UInt8Array array = AssertArrays.makeUInt8ArrayEmpty();
		array.storeMany(0, AssertArrays.makeUInt8ArrayEmpty());
		AssertArrays.assertEquals(AssertArrays.makeUInt8ArrayEmpty(), array);

		// simple
		array = AssertArrays.makeUInt8Array12345();
		array.storeMany(0, AssertArrays.makeUInt8Array12321());
		AssertArrays.assertEquals(AssertArrays.makeUInt8Array12321(), array);

		array = AssertArrays.makeUInt8Array12345();
		array.storeMany(0, AssertArrays.makeUInt8Array12321());
		AssertArrays.assertEquals(AssertArrays.makeUInt8Array12321(), array);

		array = AssertArrays.makeUInt8Array12321();
		array.storeMany(1, UInt8Array.make(new short[] { 8, 7, 6, 5, 4, 3, 3 }), 2, 3);
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 1, 5, 4, 2, 1 }), array);

		array = AssertArrays.makeUInt8Array12321();
		array.storeMany(1, Int32Array.make(new int[] { 8, 7 }));
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 1, 8, 7, 2, 1 }), array);

		// Store incompatible type
		array = AssertArrays.makeUInt8Array12321();
		try {
			array.storeMany(1, IEEE64Array.make(new double[] { 8.8, 7.7 }));
			fail();
		} catch (ClassCastException e) {
			// expected
		}

		// attempt to copy beyond this extent
		array = AssertArrays.makeUInt8Array12345();
		try {
			array.storeMany(1, AssertArrays.makeUInt8Array12321());
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}

		// insufficient source elements
		array = AssertArrays.makeUInt8Array12345();
		try {
			array.storeMany(0, AssertArrays.makeUInt8Array12321(), 2, 4);
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}

		// Store integer values outside of spec
		array = AssertArrays.makeUInt8Array12321();
		try {
			array.storeMany(1, Int32Array.make(new int[] { UINT8_MAX_VALUE + 1 }));
			fail("MAX_VALUE + 1");
		} catch (IllegalArgumentException e) {
			//expected
		}
	}

	public void testCopyToBuffer() {
		UInt8Array array = AssertArrays.makeUInt8Array12345();
		short[] out = new short[3];
		array.copyToBuffer(out, 3, 1);
		assertTrue(Arrays.equals(out, new short[] { 2, 3, 4 }));

		array = AssertArrays.makeUInt8Array12345();
		out = new short[1];
		array.copyToBuffer(out, -1, 0);
		assertTrue(Arrays.equals(out, new short[] { 1 }));

		array = AssertArrays.makeUInt8Array12345();
		out = new short[1];
		array.copyToBuffer(out, -1, 4);
		assertTrue(Arrays.equals(out, new short[] { 5 }));

		array = AssertArrays.makeUInt8Array12345();
		out = new short[3];
		array.copyToBuffer(out, 3, 0);
		assertTrue(Arrays.equals(out, new short[] { 1, 2, 3 }));

		array = AssertArrays.makeUInt8Array12345();
		out = new short[3];
		array.copyToBuffer(out, -1, 2);
		assertTrue(Arrays.equals(out, new short[] { 3, 4, 5 }));

		array = AssertArrays.makeUInt8Array12345();
		out = new short[3];
		array.copyToBuffer(out, -1, 3);
		assertTrue(Arrays.equals(out, new short[] { 4, 5, 0 }));

		array = AssertArrays.makeUInt8Array12345();
		out = new short[0];
		array.copyToBuffer(out, -1, 3);
		assertTrue(Arrays.equals(out, new short[] {
		}));

		array = AssertArrays.makeUInt8Array12345();
		out = new short[3];
		try {
			array.copyToBuffer(out, -1, -1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = AssertArrays.makeUInt8Array12345();
		out = new short[3];
		try {
			array.copyToBuffer(out, 1, 5);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

	}

	public void testIsEqual() {
		short[] charsE = new short[] {};
		short[] chars1 = new short[] { 1 };
		short[] chars12 = new short[] { 1, 2 };
		short[] chars11 = new short[] { 1, 1 };

		// Equal matches
		assertTrue(UInt8Array.make(charsE).isEqual(UInt8Array.make(charsE)));
		assertTrue(UInt8Array.make(chars1).isEqual(UInt8Array.make(chars1)));
		assertTrue(UInt8Array.make(chars12).isEqual(UInt8Array.make(chars12)));
		assertTrue(UInt8Array.make(chars1).isEqual(AssertArrays.makeInt32Array1()));
		
		// Unequal compatible matches
		assertFalse(UInt8Array.make(chars11).isEqual(UInt8Array.make(chars12)));
		assertFalse(UInt8Array.make(charsE).isEqual(UInt8Array.make(chars12)));
		assertFalse(UInt8Array.make(chars12).isEqual(UInt8Array.make(charsE)));

		// single value
		assertFalse(UInt8Array.make(chars1).isEqual(PrimIntValue.make(1)));
		
		// incompatible array
		try {
			assertFalse(UInt8Array.make(chars1).isEqual(AssertArrays.makeIEEE32Array1()));
			fail("ieee32");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testIndexOf() {
		int index = AssertArrays.makeUInt8ArrayEmpty().indexOf(PrimIntValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt8Array1().indexOf(PrimIntValue.make(1), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeUInt8Array1().indexOf(PrimIntValue.make(1), 0, 0);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt8Array12345().indexOf(PrimIntValue.make(1), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeUInt8Array12345().indexOf(PrimIntValue.make(1), 0, 2);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt8Array12345().indexOf(PrimIntValue.make(1), 1, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt8Array12345().indexOf(PrimIntValue.make(5), 0, 1);
		assertEquals(4, index);

		index = AssertArrays.makeUInt8Array12321().indexOf(PrimIntValue.make(2), 0, 1);
		assertEquals(1, index);

		index = AssertArrays.makeUInt8Array12321().indexOf(PrimIntValue.make(2), 0, 2);
		assertEquals(3, index);

		index = AssertArrays.makeUInt8Array12321().indexOf(PrimIntValue.make(1), -1, -1);
		assertEquals(4, index);

		index = AssertArrays.makeUInt8Array12321().indexOf(PrimIntValue.make(2), -1, -1);
		assertEquals(3, index);

		index = AssertArrays.makeUInt8Array12321().indexOf(PrimIntValue.make(2), -1, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt8Array12321().indexOf(PrimIntValue.make(2), -1, -2);
		assertEquals(1, index);

		index = AssertArrays.makeUInt8Array12321().indexOf(PrimIntValue.make(2), -1, -3);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt8Array12321().indexOf(PrimIntValue.make(2), -3, -1);
		assertEquals(1, index);

		index = AssertArrays.makeUInt8Array12321().indexOf(PrimIntValue.make(2), -1, 1);
		assertEquals(-1, index);

		try {
			AssertArrays.makeUInt8Array12321().indexOf(PrimIntValue.make(2), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			AssertArrays.makeUInt8Array12321().indexOf(PrimIEEE64.make(2.0f), -3, 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testIndexPast() {
		int index = AssertArrays.makeUInt8ArrayEmpty().indexPast(PrimIntValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt8Array1().indexPast(PrimIntValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt8Array1().indexPast(PrimIntValue.make(1), 0, 0);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt8Array12345().indexPast(PrimIntValue.make(1), 0, 1);
		assertEquals(1, index);

		index = AssertArrays.makeUInt8Array12345().indexPast(PrimIntValue.make(1), 0, 2);
		assertEquals(2, index);

		index = AssertArrays.makeUInt8Array12345().indexPast(PrimIntValue.make(1), 1, 1);
		assertEquals(1, index);

		index = AssertArrays.makeUInt8Array12345().indexPast(PrimIntValue.make(5), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeUInt8Array12345().indexPast(PrimIntValue.make(5), 3, 1);
		assertEquals(3, index);

		index = AssertArrays.makeUInt8Array12345().indexPast(PrimIntValue.make(5), 4, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt8Array12321().indexPast(PrimIntValue.make(2), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeUInt8Array12321().indexPast(PrimIntValue.make(2), 0, 2);
		assertEquals(2, index);

		index = AssertArrays.makeUInt8Array12321().indexPast(PrimIntValue.make(1), -1, -1);
		assertEquals(3, index);

		index = AssertArrays.makeUInt8Array12321().indexPast(PrimIntValue.make(2), -1, -1);
		assertEquals(4, index);

		index = AssertArrays.makeUInt8Array12321().indexPast(PrimIntValue.make(2), -1, 1);
		assertEquals(4, index);

		index = AssertArrays.makeUInt8Array12321().indexPast(PrimIntValue.make(2), -1, 2);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt8Array12321().indexPast(PrimIntValue.make(2), -1, -2);
		assertEquals(2, index);

		index = AssertArrays.makeUInt8Array12321().indexPast(PrimIntValue.make(2), -1, -3);
		assertEquals(0, index);

		index = AssertArrays.makeUInt8Array12321().indexPast(PrimIntValue.make(2), -3, -1);
		assertEquals(2, index);

		index = AssertArrays.makeUInt8Array12321().indexPast(PrimIntValue.make(1), -1, 1);
		assertEquals(-1, index);

		try {
			AssertArrays.makeUInt8Array12321().indexPast(PrimIntValue.make(2), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			AssertArrays.makeUInt8Array12321().indexPast(PrimIEEE64.make(2.0), -3, 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testIndexOfElements() {
		// empty
		UInt8Array array = AssertArrays.makeUInt8ArrayEmpty();
		UInt8Array search = AssertArrays.makeUInt8Array1();
		assertEquals(-1, array.indexOfElements(search));

		array = AssertArrays.makeUInt8ArrayEmpty();
		search = AssertArrays.makeUInt8ArrayEmpty();
		assertEquals(-1, array.indexOfElements(search));

		// TODO skip zero length other?
		//		array = makeUInt8Array12345();
		//		search = makeUInt8ArrayEmpty();
		//		assertEquals(-1, array.indexOfElements(search));

		// forward search
		array = UInt8Array.make(new short[] { 1, 2, 3, 1, 2 });
		search = UInt8Array.make(new short[] { 1, 2 });
		assertEquals(0, array.indexOfElements(search));
		assertEquals(3, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		array = AssertArrays.makeUInt8Array12321();
		search = UInt8Array.make(new short[] { 4, 9, 2, 8 });
		assertEquals(1, array.indexOfElements(search, 1, 2, 0, 1));
		assertEquals(3, array.indexOfElements(search, 1, 2, 0, 2));
		assertEquals(-1, array.indexOfElements(search, 1, 2, 0, 3));

		// forward search with compatible int array
		array = UInt8Array.make(new short[] { 1, 2, 3, 1, 2 });
		Int32Array searchInt32 = Int32Array.make(new int[] { 1, 2 });
		assertEquals(0, array.indexOfElements(searchInt32));
		assertEquals(3, array.indexOfElements(searchInt32, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(searchInt32, -1, 0, 0, 3));

		// reverse search		
		array = UInt8Array.make(new short[] { 1, 2, 3, 1, 2 });
		search = UInt8Array.make(new short[] { 1, 2 });
		assertEquals(3, array.indexOfElements(search, -1, 0, -2, -1));
		assertEquals(0, array.indexOfElements(search, -1, 0, -2, -2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, -2, -3));

		// overlapping search
		// TODO should this succeed?
		array = UInt8Array.make(new short[] { 1, 1, 1 });
		search = UInt8Array.make(new short[] { 1, 1 });
		assertEquals(0, array.indexOfElements(search));
		assertEquals(1, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		// nth == 0 immediate fail
		array = AssertArrays.makeUInt8Array12321();
		search = UInt8Array.make(new short[] { 1 });
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 0));

		// overflowing otherCount
		array = AssertArrays.makeUInt8Array12321();
		search = UInt8Array.make(new short[] { 1, 2 });
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
		array = AssertArrays.makeUInt8Array12321();
		search = UInt8Array.make(new short[] { 1, 2 });
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
		array = UInt8Array.make(new short[] { 1, 2, 3, 1, 2 });
		IEEE32Array searchIEEE = IEEE32Array.make(new float[] { 1.0f, 2.0f });
		try {
			assertEquals(0, array.indexOfElements(searchIEEE));
			fail("ieee");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testAddElements() {
		UInt8Array array = AssertArrays.makeUInt8ArrayEmpty();
		array.addElements(0, AssertArrays.makeUInt8Array12321(), -1, 0);
		assertEquals(AssertArrays.makeUInt8ArrayEmpty(), array);

		array = AssertArrays.makeUInt8Array1();
		array.addElements(0, UInt8Array.make(new short[] { 9 }), -1, 0);
		assertEquals(UInt8Array.make(new short[] { 10 }), array);

		array = UInt8Array.make(5);
		array.addElements(0, AssertArrays.makeUInt8Array12321(), -1, 0);
		assertEquals(AssertArrays.makeUInt8Array12321(), array);

		array = AssertArrays.makeUInt8Array12345();
		array.addElements(0, AssertArrays.makeUInt8Array12321(), -1, 0);
		assertEquals(UInt8Array.make(new short[] { 2, 4, 6, 6, 6 }), array);

		array = AssertArrays.makeUInt8Array12345();
		array.addElements(2, AssertArrays.makeUInt8Array12321(), -1, 0);
		assertEquals(UInt8Array.make(new short[] { 1, 2, 4, 6, 8 }), array);

		array = AssertArrays.makeUInt8Array12345();
		array.addElements(2, AssertArrays.makeUInt8Array12321(), -1);
		assertEquals(UInt8Array.make(new short[] { 1, 2, 4, 6, 8 }), array);

		array = AssertArrays.makeUInt8Array12345();
		array.addElements(2, AssertArrays.makeUInt8Array12321());
		assertEquals(UInt8Array.make(new short[] { 1, 2, 4, 6, 8 }), array);

		array = AssertArrays.makeUInt8Array12345();
		array.addElements(2, AssertArrays.makeUInt8Array12321(), 2, 1);
		assertEquals(UInt8Array.make(new short[] { 1, 2, 5, 7, 5 }), array);

		// element arithmetic overflows
		array = UInt8Array.make(new short[] {200, 0});
		array.addElements(0, UInt8Array.make(new short[] {200, 200}));
		assertEquals(UInt8Array.make(new short[]{144, 200}), array);

		// compatible array types
		array = AssertArrays.makeUInt8Array12345();
		array.addElements(0, AssertArrays.makeInt32Array12321(), -1, 0);
		AssertArrays.assertEquals(UInt8Array.make(new short[]{2, 4, 6, 6, 6}), array);

		array = AssertArrays.makeUInt8Array12345();
		try {
			array.addElements(2, AssertArrays.makeUInt8Array12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// incompatible arrays
		array = AssertArrays.makeUInt8ArrayEmpty();
		try {
			array.addElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
			fail("ieee");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testSubtractElements() {
		UInt8Array array = AssertArrays.makeUInt8ArrayEmpty();
		array.subtractElements(0, AssertArrays.makeUInt8Array12321(), -1, 0);
		AssertArrays.assertEquals(AssertArrays.makeUInt8ArrayEmpty(), array);

		array = AssertArrays.makeUInt8Array1();
		array.subtractElements(0, UInt8Array.make(new short[] { 1 }), -1, 0);
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 0 }), array);

		array = AssertArrays.makeUInt8Array12345();
		array.subtractElements(0, AssertArrays.makeUInt8Array12321(), -1, 0);
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 0, 0, 0, 2, 4 }), array);

		array = AssertArrays.makeUInt8Array12345();
		array.subtractElements(2, AssertArrays.makeUInt8Array12321(), -1, 0);
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 1, 2, 2, 2, 2 }), array);

		array = AssertArrays.makeUInt8Array12345();
		array.subtractElements(2, AssertArrays.makeUInt8Array12321(), -1);
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 1, 2, 2, 2, 2 }), array);

		array = AssertArrays.makeUInt8Array12345();
		array.subtractElements(2, AssertArrays.makeUInt8Array12321());
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 1, 2, 2, 2, 2 }), array);

		array = AssertArrays.makeUInt8Array12345();
		array.subtractElements(2, AssertArrays.makeUInt8Array12321(), 2, 1);
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 1, 2, 1, 1, 5 }), array);

		// element arithmetic overflows
		array = UInt8Array.make(new short[] {0, 200});
		array.subtractElements(0, UInt8Array.make(new short[] {200, 200}));
		assertEquals(UInt8Array.make(new short[]{56, 0}), array);

		// compatible array types
		array = AssertArrays.makeUInt8Array12345();
		array.subtractElements(0, AssertArrays.makeInt32Array12321(), -1, 0);
		AssertArrays.assertEquals(UInt8Array.make(new short[]{0, 0, 0, 2, 4}), array);

		// extend count beyond end of array
		array = AssertArrays.makeUInt8Array12345();
		try {
			array.subtractElements(2, AssertArrays.makeUInt8Array12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// incompatible arrays
		array = AssertArrays.makeUInt8ArrayEmpty();
		try {
			array.subtractElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
			fail("ieee");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testCompare() {
		// same
		UInt8Array array1 = AssertArrays.makeUInt8ArrayEmpty();
		UInt8Array array2 = AssertArrays.makeUInt8ArrayEmpty();
		assertEquals(0, array1.compare(array2));

		array1 = AssertArrays.makeUInt8Array12345();
		array2 = AssertArrays.makeUInt8Array12345();
		assertEquals(0, array1.compare(array2));

		array1 = AssertArrays.makeUInt8Array12321();
		array2 = AssertArrays.makeUInt8Array12321();
		assertEquals(0, array1.compare(array2));

		// different
		array1 = AssertArrays.makeUInt8Array12321();
		array2 = AssertArrays.makeUInt8Array12345();
		assertEquals(-1, array1.compare(array2));

		array1 = AssertArrays.makeUInt8Array12345();
		array2 = AssertArrays.makeUInt8Array12321();
		assertEquals(1, array1.compare(array2));

		// auto-filling with 0
		array1 = AssertArrays.makeUInt8ArrayEmpty();
		array2 = AssertArrays.makeUInt8Array1();
		assertEquals(-1, array1.compare(array2));

		array1 = AssertArrays.makeUInt8Array1();
		array2 = AssertArrays.makeUInt8ArrayEmpty();
		assertEquals(1, array1.compare(array2));

		array1 = UInt8Array.make(new short[] { 0, 0 });
		array2 = UInt8Array.make(new short[] { 0 });
		assertEquals(0, array1.compare(array2));

		// compare sub-regions		
		array1 = AssertArrays.makeUInt8Array12321();
		array2 = AssertArrays.makeUInt8Array12345();
		assertEquals(1, array1.compare(array2, 2, 2, 1));

		array1 = AssertArrays.makeUInt8Array12321();
		array2 = AssertArrays.makeUInt8Array12345();
		assertEquals(1, array1.compare(array2, 2, 2));

		array1 = AssertArrays.makeUInt8Array12321();
		array2 = AssertArrays.makeUInt8Array12345();
		assertEquals(0, array1.compare(array2, 1, 4));

		// trim down count
		array1 = AssertArrays.makeUInt8Array12321();
		array2 = AssertArrays.makeUInt8Array12345();
		assertEquals(-1, array1.compare(array2, 10));

		// different array types
		array1 = AssertArrays.makeUInt8Array12345();
		Int32Array array2Int32 = AssertArrays.makeInt32Array12345();
		assertEquals(0, array1.compare(array2Int32));

		// incompatible array types
		array1 = AssertArrays.makeUInt8Array12345();
		IEEE32Array array2IEEE32 = AssertArrays.makeIEEE32Array12345();
		try {
			array1.compare(array2IEEE32);
			fail("ieee32");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testCopyGrow() {
		UInt8Array array = AssertArrays.makeUInt8ArrayEmpty();
		UInt8Array copy = (UInt8Array) array.copyGrow(0);
		AssertArrays.assertEquals(AssertArrays.makeUInt8ArrayEmpty(), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeUInt8Array12345();
		copy = (UInt8Array) array.copyGrow(0);
		AssertArrays.assertEquals(AssertArrays.makeUInt8Array12345(), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeUInt8Array12345();
		copy = (UInt8Array) array.copyGrow(3);
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 1, 2, 3, 4, 5, 0, 0, 0 }), copy);
		assertNotSame(array, copy);
	}

	public void testCopy() {
		// full copy
		UInt8Array array = AssertArrays.makeUInt8ArrayEmpty();
		UInt8Array copy = (UInt8Array) array.copy();
		AssertArrays.assertEquals(AssertArrays.makeUInt8ArrayEmpty(), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeUInt8Array12345();
		copy = (UInt8Array) array.copy();
		AssertArrays.assertEquals(AssertArrays.makeUInt8Array12345(), copy);
		AssertArrays.assertEquals(AssertArrays.makeUInt8Array12345(), array);
		assertNotSame(array, copy);

		// partial copy
		array = AssertArrays.makeUInt8Array12345();
		copy = (UInt8Array) array.copy(2, 1);
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 2, 3 }), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeUInt8Array12345();
		copy = (UInt8Array) array.copy(2, 0);
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 1, 2 }), copy);

		array = AssertArrays.makeUInt8Array12345();
		copy = (UInt8Array) array.copy(2, 3);
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 4, 5 }), copy);

		// partial copy with too large count
		array = AssertArrays.makeUInt8Array12345();
		try {
			array.copy(5, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// partial copy with trailing space
		array = AssertArrays.makeUInt8Array12345();
		copy = (UInt8Array) array.copy(2, 1);
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 2, 3 }), copy);

		// partial copy with leading space
		array = AssertArrays.makeUInt8Array12345();
		copy = (UInt8Array) array.copy(2, 1, 1);
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 0, 2, 3 }), copy);

		// partial copy with leading space
		array = AssertArrays.makeUInt8Array12345();
		copy = (UInt8Array) array.copy(2, 1, 0, 1);
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 2, 3, 0 }), copy);

		// partial copy with leading and trailing space
		array = AssertArrays.makeUInt8Array12345();
		copy = (UInt8Array) array.copy(2, 1, 2, 1);
		AssertArrays.assertEquals(UInt8Array.make(new short[] { 0, 0, 2, 3, 0 }), copy);
	}

	public void testToString() {
		assertEquals("[empty]", AssertArrays.makeUInt8ArrayEmpty().toString());
		assertEquals("ABC", UInt8Array.make(new short[] { 65,66, 67 }).toString());
	}
	
//	public void testString() {
//		UInt8Array array = UInt8Array.string("");
//		assertEquals(0, array.count());
//		
//		array = UInt8Array.string("a \u0424\t");
//		assertEquals(4, array.count());
//		assertEquals('a', array.uInt8At(0));
//		assertEquals(' ', array.uInt8At(1));
//		assertEquals('\u0424', array.uInt8At(2));
//		assertEquals('\t', array.uInt8At(3));
//	}
//	
//	public void testAsString() {
//		UInt8Array array = UInt8Array.string("");
//		assertEquals("", array.asString());
//		
//		array = UInt8Array.string("a \u0424\t");
//		assertEquals("a \u0424\t", array.asString());
//	}

	public void testZeroElements() {
		UInt8Array array = UInt8Array.make(0);
		array.zeroElements(0, -1);
		AssertArrays.assertEquals(UInt8Array.make(0), array);
		
		// zero all elements
		array = UInt8Array.make(1);
		array.storeUInt8(0, (short)1);
		array.zeroElements(0, -1);
		AssertArrays.assertEquals(UInt8Array.make(new short[] {0}), array);

		array = AssertArrays.makeUInt8Array12345();
		array.zeroElements();
		AssertArrays.assertEquals(UInt8Array.make(new short[] {0, 0, 0, 0, 0}), array);
		
		array = AssertArrays.makeUInt8Array12345();
		array.zeroElements(0, -1);
		AssertArrays.assertEquals(UInt8Array.make(new short[] {0, 0, 0, 0, 0}), array);

		// zero subset of elements
		array = AssertArrays.makeUInt8Array12345();
		array.zeroElements(1, -1);
		AssertArrays.assertEquals(UInt8Array.make(new short[] {1, 0, 0, 0, 0}), array);

		array = AssertArrays.makeUInt8Array12345();
		array.zeroElements(1, 2);
		AssertArrays.assertEquals(UInt8Array.make(new short[] {1, 0, 0, 4, 5}), array);

		array = AssertArrays.makeUInt8Array12345();
		array.zeroElements(4, 1);
		AssertArrays.assertEquals(UInt8Array.make(new short[] {1, 2, 3, 4, 0}), array);

		// silently truncate from
		array = AssertArrays.makeUInt8Array12345();
		//TODO should this actually throw an exception?
		array.zeroElements(5, -1);
		AssertArrays.assertEquals(AssertArrays.makeUInt8Array12345(), array);

		// extend count outside range
		array = AssertArrays.makeUInt8Array12345();
		try {
			array.zeroElements(4, 2);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testBitCount() {
		UInt8Array array = AssertArrays.makeUInt8Array1(); 
		assertEquals(((PrimIntegerSpec)array.spec()).bitCount(), Math.abs(array.bitCount()));
		assertTrue(array.bitCount() >= 0);
	}
	
	public void testInt32At() {
		UInt8Array array = UInt8Array.make(8);
		
		array.storeInt32(0, 1);
		assertEquals(1, array.int32At(0));
		
		array.storeInt32(0, Integer.MIN_VALUE);
		assertEquals(Integer.MIN_VALUE, array.int32At(0));
		
		array.storeInt32(0, Integer.MAX_VALUE);
		assertEquals(Integer.MAX_VALUE, array.int32At(0));
		
		array.storeInt32(1, 99);
		assertEquals(99, array.int32At(1));
		
	}
}