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

import junit.framework.Assert;

import info.dgjones.abora.gold.collection.basic.IEEE32Array;
import info.dgjones.abora.gold.collection.basic.IEEE64Array;
import info.dgjones.abora.gold.collection.basic.Int32Array;
import info.dgjones.abora.gold.collection.basic.Int8Array;
import info.dgjones.abora.gold.collection.basic.IntegerVarArray;
import info.dgjones.abora.gold.collection.basic.PrimFloatArray;
import info.dgjones.abora.gold.collection.basic.PrimIntegerArray;
import info.dgjones.abora.gold.collection.basic.PtrArray;
import info.dgjones.abora.gold.collection.basic.UInt32Array;
import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class AssertArrays {

	/** Class not instantiable */
	private AssertArrays() {
		super();
	}

	//////////////////////////////////////////////
	// Asserts

	public static void assertEquals(PtrArray expected, PtrArray actual) {
		Assert.assertEquals(expected.count(), actual.count());
		for (int i = 0; i < expected.count(); i++) {
			Heaper expectedValue = expected.fetch(i);
			Heaper actualValue = actual.fetch(i);
			Assert.assertEquals(expectedValue, actualValue);
		}
	}

	public static void assertEquals(PrimIntegerArray expected, PrimIntegerArray actual) {
		Assert.assertEquals(expected.count(), actual.count());
		for (int i = 0; i < expected.count(); i++) {
			int expectedValue = expected.integerAt(i);
			int actualValue = actual.integerAt(i);
			Assert.assertEquals(expectedValue, actualValue);
		}
	}

	public static void assertEquals(PrimFloatArray expected, PrimFloatArray actual) {
		assertEquals(expected, actual, 0.0001);
	}
	
	public static void assertEquals(PrimFloatArray expected, PrimFloatArray actual, double diff) {
		Assert.assertEquals(expected.count(), actual.count());
		for (int i = 0; i < expected.count(); i++) {
			double expectedValue = expected.floatAt(i);
			double actualValue = actual.floatAt(i);
			Assert.assertEquals(expectedValue, actualValue, diff);
		}
	}

	//////////////////////////////////////////////
	// Int8
	
	public static Int8Array makeInt8ArrayEmpty() {
		return Int8Array.make(new byte[] {
		});
	}
	public static Int8Array makeInt8Array1() {
		return Int8Array.make(new byte[] { 1 });
	}
	public static Int8Array makeInt8Array12345() {
		return Int8Array.make(new byte[] { 1, 2, 3, 4, 5 });
	}
	public static Int8Array makeInt8Array12321() {
		return Int8Array.make(new byte[] { 1, 2, 3, 2, 1 });
	}

	//////////////////////////////////////////////
	// Int16
	
//	public static Int16Array makeInt16ArrayEmpty() {
//		return Int16Array.make(new short[] {
//		});
//	}
//	public static Int16Array makeInt16Array1() {
//		return Int16Array.make(new short[] { 1 });
//	}
//	public static Int16Array makeInt16Array12345() {
//		return Int16Array.make(new short[] { 1, 2, 3, 4, 5 });
//	}
//	public static Int16Array makeInt16Array12321() {
//		return Int16Array.make(new short[] { 1, 2, 3, 2, 1 });
//	}

	//////////////////////////////////////////////
	// Int32
	
	public static Int32Array makeInt32ArrayEmpty() {
		return Int32Array.make(new int[] {
		});
	}
	public static Int32Array makeInt32Array1() {
		return Int32Array.make(new int[] { 1 });
	}
	public static Int32Array makeInt32Array12345() {
		return Int32Array.make(new int[] { 1, 2, 3, 4, 5 });
	}
	public static Int32Array makeInt32Array12321() {
		return Int32Array.make(new int[] { 1, 2, 3, 2, 1 });
	}

	//////////////////////////////////////////////
	// Int64
	
//	public static Int64Array makeInt64ArrayEmpty() {
//		return Int64Array.make(new long[] {
//		});
//	}
//	public static Int64Array makeInt64Array1() {
//		return Int64Array.make(new long[] { 1 });
//	}
//	public static Int64Array makeInt64Array12345() {
//		return Int64Array.make(new long[] { 1, 2, 3, 4, 5 });
//	}
//	public static Int64Array makeInt64Array12321() {
//		return Int64Array.make(new long[] { 1, 2, 3, 2, 1 });
//	}

	//////////////////////////////////////////////
	// UInt8
	
	public static UInt8Array makeUInt8ArrayEmpty() {
		return UInt8Array.make(new short[] {
		});
	}
	public static UInt8Array makeUInt8Array1() {
		return UInt8Array.make(new short[] { 1 });
	}
	public static UInt8Array makeUInt8Array12345() {
		return UInt8Array.make(new short[] { 1, 2, 3, 4, 5 });
	}
	public static UInt8Array makeUInt8Array12321() {
		return UInt8Array.make(new short[] { 1, 2, 3, 2, 1 });
	}

	//////////////////////////////////////////////
	// UInt16
	
//	public static UInt16Array makeUInt16ArrayEmpty() {
//		return UInt16Array.make(new char[] {
//		});
//	}
//	public static UInt16Array makeUInt16Array1() {
//		return UInt16Array.make(new char[] { 1 });
//	}
//	public static UInt16Array makeUInt16Array12345() {
//		return UInt16Array.make(new char[] { 1, 2, 3, 4, 5 });
//	}
//	public static UInt16Array makeUInt16Array12321() {
//		return UInt16Array.make(new char[] { 1, 2, 3, 2, 1 });
//	}

	//////////////////////////////////////////////
	// UInt32
	
	public static UInt32Array makeUInt32ArrayEmpty() {
		return UInt32Array.make(new long[] {
		});
	}
	public static UInt32Array makeUInt32Array1() {
		return UInt32Array.make(new long[] { 1 });
	}
	public static UInt32Array makeUInt32Array12345() {
		return UInt32Array.make(new long[] { 1, 2, 3, 4, 5 });
	}
	public static UInt32Array makeUInt32Array12321() {
		return UInt32Array.make(new long[] { 1, 2, 3, 2, 1 });
	}

	//////////////////////////////////////////////
	// IntegerValue
	
	public static IntegerVarArray makeIntegerVarArrayEmpty() {
		return IntegerVarArray.make(new int[] {
		});
	}
	public static IntegerVarArray makeIntegerVarArray1() {
		return IntegerVarArray.make(new int[] { 1 });
	}
	public static IntegerVarArray makeIntegerVarArray12345() {
		return IntegerVarArray.make(new int[] { 1, 2, 3, 4, 5 });
	}
	public static IntegerVarArray makeIntegerVarArray12321() {
		return IntegerVarArray.make(new int[] { 1, 2, 3, 2, 1 });
	}

	//////////////////////////////////////////////
	// IEEE32
	
	public static IEEE32Array makeIEEE32ArrayEmpty() {
		return IEEE32Array.make(new float[] {});
	}
	public static IEEE32Array makeIEEE32Array1() {
		return IEEE32Array.make(new float[] {1.1f});
	}
	public static IEEE32Array makeIEEE32Array12345() {
		return IEEE32Array.make(new float[] {1.1f, 2.2f, 3.3f, 4.4f, 5.5f});
	}
	public static IEEE32Array makeIEEE32Array12321() {
		return IEEE32Array.make(new float[] {1.1f, 2.2f, 3.3f, 2.2f, 1.1f});
	}

	//////////////////////////////////////////////
	// IEEE64
	
	public static IEEE64Array makeIEEE64ArrayEmpty() {
		return IEEE64Array.make(new double[] {});
	}
	public static IEEE64Array makeIEEE64Array1() {
		return IEEE64Array.make(new double[] {1.1});
	}
	public static IEEE64Array makeIEEE64Array12345() {
		return IEEE64Array.make(new double[] {1.1, 2.2, 3.3, 4.4, 5.5});
	}
	public static IEEE64Array makeIEEE64Array12321() {
		return IEEE64Array.make(new double[] {1.1, 2.2, 3.3, 2.2, 1.1});
	}

	///////////////////////////////////////////////
	// PtrArry
	
	public static PtrArray makePtrArray12345() {
		return PtrArray.make(new Heaper[] {IntegerPos.make(1), IntegerPos.make(2), IntegerPos.make(3), IntegerPos.make(4), IntegerPos.make(5)});
	}

	public static PtrArray makePtrArrayEmpty() {
		return PtrArray.make(0);
	}

	public static PtrArray makePtrArray1() {
		return PtrArray.make(new Heaper[] {IntegerPos.make(1)});
	}

	public static PtrArray makePtrArray12321() {
		return PtrArray.make(new Heaper[] {IntegerPos.make(1), IntegerPos.make(2), IntegerPos.make(3), IntegerPos.make(2), IntegerPos.make(1)});
	}

	public static void assertStringContents(String expected, UInt8Array actualArray) {
		Assert.assertEquals(expected, actualArray.toString());
		
	}

}
