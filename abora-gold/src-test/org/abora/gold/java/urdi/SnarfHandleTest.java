package org.abora.gold.java.urdi;

import org.abora.collection.basic.AssertArrays;
import org.abora.gold.AboraGoldTestCase;
import org.abora.gold.collection.basic.UInt8Array;
import org.abora.gold.java.exception.AboraRuntimeException;


public class SnarfHandleTest extends AboraGoldTestCase {

	public void testMake() {
		UInt8Array array = AssertArrays.makeUInt8Array12345();
		SnarfHandle handle = new SnarfHandle(12, array);
		
		assertNotNull(handle);
		assertEquals(12, handle.getSnarfID());
		assertFalse(handle.isWritable());
	}

	
	public void testGet32() {
		UInt8Array array = UInt8Array.make(12);
		array.storeInt32(0, 987);
		array.storeInt32(4, -6789);
		SnarfHandle handle = new SnarfHandle(12, array);

		assertEquals(987, handle.get32(0));
		assertEquals(-6789, handle.get32(4));
		
		try {
			handle.get32(-1);
			fail();
		} catch (ArrayIndexOutOfBoundsException e) {
			// expected
		}

		try {
			handle.get32(12);
			fail();
		} catch (ArrayIndexOutOfBoundsException e) {
			// expected
		}
}

	public void testGetDataP() {
		UInt8Array array = AssertArrays.makeUInt8Array12345();
		SnarfHandle handle = new SnarfHandle(12, array);
		
		assertSame(array, handle.getDataP());
	}

	public void testGetDataSize() {
		UInt8Array array = AssertArrays.makeUInt8Array12345();
		SnarfHandle handle = new SnarfHandle(12, array);
		
		assertEquals(5, handle.getDataSize());
	}
	
	
	public void testIsWritable() {
		UInt8Array array = AssertArrays.makeUInt8Array12345();
		SnarfHandle handle = new SnarfHandle(12, array);
		
		assertFalse(handle.isWritable());
		
		handle.makeWritable();
		assertTrue(handle.isWritable());

		handle.makeWritable();
		assertTrue(handle.isWritable());
	}

	public void testMakeWritableShouldNotModifyOriginalArray() {
		UInt8Array array = UInt8Array.make(12);
		array.storeInt32(0, 987);
		array.storeInt32(4, -6789);
		SnarfHandle handle = new SnarfHandle(12, array);

		handle.makeWritable();
		
		assertEquals(987, handle.get32(0));
		assertEquals(987, array.int32At(0));
		assertEquals(12, handle.getDataSize());

		handle.put32(0, 12);
		
		assertEquals(12, handle.get32(0));
		assertEquals(987, array.int32At(0));
	}

	public void testPut32() {
		UInt8Array array = UInt8Array.make(12);
		array.storeInt32(0, 987);
		array.storeInt32(4, -6789);
		SnarfHandle handle = new SnarfHandle(12, array);

		try {
			handle.put32(0, 123456);
			fail();
		} catch (AboraRuntimeException e) {
			assertMustBeWritable(e);
		}
		
		handle.makeWritable();
		
		handle.put32(0, -123456);
		handle.put32(4, 963);
		
		assertEquals(-123456, handle.get32(0));
		assertEquals(963, handle.get32(4));
		
		try {
			handle.put32(-1, 12);
			fail();
		} catch (ArrayIndexOutOfBoundsException e) {
			// expected
		}

		try {
			handle.put32(12, 12);
			fail();
		} catch (ArrayIndexOutOfBoundsException e) {
			// expected
		}
	}


	private void assertMustBeWritable(AboraRuntimeException e) {
		assertEquals("Must be writable", e.getMessage());
	}
	
	public void testGetData() {
		UInt8Array array = UInt8Array.make(12);
		array.storeInt32(0, 987);
		array.storeInt32(4, -6789);
		SnarfHandle handle = new SnarfHandle(12, array);

		
		try {
			handle.getData();
			fail();
		} catch (AboraRuntimeException e) {
			assertMustBeWritable(e);
		}
		
		handle.makeWritable();
		
		UInt8Array contents = handle.getData();
		assertNotNull(contents);
		
		// as we became writable, different array
		assertNotSame(array, contents);
		
		// but should have consistent contents on subsequent calls
		UInt8Array contents2 = handle.getData();
		assertSame(contents, contents2);
		
		// directly tied into underlying data structure used by handle
		contents.storeInt32(0, 12345);
		assertEquals(12345, handle.get32(0));
	}
	
	public void testPrintOn() {
		UInt8Array array = UInt8Array.make(12);
		array.storeInt32(0, 987);
		array.storeInt32(4, -6789);
		SnarfHandle handle = new SnarfHandle(12, array);
		
		assertEquals("SnarfHandle(12,Read)", handle.toString());
		
		handle.makeWritable();
		assertEquals("SnarfHandle(12,Write)", handle.toString());
	}

	public void testMoveBytes() {
		UInt8Array array = UInt8Array.make(12);
		array.storeInt32(0, 987);
		array.storeInt32(4, -6789);
		SnarfHandle handle = new SnarfHandle(12, array);
		
		try {
			handle.moveBytes(0, 4, 8);
			fail();
		} catch (AboraRuntimeException e) {
			assertMustBeWritable(e);
		}
		
		handle.makeWritable();
		
		// count == 0 - Do nothing
		handle.moveBytes(0, 4, 0);
		assertContents(handle, 987, -6789, 0);
		
		// Source == Destination - do nothing
		handle.moveBytes(4, 4, 8);
		assertContents(handle, 987, -6789, 0);
		
		handle.moveBytes(0, 4, 8);
		assertContents(handle, 0, 987, -6789);
		
		handle.moveBytes(4, 0, 8);
		assertContents(handle, 987, -6789, 0);
		
		//TODO test out of boundaries
	}


	private void assertContents(SnarfHandle handle, int i0, int i4, int i8) {
		assertEquals(i0, handle.get32(0));
		assertEquals(i4, handle.get32(4));
		assertEquals(i8, handle.get32(8));
		
	}

}
