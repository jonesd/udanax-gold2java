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
package info.dgjones.abora.gold.java.urdi;

import java.io.PrintWriter;

import info.dgjones.abora.gold.collection.basic.UInt8Array;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class SnarfHandle extends Heaper {

	private final int snarfId;
	private boolean writable = false;
	private UInt8Array array = null;
	
	public SnarfHandle(int snarfId, UInt8Array array) {
		super();
		
		this.snarfId = snarfId;
		this.array = array;
	}
	
	public int get32(int bytePosition) {
		return array.int32At(bytePosition);
	}
	
	public UInt8Array getDataP() {
		//TODO should this be aliased on our internal data, or a clone?
//		mustBeWritable();
		return array;
	}

	private void mustBeWritable() {
		if (!isWritable()) {
			throw new AboraRuntimeException("Must be writable");
		}
	}
	
	public int getSnarfID() {
		return snarfId;
	}
	
	public int getDataSize() {
		return array.count();
	}
	
	public void makeWritable() {
		if (isWritable()) {
			return;
		}
		array = (UInt8Array)array.copy();
		writable = true;
	}
	
	public void moveBytes(int source, int destination, int count) {
		//TODO implementation/arguments/efficiency?
		mustBeWritable();
		if (source == destination) {
			//TODO does this indicate a problem in the caller?
			return;
		}
		//TODO use optimized system.arraycopy?
//		array.copyBytes(source, destination, count);
		if (source > destination) {
			for (int i = 0; i < count ; i++) {
				int value = array.at(source+i);
				//TODO clear old?
				array.put(source+i, 0);
				array.put(destination+i, value);
			}
		} else {
			for (int i = count - 1; i >= 0; i--) {
				int value = array.at(source+i);
				//TODO clear old?
				array.put(source+i, 0);
				array.put(destination+i, value);
			}
		}
	}
	
	public void put32(int bytePosition, int value) {
		mustBeWritable();
		array.storeInt32(bytePosition, value);
	}
	
	public boolean isWritable() {
		return writable;
	}

	public void printOn(PrintWriter oo) {
		oo.print(getAboraClass().name());
		oo.print("(");
		oo.print(snarfId);
		oo.print(",");
		oo.print(isWritable() ? "Write" : "Read");
		oo.print(")");
	}

	public UInt8Array getData() {
			//TODO should this be aliased on our internal data, or a clone?
			mustBeWritable();
			return getDataP();
		}
}
