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
package info.dgjones.abora.gold.xcvr;

import info.dgjones.abora.collection.basic.AssertArrays;
import info.dgjones.abora.gold.AboraGoldTestCase;
import info.dgjones.abora.gold.cobbler.Cookbook;
import info.dgjones.abora.gold.collection.basic.UInt8Array;

public class TextyXmtrTest extends AboraGoldTestCase {

	private Cookbook book;
	private TransferSpecialist specialist;
	private UInt8Array array;
	private XnWriteStream stream;
	private TextyXmtr xmtr;

	protected void setUp() throws Exception {
		super.setUp();

		book = null;
		specialist = TransferGeneralist.make(book);
		array = UInt8Array.make(100);
		stream = XnWriteStream.make(array);
		xmtr = (TextyXmtr) TextyXmtr.make(specialist, stream);
	}

	public void testSendBooleanVarTrue() {
		xmtr.sendBooleanVar(true);
		AssertArrays.assertStringContents("1;", array);
	}

	public void testSendBooleanVarFalse() {
		xmtr.sendBooleanVar(false);
		AssertArrays.assertStringContents("0;", array);
	}

	//TODO finish off tests
}
