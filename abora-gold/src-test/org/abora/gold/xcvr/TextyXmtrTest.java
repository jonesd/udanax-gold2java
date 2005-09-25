package org.abora.gold.xcvr;

import org.abora.collection.basic.AssertArrays;
import org.abora.gold.AboraGoldTestCase;
import org.abora.gold.cobbler.Cookbook;
import org.abora.gold.collection.basic.UInt8Array;

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
