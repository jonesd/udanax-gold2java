package org.abora.gold.testing;
import java.io.PrintWriter;
import java.io.StringWriter;

import junit.framework.TestCase;


public class TestUdanaxGold extends TestCase {

	public TestUdanaxGold() {
		super();
	}

	public TestUdanaxGold(String arg0) {
		super(arg0);
	}

	public void testPlaceholder() {
		// just to keep junit happy while tests are commented out
	}
	
	public void xtestHashSetTester() {
		HashSetTester tester = new HashSetTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}
}
