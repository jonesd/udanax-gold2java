package org.abora.gold.collection.tables.test;

import junit.framework.Test;
import junit.framework.TestSuite;



public class AllTests extends PairTest {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for org.abora.gold.collection.tables.test");
		//$JUnit-BEGIN$
		suite.addTestSuite(PairTest.class);
		//$JUnit-END$
		return suite;
	}
}
