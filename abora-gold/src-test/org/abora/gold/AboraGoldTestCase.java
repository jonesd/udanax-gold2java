package org.abora.gold;

import org.abora.gold.java.AboraStartup;

import junit.framework.TestCase;


public class AboraGoldTestCase extends TestCase {

	public AboraGoldTestCase() {
		super();
	}

	public AboraGoldTestCase(String name) {
		super(name);
	}

	protected void setUp() throws Exception {
		super.setUp();
		
		AboraStartup.startUp();
	}
	

}
