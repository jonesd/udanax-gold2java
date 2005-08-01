package org.abora.gold.testing;
import java.io.IOException;

import org.abora.gold.spaces.basic.FilterTester;
import org.abora.gold.spaces.basic.SequenceTester;
import org.abora.gold.testing.UdanaxGoldTestCase;


public class TestUdanaxGoldIntegration extends UdanaxGoldTestCase {

	public TestUdanaxGoldIntegration() {
		super();
	}

	public TestUdanaxGoldIntegration(String name) {
		super(name);
	}

	public void testRegionSequenceTester() throws IOException {
		//WARNING Takes 15s to run on Mac 2x2.0GHz
		SequenceTester tester = new SequenceTester();
		assertTester(tester);
	}
	
	public void testRegionFilterTester() throws IOException {
		//WARNING Takes ~80s to run on Mac 2x2.0GHz
		//TODO tonnes of warnings here, and order of results seems to change, breaking
		// the whole comparison mechanism - what is wrong?
		FilterTester tester = new FilterTester();
		runTester(tester);
//TODO		assertTester(tester);
	}

}
