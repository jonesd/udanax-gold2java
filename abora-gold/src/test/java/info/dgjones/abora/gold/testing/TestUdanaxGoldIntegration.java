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
package info.dgjones.abora.gold.testing;
import java.io.IOException;

import info.dgjones.abora.gold.spaces.basic.FilterTester;
import info.dgjones.abora.gold.spaces.basic.SequenceTester;
import info.dgjones.abora.gold.testing.UdanaxGoldTestCase;


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
