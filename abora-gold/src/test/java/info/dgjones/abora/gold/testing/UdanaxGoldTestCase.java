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

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringWriter;

import info.dgjones.abora.gold.AboraGoldTestCase;
import info.dgjones.abora.gold.java.AboraSupport;


public abstract class UdanaxGoldTestCase extends AboraGoldTestCase {

	public UdanaxGoldTestCase() {
		super();
	}
	
	public UdanaxGoldTestCase(String name) {
		super(name);
	}

	protected String runTester(Tester tester) {
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		//TODO this kind of replace the general system logger with our test logger
		// collector is really duplicating some Tester functionality.
		PrintWriter previousAboraLogger = AboraSupport.logger;
		try {
			previousAboraLogger = oo;
			tester.allTestsOn(oo);
		} finally {
			AboraSupport.logger = previousAboraLogger;
		}
		oo.flush();
		return stringWriter.toString();
	}

	protected void assertTester(Tester tester) throws IOException {
		String actual = runTester(tester);
		String expected = loadExpected(tester);
		assertEquals(expected, actual);
	}

	protected String loadExpected(Tester tester) throws IOException {
		String filename = expectedFilename(tester);
		InputStream inputStream = ClassLoader.getSystemResourceAsStream(filename);
		if (inputStream == null) {
			//TODO hack
			inputStream = new FileInputStream("src/test/resources/"+filename);
		}
		assertNotNull("Found trace file named: "+filename, inputStream);
		
		try {
			String expected = readInputStream(inputStream);
			return expected;
		} finally {
			inputStream.close();
		}
	}

	private String readInputStream(InputStream inputStream) throws IOException {
		StringBuffer expectedBuffer = new StringBuffer();
		Reader reader = new InputStreamReader(inputStream);
		try {
			char[] bytes = new char[1024];
			int read = -1;
			while ((read = reader.read(bytes)) != -1) {
				expectedBuffer.append(bytes, 0, read);
			}
		} finally {
			reader.close();
		}
		String expected = expectedBuffer.toString();
		return expected;
	}

	private String expectedFilename(Tester tester) {
		String filename = "traces"+File.separator;
		String testerName = tester.getClass().getName();
		filename += testerName.substring(testerName.lastIndexOf('.')+1);
		filename += ".trace.log";
		return filename;
	}

}
