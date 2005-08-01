package org.abora.gold.testing;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringWriter;

import org.abora.gold.AboraGoldTestCase;
import org.abora.gold.java.AboraSupport;


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
			inputStream = new FileInputStream("src-test/"+filename);
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
		String filename = this.getClass().getName();
		filename = filename.substring(0, filename.lastIndexOf('.')+1);
		String testerName = tester.getClass().getName();
		filename += testerName.substring(testerName.lastIndexOf('.')+1);
		filename = filename.replace('.', File.separatorChar) + ".trace.txt";
		return filename;
	}

}
