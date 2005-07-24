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
import org.abora.gold.be.canopy.CanopyCache;
import org.abora.gold.cobbler.BootMaker;
import org.abora.gold.cobbler.Connection;
import org.abora.gold.collection.grand.GrandHashTableTester;
import org.abora.gold.collection.sets.MuSet;
import org.abora.gold.collection.settable.SetTableTester;
import org.abora.gold.cross.CrossTester;
import org.abora.gold.diskman.DiskTester;
import org.abora.gold.fbtest.BackendBootMaker;
import org.abora.gold.fbtest.WorksBootMaker;
import org.abora.gold.java.AboraHeaper;
import org.abora.gold.negoti8.ProtocolBroker;
import org.abora.gold.nkernel.VolumeTester;
import org.abora.gold.nkernel.WorksTester;
import org.abora.gold.primtab.PrimIndexTableTester;
import org.abora.gold.primtab.PrimPtrTableTester;
import org.abora.gold.sheph.ShepherdLockTester;
import org.abora.gold.snarf.DiskManager;
import org.abora.gold.snarf.FakePacker;
import org.abora.gold.snarf.MockTurtle;
import org.abora.gold.spaces.basic.FilterTester;
import org.abora.gold.spaces.basic.IDTester;
import org.abora.gold.spaces.basic.RealTester;
import org.abora.gold.spaces.basic.SequenceTester;
import org.abora.gold.spaces.integers.IntegerRegionTester;
import org.abora.gold.tabent.TableEntryTester;
import org.abora.gold.xcvr.Binary2XcvrMaker;
import org.abora.gold.xcvr.BogusXcvrMaker;
import org.abora.gold.xcvr.ShuffleTester;
import org.abora.gold.xcvr.TextyXcvrMaker;
import org.abora.gold.xcvr.XcvrMaker;
import org.abora.gold.xpp.become.BecomeTester;


public class TestUdanaxGold extends AboraGoldTestCase {

	public TestUdanaxGold() {
		super();
	}

	public TestUdanaxGold(String arg0) {
		super(arg0);
	}
	
	protected String runTester(Tester tester) {
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
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
	
	public void testBecomeTester() throws IOException {
		BecomeTester tester = new BecomeTester();
		assertTester(tester);
	}

	public void xtestDiskTester() {
		DiskTester tester = new DiskTester();
		runTester(tester);
	}

	public void xtestGrandHashTableTester() {
		GrandHashTableTester tester = new GrandHashTableTester();
		runTester(tester);
	}

	public void testHashSetTester() throws IOException {
		HashSetTester tester = new HashSetTester();
		assertTester(tester);
	}
	
	public void testHelloTester() throws IOException {
		HelloTester tester = new HelloTester();
		assertTester(tester);
	}
	
	public void testHashTableTester() throws IOException {
		HashTableTester tester = new HashTableTester();
		assertTester(tester);
	}
	
	public void testImmuSetTester() throws IOException {
		ImmuSetTester tester = new ImmuSetTester();
		assertTester(tester);
	}

	public void testIntegerTableTester() throws IOException {
		IntegerTableTester tester = new IntegerTableTester();
		assertTester(tester);
	}

	public void testPrimIndexTableTester() throws IOException {
		PrimIndexTableTester tester = new PrimIndexTableTester();
		assertTester(tester);
	}

	public void testPrimPtrTableTester() throws IOException {
		PrimPtrTableTester tester = new PrimPtrTableTester();
		assertTester(tester);
	}

	public void testRegionCrossTester() throws IOException {
		CrossTester tester = new CrossTester();
		assertTester(tester);
	}

	public void xtestRegionFilterTester() {
		FilterTester tester = new FilterTester();
		runTester(tester);
	}

	public void xtestRegionIDTester() {
		IDTester tester = new IDTester();
		runTester(tester);
	}

	public void testRegionIntegerRegionTester() throws IOException {
		IntegerRegionTester tester = new IntegerRegionTester();
		assertTester(tester);
	}

	public void testRegionRealTester() throws IOException {
		RealTester tester = new RealTester();
		assertTester(tester);
	}

	public void xtestRegionSequenceTester() {
		//TODO infinite loop?
		SequenceTester tester = new SequenceTester();
		runTester(tester);
	}

	public void xtestSetTableTester() {
		SetTableTester tester = new SetTableTester();
		runTester(tester);
	}

	public void xtestShepherdLockTester() {
		ShepherdLockTester tester = new ShepherdLockTester();
		runTester(tester);
	}

	public void xtestShuffleTester() {
		ShuffleTester tester = new ShuffleTester();
		runTester(tester);
	}

	public void testTableEntryTester() throws IOException {
		TableEntryTester tester = new TableEntryTester();
		assertTester(tester);
	}

	public void xtestVolumeTester() {
		VolumeTester tester = new VolumeTester();
		runTester(tester);
	}

	public void xtestWorksTester() throws Exception {
		WorksTester tester = new WorksTester();
		runTester(tester);
	}

}
