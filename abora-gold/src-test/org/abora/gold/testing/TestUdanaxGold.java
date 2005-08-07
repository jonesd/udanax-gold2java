package org.abora.gold.testing;
import java.io.IOException;

import org.abora.gold.collection.grand.GrandHashTableTester;
import org.abora.gold.collection.settable.SetTableTester;
import org.abora.gold.cross.CrossTester;
import org.abora.gold.diskman.DiskTester;
import org.abora.gold.java.AboraStartup;
import org.abora.gold.nkernel.VolumeTester;
import org.abora.gold.nkernel.WorksTester;
import org.abora.gold.primtab.PrimIndexTableTester;
import org.abora.gold.primtab.PrimPtrTableTester;
import org.abora.gold.sheph.ShepherdLockTester;
import org.abora.gold.spaces.basic.IDTester;
import org.abora.gold.spaces.basic.RealTester;
import org.abora.gold.spaces.integers.IntegerRegionTester;
import org.abora.gold.tabent.TableEntryTester;
import org.abora.gold.xcvr.ShuffleTester;
import org.abora.gold.xpp.become.BecomeTester;


public class TestUdanaxGold extends UdanaxGoldTestCase {

	public TestUdanaxGold() {
		super();
	}

	public TestUdanaxGold(String arg0) {
		super(arg0);
	}
	
	public void testBecomeTester() throws IOException {
		BecomeTester tester = new BecomeTester();
		assertTester(tester);
	}

	public void testDiskTester() throws Exception {
		//TODO need a full disk initialazion here (see AboraStartup and DiskIniter),
		// but at the moment all the Urdi/SnarfHandle related code is simply placeholder
		AboraStartup.getInstance().useRealDisk();
		try {		
			DiskTester tester = new DiskTester();
			runTester(tester);
		} finally {
			AboraStartup.getInstance().useFakeDisk();
		}
	}

	public void testGrandHashTableTester() throws IOException {
		GrandHashTableTester tester = new GrandHashTableTester();
		assertTester(tester);
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
	
	public void testRegionIDTester() throws IOException {
		IDTester tester = new IDTester();
		assertTester(tester);
	}

	public void testRegionIntegerRegionTester() throws IOException {
		IntegerRegionTester tester = new IntegerRegionTester();
		assertTester(tester);
	}

	public void testRegionRealTester() throws IOException {
		RealTester tester = new RealTester();
		assertTester(tester);
	}


	public void testSetTableTester() throws IOException {
		SetTableTester tester = new SetTableTester();
		assertTester(tester);
	}

	public void testShepherdLockTester() {
		//TODO needs StackExaminer - but that is mostly stubbed out for now
		ShepherdLockTester tester = new ShepherdLockTester();
		runTester(tester);
	}

	public void testShuffleTester() throws IOException {
		ShuffleTester tester = new ShuffleTester();
		assertTester(tester);
	}

	public void testTableEntryTester() throws IOException {
		TableEntryTester tester = new TableEntryTester();
		assertTester(tester);
	}

	public void testVolumeTester() {
		//TODO testcode stubbed out - some disk accessing and type changing?
		VolumeTester tester = new VolumeTester();
		runTester(tester);
	}

	public void testWorksTester() throws Exception {
		//TODO setup Test account to login to?
		WorksTester tester = new WorksTester();
		runTester(tester);
	}

}
