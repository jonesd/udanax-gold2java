package org.abora.gold.testing;
import java.io.PrintWriter;
import java.io.StringWriter;

import junit.framework.TestCase;

import org.abora.gold.collection.grand.GrandHashTableTester;
import org.abora.gold.collection.settable.SetTableTester;
import org.abora.gold.cross.CrossTester;
import org.abora.gold.diskman.DiskTester;
import org.abora.gold.nkernel.VolumeTester;
import org.abora.gold.nkernel.WorksTester;
import org.abora.gold.primtab.PrimIndexTableTester;
import org.abora.gold.primtab.PrimPtrTableTester;
import org.abora.gold.sheph.ShepherdLockTester;
import org.abora.gold.spaces.basic.FilterTester;
import org.abora.gold.spaces.basic.IDTester;
import org.abora.gold.spaces.basic.RealTester;
import org.abora.gold.spaces.basic.SequenceTester;
import org.abora.gold.spaces.integers.IntegerRegionTester;
import org.abora.gold.tabent.TableEntryTester;
import org.abora.gold.xcvr.ShuffleTester;
import org.abora.gold.xpp.become.BecomeTester;


public class TestUdanaxGold extends TestCase {

	public TestUdanaxGold() {
		super();
	}

	public TestUdanaxGold(String arg0) {
		super(arg0);
	}

	public void testBecomeTester() {
		BecomeTester tester = new BecomeTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}

	public void xtestDiskTester() {
		DiskTester tester = new DiskTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}

	public void xtestGrandHashTableTester() {
		GrandHashTableTester tester = new GrandHashTableTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}

	public void testHashSetTester() {
		HashSetTester tester = new HashSetTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}
	
	public void testHelloTester() {
		HelloTester tester = new HelloTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}
	
	public void testHashTableTester() {
		HashTableTester tester = new HashTableTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}
	
	public void testImmuSetTester() {
		ImmuSetTester tester = new ImmuSetTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}

	public void testIntegerTableTester() {
		IntegerTableTester tester = new IntegerTableTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}

	public void testPrimIndexTableTester() {
		PrimIndexTableTester tester = new PrimIndexTableTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}

	public void testPrimPtrTableTester() {
		PrimPtrTableTester tester = new PrimPtrTableTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}

	public void xtestRegionCrossTester() {
		CrossTester tester = new CrossTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}

	public void xtestRegionFilterTester() {
		FilterTester tester = new FilterTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}

	public void xtestRegionIDTester() {
		IDTester tester = new IDTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}

	public void testRegionIntegerRegionTester() {
		IntegerRegionTester tester = new IntegerRegionTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}

	public void xtestRegionRealTester() {
		RealTester tester = new RealTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}

	public void xtestRegionSequenceTester() {
		SequenceTester tester = new SequenceTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}

	public void xtestSetTableTester() {
		SetTableTester tester = new SetTableTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}

	public void xtestShepherdLockTester() {
		ShepherdLockTester tester = new ShepherdLockTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}

	public void xtestShuffleTester() {
		ShuffleTester tester = new ShuffleTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}

	public void testTableEntryTester() {
		TableEntryTester tester = new TableEntryTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}

	public void xtestVolumeTester() {
		VolumeTester tester = new VolumeTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}

	public void xtestWorksTester() {
		WorksTester tester = new WorksTester();
		StringWriter stringWriter = new StringWriter();
		PrintWriter oo = new PrintWriter(stringWriter);
		tester.allTestsOn(oo);
	}
}
